//> using scala "3.6.3"
//> using dep "tech.neander::langoustine-app::0.0.22"
//> using dep "io.chrisdavenport::crossplatformioapp::0.1.0"
//> using dep "co.fs2::fs2-io::3.11.0"
//> using dep "org.typelevel::cats-effect::3.5.7"
//> using dep "org.typelevel::cats-parse::1.1.0"
//> using dep "org.typelevel::cats-mtl::1.5.0"
//> using dep "io.circe::circe-core::0.14.10"
//> using dep "org.http4s::http4s-ember-server::0.23.30"
//> using dep "org.http4s::http4s-dsl::0.23.30"
//> using dep "org.http4s::http4s-circe::0.23.30"
//> using options "-Wunused:all", "-Ykind-projector:underscores", "-Wnonunit-statement", "-Wvalue-discard"
package badlang

import cats.data.OptionT
import cats.effect.IO
import cats.effect.kernel.Resource
import cats.implicits.*
import fs2.io.file.Files
import fs2.io.file.Path
import io.chrisdavenport.crossplatformioapp.CrossPlatformIOApp
import jsonrpclib.fs2.given
import langoustine.lsp.LSPBuilder
import langoustine.lsp.aliases.Definition
import langoustine.lsp.aliases.TextDocumentContentChangeEvent
import langoustine.lsp.app.LangoustineApp
import langoustine.lsp.enumerations.MessageType
import langoustine.lsp.enumerations.SymbolKind
import langoustine.lsp.enumerations.TextDocumentSyncKind
import langoustine.lsp.requests.initialize
import langoustine.lsp.requests.initialized
import langoustine.lsp.requests.textDocument
import langoustine.lsp.requests.window
import langoustine.lsp.runtime.DocumentUri
import langoustine.lsp.runtime.Opt
import langoustine.lsp.structures.DiagnosticOptions
import langoustine.lsp.structures.DocumentSymbol
import langoustine.lsp.structures.InitializeResult
import langoustine.lsp.structures.InlayHint
import langoustine.lsp.structures.Location
import langoustine.lsp.structures.RelatedFullDocumentDiagnosticReport
import langoustine.lsp.structures.ServerCapabilities
import langoustine.lsp.structures.ShowMessageParams
import langoustine.lsp.structures.TextEdit
import langoustine.lsp.structures.WorkspaceEdit
import parser.*

import java.nio.file.NoSuchFileException

object Server {

  def make(
    cache: DocumentCache[DocumentUri],
    docs: TextDocuments,
  ): LSPBuilder[IO] =
    LSPBuilder
      .create[IO]
      .handleRequest(initialize) { in =>
        IO(
          InitializeResult(capabilities =
            ServerCapabilities(
              textDocumentSync = Opt(TextDocumentSyncKind.Full),
              diagnosticProvider = Opt(
                DiagnosticOptions(
                  interFileDependencies = false,
                  workspaceDiagnostics = false,
                )
              ),
              definitionProvider = Opt(true),
              referencesProvider = Opt(true),
              renameProvider = Opt(true),
              inlayHintProvider = Opt(true),
              documentSymbolProvider = Opt(true),
            )
          )
        )
      }
      .handleNotification(textDocument.didChange) {
        (
          in,
          _,
        ) =>
          in.contentChanges
            .takeRight(1)
            .map {
              // ranged change, shouldn't happen as we accept full diffs only
              case TextDocumentContentChangeEvent.S0(_, _, text) => text
              // full change
              case TextDocumentContentChangeEvent.S1(text) => text
            }
            .traverse_(cache.set(in.textDocument.uri, _))
      }
      .handleNotification(textDocument.didOpen) { in =>
        cache.set(in.params.textDocument.uri, in.params.textDocument.text)
      }
      .handleNotification(textDocument.didClose) { in =>
        cache.remove(in.params.textDocument.uri)
      }
      .handleNotification(initialized)(
        _.toClient
          .notification(
            window.showMessage,
            ShowMessageParams(MessageType.Info, "hello from badlang server!"),
          )
      )
      .handleRequest(textDocument.rename) { in =>
        import analysis.*
        import parser.*
        OptionT(docs.getParsed(in.params.textDocument.uri))
          .subflatMap { file =>
            file
              .findNameAt(in.params.position.toModel)
              .map(_.value)
              .map { name =>
                val edits =
                  file
                    .names
                    .filter(_.value == name)
                    .map { n =>
                      TextEdit(n.range.toLSP, in.params.newName)
                    }
                    .toVector

                WorkspaceEdit(
                  changes = Opt(
                    Map(in.params.textDocument.uri -> edits)
                  )
                )

              }
          }
          .value
          .map(_.toOpt)
      }
      .handleRequest(textDocument.references) { in =>
        docs
          .getParsed(in.params.textDocument.uri)
          .nested
          .map { file =>

            import analysis.*
            import parser.*

            file
              .findDefinitionAt(in.params.position.toModel)
              .toList
              .flatMap(sym => file.findReferences(sym.value))
          }
          .map {
            _.map { sym =>
              Location(in.params.textDocument.uri, sym.range.toLSP)
            }.toVector
          }
          .value
          .map(_.toOpt)
      }
      .handleRequest(textDocument.definition) { in =>
        OptionT(docs.getParsed(in.params.textDocument.uri))
          .subflatMap { file =>
            import analysis.*
            import parser.*

            file
              .findReferenceAt(in.params.position.toModel)
              .flatMap(sym => file.findDefinition(sym.value))
          }
          .map { sym =>
            Definition(Location(in.params.textDocument.uri, sym.range.toLSP))
          }
          .value
          .map(_.toOpt)
      }
      .handleRequest(textDocument.inlayHint) { in =>

        import analysis.*
        import parser.*
        OptionT(docs.getParsed(in.params.textDocument.uri))
          .subflatMap(v => v.typecheck.as(v).toOption)
          .map { file =>
            file.execute.toVector.map { line =>
              InlayHint(
                position = line.opRange.end.toLSP,
                label = " // " + line.text,
                paddingLeft = Opt(true),
              )
            }
          }
          .value
          .map(_.toOpt)
      }
      .handleRequest(textDocument.documentSymbol) { in =>
        docs
          .getParsed(in.params.textDocument.uri)
          .nested
          .map { file =>
            import parser.*

            file
              .ops
              .value
              .mapFilter { op =>
                op.value match {
                  case Op.Let(name, value) =>
                    DocumentSymbol(
                      name = name.value.value,
                      kind = SymbolKind.Variable,
                      range = op.range.toLSP,
                      selectionRange = name.range.toLSP,
                    ).some
                  case _ => None
                }
              }
          }
          .value
          .map(_.map(_.toVector).toOpt)
      }
      .handleRequest(textDocument.diagnostic) { in =>
        diagnostics(in.params, docs)
      }

  import parser.T

  extension (
    docs: TextDocuments
  )

    def getParsed(
      uri: DocumentUri
    ): IO[Option[SourceFile[T]]] = docs.get(uri).map(td => parser.parse(td.content).toOption)

  extension [A](
    option: Option[A]
  ) def toOpt: Opt[A] = option.fold(Opt.empty)(Opt(_))

}

object main extends CrossPlatformIOApp with LangoustineApp {

  // Workaround for vscode not killing the process in time
  val killEmAll = {
    val killOthers = Files[IO]
      .readUtf8(Path("badlang.pid"))
      .compile
      .string
      .map(_.trim.toLong)
      .attemptNarrow[NoSuchFileException]
      .flatMap {
        _.traverse_ { pid =>
          fs2
            .io
            .process
            .ProcessBuilder("kill", pid.toString)
            .spawn[IO]
            .use(_.exitValue)
            .void
        }
      }

    killOthers *> fs2
      .Stream
      .emit(getpid().toString)
      .through(fs2.text.utf8.encode[IO])
      .through(Files[IO].writeAll(Path("badlang.pid")))
      .compile
      .drain
  }

  override def server(
    args: List[String]
  ): Resource[IO, LSPBuilder[IO]] =
    killEmAll.toResource *> DocumentCache
      .make[DocumentUri]
      .toResource
      .flatMap { cache =>
        val docs = TextDocuments.cached(cache)

        Api
          .run(cache, docs)
          .as(Server.make(cache, docs))
      }

}
