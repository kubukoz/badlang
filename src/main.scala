//> using scala "3.3.0-RC5"
//> using lib "tech.neander::langoustine-app::0.0.20"
//> using lib "org.http4s::http4s-ember-server::0.23.19-RC3"
//> using lib "org.http4s::http4s-circe::0.23.19-RC3"
//> using lib "org.http4s::http4s-dsl::0.23.19-RC3"
//> using lib "co.fs2::fs2-io::3.7.0-RC5"
//> using lib "io.circe::circe-generic:0.14.5"
//> using lib "io.chrisdavenport::crossplatformioapp::0.1.0"
//> using lib "org.typelevel::cats-parse::0.3.9"
//> using option "-Wunused:all"
package badlang

import cats.data.OptionT
import cats.effect.IO
import cats.effect.implicits._
import cats.effect.kernel.Resource
import cats.implicits._
import cats.parse.Caret
import cats.parse.LocationMap
import fs2.io.file.Files
import fs2.io.file.Path
import io.chrisdavenport.crossplatformioapp.CrossPlatformIOApp
import jsonrpclib.fs2.given
import langoustine.lsp.LSPBuilder
import langoustine.lsp.aliases.Definition
import langoustine.lsp.aliases.DocumentDiagnosticReport
import langoustine.lsp.aliases.TextDocumentContentChangeEvent
import langoustine.lsp.app.LangoustineApp
import langoustine.lsp.enumerations.DiagnosticSeverity
import langoustine.lsp.enumerations.MessageType
import langoustine.lsp.enumerations.TextDocumentSyncKind
import langoustine.lsp.requests.initialize
import langoustine.lsp.requests.initialized
import langoustine.lsp.requests.textDocument
import langoustine.lsp.requests.window
import langoustine.lsp.runtime.DocumentUri
import langoustine.lsp.runtime.Opt
import langoustine.lsp.structures.Diagnostic
import langoustine.lsp.structures.DiagnosticOptions
import langoustine.lsp.structures.InitializeResult
import langoustine.lsp.structures.Location
import langoustine.lsp.structures.Position
import langoustine.lsp.structures.RelatedFullDocumentDiagnosticReport
import langoustine.lsp.structures.ServerCapabilities
import langoustine.lsp.structures.ShowMessageParams

import java.nio.file.NoSuchFileException

case class Document(
  content: String,
  cached: Boolean,
)

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
      .handleNotification(textDocument.didClose) { in =>
        cache.remove(in.params.textDocument.uri)
      }
      .handleNotification(textDocument.didSave) { in =>
        cache.remove(in.params.textDocument.uri)
      }
      .handleNotification(initialized)(
        _.toClient
          .notification(
            window.showMessage,
            ShowMessageParams(MessageType.Info, "hello from badlang server!"),
          )
      )
      .handleRequest(textDocument.definition) { in =>
        OptionT(docs.getParsed(in.params.textDocument.uri))
          .subflatMap { file =>

            import parser.toModel

            val useSiteSymbol = file.ops.value.map(_.value).collectFirstSome {
              case Op.Inc(name) if name.range.contains(in.params.position.toModel) => name.some
              case Op.Show(names) => names.value.find(_.range.contains(in.params.position.toModel))
              case (_: Op.Let[_]) | (_: Op.Inc[_]) => None
            }

            def declarationSite(
              of: Name
            ) = file.ops.value.map(_.value).collectFirst {
              case Op.Let(name, _) if name.value == of => name
            }

            useSiteSymbol.flatMap(sym => declarationSite(sym.value))
          }
          .map { sym =>
            Definition(Location(in.params.textDocument.uri, sym.range.toLSP))
          }
          .value
          .map(_.toOpt)
      }
      .handleRequest(textDocument.diagnostic) { in =>
        docs.get(in.params.textDocument.uri).map(_.content).map { fileText =>
          val map = LocationMap(fileText)
          val lastOffset = map.toCaretUnsafe(fileText.length)
          extension (
            c: Caret
          ) def toLSPPosition: Position = Position(c.line, c.col)

          val items =
            parser.parse(fileText) match {
              case Right(_) => Vector.empty
              case Left((msg, offset)) =>
                Vector(
                  Diagnostic(
                    range = langoustine
                      .lsp
                      .structures
                      .Range(
                        map.toCaretUnsafe(offset).toLSPPosition,
                        lastOffset.toLSPPosition,
                      ),
                    severity = Opt(DiagnosticSeverity.Error),
                    message = "Parsing error: expected " + msg,
                  )
                )
            }

          DocumentDiagnosticReport(
            RelatedFullDocumentDiagnosticReport(
              kind = "full",
              items = items,
            )
          )
        }
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
