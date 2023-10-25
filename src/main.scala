//> using scala "3.3.1"
//> using lib "tech.neander::langoustine-app::0.0.21"
//> using lib "org.http4s::http4s-ember-server::0.23.23"
//> using lib "org.http4s::http4s-circe::0.23.23"
//> using lib "org.http4s::http4s-dsl::0.23.23"
//> using lib "co.fs2::fs2-io::3.9.2"
//> using lib "io.circe::circe-generic::0.14.6"
//> using lib "io.chrisdavenport::crossplatformioapp::0.1.0"
//> using lib "org.typelevel::cats-parse::0.3.10"
//> using lib "org.typelevel::cats-mtl::1.4.0"
//> using lib "com.lihaoyi::pprint::0.8.1"
//> using options "-Wunused:all", "-Ykind-projector:underscores", "-Wnonunit-statement", "-Wvalue-discard"
package badlang

import cats.data.OptionT
import cats.effect.IO
import cats.effect.kernel.Resource
import cats.implicits.*
import cats.parse.Caret
import cats.parse.LocationMap
import io.chrisdavenport.crossplatformioapp.CrossPlatformIOApp
import jsonrpclib.fs2.given
import langoustine.lsp.Communicate
import langoustine.lsp.Invocation
import langoustine.lsp.LSPBuilder
import langoustine.lsp.aliases.Definition
import langoustine.lsp.aliases.DocumentDiagnosticReport
import langoustine.lsp.aliases.TextDocumentContentChangeEvent
import langoustine.lsp.app.LangoustineApp
import langoustine.lsp.enumerations.DiagnosticSeverity
import langoustine.lsp.enumerations.InlayHintKind
import langoustine.lsp.enumerations.MessageType
import langoustine.lsp.enumerations.SymbolKind
import langoustine.lsp.enumerations.TextDocumentSyncKind
import langoustine.lsp.requests.LSPNotification
import langoustine.lsp.requests.LSPRequest
import langoustine.lsp.requests.initialize
import langoustine.lsp.requests.initialized
import langoustine.lsp.requests.textDocument
import langoustine.lsp.requests.window
import langoustine.lsp.runtime.DocumentUri
import langoustine.lsp.runtime.Opt
import langoustine.lsp.structures.Diagnostic
import langoustine.lsp.structures.DiagnosticOptions
import langoustine.lsp.structures.DocumentSymbol
import langoustine.lsp.structures.InitializeResult
import langoustine.lsp.structures.InlayHint
import langoustine.lsp.structures.Location
import langoustine.lsp.structures.Position
import langoustine.lsp.structures.RelatedFullDocumentDiagnosticReport
import langoustine.lsp.structures.ServerCapabilities
import langoustine.lsp.structures.ShowMessageParams
import langoustine.lsp.structures.TextEdit
import langoustine.lsp.structures.WorkspaceEdit

case class Document(
  content: String,
  cached: Boolean,
)

object Server {

  def make(
    cache: DocumentCache[DocumentUri],
    docs: TextDocuments,
  ): LSPBuilder[IO] =
    /* logged */
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
        docs.get(in.params.textDocument.uri).map(_.content).map { fileText =>
          val map = LocationMap(fileText)
          val lastOffset = map.toCaretUnsafe(fileText.length)
          extension (
            c: Caret
          ) def toLSPPosition: Position = Position(c.line, c.col)

          import analysis.*
          import parser.*

          def withParsed(
            parsed: SourceFile[parser.T]
          ) = {
            val lints = parsed.lint.fold(_.toList, _ => Nil)

            parsed
              .typecheck
              .match {
                case Right(_)     => Vector.empty
                case Left(errors) => errors.toList.toVector
              }
              .concat(lints)
              .map { diag =>
                Diagnostic(
                  range = diag.range.toLSP,
                  severity = Opt(diag.level match {
                    case badlang.DiagnosticLevel.Error   => DiagnosticSeverity.Error
                    case badlang.DiagnosticLevel.Warning => DiagnosticSeverity.Warning
                  }),
                  message = diag.issue.message,
                )
              }
          }
          val items =
            parser.parse(fileText) match {
              case Right(parsed) => withParsed(parsed)

              case Left((msg, offset)) =>
                val parseError = Vector(
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

                // minimal-effort attempt: parse all previous lines and get some diagnostics from that.
                // technically we could try and parse each line separately anyway (and probably should), but this is a start.
                val parsedEarlier = {
                  val lineFailed = map.toCaretUnsafe(offset)
                  val lineStart = map.toOffset(lineFailed.line, 0).getOrElse(sys.error("no offset"))
                  parser.parse(fileText.take(lineStart))
                }

                val extraDiags = parsedEarlier.fold(_ => Nil, withParsed)

                parseError ++ extraDiags
            }

          DocumentDiagnosticReport(
            RelatedFullDocumentDiagnosticReport(
              kind = "full",
              items = items,
            )
          )
        }
      }

  private def logged(
    b: LSPBuilder[IO]
  ): LSPBuilder[IO] =
    new LSPBuilder[IO] {
      export b.{handleNotification => _, handleRequest => _, *}

      override def handleRequest[X <: LSPRequest](
        t: X
      )(
        f: Invocation[t.In, IO] => IO[t.Out]
      ): LSPBuilder[IO] = logged {
        b.handleRequest(t) { inv =>
          IO.consoleForIO
            .errorln("handling " + t.requestMethod + " with input: " + pprint(inv.params)) *>
            f(inv)
              .onError { case e =>
                IO.consoleForIO
                  .printStackTrace(new Throwable("failed invocation of " + t.requestMethod, e))
              }
              .flatTap { out =>
                IO.consoleForIO.errorln("handled " + t.requestMethod + s": ${pprint(out)}")
              }
        }
      }

      override def handleNotification[X <: LSPNotification](
        t: X
      )(
        f: Invocation[t.In, IO] => IO[Unit]
      ): LSPBuilder[IO] = logged {
        b.handleNotification(t) { inv =>
          IO.consoleForIO
            .println("handling " + t.notificationMethod + " with input: " + pprint(inv.params)) *>
            f(inv)
              .onError { case e =>
                IO.consoleForIO
                  .printStackTrace(new Throwable("failed invocation of " + t.notificationMethod, e))
              }
              .flatTap { out =>
                IO.consoleForIO
                  .println("handled " + t.notificationMethod + s": ${pprint(out)}")
              }
        }
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

  override def server(
    args: List[String]
  ): Resource[IO, LSPBuilder[IO]] = DocumentCache
    .make[DocumentUri]
    .toResource
    .flatMap { cache =>
      val docs = TextDocuments.cached(cache)

      // Api
      //   .run(cache, docs)
      Resource
        .unit
        .as {
          Server.make(cache, docs)
        }
    }

}
