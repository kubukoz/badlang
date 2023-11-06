//> using scala "3.3.0"
//> using lib "tech.neander::langoustine-app::0.0.21"
//> using lib "io.chrisdavenport::crossplatformioapp::0.1.0"
//> using lib "co.fs2::fs2-io::3.9.3"
//> using lib "org.typelevel::cats-effect::3.5.2"
//> using lib "org.typelevel::cats-parse::0.3.10"
//> using lib "org.typelevel::cats-mtl::1.4.0"
//> using lib "io.circe::circe-core::0.14.6"
//> using lib "org.http4s::http4s-ember-server::0.23.23"
//> using lib "org.http4s::http4s-dsl::0.23.23"
//> using lib "org.http4s::http4s-circe::0.23.23"
//> using options "-Wunused:all", "-Ykind-projector:underscores", "-Wnonunit-statement", "-Wvalue-discard"
package badlang

import analysis.*
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
import langoustine.lsp.enumerations.TextDocumentSyncKind
import langoustine.lsp.requests.initialize
import langoustine.lsp.requests.initialized
import langoustine.lsp.requests.textDocument
import langoustine.lsp.requests.window
import langoustine.lsp.runtime.DocumentUri
import langoustine.lsp.runtime.Opt
import langoustine.lsp.structures.DiagnosticOptions
import langoustine.lsp.structures.InitializeResult
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
      .handleRequest(textDocument.definition) { in =>
        docs
          .get(in.params.textDocument.uri)
          .map(td => parser.parse(td.content).toOption)
          .map {
            case None => None
            case Some(file) =>
              file
                .findReferenceAt(in.params.position.toModel)
                .flatMap(sym => file.findDefinition(sym.value))
                .map { sym =>
                  Definition(Location(in.params.textDocument.uri, sym.range.toLSP))
                }
          }
          .map(_.toOpt)
      }
      .handleRequest(textDocument.diagnostic)(in => diagnostics(in.params, docs))

  extension [A](
    option: Option[A]
  ) def toOpt: Opt[A] = option.fold(Opt.empty)(Opt(_))

}

object main extends LangoustineApp {

  override def server(
    args: List[String]
  ): Resource[IO, LSPBuilder[IO]] = DocumentCache
    .make[DocumentUri]
    .toResource
    .map { cache =>
      val docs = TextDocuments.cached(cache)
      Server.make(cache, docs)

    }

}
