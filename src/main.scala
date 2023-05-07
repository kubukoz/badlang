//> using scala "3.3.0-RC5"
//> using lib "tech.neander::langoustine-app::0.0.20"
//> using lib "org.http4s::http4s-ember-server::0.23.18"
//> using lib "org.http4s::http4s-circe::0.23.18"
//> using lib "org.http4s::http4s-dsl::0.23.18"
//> using lib "io.circe::circe-generic:0.14.5"
//> using lib "io.chrisdavenport::crossplatformioapp::0.1.0"
//> using option "-Wunused:all"
import cats.effect.IO
import cats.effect.implicits._
import cats.effect.kernel.Resource
import cats.implicits._
import io.chrisdavenport.crossplatformioapp.CrossPlatformIOApp
import jsonrpclib.fs2.given
import langoustine.lsp.LSPBuilder
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
import langoustine.lsp.structures.InitializeResult
import langoustine.lsp.structures.ServerCapabilities
import langoustine.lsp.structures.ShowMessageParams

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
              textDocumentSync = Opt(TextDocumentSyncKind.Full)
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

}

object main extends CrossPlatformIOApp with LangoustineApp {

  override def server(
    args: List[String]
  ): Resource[IO, LSPBuilder[IO]] = DocumentCache
    .make[DocumentUri]
    .toResource
    .flatMap { cache =>
      val docs = TextDocuments.cached(cache)

      Api
        .run(cache, docs)
        .as(Server.make(cache, docs))
    }

}
