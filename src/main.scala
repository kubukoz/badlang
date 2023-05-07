//> using scala "3.3.0-RC5"
//> using lib "tech.neander::langoustine-app::0.0.20"
//> using lib "org.http4s::http4s-ember-server::0.23.18"
//> using lib "org.http4s::http4s-circe::0.23.18"
//> using lib "org.http4s::http4s-dsl::0.23.18"
//> using lib "io.circe::circe-generic:0.14.5"
//> using lib "io.chrisdavenport::crossplatformioapp::0.1.0"
//> using option "-Wunused:all"
import cats.data.OptionT
import cats.effect.IO
import cats.effect.implicits._
import cats.effect.kernel.Resource
import cats.implicits._
import fs2.io.file.Files
import fs2.io.file.Path
import io.chrisdavenport.crossplatformioapp.CrossPlatformIOApp
import io.circe.Encoder
import io.circe.KeyEncoder
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
import org.http4s.HttpRoutes
import org.http4s.Uri
import org.http4s.ember.server.EmberServerBuilder

import scala.concurrent.duration.Duration

trait DocumentCache[Uri] {

  def getKeys: IO[List[Uri]]

  def get(
    uri: Uri
  ): IO[Option[String]]

  def set(
    uri: Uri,
    content: String,
  ): IO[Unit]

  def remove(
    uri: Uri
  ): IO[Unit]

}

object DocumentCache {

  def make[Uri]: IO[DocumentCache[Uri]] = IO.ref(Map.empty[Uri, String]).map { state =>
    new DocumentCache {
      override def getKeys: IO[List[Uri]] = state.get.map(_.keys.toList)
      override def get(
        uri: Uri
      ): IO[Option[String]] = state.get.map(_.get(uri))

      override def set(
        uri: Uri,
        content: String,
      ): IO[Unit] = state.update(_.updated(uri, content))

      override def remove(
        uri: Uri
      ): IO[Unit] = state.update(_ - uri)
    }
  }

}

trait TextDocuments {

  def get(
    uri: DocumentUri
  ): IO[Document]

}

case class Document(
  content: String,
  cached: Boolean,
)

object TextDocuments {

  def cached(
    cache: DocumentCache[DocumentUri]
  ): TextDocuments =
    new TextDocuments {

      override def get(
        uri: DocumentUri
      ): IO[Document] = OptionT(cache.get(uri))
        .map(Document(_, true))
        .getOrElseF(
          Files[IO]
            .readAll(Path(Uri.fromString(uri.value).toTry.get.path.renderString))
            .through(fs2.text.utf8.decode[IO])
            .compile
            .string
            .map(Document(_, false))
        )

    }

}

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

object Api {

  def run(
    cache: DocumentCache[DocumentUri],
    docs: TextDocuments,
  ): Resource[IO, org.http4s.server.Server] = {
    import org.http4s.dsl.io._
    import org.http4s.circe.CirceEntityCodec._
    import io.circe.generic.auto._

    given KeyEncoder[DocumentUri] = KeyEncoder[String].contramap(_.value)

    EmberServerBuilder
      .default[IO]
      .withShutdownTimeout(Duration.Zero)
      .withHttpApp(
        HttpRoutes
          .of[IO] {
            case GET -> Root / "docs" / "cache" =>
              cache
                .getKeys
                .flatMap { keys =>
                  keys.traverse { key =>
                    docs.get(key).map(v => key -> v.content)
                  }
                }
                .map(_.toMap)
                .flatMap(Ok(_))
            case GET -> Root / "docs" / uri => docs.get(DocumentUri(uri)).flatMap(Ok(_))

          }
          .orNotFound
      )
      .build
  }

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
