package badlang

import cats.effect.IO
import cats.effect.kernel.Resource
import cats.implicits.*
import io.circe.Encoder
import io.circe.KeyEncoder
import langoustine.lsp.runtime.DocumentUri
import org.http4s.HttpRoutes
import org.http4s.ember.server.EmberServerBuilder

import scala.concurrent.duration.Duration

object Api {

  def run(
    cache: DocumentCache[DocumentUri],
    docs: TextDocuments,
  ): Resource[IO, org.http4s.server.Server] = {
    import org.http4s.dsl.io.*
    import org.http4s.circe.CirceEntityCodec.*
    import io.circe.generic.auto.*

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
