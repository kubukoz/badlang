package badlang

import cats.data.OptionT
import cats.effect.IO
import cats.effect.implicits.*
import fs2.io.file.Files
import fs2.io.file.Path
import langoustine.lsp.runtime.DocumentUri
import org.http4s.Uri

trait TextDocuments {

  def get(
    uri: DocumentUri
  ): IO[Document]

}

object TextDocuments {

  def cached(
    cache: DocumentCache[DocumentUri]
  ): TextDocuments =
    new TextDocuments {

      override def get(
        uri: DocumentUri
      ): IO[Document] = OptionT(cache.get(uri))
        .map(Document(_, true))
        .getOrElseF {
          Files[IO]
            .readUtf8(Path(Uri.fromString(uri.value).toTry.get.path.renderString))
            .compile
            .string
            .map(Document(_, false))
        }

    }

}
