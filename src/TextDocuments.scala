import cats.data.OptionT
import cats.effect.IO
import cats.effect.implicits._
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
