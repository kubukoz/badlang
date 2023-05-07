package badlang

import cats.effect.IO

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
