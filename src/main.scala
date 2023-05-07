//> using scala "3.3.0-RC5"
//> using lib "tech.neander::langoustine-app::0.0.20"
import cats.effect.IO
import jsonrpclib.fs2.given
import langoustine.lsp.LSPBuilder
import langoustine.lsp.app.LangoustineApp
import langoustine.lsp.enumerations.MessageType
import langoustine.lsp.requests.initialize
import langoustine.lsp.requests.initialized
import langoustine.lsp.requests.window.showMessage
import langoustine.lsp.structures.InitializeResult
import langoustine.lsp.structures.ServerCapabilities
import langoustine.lsp.structures.ShowMessageParams

object main extends LangoustineApp.Simple {

  override def server: IO[LSPBuilder[cats.effect.IO]] = IO.pure(
    LSPBuilder
      .create[IO]
      .handleRequest(initialize) { in =>
        IO(InitializeResult(capabilities = ServerCapabilities()))
      }
      .handleNotification(initialized)(in =>
        in.toClient
          .notification(
            showMessage,
            ShowMessageParams(MessageType.Info, "hello from badlang server!"),
          )
      )
  )

}
