import { ExtensionContext, window } from "vscode";
import { LanguageClient } from "vscode-languageclient/node";

export function activate(context: ExtensionContext) {

  const outputChannel = window.createOutputChannel("Badlang");

  const lspClient = new LanguageClient(
    "badlang",
    "Badlang",
    {
      command: "/Users/kubukoz/projects/badlang/launch.sh"
    },
    {
      documentSelector: [{ language: "badlang" }],
      outputChannel,
    }
  );

  lspClient.start();

  context.subscriptions.push(
    lspClient,
  );
}
