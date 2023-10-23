import { initServices, MonacoLanguageClient } from "monaco-languageclient";
import { URI } from "vscode-uri";
import {
  toSocket,
  WebSocketMessageReader,
  WebSocketMessageWriter,
} from "vscode-ws-jsonrpc";
import { createConfiguredEditor, createModelReference } from "vscode/monaco";
import "./main.scss";

import getConfigurationServiceOverride from "@codingame/monaco-vscode-configuration-service-override";
import getKeybindingsServiceOverride from "@codingame/monaco-vscode-keybindings-service-override";
import { CloseAction, ErrorAction } from "vscode-languageclient";

import { languages } from "monaco-editor";

const performInit = async () => {
  await initServices({
    userServices: {
      // ...getThemeServiceOverride(),
      // ...getTextmateServiceOverride(),
      ...getConfigurationServiceOverride(
        URI.file("/Users/kubukoz/projects/badlang-demo")
      ),
      ...getKeybindingsServiceOverride(),
    },
    debugLogging: true,
  });

  languages.register({
    id: "badlang",
    extensions: [".bad"],
    aliases: ["Badlang", "badlang"],
    mimetypes: ["text/badlang"],
  });
};
const start = async () => {
  await performInit();

  const uri = URI.parse("/Users/kubukoz/projects/badlang-demo/demo.bad");

  const text = `LET x 40
LET y 10

SHOW x y
`;

  const modelRef = await createModelReference(uri, text);

  modelRef.object.setLanguageId("badlang");

  createConfiguredEditor(document.getElementById("editor")!, {
    model: modelRef.object.textEditorModel,
    glyphMargin: true,
    lightbulb: {
      enabled: true,
    },
    automaticLayout: true,
    wordBasedSuggestions: false,
  });

  const url = "ws://localhost:30000";
  const webSocket = new WebSocket(url);

  webSocket.onopen = async () => {
    const socket = toSocket(webSocket);
    const reader = new WebSocketMessageReader(socket);
    const writer = new WebSocketMessageWriter(socket);

    const languageClient = new MonacoLanguageClient({
      name: "Badlang Web IDE",
      clientOptions: {
        documentSelector: ["badlang"],
        errorHandler: {
          error: () => ({ action: ErrorAction.Continue }),
          closed: () => ({ action: CloseAction.DoNotRestart }),
        },
      },
      connectionProvider: {
        get: async () => ({
          reader,
          writer,
        }),
      },
    });

    languageClient.start();
    console.log("started server");
    reader.onClose(() => languageClient.stop());
  };
};

start();
