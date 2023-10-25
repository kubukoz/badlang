import getConfigurationServiceOverride from "@codingame/monaco-vscode-configuration-service-override";
import getEditorServiceOverride, {
  OpenEditor,
} from "@codingame/monaco-vscode-editor-service-override";
import getKeybindingsServiceOverride from "@codingame/monaco-vscode-keybindings-service-override";
import { languages } from "monaco-editor";
import { MonacoLanguageClient, initServices } from "monaco-languageclient";
import { CloseAction, ErrorAction } from "vscode-languageclient";
import { URI } from "vscode-uri";
import {
  WebSocketMessageReader,
  WebSocketMessageWriter,
  toSocket,
} from "vscode-ws-jsonrpc";
import { createConfiguredEditor, createModelReference } from "vscode/monaco";
import "./main.scss";

const ed: OpenEditor = async (modelRef, options, sideBySide) => {
  console.log("trying to open", modelRef, options, sideBySide);
  return Promise.resolve(undefined) as Promise<undefined>;
};

const performInit = async () => {
  await initServices({
    userServices: {
      ...getConfigurationServiceOverride(URI.file("/workspace")),
      ...getKeybindingsServiceOverride(),
      ...getEditorServiceOverride(ed),
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

  const uri = URI.parse("/workspace/demo.bad");

  const text = `LET x 40
LET y 10
LET z 50

SHOW x y
SHOW z
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
