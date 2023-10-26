import getConfigurationServiceOverride from "@codingame/monaco-vscode-configuration-service-override";
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

let allData = {
  badlang: {
    language: {
      id: "badlang",
      extensions: [".bad"],
      aliases: ["Badlang", "badlang"],
      mimetypes: ["text/badlang"],
    },
    input: `LET x 40
LET y 10
LET z 50

SHOW x y
SHOW z
`,
    workspace: URI.file("/workspace"),
    filename: "demo.bad",
    url: "ws://localhost:30000",
  },
  scala: {
    language: {
      id: "scala",
      extensions: [".scala", ".sc"],
      aliases: ["Scala", "scala"],
      mimetypes: ["text/scala"],
    },
    input: `//> using lib "org.typelevel::cats-effect:3.5.2"
import cats.effect._, cats.implicits._

object Demo {
  val numbers = (1 to 10).filter(_ % 2 == 0)

  def hello =
    numbers.toList
      .traverse_(IO.println(_))
  }
`,
    workspace: URI.file("/Users/kubukoz/projects/lsp-ws-proxy/workspace"),
    filename: "demo.scala",
    url: "ws://localhost:30001",
  },
  rust: {
    language: {
      id: "rust",
      extensions: [".rs"],
      aliases: ["Rust", "rust"],
      mimetypes: ["text/rust"],
    },
    input: `fn main() {
  let x = 40;
  let y = 2.0;
  let s = Foo { x, y };
  dbg!(&s);
}

#[derive(Debug)]
struct Foo {
  x: i32,
  y: i32,
}
`,
    workspace: URI.file("/Users/kubukoz/projects/lsp-ws-proxy/rs-workspace"),
    filename: "src/main.rs",
    url: "ws://localhost:30002",
  },
};

const langId = (new URLSearchParams(window.location.search).get("lang") ||
  "badlang") as keyof typeof allData;

let data = allData[langId];

const performInit = async () => {
  await initServices({
    userServices: {
      ...getConfigurationServiceOverride(data.workspace),
      ...getKeybindingsServiceOverride(),
    },
    debugLogging: true,
  });

  languages.register(data.language);
};
const start = async () => {
  await performInit();

  const uri = URI.file(data.workspace.path + "/" + data.filename);

  const text = data.input;

  const modelRef = await createModelReference(uri, text);

  modelRef.object.setLanguageId(data.language.id);

  createConfiguredEditor(document.getElementById("editor")!, {
    model: modelRef.object.textEditorModel,
    glyphMargin: true,
    lightbulb: {
      enabled: true,
    },
    automaticLayout: true,
    wordBasedSuggestions: false,
  });

  const url = data.url;
  const webSocket = new WebSocket(url);

  webSocket.onopen = async () => {
    const socket = toSocket(webSocket);
    const reader = new WebSocketMessageReader(socket);
    const writer = new WebSocketMessageWriter(socket);

    document.getElementById(
      "title"
    )!.innerHTML = `${data.language.aliases[0]} Web IDE`;

    const languageClient = new MonacoLanguageClient({
      name: `${data.language.aliases[0]} Web IDE`,
      clientOptions: {
        documentSelector: [data.language.id],
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
