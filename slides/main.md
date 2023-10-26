---
author: ""
date: ""
paging: ""
---

# Let's build an IDE!
## Jakub Kozłowski | Scala in the city | 26 X 2023

---

## Why build an IDE?

1. For fun
1. For your proprietary DSL
1. **So that you have a talk topic**

---

## What are our options?

### UI

- Write your own frontend?
- [Monaco?](https://github.com/microsoft/monaco-editor/)
- Existing editor?

### Analysis

- Just syntax <!-- can be done in any environment -->
- "Best effort" semantic analysis <!-- can be done in any environment -->
- Full-blown analysis <!-- may require special setup e.g. a JVM -->

We'll use **Monaco** and communicate with a compiler.

---

## The problem(s)

- Each editor has **its own extension API**
- Editors may require **special environments** (e.g. JVM or Node)
  - Process boundaries must sometimes be crossed

---

## This is manageable...

```
~~~graph-easy --as=boxart
[ Your IDE ] -> {start: east; end: west;} [ Your lang ]
~~~
```

---


## This is not

```
~~~graph-easy --as=boxart
[ VS Code ] { rows: 7; }
[ VS Code ] -> {start: east; end: west;} [ Scala ] { origin: VS Code; offset: 2,0; rows: 2; columns: 4;}
[ VS Code ] -> {start: east; end: west;} [ Your lang ] { origin: Scala; offset: 0,2; rows: 2; columns: 2;}
[ VS Code ] -> {start: east; end: west;} [ Rust ] { origin: Your lang; offset: 0,2; rows: 2;}
[ Your IDE ] { origin: Scala; offset: 2,0; rows: 7; } -> {start: west; end: east;} [ Scala ]
[ Your IDE ] -> {start: west; end: east;} [ Your lang ]
[ Your IDE ] -> {start: west; end: east;} [ Rust ]
[ Neovim ] {origin: Rust; offset: 0,2;} -> {start: north; end: south; } [ Your lang ]
[ Neovim ] -> {start: north; end: south; } [ Rust ]
[ Neovim ] -> {start: north; end: south; } [ Scala ]
~~~
```

---

## There must be a better way

```
~~~graph-easy --as=boxart
[ VS Code ] { origin: Something; offset: -2, 0; } -> {start: east; end: west;} [Something]
[ Your IDE ] { origin: VS Code; offset: 0, 2; } -> {start: east; end: west;} [Something]
[ Neovim ] { origin: Your IDE; offset: 0, 2; } -> {start: east; end: west;} [Something]
[ Something ] -> [ Scala ] { origin: Something; offset: 2, 0; }
[ Something ] -> [ Rust ] { origin: Scala; offset: 0, 2; }
[ Something ] -> [ Your lang ] { origin: Rust; offset: 0, 2; }
~~~
```

This is [LSP](https://microsoft.github.io/language-server-protocol/)!

---

## LSP in 10 seconds

### 1. Initialize connection

```
~~~graph-easy --as=boxart
[ Client ] {columns: 2;} - initialize request -> [ Server ]
[Server] - initialize response -> {start: south; end: south;} [Client]
~~~
```

---

## LSP in 10 seconds

### 2. Send notifications

```
~~~graph-easy --as=boxart
[ Client ] {columns: 2; rows: 5;} - textDocument/didOpen notification -> [ Server ] {rows: 5;}
[ Client ] - textDocument/didChange notification -> [ Server ]
[ Client ] - textDocument/didChange notification -> [ Server ]
[ Client ] - ... -> [ Server ]
[ Client ] {columns: 2; rows: 7;} - textDocument/didClose notification -> [ Server ] {rows: 7;}
[Server] - window/showMessage notification -> [Client]
~~~
```

---

## LSP in 10 seconds

### 2.5. Send notifications and requests

```
~~~graph-easy --as=boxart
[ Client ] - textDocument/definition request -> {start: east,1; end:west,1;}[ Server ]
[ Server ] - textDocument/definition response -> {start: west,3; end: east,3;} [ Client ]
[ Client ] - textDocument/diagnostic request -> {start: east,5; end:west,5;}[ Server ]
[ Server ] - textDocument/diagnostic response -> {start: west,7; end: east,7;} [ Client ]
[ Server ] - workspace/configuration request -> {start: west,8; end:east,8;}[ Client ]
[ Client ] - workspace/configuration response -> {start: east,10; end: west,10;} [ Server ]
~~~
```

---

## LSP in 10 seconds

### Request

```json
{
  "jsonrpc": "2.0",
  "id": 2,
  "method": "textDocument/definition",
  "params": {
    "textDocument": { "uri": "file:///workspace/demo.bad" },
    "position": { "line": 5, "character": 5 }
  }
}
```

### Response

```json
{
  "jsonrpc": "2.0",
  "id": 2,
  "result": {
    "uri": "file:///workspace/demo.bad",
    "range": {
      "start": { "line": 2, "character": 4 },
      "end": { "line": 2, "character": 5 }
    }
  }
}
```

---

## Let's get to work!

---

# Thank you

Slides/code: will be posted on the event

[Get in touch](https://linktr.ee/kubukoz)
