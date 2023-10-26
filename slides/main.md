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

## No, for real, why?

- You want editor support for your language
- You're not happy with existing editors

---

## Step 1

How much do you want to do from scratch?

### UI

- Write your own frontend?
- [Monaco?](https://github.com/microsoft/monaco-editor/)
- Existing editor?

### Analysis

- Just syntax <!-- can be done in any environment -->
- "Best effort" semantic analysis <!-- can be done in any environment -->
- Using compiler tools <!-- may require special setup e.g. a JVM -->

---

## 5 years later...

```
~~~graph-easy --as=boxart
[ VS Code ] { rows: 9; }
[ VS Code ] -> {start: east; end: west;} [ Scala ] { origin: VS Code; offset: 4,0; rows: 2;}
[ VS Code ] -> {start: east; end: west;} [ Java ] { origin: Scala; offset: 0,2; rows: 2;}
[ VS Code ] -> {start: east; end: west;} [ Rust ] { origin: Java; offset: 0,2; rows: 2;}
[ Your IDE ] { origin: Scala; offset: 4,0; rows: 9; } -> {start: west; end: east;} [ Scala ]
[ Your IDE ] -> {start: west; end: east;} [ Java ]
[ Your IDE ] -> {start: west; end: east;} [ Rust ]
[ Neovim ] {origin: Rust; offset: -2,2;} -> {start: north; end: east; } [ Java ]
[ Neovim ] -> {start: north; end: south; } [ Rust ]
[ Neovim ] -> {start: north; end: south; } [ Scala ]
~~~
```

---

## With LSP

```
~~~graph-easy --as=boxart
[ VS Code ] { origin: LSP; offset: -2, 0; } -> {start: east; end: west;} [LSP]
[ Your IDE ] { origin: VS Code; offset: 0, 2; } -> {start: east; end: west;} [LSP]
[ Neovim ] { origin: Your IDE; offset: 0, 2; } -> {start: east; end: west;} [LSP]
[ LSP ] -> [ Scala ] { origin: LSP; offset: 2, 0; }
[ LSP ] -> [ Rust ] { origin: Scala; offset: 0, 2; }
[ LSP ] -> [ Java ] { origin: Rust; offset: 0, 2; }
~~~
```
