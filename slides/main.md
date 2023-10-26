
# Let's build an IDE!
Language servers and clients in action

---

## Without LSP

```
~~~graph-easy --as=boxart
[ IntelliJ ] { rows: 9; }
[ IntelliJ ] -> {start: east; end: west;} [ Scala ] { origin: IntelliJ; offset: 4,0; rows: 2;}
[ IntelliJ ] -> {start: east; end: west;} [ Java ] { origin: Scala; offset: 0,2; rows: 2;}
[ IntelliJ ] -> {start: east; end: west;} [ Rust ] { origin: Java; offset: 0,2; rows: 2;}
[ VS Code ] { origin: Scala; offset: 4,0; rows: 9; } -> {start: west; end: east;} [ Scala ]
[ VS Code ] -> {start: west; end: east;} [ Java ]
[ VS Code ] -> {start: west; end: east;} [ Rust ]
[ Neovim ] {origin: Rust; offset: -2,4;} -> {start: north; end: east; } [ Java ]
[ Neovim ] -> {start: north; end: south; } [ Rust ]
[ Neovim ] -> {start: north; end: south; } [ Scala ]
~~~
```

---

## With LSP

```
~~~graph-easy --as=boxart
[ IntelliJ ] { origin: LSP; offset: -2, 0; } -> {start: east; end: west;} [LSP]
[ VS Code ] { origin: IntelliJ; offset: 0, 2; } -> {start: east; end: west;} [LSP]
[ Neovim ] { origin: VS Code; offset: 0, 2; } -> {start: east; end: west;} [LSP]
[ LSP ] -> [ Scala ] { origin: LSP; offset: 2, 0; }
[ LSP ] -> [ Rust ] { origin: Scala; offset: 0, 2; }
[ LSP ] -> [ Java ] { origin: Rust; offset: 0, 2; }
~~~
```
