{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    nixpkgs-2.url = "/Users/kubukoz/projects/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, nixpkgs-2, flake-utils, ... }@inputs:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs2 = import nixpkgs-2 {
          inherit system;
        };
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ (_: _: { inherit (pkgs2) slides; }) ];
        };
      in
      {
        devShell = pkgs.mkShell {
          buildInputs = [
            pkgs.yarn
            pkgs.nodejs
            pkgs.sbt
            pkgs.jless
            pkgs.slides
            pkgs.graph-easy
          ];
          nativeBuildInputs = [
            pkgs.s2n-tls
          ];
        };
      }
    );
}
