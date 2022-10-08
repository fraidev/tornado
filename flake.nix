{
  description = "Nix Flake";

  inputs = {
    nixpkgs.url = "github:nix-ocaml/nix-overlays";
    flake-utils.url = "github:numtide/flake-utils";
    nix-filter.url = "github:numtide/nix-filter";

    ocaml-overlays.url = "github:nix-ocaml/nix-overlays";
    ocaml-overlays.inputs = {
      nixpkgs.follows = "nixpkgs";
      flake-utils.follows = "flake-utils";
    };
  };

  outputs = { self, nixpkgs, ocaml-overlays, nix-filter, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = (ocaml-overlays.makePkgs {
          inherit system;
          extraOverlays = [ (import ./nix/overlay.nix) ];
        }).extend
          (self: super: { ocamlPackages = super.ocaml-ng.ocamlPackages_5_0; });
      in let
        tornado = pkgs.callPackage ./nix {
          doCheck = true;
          inherit nix-filter;
        };
      in rec {
        packages = { inherit tornado; };
        devShell = import ./nix/shell.nix { inherit pkgs tornado; };
      });
}
