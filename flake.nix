{
  description = "Nix Flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    nix-filter.url = "github:numtide/nix-filter";

    ocaml-overlays.url = "github:anmonteiro/nix-overlays/0081a01960591e7415986eca055887ca76689799";
    ocaml-overlays.inputs = {
      nixpkgs.follows = "nixpkgs";
      flake-utils.follows = "flake-utils";
    };
  };

  outputs = { self, nixpkgs, ocaml-overlays, nix-filter, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = (ocaml-overlays.makePkgs {
        inherit system;
        extraOverlays = [
          (import ./nix/overlay.nix)
        ];
      }).extend (self: super: {
        ocamlPackages = super.ocaml-ng.ocamlPackages_5_00;
      }); in
      let tornado = pkgs.callPackage ./nix { doCheck = true; }; in
      rec {
        packages = { inherit tornado; };
        devShell = import ./nix/shell.nix { inherit pkgs tornado; };
      });
}
