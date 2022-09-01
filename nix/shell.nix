{ pkgs, tornado }:

with pkgs; with ocamlPackages; mkShell {
  inputsFrom = [ tornado ];
  packages = [
    # Make developer life easier

    # formatters
    nixfmt
    ocamlformat
  # ] ++ (pkgs.lib.optional (system != "x86_64-darwin") tilt)
  # ++ (with pkgs.ocaml-ng.ocamlPackages_5_00; [
    # OCaml developer tooling
    ocaml
    findlib
    dune
    odoc
    ocaml-lsp
  ];
  # ]);
}
