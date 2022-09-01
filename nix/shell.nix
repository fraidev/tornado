{ pkgs, tornado }:

with pkgs;
with ocamlPackages;
mkShell {
  inputsFrom = [ tornado ];
  packages = [ nixfmt ocamlformat ocaml findlib dune odoc ocaml-lsp ];
}
