{ pkgs, tornado }:

with pkgs;
with ocamlPackages;
mkShell {
  OCAMLRUNPARAM = "b";
  inputsFrom = [ tornado ];
  packages = [ nixfmt ocamlformat ocaml findlib dune odoc ocaml-lsp ];
}
