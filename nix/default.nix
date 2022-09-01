{ pkgs, doCheck ? true }:

let inherit (pkgs) lib stdenv ocamlPackages;

in with ocamlPackages;
buildDunePackage rec {
  pname = "tornado";
  version = "0.0.0-dev";

  src = lib.filterSource {
    src = ./..;
    dirs = [ "lib" ];
    files = [ "dune-project" ];
  };

  propagatedBuildInputs = [
    eio
    piaf
    bencode
    stdint
    eio_main
    uri
    ppx_deriving
    ppx_sexp_conv
    ppx_let
  ]
  # checkInputs are here because when cross compiling dune needs test dependencies
  # but they are not available for the build phase. The issue can be seen by adding strictDeps = true;.
    ++ checkInputs;

  checkInputs = [ alcotest ];
}
