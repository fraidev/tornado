{ pkgs, doCheck ? true, static ? false, nix-filter }:

let inherit (pkgs) lib stdenv ocamlPackages;

in with ocamlPackages;
buildDunePackage rec {
  pname = "tornado";
  version = "0.0.0-dev";

  src = with nix-filter.lib;
    filter {
      root = ../.;
      include = [ "bin" "lib" "dune-project" "tornado.opam" ];
    };

  # This is the same as standard dune build but with static support
  buildPhase = ''
    runHook preBuild
    echo "running ${if static then "static" else "release"} build"
    dune build -p ${pname} --profile=${if static then "static" else "release"}
    runHook postBuild
  '';

  checkInputs = [ alcotest ];
  propagatedBuildInputs =
    [ eio piaf eio_main bencode stdint uri ppx_deriving progress ]
    ++ checkInputs;
  # checkInputs are here because when cross compiling dune needs test dependencies
  # but they are not available for the build phase. The issue can be seen by adding strictDeps = true;.

  # Remove every directory which could have links to other store paths.
  # This makes the result much smaller
  isLibrary = false;
}
