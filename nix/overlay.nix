final: prev:
let
  disableCheck = package: package.overrideAttrs (o: { doCheck = false; });
  addCheckInputs = package:
    package.overrideAttrs ({ buildInputs ? [ ], checkInputs, ... }: {
      buildInputs = buildInputs ++ checkInputs;
    });
in {
  ocaml-ng = builtins.mapAttrs (_: ocamlVersion:
    ocamlVersion.overrideScope' (oself: osuper: {
      progress = osuper.progress.overrideAttrs (_: {
        src = prev.fetchFromGitHub {
          owner = "craigfe";
          repo = "progress";
          rev = "76e7c791bd17c28b3e605d9c383102a30345e029";
          sha256 = "sha256-IqfDPI8kwvbB2U8xfiOdsxf6VuKnzHIKd0rsyNRWfK8=";
        };

        propagatedBuildInputs = with osuper; [
          terminal
          fmt
          logs
          mtime
          uucp
          uutf
          vector
          optint
        ];
      });
    })) prev.ocaml-ng;
}
