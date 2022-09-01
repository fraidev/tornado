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
      alcotest = osuper.alcotest.overrideAttrs (o: {
        propagatedBuildInputs =
          prev.lib.lists.remove osuper.uuidm o.propagatedBuildInputs;
        });

      piaf = osuper.piaf.overrideAttrs (o: {
        src = prev.fetchFromGitHub {
            owner = "anmonteiro";
            repo = "piaf";
            rev = "b7ada509c8a262d155ae5d6e9ca7c98d14f9ad8a";
            sha256 = "sha256-X3dDiZlYkzOEbuf7jFqjmV7SGsE6hi/nezbn6ht6Wdk=";
            fetchSubmodules = true;
          };
          patches = [];
          propagatedBuildInputs = with osuper; [ eio multipart_form sendfile ipaddr uri ssl magic-mime eio-ssl
          httpaf-eio h2-eio ];
        });

      bencode = osuper.buildDunePackage {
        pname = "bencode";
        version = "2.0";
        src = builtins.fetchurl {
          url = "https://github.com/rgrinberg/bencode/archive/2.0.tar.gz";
          sha256 =
            "233eae0817126e9c4a781bf1329d834672b77954e6b983a4d0a298d1e2ff0756";
        };
        propagatedBuildInputs = with osuper; [ ];
      };

    })) prev.ocaml-ng;
}
