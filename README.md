# Tornado

Tornado is a BitTorrent client written entirely in OCaml.

[![asciicast](https://asciinema.org/a/518394.png)](https://asciinema.org/a/518394?t=25&rows=18&speed=5&)

Note: speed x5 on this video.

## Setup

### Nix

Just run:
```
nix develop -c $SHELL
```

### Opam

First, create a switch like so:

```bash
opam switch create . 5.0.0~alpha1+options --no-install
```

Then you can run:

```
opam pin stdint https://github.com/andrenth/ocaml-stdint.git#322a8a4a8c69e4a0b75763460b915200356e3af3
opam install ocamlformat
opam install ocaml-lsp-server
opam install . --deps-only --with-test
```

## Build, tests and Downloading a file

Build the codebase with:

```
dune build
```

Run test:

```
dune build @runtest --force --no-buffer
```

Execute it for download a torrent file:

```
dune exec bin/cli.exe debian-11.4.0-arm64-netinst.iso.torrent
```

