# Ubuntu 24.04, with git etc. preinstalled
FROM mcr.microsoft.com/devcontainers/base:noble

RUN apt-get update
RUN apt-get install -y opam

# For OCaml's opium
RUN apt-get install -y libev-dev

RUN apt-get install -y elm-compiler

USER vscode

# TODO: consider `--disable-sandboxing` if opam acts up in Docker
RUN opam init -y

# We're told by `opam init` to run this
RUN echo 'test -r /home/vscode/.opam/opam-init/init.sh && . /home/vscode/.opam/opam-init/init.sh > /dev/null 2> /dev/null || true' >> "$HOME/.bashrc"

RUN opam switch create 5.2.0

RUN opam install -y core core_unix dune
RUN opam install -y yojson atdgen opium domainslib
RUN opam install -y ocaml-lsp-server ocamlformat
