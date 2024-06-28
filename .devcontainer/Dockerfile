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

# Install elm-format. The official installation method is to use `npm install
# -g elm-format`, but npm is a heavyweight dependency that slows down image
# creation. This tarball contains a single binary and nothing else.
#
# Here we put elm-format in the user-writable folder that happens to be in
# PATH. The next time we modify the higher-up commands it might be nice to move
# this up and install elm-format in /usr/local/bin.
RUN curl -L https://github.com/avh4/elm-format/releases/download/0.8.7/elm-format-0.8.7-linux-x64.tgz | tar -C ~/.opam/5.2.0/bin -xz

# For Windows workaround. See readme.
RUN mkdir "$HOME/warmup_build" \
 && mkdir "$HOME/src_build"

# To persist bash history and sync it eagerly. Adapted from
# https://code.visualstudio.com/remote/advancedcontainers/persist-bash-history
RUN SNIPPET="if [ -d \"\$WORKSPACE_FOLDER\" ]; then \
        export PROMPT_COMMAND='history -a' \
        && export HISTFILE=\"\$WORKSPACE_FOLDER/.bash_history\" \
        && touch \"\$HISTFILE\"; \
    fi" \
    && echo "$SNIPPET" >> "$HOME/.bashrc"