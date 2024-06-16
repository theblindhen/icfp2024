The Blind Hen 2024 ICFP Contest submission
==========================================

Team
----

Jonas B. Jensen,
Christoffer R. Nielsen,
Johan S. H. Rosenkilde, and
Kasper Svendsen

All in Copenhagen, Denmark

How to build
------------

Use `.devcontainer/Dockerfile` in a container or replicate the environment on
the host. On Linux there are complications with permissions when the UID
inside the container (1001) doesn't match the UID on the host. See "Docker on
Linux" below.

Now go into the right subdirectory and build and run one of these commands:

    $ dune build
    $ dune test
    $ dune exec PROG_NAME

On subsequent terminal sessions, do the following unless you allowed `opam init`
to modify your shell rc:

    $ eval `opam config env`

### Docker on Linux

Ensure `/etc/subuid` has an entry for your user. Note the first number in that
line, which is the UID offset. The `vscode` user in the Docker image has UID
1001, which will be added to that number. In the following examples, we'll
assume that the two numbers added together is 101001.

Edit `/etc/docker/daemon.json`:

```json
{
    "userns-remap": "YOUR_USER_NAME"
}
```

Restart Docker: `systemctl restart docker`.

Now use ACLs to allow both users to read and write, to set this as the default
also, and to add all permissions to the ACL mask:

```sh
setfacl -R -m u:101001:rwX .
setfacl -R -m u:YOUR_USER_NAME:rwX .
setfacl -R -d -m u:101001:rwX .
setfacl -R -d -m u:YOUR_USER_NAME:rwX .
setfacl -R -d -m m::rwX .
```

How to develop
--------------

### Copilot in VSCode

Intall the Microsoft-built version of VSCode, not the open-source ones. Then
install GitHub Copilot from the market place and sign in to your GitHub account
when it prompts you. Also install GitHub Copilot Chat.

### IDE

For VSCode support, if not using the Dev Container, install OCaml Platform from
the VSCode Marketplace. Open the workspace `icfp2024.code-workspace` in this
directory to ensure we all use the same settings (format on save!).

### Documentation

Core library documentation can be found at
https://ocaml.org/p/core/latest/doc/Core/index.html

We use the GUI library Bogue, which has a good introductory guide at
http://sanette.github.io/bogue/Principles.html
and API documentation at
https://ocaml.org/p/bogue/latest/doc/Bogue/index.html
