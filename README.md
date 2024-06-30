The Blind Hen 2024 ICFP Contest submission
==========================================

Team
----

Jonas B. Jensen,
Christoffer R. Nielsen,
Johan S. H. Rosenkilde, and
Kasper Svendsen

Jonas, Johan and Kasper in Aarhus, Denmark. Christoffer in Dubai, UAE

How to build
------------

Open the project in a devcontainer (see below) or replicate the environment on
the host.

### Using the devcontainer

The setup is specified in `.devcontainer/Dockerfile`, but if you use VSCode you don't need to know any of the details to use it:

- Check out the repo and open the workspace `icfp2024.code-workspace` in VSCode.
- VSCode should show a popup ask you whether you want to open the workspace in a container. Click yes. If this popup doesn't appear, use View -> Command palette -> "Dev containers: Open workspace in container".
- VSCode may tell you to install Docker, and should help you do so. Once Docker is installed, open it once to make sure you have accepted terms, etc. Once Docker is installed, you may need to repeat telling VSCode to open the workspace in a container.
- Opening the container the first time will take a few minutes as Docker downloads the base image and installs the necessary packages (in the container).
- Once this is done, terminals you open will be inside the container. From now you can follow the instructions below.

On Windows there are complications with file ownership and git configuration.
See "Docker on Windows" below.

On Linux there are complications with permissions when the UID
inside the container (1001) doesn't match the UID on the host. See "Docker on
Linux" below.

### Test the setup

Get a terminal and do:

    $ cd warmup
    $ dune build
    $ dune test
    $ dune exec mine

This should print something and start mining icfpennies. Just kill it with Ctrl+C.

Test the frontend by opening another terminal and run:

    $ cd frontend
    $ make elm.js

This generates `elm.js`, which is served by the warmup server. To run that go to your first terminal and run (still in the `warmup` directory):

    $ dune exec server

VSCode should open a popup saying it forwarded port 3000 in the container. Click this to open http://localhost:3000/ in the browser.

### Docker on Windows

Inside the container, do this once:

```sh
rm -rf warmup/_build src/_build
ln -s ~/warmup_build warmup/_build
ln -s ~/src_build src/_build
```

Set your git credentials and other personal git settings in a WSL2 shell. They
will be copied in from there every time you start the container.
**In a WSL2 shell (NOT in the VSCode terminal)** do this once:

```sh
git config --global user.name "Your Name"
git config --global user.email "your.email@address"
```

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

This repo contains a Dev Container. Follow [the Installation section of the
official guide](https://code.visualstudio.com/docs/devcontainers/containers#_installation).
Then click the `><` icon in the lower left corner of VSCode and select
_Reopen in Container_.

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
