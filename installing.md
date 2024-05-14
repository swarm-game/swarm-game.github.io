---
title: Installing
---

Installing
==========

Swarm supports Linux, Mac OS, and Windows.  It is recommended that you
use a relatively large terminal window, *e.g.* 170 columns x 40 rows
or larger.  (To find out the size of your terminal on a POSIX system,
you can type `stty size` at a command prompt.) If it's not big enough,
try decreasing the font size. You can read about and/or share
recommended terminal settings in [this GitHub
issue](https://github.com/swarm-game/swarm/issues/447).

- [Installing via binaries](#installing-via-binaries)
- [Installing from Hackage](#installing-from-Hackage)
- [Installing from source](#installing-from-source)

Installing via binaries
-----------------------

Currently we have one binary release built on [Ubuntu Bionic](https://github.com/docker-library/buildpack-deps/blob/98a5ab81d47a106c458cdf90733df0ee8beea06c/ubuntu/bionic/Dockerfile); it
will probably work on any GNU/Linux.  We hope to add MacOS binaries in the
near future.

You can download the `swarm` binary and compressed data directory from
the [latest release](https://github.com/swarm-game/swarm/releases). If
you want to run the binary simply as `swarm`, you have to put it in
one of the directories in your `PATH`:
```bash
chmod +x ./swarm          # make it executable
echo $PATH | tr ':' '\n'  # choose one of the listed directories
mv ./swarm /my/chosen/bin/directory/
```
You will also need to extract the data directory to a local Swarm folder so
the executable can find it:
```bash
mkdir -p ~/.local/share/swarm/
unzip swarm-data.zip -d ~/.local/share/swarm
```

Installing from Hackage
-----------------------

If you can't use the provided binaries, or prefer installing [from
Hackage](https://hackage.haskell.org/package/swarm), you should be
able to install with

    cabal install swarm

Currently Swarm supports GHC 9.2, 9.4, or 9.6 (GHC 9.8 support should
be coming soon; it's possible that it already supports 9.8 but we
forgot to update this message).

If you don't already have the `cabal` tool, first [install
`ghcup`](https://www.haskell.org/ghcup/), then run `ghcup install
cabal` (if `cabal` was not automatically downloaded as part of
`ghcup`'s installation).

You may need to add `~/.cabal/bin` to your `PATH`; alternatively, you
can install with `cabal install --installdir=<DIR> swarm` to have
`cabal` install the `swarm` executable in a `<DIR>` of your choosing.

Installing from source
----------------------

If you want the latest unreleased bleeding-edge features, or want to
contribute to Swarm development, [head over to the Swarm repository on
GitHub](https://github.com/swarm-game/swarm/#readme) for instructions
on building from source.

Configuring your editor
=======================

Although you can write commands and definitions directly in the Swarm
REPL, once you get beyond the basics you'll probably want to use an
external editor for writing Swarm programs.  Swarm has support for
external editors with syntax highlighting and LSP integration:

![Editor with problem popup](/images/editor.png)

See the [`editors`
folder](https://github.com/swarm-game/swarm/tree/main/editors) for
details on how to configure your editor.  Currently, VS Code, Emacs,
and Vim/Neovim are officially supported, but more may be added in the
future.  [See this PR](https://github.com/swarm-game/swarm/pull/1518)
for an example of the process of adding support for a new editor.
