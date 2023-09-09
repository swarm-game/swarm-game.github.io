---
title: Installing
---

Installing
==========

**NOTE**: Compiling Swarm with GHC 9.2.5 and optimizations enabled
seems to result in very long freezes/delays (tens of seconds) when
starting Swarm (see
[#1000](https://github.com/swarm-game/swarm/issues/1000)).  We
recommend either building Swarm with a different version of GHC
(*e.g.* 9.4.x), or building with optimizations turned off (which does
not seem to affect the game performance very much).

**NOTE**: Swarm requires a POSIX-style terminal environment that
supports `terminfo`.  Linux and MacOS should work out of the box.  On
Windows, you will need to use [Windows Subsystem for
Linux](https://learn.microsoft.com/en-us/windows/wsl/); you should
then be able to follow instructions for installing on Linux.

It is recommended that you use a relatively large terminal window
(*e.g.* 170 columns x 40 rows or larger).  To find out the size of
your terminal, you can type `stty size` at a command prompt. If it's
not big enough, try decreasing the font size. You can read about
and/or share recommended terminal settings in [this GitHub
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
You will also need to extract the data to your local Swarm folder so
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
contribute to Swarm development, you can build from source.

1. Clone the Swarm repository, e.g.

       git clone https://github.com/swarm-game/swarm.git

1. If you don't already have the `stack` tool:
    1. Get the [`ghcup` tool](https://www.haskell.org/ghcup/), a handy
       one-stop utility for managing all the different pieces of a
       Haskell toolchain.
    1. Use `ghcup` to install `stack`:

           ghcup install stack

1. Now use `stack` to build and run Swarm:

       cd /path/to/the/swarm/repo
       stack run

1. Go get a snack while `stack` downloads a Haskell compiler and
   all of Swarm's dependencies.


Configuring your editor
=======================

Although you can write commands and definitions directly in the Swarm
REPL, once you get beyond the basics you'll probably want to use an
external editor for writing Swarm programs.  Swarm has support for
external editors with highlighting and LSP integration:

![Editor with problem popup](/images/editor.png)

See the [`editors`
folder](https://github.com/swarm-game/swarm/tree/main/editors) for
details on how to configure your editor.  Currently, emacs and VS Code
are officially supported, but more can be added.
