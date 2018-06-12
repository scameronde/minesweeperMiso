# Miso example

This project builds a minesweeper clone, the output is included in this repository for demonstration purposes:
[Play Minesweeper!](http://scameronde.github.io/minesweeperMiso/index.html)

This is a fork of https://github.com/dc25/minesweeperMiso.

I removed the unnecessary use of a state monad, added a basic nix configuration and restructured the code base to my liking. The functionality is still the same.

## Building with `nix`

To build the code in this way, you have to have the nix package manager installed:

```shell
curl https://nixos.org/nix/install | sh
```

In the base directory of the project run

```shell
nix-build
```

and then open the html file found at

```shell
result/bin/minesweeper.jsexe/index.html
```

## Building with `stack`

Run the following commands on Mac or Linux from inside a **terminal emulator**.

Clone this repository 

```shell
git clone git@github.com:scameronde/minesweeperMiso.git
```

**Note:** The `stack` project file is surprisingly in the `src` directory, hence enter

```shell
cd minesweeperMiso/src
```

Next, run stack to build the javascript code. This might take **very long**, but only the **first build**.

```shell
stack build
```

When the build is finished pay attention to the stack output, it contains the path, where the resulting Javascript/HTML can be found.

To open the generated Javascript run:

```shell
firefox .stack-work/install/x86_64-linux/lts-6.30/ghcjs-0.2.0.9006030_ghc-7.10.3/bin/minesweeper.jsexe/index.html
```


