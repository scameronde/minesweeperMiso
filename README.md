# Miso example

This is a fork of https://github.com/dc25/minesweeperMiso.

I removed the unnecessary use of a state monad, added a basic nix configuration and restructured the code base to my liking. The functionality is still the same.

To build the code you have to have the nix package manager installed:

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

