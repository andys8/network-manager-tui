# network-manager-tui (deprecated and not maintained anymore)

A simple network manager command-line tool

![screenshot](https://raw.githubusercontent.com/andys8/network-manager-tui/master/screenshot.png)

## Usage

```sh
nmt
```

## Dependencies

[`nmcli`](https://developer.gnome.org/NetworkManager/stable/nmcli.html) (command-line tool for controlling NetworkManager) has to be installed

## Installation

### Clone repository

```sh
git clone https://github.com/andys8/network-manager-tui.git
cd network-manager-tui
stack install
```

### Stack

```sh
stack install network-manager-tui # --resolver=lts-14
```

#### Nix

```sh
nix-env -f "<nixpkgs>" -iA haskellPackages.network-manager-tui
```

