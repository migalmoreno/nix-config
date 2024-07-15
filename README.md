

# Nix Configuration

This is my personal set of configuration files built around the [Nix](https://nixos.org/) package manager.  

This repository makes use of [Flakes](https://nix.dev/concepts/flakes.html) to manage and pin its dependencies. You can rebuild the current hostname's entire system configuration and home configuration with this command:  

    sudo nixos-rebuild switch --flake .

If you only want to rebuild the current hostname's home configuration via [Home Manager](https://nix-community.github.io/home-manager/), do this:  

    home-manager switch --flake .

To deploy a `<hostname>` located in a remote `<host>` from a non-NixOS system, use this:  

    nix-shell -p nixos-rebuild --run "nixos-rebuild switch --flake .#<hostname> --fast --build-host <host> --target-host <host> --use-remote-sudo"

