{ inputs, ... }:

with inputs;

nixpkgs.lib.nixosSystem {
  system = "x86_64-linux";
  modules = [
    nixos-wsl.nixosModules.default
    nur.nixosModules.nur
    home-manager.nixosModules.home-manager
    sops-nix.nixosModules.sops
    agenix.nixosModules.default
    ({ lib, pkgs, config, ... }: {
      user = "nixos";
      environment.systemPackages = with pkgs; [
        ssh-to-age
        sops
        vim
        agenix.packages.${system}.default
        git-agecrypt
      ];
      sops = {
        defaultSopsFile = ../secrets.yaml;
        defaultSopsFormat = "yaml";
        age = {
          sshKeyPaths = ["/home/${config.user}/.ssh/id_ed25519"];
          keyFile = "/home/${config.user}/.config/sops/age/keys.txt";
          generateKey = false;
        };
        secrets = {
          "work/email".owner = "root";
          "work/ca-bundle.crt" = {};
          "work/dnsAddresses" = {};
        };
        templates = {
          "work/ca-bundle.crt" = {
            content = ''${config.sops.placeholder."work/ca-bundle.crt"}'';
            owner = "root";
          };
        };
      };
      age = {
        identityPaths = [ "/home/${config.user}/.ssh/id_ed25519" ];
        secrets = {};
      };
      networking.firewall.enable = false;
      security.pki.certificateFiles = [ ../secrets/ca-bundle.crt ];
      systemd.services.wsl-vpnkit = {
        enable = true;
        description = "wsl-vpnkit";
        after = [ "network.target" ];

        serviceConfig = {
          ExecStart = "${pkgs.wsl-vpnkit}/bin/wsl-vpnkit";
          Restart = "always";
          KillMode = "mixed";
        };
      };
      system.stateVersion = "22.05";
      users.extraGroups.docker.members = [ "nixos" ];
      time.timeZone = "Europe/Madrid";
      virtualisation.docker = {
        enable = true;
        daemon.settings = {
          bip = "10.2.20.10/16";
          dns = config.secrets.work.dnsAddresses;
          dns-search = [ config.secrets.work.domain ];
          live-restore = false;
          default-address-pools = [{ base = "10.1.0.0/16"; size = 24; }];
        };
      };
      wsl = {
        enable = true;
        defaultUser = config.user;
        wslConf = {
          network.generateResolvConf = true;
          network.generateHosts = true;
          user.default = config.user;
        };
      };
      home-manager.users.${config.user}.home.packages = [
        pkgs.libreoffice
      ];
    }
    )
    ../modules/browsers
    ../modules/common
    ../modules/development
    ../modules/networking
    ../modules/wm
  ];
}
