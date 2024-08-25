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
      user = "saiph";
      git.username = config.secrets.work.fullname;
      git.email = config.secrets.work.email;
      environment.systemPackages = with pkgs; [
        agenix.packages.${system}.default
        libreoffice
      ];
      sops = {
        defaultSopsFile = ../secrets.yaml;
        defaultSopsFormat = "yaml";
        age = {
          sshKeyPaths = ["/home/${config.user}/.ssh/id_ed25519"];
          keyFile = "/home/${config.user}/.config/sops/age/keys.txt";
          generateKey = false;
        };
        secrets = {};
        templates = {};
      };
      age = {
        identityPaths = [ "/home/${config.user}/.ssh/id_ed25519" ];
        secrets = {};
      };
      networking.hostName = "orion";
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
      users.extraGroups.docker.members = [ config.user ];
      time.timeZone = "Europe/Madrid";
      virtualisation.docker = {
        daemon.settings = {
          bip = "10.2.20.10/16";
          dns = config.secrets.work.dnsAddresses;
          dns-search = [ (builtins.elemAt config.secrets.work.domains 0) ];
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
      boot.binfmt.emulatedSystems = [ "aarch64-linux" ];
      home-manager.users.${config.user} = {
        programs.ssh = {
          enable = true;
          matchBlocks = {
            auriga = {
              hostname = config.secrets.hosts.auriga.address;
              user = "root";
            };
            capella = {
              hostname = config.secrets.hosts.auriga.address;
              user = "capella";
            };
          };
        };
      };
    })
    ../modules/browsers
    ../modules/common
    ../modules/development
    ../modules/networking/syncthing.nix
    ../modules/virtualisation/docker.nix
    ../modules/wm
  ];
}
