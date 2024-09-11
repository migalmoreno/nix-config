{ inputs, ... }:

with inputs;

nixpkgs.lib.nixosSystem {
  system = "x86_64-linux";
  modules = [
    nixos-wsl.nixosModules.default
    nur.nixosModules.nur
    home-manager.nixosModules.home-manager
    sops-nix.nixosModules.sops
    (
      {
        lib,
        pkgs,
        config,
        ...
      }:
      {
        user = "saiph";
        git.username = config.secrets.work.fullname;
        git.email = config.secrets.work.email;
        environment.systemPackages = with pkgs; [
          libreoffice
          git-agecrypt
        ];
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
        hardware.graphics.enable = true;
        virtualisation.docker = {
          daemon.settings = {
            bip = "10.2.20.10/16";
            dns = config.secrets.work.dnsAddresses;
            dns-search = [ (builtins.elemAt config.secrets.work.domains 0) ];
            live-restore = false;
            default-address-pools = [
              {
                base = "10.1.0.0/16";
                size = 24;
              }
            ];
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
        sops = {
          age.sshKeyPaths = [ "/home/${config.user}/.ssh/id_ed25519" ];
          secrets = {
            "hosts/orion/syncthing/key".owner = config.user;
            "hosts/orion/syncthing/cert".owner = config.user;
          };
        };
        services.syncthing = with config.home-manager.users.${config.user}; {
          user = config.user;
          key = config.sops.secrets."hosts/orion/syncthing/key".path;
          cert = config.sops.secrets."hosts/orion/syncthing/cert".path;
          dataDir = "${xdg.dataHome}/syncthing";
          configDir = "${xdg.configHome}/syncthing";
          overrideDevices = true;
          overrideFolders = true;
          settings = {
            devices = {
              lyra.id = "M4GMJIA-KU75HGM-BTXRUNS-MVXZQFD-N2YG5KQ-6V2RQHZ-CNORKP5-H2WN6AP";
            };
            folders = {
              work-projects = {
                path = "~/src/work";
                devices = [ "lyra" ];
              };
              work-notes = {
                path = "~/notes";
                devices = [ "lyra" ];
              };
              work-documents = {
                path = "~/documents";
                devices = [ "lyra" ];
              };
            };
          };
        };
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
      }
    )
    ../modules/bash.nix
    ../modules/browsers
    ../modules/desktop
    ../modules/development
    ../modules/home.nix
    ../modules/networking/syncthing.nix
    ../modules/nix.nix
    ../modules/secrets.nix
    ../modules/security/gpg.nix
    ../modules/shellutils.nix
    ../modules/sops.nix
    ../modules/terminals.nix
    ../modules/virtualisation/docker.nix
  ];
}
