{ inputs, overlays, ... }:

with inputs;

nixpkgs.lib.nixosSystem {
  system = "x86_64-linux";
  modules = [
    nixos-wsl.nixosModules.default
    nur.nixosModules.nur
    home-manager.nixosModules.home-manager
    ordenada.nixosModules.ordenada
    sops-nix.nixosModules.sops
    ../presets/development.nix
    (
      {
        lib,
        pkgs,
        config,
        ...
      }:
      {
        time.timeZone = "Europe/Madrid";
        hardware.graphics.enable = true;
        nixpkgs.overlays = overlays;
        networking.hostName = "orion";
        networking.firewall.enable = false;
        boot.binfmt.emulatedSystems = [ "aarch64-linux" ];
        environment.systemPackages = with pkgs; [
          libreoffice
          git-agecrypt
          pavucontrol
          alacritty
        ];
        ordenada = {
          users.saiph = { };
          features = with config.ordenada.features; {
            userInfo.username = "saiph";
            git = with pkgs.secrets.work; {
              username = fullname;
              email = email;
              gitLinkRemotes = {
                "git.${builtins.elemAt domains 0}" = "git-link-gitlab";
                "git-ext.${builtins.elemAt domains 1}" = "git-link-gitlab";
              };
            };
            sway = {
              autoStartTty = "/dev/pts/0";
              modifier = "Mod2";
            };
            age = {
              enable = true;
              identities = [ "${userInfo.homeDirectory}/.ssh/id_ed25519" ];
            };
            passage = {
              enable = true;
              identitiesFile = "${userInfo.homeDirectory}/.ssh/id_ed25519";
            };
          };
        };
        security.pki.certificateFiles = [ ../secrets/ca-bundle.crt ];
        virtualisation.docker = {
          daemon.settings = {
            bip = "10.2.20.10/16";
            dns = pkgs.secrets.work.dnsAddresses;
            dns-search = [ (builtins.elemAt pkgs.secrets.work.domains 0) ];
            live-restore = false;
            default-address-pools = [
              {
                base = "10.1.0.0/16";
                size = 24;
              }
            ];
          };
        };
        virtualisation.oci-containers.containers = {
          whoogle-search = {
            image = "benbusby/whoogle-search";
            ports = [ "5000:5000" ];
            extraOptions = [ "--network=host" ];
            environment = {
              WHOOGLE_MINIMAL = "1";
              WHOOGLE_CONFIG_VIEW_IMAGE = "1";
              WHOOGLE_RESULTS_PER_PAGE = "50";
              WHOOGLE_CONFIG_SEARCH_LANGUAGE = "lang_en";
            };
          };
        };
        sops = {
          defaultSopsFile = ../secrets.yaml;
          age.sshKeyPaths = [ "${config.home-manager.users.saiph.home.homeDirectory}/.ssh/id_ed25519" ];
          secrets = {
            "hosts/orion/syncthing/key".owner = "saiph";
            "hosts/orion/syncthing/cert".owner = "saiph";
          };
        };
        systemd.services.syncthing.environment.STNODEFAULTFOLDER = "true";
        services.syncthing = with config.home-manager.users.saiph; {
          enable = true;
          user = "saiph";
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
        wsl = {
          enable = true;
          defaultUser = "saiph";
          wslConf = {
            network.generateResolvConf = true;
            network.generateHosts = true;
            user.default = "saiph";
          };
        };
        systemd.services.wsl-vpnkit = {
          enable = true;
          description = "wsl-vpnkit";
          wantedBy = [ "multi-user.target" ];
          after = [ "network.target" ];
          serviceConfig = {
            ExecStart = "${pkgs.wsl-vpnkit}/bin/wsl-vpnkit";
            Restart = "on-failure";
            KillMode = "mixed";
          };
        };
        home-manager.users.saiph = {
          systemd.user.services.wsl-clipboard = {
            Unit = {
              Description = "WSL clipboard sharing";
              After = [ "sway-session.target" ];
            };
            Install = {
              WantedBy = [ "default.target" ];
            };
            Service = {
              Type = "exec";
              ExecStart = "${pkgs.wl-clipboard}/bin/wl-paste --watch /mnt/c/Windows/System32/clip.exe";
              Restart = "on-failure";
            };
          };
        };
        system.stateVersion = "22.05";
      }
    )
  ];
}
