{ inputs, overlays, ... }:

inputs.nixpkgs.lib.nixosSystem {
  system = "x86_64-linux";
  specialArgs = { inherit inputs; };
  modules = [
    (
      { config, pkgs, ... }:
      {
        imports = [ ../profiles/development.nix ];
        ordenada.features = with config.ordenada.features; {
          hostInfo.hostName = "orion";
          userInfo.username = "saiph";
          base.nixosPackages = with pkgs; [
            libreoffice
            git-agecrypt
            pavucontrol
            alacritty
          ];
          syncthing = {
            devices.lyra.id = "M4GMJIA-KU75HGM-BTXRUNS-MVXZQFD-N2YG5KQ-6V2RQHZ-CNORKP5-H2WN6AP";
            folders = {
              work-projects = {
                path = "~/src/work";
                devices = [ "lyra" ];
              };
              public-notes = {
                path = "~/notes";
                devices = [ "lyra" ];
              };
              work-documents = {
                path = "~/documents";
                devices = [ "lyra" ];
              };
            };
          };
          wsl.enable = true;
          git = with pkgs.secrets.work; {
            inherit email;
            username = fullname;
            gitLinkRemotes = {
              "git.${builtins.elemAt domains 0}" = "git-link-gitlab";
              "git-ext.${builtins.elemAt domains 1}" = "git-link-gitlab";
            };
          };
          javascript.node = pkgs.nodejs_20;
          home.autoStartWmOnTty = "/dev/pts/0";
          sway.modifier = "Mod2";
          waybar.modules = with waybar.defaultModules; [
            swayWorkspaces
            swayMode
            swayWindow
            swaync
          ];
          qemu.enable = true;
          age = {
            enable = true;
            identities = [ "${userInfo.homeDirectory}/.ssh/id_ed25519" ];
          };
          passage = {
            enable = true;
            identitiesFile = "${userInfo.homeDirectory}/.ssh/id_ed25519";
          };
        };
        home-manager.users.${config.ordenada.features.userInfo.username}.imports = [
          inputs.ordenada.homeModules.ordenada
          {
            home.stateVersion = config.system.stateVersion;
            dconf.settings = {
              "org/gnome/desktop/wm/preferences" = {
                button-layout = ":minimize,maximize,close";
              };
            };
          }
        ];
        nixpkgs = { inherit overlays; };
        networking.firewall.enable = false;
        boot.binfmt.emulatedSystems = [ "aarch64-linux" ];
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
          tubo-frontend = {
            image = "migalmoreno/tubo-frontend";
            ports = [ "8080:8080" ];
            environment = {
              BACKEND_URL = "http://${pkgs.secrets.hosts.auriga.address}:3000";
            };
          };
        };
        system.stateVersion = "22.05";
      }
    )
  ];
}
