{ inputs, overlays, ... }:

inputs.nixpkgs.lib.nixosSystem {
  system = "x86_64-linux";
  specialArgs = { inherit inputs; };
  modules = [
    (
      { config, pkgs, ... }:
      {
        imports = [
          ../../profiles/development.nix
          ../../profiles/sops.nix
          ./syncthing.nix
          ./wsl.nix
        ];
        time.timeZone = "Europe/Madrid";
        hardware.keyboard.qmk.enable = true;
        services.udev.packages = with pkgs; [
          via
          vial
        ];
        nixpkgs = { inherit overlays; };
        networking.hostName = "orion";
        networking.firewall.enable = false;
        boot.binfmt.emulatedSystems = [ "aarch64-linux" ];
        environment.systemPackages = with pkgs; [
          libreoffice
          git-agecrypt
          pavucontrol
          alacritty
          via
          vial
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
            javascript.node = pkgs.nodejs_20;
            sway = {
              autoStartTty = "/dev/pts/0";
              modifier = "Mod2";
            };
            waybar.modules = with config.ordenada.features.waybar.defaultModules; [
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
            irc.accounts.soju.network = pkgs.secrets.hosts.auriga.address;
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
        sops.age.sshKeyPaths = [ "${config.ordenada.features.userInfo.homeDirectory}/.ssh/id_ed25519" ];
        system.stateVersion = "22.05";
      }
    )
  ];
}
