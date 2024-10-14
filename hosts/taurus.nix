{ inputs, overlays, ... }:

with inputs;

nixpkgs.lib.nixosSystem {
  system = "aarch64-linux";
  modules = [
    home-manager.nixosModules.home-manager
    ordenada.nixosModules.ordenada
    (import "${mobile-nixos}/lib/configuration.nix" { device = "oneplus-enchilada"; })
    (
      {
        config,
        lib,
        pkgs,
        ...
      }:
      {
        nixpkgs.overlays = overlays;
        mobile.beautification = {
          silentBoot = false;
          splash = true;
        };
        time.timeZone = "Europe/Madrid";
        networking.firewall.enable = false;
        services.xserver.desktopManager.phosh = {
          enable = true;
          user = "maia";
          group = "users";
        };
        networking.interfaces.wlan0.useDHCP = true;
        networking.hostName = "taurus";
        programs.calls.enable = true;
        environment.systemPackages = with pkgs; [
          chatty
          gnome-console
          megapixels
          portfolio-filemanager
        ];
        ordenada = {
          users.maia = { };
          features = {
            userInfo.username = "maia";
            home = {
              enable = true;
              extraGroups = [
                "dialout"
                "feedbackd"
                "video"
                "wheel"
              ];
            };
            networking.enable = true;
            nix.enable = true;
            ssh = {
              enable = true;
              userAuthorizedKeys = [
                pkgs.secrets.personal.publicSshKey
                pkgs.secrets.work.publicSshKey
              ];
            };
          };
        };
        hardware.enableRedistributableFirmware = true;
        hardware.graphics.enable = true;
        hardware.sensor.iio.enable = true;
        mobile.quirks.qualcomm.sdm845-modem.enable = true;
        mobile.quirks.audio.alsa-ucm-meld = true;
        users.users.root.password = pkgs.secrets.hosts.taurus.password;
        users.users.maia.password = pkgs.secrets.hosts.taurus.password;
        system.stateVersion = "24.05";
      }
    )
  ];
}
