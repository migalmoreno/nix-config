{ inputs, overlays, ... }:

inputs.nixpkgs.lib.nixosSystem {
  system = "aarch64-linux";
  modules = [
    (
      { config, pkgs, ... }:
      {
        imports = [
          (import "${inputs.mobile-nixos}/lib/configuration.nix" { device = "oneplus-enchilada"; })
          ../../profiles/ssh.nix
          ../../profiles/nix.nix
        ];
        nixpkgs.overlays = overlays;
        time.timeZone = "Europe/Madrid";
        services.xserver.desktopManager.phosh = {
          enable = true;
          user = "maia";
          group = "users";
        };
        networking = {
          hostName = "taurus";
          useDHCP = false;
          firewall.enable = false;
          interfaces.wlan0.useDHCP = true;
          networkmanager.enable = true;
        };
        programs.calls.enable = true;
        environment.systemPackages = with pkgs; [
          chatty
          gnome-console
          megapixels
          portfolio-filemanager
        ];
        users.users = {
          root = {
            password = pkgs.secrets.hosts.taurus.password;
            openssh.authorizedKeys.keys = [
              pkgs.secrets.personal.publicSshKey
              pkgs.secrets.work.publicSshKey
            ];
          };
          maia = {
            isNormalUser = true;
            password = pkgs.secrets.hosts.taurus.password;
            extraGroups = [
              "wheel"
              "dialout"
              "feedbackd"
              "video"
              "wheel"
              "networkmanager"
            ];
          };
        };
        hardware = {
          enableRedistributableFirmware = true;
          graphics.enable = true;
          sensor.iio.enable = true;
        };
        mobile.quirks.qualcomm.sdm845-modem.enable = true;
        mobile.quirks.audio.alsa-ucm-meld = true;
        mobile.beautification = {
          silentBoot = false;
          splash = true;
        };
        system.stateVersion = "24.05";
      }
    )
  ];
}
