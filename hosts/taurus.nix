{ inputs, ... }:

with inputs;

nixpkgs.lib.nixosSystem {
  system = "aarch64-linux";
  modules = [
    home-manager.nixosModules.home-manager
    (import "${mobile-nixos}/lib/configuration.nix" { device = "oneplus-enchilada"; })
    (
      {
        config,
        lib,
        pkgs,
        ...
      }:
      {
        user = "maia";
        mobile.beautification = {
          silentBoot = false;
          splash = true;
        };
        time.timeZone = "Europe/Madrid";
        networking.firewall.enable = false;
        services.xserver.desktopManager.phosh = {
          enable = true;
          user = config.user;
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
        hardware.enableRedistributableFirmware = true;
        hardware.graphics.enable = true;
        hardware.sensor.iio.enable = true;
        users.users.root = {
          password = config.secrets.hosts.taurus.password;
        };
        mobile.quirks.qualcomm.sdm845-modem.enable = true;
        mobile.quirks.audio.alsa-ucm-meld = true;
        users.users.${config.user} = {
          isNormalUser = true;
          password = config.secrets.hosts.taurus.password;
          extraGroups = [
            "dialout"
            "feedbackd"
            "video"
            "wheel"
            "networkmanager"
          ];
          openssh.authorizedKeys.keys = [
            config.secrets.personal.publicSshKey
            config.secrets.work.publicSshKey
          ];
        };
        system.stateVersion = "24.05";
      }
    )
    ../modules/home.nix
    ../modules/networking
    ../modules/networking/ssh.nix
    ../modules/nix.nix
    ../modules/secrets.nix
  ];
}
