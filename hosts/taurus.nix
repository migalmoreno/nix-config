{ inputs, ... }:

with inputs;

nixpkgs.lib.nixosSystem {
  system = "aarch64-linux";
  modules = [
    home-manager.nixosModules.home-manager
    (import "${mobile-nixos}/lib/configuration.nix" {
      device = "oneplus-enchilada";
    })
    ({ config, lib, pkgs, ... }: {
      user = "maia";
      services.openssh = {
        enable = true;
        settings = {
          PasswordAuthentication = true;
          KbdInteractiveAuthentication = false;
          PermitRootLogin = "yes";
        };
      };
      users.users.root = {
        password = config.secrets.hosts.taurus.password;
      };
      mobile.quirks.qualcomm.sdm845-modem.enable = true;
      mobile.quirks.audio.alsa-ucm-meld = true;
      users.users.${config.user} = {
        isNormalUser = true;
        password = config.secrets.hosts.taurus.password;
        extraGroups = [ "dialout" "feedbackd" "video" "wheel" "networkmanager" ];
        openssh.authorizedKeys.keys = [
          config.secrets.personal.publicSshKey
          config.secrets.work.publicSshKey
        ];
      };
      mobile.beautification = {
        silentBoot = false;
        splash = true;
      };
      services.xserver.desktopManager.phosh = {
        enable = true;
        user = config.user;
        group = "users";
      };
      networking.interfaces.wlan0.useDHCP = true;
      networking.networkmanager.enable = true;
      networking.hostName = "taurus";
      programs.calls.enable = true;
      environment.systemPackages = with pkgs; [
        chatty
        gnome-console
        megapixels
        portfolio-filemanager
      ];
      hardware.sensor.iio.enable = true;
    })
    ../modules/common
  ];
}
