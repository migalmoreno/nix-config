{ inputs, ... }:

with inputs;

nixpkgs.lib.nixosSystem {
  system = "aarch64-linux";
  modules = [
    nixos-hardware.nixosModules.raspberry-pi-4
    home-manager.nixosModules.home-manager
    nixarr.nixosModules.default
    ({ config, lib, pkgs, ... }: {
      user = "capella";
      hardware = {
        enableRedistributableFirmware = true;
        raspberry-pi."4" = {
          apply-overlays-dtmerge.enable = true;
          fkms-3d.enable = true;
        };
      };
      networking.hostName = "auriga";
      networking.interfaces.wlan0.useDHCP = true;
      networking.firewall.enable = false;
      networking.firewall.allowedTCPPorts = [ 80 443 22 ];
      boot = {
        kernelPackages = pkgs.linuxKernel.packages.linux_rpi4;
        kernelModules = [ "v4l2loopback" ];
        extraModulePackages = with config.boot.kernelPackages; [ v4l2loopback ];
        initrd.availableKernelModules = [ "xhci_pci" "usbhid" "usb_storage" ];
        loader = {
          efi.canTouchEfiVariables = false;
          timeout = 0;
          grub = {
            enable = true;
            efiInstallAsRemovable = true;
            efiSupport = true;
            device = "nodev";
          };
        };
      };
      fileSystems = {
        "/" = {
          device = "/dev/disk/by-label/ROOT";
          fsType = "ext4";
          options = [ "noatime" ];
        };
        "/boot" = {
          device = "/dev/disk/by-label/BOOT";
          fsType = "vfat";
        };
      };
      swapDevices = [
        {
          device = "/dev/disk/by-label/SWAP";
        }
      ];
      time.timeZone = "Europe/Madrid";
      environment.systemPackages = with pkgs; [
        emacs
        git
        rsync
      ];
      services.openssh = {
        enable = true;
        settings = {
          PasswordAuthentication = false;
          KbdInteractiveAuthentication = false;
          PermitRootLogin = "yes";
        };
      };
      nixarr = {
        enable = true;
        jellyfin = {
          enable = true;
        };
        radarr = {
          enable = true;
        };
        sonarr = {
          enable = true;
        };
        prowlarr = {
          enable = true;
        };
        transmission = {
          enable = true;
        };
      };
      services.syncthing = {
        enable = true;
      };
      systemd.services.syncthing.environment.STNODEFAULTFOLDER = "true";
      users.users.root = {
        openssh.authorizedKeys.keys = [
          config.secrets.personal.publicSshKey
          config.secrets.work.publicSshKey
        ];
      };
      users.users.${config.user} = {
        isNormalUser = true;
        extraGroups = [ "wheel" ];
        openssh.authorizedKeys.keys = [
          config.secrets.personal.publicSshKey
          config.secrets.work.publicSshKey
        ];
      };
      users.users.streamer.extraGroups = [ "video" ];
      system.stateVersion = "24.05";
    })
    ../modules/common
    ../modules/networking
    ../modules/virtualisation/docker.nix
  ];
}
