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
        raspberry-pi."4".apply-overlays-dtmerge.enable = true;
        deviceTree = {
          enable = true;
          filter = "*rpi-4-*.dtb";
        };
      };
      networking.hostName = "auriga";
      networking.interfaces.wlan0.useDHCP = true;
      networking.firewall.enable = false;
      networking.firewall.allowedTCPPorts = [ 80 443 22 ];
      boot = {
        kernelPackages = pkgs.linuxKernel.packages.linux_rpi4;
        initrd.availableKernelModules = [ "xhci_pci" "usbhid" "usb_storage" ];
        loader = {
          systemd-boot.enable = false;
          generic-extlinux-compatible.enable = false;
          efi.canTouchEfiVariables = true;
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
          device = "/dev/disk/by-label/root";
          fsType = "ext4";
          options = [ "noatime" ];
        };
        "/boot" = {
          device = "/dev/disk/by-label/boot";
          fsType = "vfat";
        };
      };
      swapDevices = [
        {
          device = "/dev/disk/by-label/swap";
        };
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
      system.stateVersion = "24.05";
    })
    ../modules/common
    ../modules/networking
    ../modules/networking/docker.nix
  ];
}
