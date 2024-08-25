{ inputs, ... }:

with inputs;

nixpkgs.lib.nixosSystem {
  system = "x86_64-linux";
  modules = [
    nur.nixosModules.nur
    home-manager.nixosModules.home-manager
    ({ lib, pkgs, config, ... }: {
      user = "vega";
      networking.hostName = "lyra";
      networking.firewall.enable = false;
      time.timeZone = "Europe/Madrid";
      hardware.firmware = [ pkgs.sof-firmware ];
      environment.systemPackages = with pkgs; [
        emacs
        git
      ];
      git.username = "Miguel Ángel Moreno";
      git.email = "mail@migalmoreno.com";
      boot = {
        initrd.availablkeKernelModules = [
          "xhci_pci"
          "thunderbolt"
          "usb_storage"
          "usbhid"
        ];
        kernelModules = [
          "kvm-intel"
          "i2c-dev"
          "ddcci"
          "ddcci_backlight"
          "v4l2loopback"
        ];
        extraModulePackages = config.boot.kernelPackages; [
          ddcci-driver
          v4l2loopback
        ];
        extraModprobeConfig = ''
        options ddcci dyndbg delay=120
        options ddcci-backlight dyndbg
        options v4l2loopback exclusive_caps=1 max_buffers=2 video_nr=-1
        '';
        loader = {
          efi.canTouchEfiVariables = true;
          efi.efiSysMountPoint = "/boot/efi";
          timeout = 0;
          grub = {
            enable = true;
            efiSupport = true;
            enableCryptodisk = true;
            device = "nodev";
          };
        };
        binfmt.emulatedSystems = [ "aarch64-linux" ];
        initrd.luks.devices."system-root".device = "/dev/disk/by-uuid/0f74821b-da48-4f0c-9f94-f39e646da1bf";
      };
      fileSystems = {
        "/" = {
          device = "/dev/mapper/system-root";
          fsType = "ext4";
          options = [ "noatime" ];
        };
        "/boot/efi" = {
          device = "/dev/nvme0n1p1";
          fsType = "vfat";
        };
      };
      services.udev.extraRules = ''
      ACTION=="add", \
      SUBSYSTEM=="backlight", \
      KERNEL="intel_backlight", \
      MODE="0666", \
      RUN+="${pkgs.coreutils}/bin/chmod a+w /sys/class/backlight/%k/brightness"
      '';
      services.syncthing = {
        enable = true;
        user = config.user;
      };
      services.guix.enable = true;
      systemd.services.syncthing.environment.STNODEFAULTFOLDER = "true";
      services.spice-vdagentd.enable = true;
      virtualisation.spiceUSBRedirection.enable = true;
      system.stateVersion = "24.05";
    })
    ../modules/browsers
    ../modules/common
    ../modules/development
    ../modules/networking
    ../modules/networking/syncthing.nix
    ../modules/virtualisation/docker.nix
    ../modules/wm
  ];
};
