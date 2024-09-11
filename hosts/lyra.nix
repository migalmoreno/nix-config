{ inputs, ... }:

with inputs;

nixpkgs.lib.nixosSystem {
  system = "x86_64-linux";
  modules = [
    nur.nixosModules.nur
    home-manager.nixosModules.home-manager
    sops-nix.nixosModules.sops
    (
      {
        lib,
        pkgs,
        config,
        ...
      }:
      {
        user = "vega";
        networking.hostName = "lyra";
        networking.firewall.enable = false;
        time.timeZone = "Europe/Madrid";
        hardware.firmware = [ pkgs.sof-firmware ];
        hardware.enableRedistributableFirmware = true;
        hardware.graphics.enable = true;
        environment.systemPackages = with pkgs; [
          emacs
          git
          git-agecrypt
        ];
        git.username = "Miguel Ángel Moreno";
        git.email = "mail@migalmoreno.com";
        boot = {
          initrd.availableKernelModules = [
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
          extraModulePackages = with config.boot.kernelPackages; [
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
          initrd.luks.devices = {
            system-root = {
              device = "/dev/disk/by-uuid/0f74821b-da48-4f0c-9f94-f39e646da1bf";
            };
          };
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
        services.guix.enable = true;
        sops = {
          age.sshKeyPaths = [ "/home/${config.user}/.ssh/id_ed25519" ];
          secrets = {
            "hosts/lyra/syncthing/key".owner = config.user;
            "hosts/lyra/syncthing/cert".owner = config.user;
          };
        };
        services.syncthing = with config.home-manager.users.${config.user}; {
          user = config.user;
          key = config.sops.secrets."hosts/lyra/syncthing/key".path;
          cert = config.sops.secrets."hosts/lyra/syncthing/cert".path;
          dataDir = "${xdg.dataHome}/syncthing";
          configDir = "${xdg.configHome}/syncthing";
          overrideDevices = true;
          overrideFolders = true;
          settings = {
            devices = {
              auriga.id = "TA7YNZV-D5NBYBY-AFB2UU5-S4YNLRA-DCVV5CB-QP333MS-K7MVD4O-NIWUBQB";
              orion.id = "JHG7EZC-D52KXLP-AN45CEX-ADKNSST-J4R3XDF-NDI26JH-JIJYZJ5-AEJHFQO";
              taurus.id = "NLIYTEA-LIL3CAW-N62ZGBA-6DRE2ZM-UNPTBLN-J7JQGBY-RVLYJNT-GD6ZTAG";
            };
            folders = {
              documents = {
                path = "~/documents";
                devices = [ "auriga" ];
              };
              pictures = {
                path = "~/pictures";
                devices = [
                  "auriga"
                  "taurus"
                ];
              };
              notes = {
                path = "~/notes";
                devices = [
                  "auriga"
                  "taurus"
                ];
              };
              videos = {
                path = "~/videos";
                devices = [ "auriga" ];
              };
              work-projects = {
                path = "~/src/work";
                devices = [ "orion" ];
              };
              work-notes = {
                path = "~/notes/work";
                devices = [ "orion" ];
              };
              work-documents = {
                path = "~/documents/work";
                devices = [ "orion" ];
              };
            };
          };
        };
        users.users.${config.user} = {
          isNormalUser = true;
          extraGroups = [ "wheel" ];
        };
        home-manager.users.${config.user} = with config.secrets.hosts; {
          programs.ssh = {
            enable = true;
            matchBlocks = {
              auriga = {
                hostname = auriga.address;
                user = "root";
              };
              capella = {
                hostname = auriga.address;
                user = "capella";
              };
              cygnus = {
                hostname = cygnus.address;
                user = "root";
              };
            };
          };
        };
        system.stateVersion = "24.05";
      }
    )
    ../modules/bash.nix
    ../modules/browsers
    ../modules/desktop
    ../modules/development
    ../modules/home.nix
    ../modules/networking
    ../modules/networking/syncthing.nix
    ../modules/nix.nix
    ../modules/secrets.nix
    ../modules/security/gpg.nix
    ../modules/security/password-store.nix
    ../modules/shellutils.nix
    ../modules/sops.nix
    ../modules/terminals.nix
    ../modules/virtualisation/android.nix
    ../modules/virtualisation/docker.nix
    ../modules/virtualisation/qemu.nix
  ];
}
