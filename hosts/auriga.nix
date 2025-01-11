{ inputs, overlays, ... }:

with inputs;

nixpkgs.lib.nixosSystem {
  system = "aarch64-linux";
  modules = [
    nixos-hardware.nixosModules.raspberry-pi-4
    home-manager.nixosModules.home-manager
    nixarr.nixosModules.default
    sops-nix.nixosModules.sops
    ordenada.nixosModules.ordenada
    ../services/cgit.nix
    (
      {
        config,
        lib,
        pkgs,
        ...
      }:
      {
        hardware = {
          enableRedistributableFirmware = true;
          raspberry-pi."4" = {
            apply-overlays-dtmerge.enable = true;
            fkms-3d.enable = true;
          };
        };
        hardware.graphics.enable = true;
        networking.hostName = "auriga";
        networking.interfaces.wlan0.useDHCP = true;
        networking.firewall.enable = true;
        nixpkgs.overlays = overlays;
        boot = {
          kernelPackages = pkgs.linuxKernel.packages.linux_rpi4;
          kernelModules = [ "v4l2loopback" ];
          extraModulePackages = with config.boot.kernelPackages; [ v4l2loopback ];
          initrd.availableKernelModules = [
            "xhci_pci"
            "usbhid"
            "usb_storage"
          ];
          loader = {
            generic-extlinux-compatible.enable = false;
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
        swapDevices = [ { device = "/dev/disk/by-label/SWAP"; } ];
        time.timeZone = "Europe/Madrid";
        environment.systemPackages = with pkgs; [
          emacs
          git
          rsync
        ];
        ordenada = {
          users.capella = { };
          features = {
            userInfo.username = "capella";
            home.enable = true;
            bash.enable = true;
            networking.enable = true;
            ssh = {
              enable = true;
              rootAuthorizedKeys = [
                pkgs.secrets.personal.publicSshKey
                pkgs.secrets.work.publicSshKey
              ];
            };
            tailscale.enable = true;
            nix.enable = true;
            docker.enable = true;
          };
        };
        nixarr = {
          enable = true;
          jellyfin.enable = true;
          radarr.enable = true;
          sonarr = {
            enable = true;
            package = pkgs.sonarr.overrideAttrs (lib.const { doCheck = false; });
          };
          prowlarr.enable = true;
          transmission.enable = true;
        };
        users.users.streamer.extraGroups = [ "video" ];
        services.jellyseerr = {
          enable = true;
          openFirewall = true;
        };
        sops = {
          defaultSopsFile = ../secrets.yaml;
          age.sshKeyPaths = [ "/etc/ssh/ssh_host_ed25519_key" ];
          secrets = {
            "hosts/auriga/syncthing/key" = { };
            "hosts/auriga/syncthing/cert" = { };
          };
        };
        systemd.services.syncthing.environment.STNODEFAULTFOLDER = "true";
        services.syncthing = {
          enable = true;
          key = config.sops.secrets."hosts/auriga/syncthing/key".path;
          cert = config.sops.secrets."hosts/auriga/syncthing/cert".path;
          overrideDevices = true;
          overrideFolders = true;
          settings = {
            devices = {
              lyra.id = "M4GMJIA-KU75HGM-BTXRUNS-MVXZQFD-N2YG5KQ-6V2RQHZ-CNORKP5-H2WN6AP";
            };
            folders = {
              documents = {
                path = "~/documents";
                devices = [ "lyra" ];
              };
              pictures = {
                path = "~/pictures";
                devices = [ "lyra" ];
              };
              notes = {
                path = "~/notes";
                devices = [ "lyra" ];
              };
              videos = {
                path = "~/videos";
                devices = [ "lyra" ];
              };
            };
          };
        };
        services.redlib.enable = true;
        services.soju = {
          enable = true;
          listen = [ "irc+insecure://" ];
        };
        services.whoogle-search = {
          enable = true;
          listenAddress = "0.0.0.0";
          extraEnv = {
            WHOOGLE_CONFIG_SEARCH_LANGUAGE = "lang_en";
            WHOOGLE_CONFIG_VIEW_IMAGE = "1";
            WHOOGLE_MINIMAL = "1";
            WHOOGLE_RESULTS_PER_PAGE = "50";
            WHOOGLE_SHOW_FAVICONS = "0";
          };
        };
        networking.firewall.allowedTCPPorts = [
          6667
          3000
        ];
        virtualisation.oci-containers.containers = {
          tubo = {
            image = "migalmoreno/tubo";
            ports = [ "3000:3000" ];
            extraOptions = [ "--network=host" ];
          };
        };
        nix.gc = {
          automatic = true;
          options = "--delete-older-than 30d";
        };
        nixpkgs.config.permittedInsecurePackages = [
          "aspnetcore-runtime-wrapped-6.0.36"
          "aspnetcore-runtime-6.0.36"
          "dotnet-sdk-wrapped-6.0.428"
          "dotnet-sdk-6.0.428"
        ];
        system.stateVersion = "24.05";
      }
    )
  ];
}
