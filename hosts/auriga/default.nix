{ inputs, overlays, ... }:

inputs.nixpkgs.lib.nixosSystem {
  system = "aarch64-linux";
  specialArgs = { inherit inputs; };
  modules = [
    (
      { config, pkgs, ... }:
      {
        imports = [
          inputs.nixos-hardware.nixosModules.raspberry-pi-4
          ./cgit.nix
          ./filestash.nix
          ./homepage.nix
          ./irc.nix
          ./nixarr.nix
          ./searxng.nix
          ./syncthing.nix
          ./tubo.nix
          ../../profiles/server.nix
        ];
        hardware = {
          enableRedistributableFirmware = true;
          raspberry-pi."4" = {
            apply-overlays-dtmerge.enable = true;
            fkms-3d.enable = true;
          };
        };
        hardware.graphics.enable = true;
        networking = {
          hostName = "auriga";
          useDHCP = false;
          interfaces.wlan0.useDHCP = true;
          firewall.enable = true;
          networkmanager.enable = true;
        };
        nixpkgs = { inherit overlays; };
        boot = {
          kernelPackages = pkgs.linuxKernel.packages.linux_rpi4;
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
        swapDevices = [
          {
            device = "/dev/disk/by-label/SWAP";
          }
          {
            device = "/var/lib/swapfile";
            size = 16 * 1024;
          }
        ];
        time.timeZone = "Europe/Madrid";
        environment.systemPackages = with pkgs; [
          emacs
          git
          rsync
        ];
        users.users = {
          root.openssh.authorizedKeys.keys = [
            pkgs.secrets.personal.publicSshKey
            pkgs.secrets.work.publicSshKey
          ];
          capella = {
            isNormalUser = true;
            extraGroups = [ "wheel" ];
          };
        };
        sops = {
          age.sshKeyPaths = [ "/etc/ssh/ssh_host_ed25519_key" ];
          secrets."hosts/auriga/grafana/password" = { };
          templates."homepage.env".content = ''
            HOMEPAGE_VAR_GRAFANA_PASSWORD=${config.sops.placeholder."hosts/auriga/grafana/password"}
          '';
        };
        services.redlib = {
          enable = true;
          port = 8000;
          settings = {
            "REDLIB_DEFAULT_USE_HLS" = "on";
            "REDLIB_DEFAULT_HIDE_AWARDS" = "on";
            "REDLIB_DEFAULT_HIDE_SCORE" = "on";
            "REDLIB_DEFAULT_REMOVE_DEFAULT_FEEDS" = "on";
          };
        };
        services.adguardhome = {
          enable = false;
          port = 3003;
          settings = { };
        };
        services.grafana = {
          enable = true;
          settings = {
            server = {
              http_addr = "0.0.0.0";
              http_port = 3030;
            };
          };
        };
        services.prometheus = {
          enable = true;
          exporters = {
            node = {
              enable = true;
              enabledCollectors = [ "systemd" ];
            };
          };
          scrapeConfigs = [
            {
              job_name = "node";
              static_configs = [
                {
                  targets = [ "localhost:${toString config.services.prometheus.exporters.node.port}" ];
                }
              ];
            }
          ];
        };
        virtualisation.podman = {
          enable = true;
          autoPrune = {
            enable = true;
            flags = [ "--all" ];
          };
        };
        systemd.services.podman-auto-update = {
          enable = false;
          wantedBy = [ "multi-user.target" ];
        };
        systemd.timers.podman-auto-update = {
          enable = false;
          timerConfig = {
            OnCalendar = "daily";
            RandomizedDelaySec = 900;
            Persistent = true;
          };
        };
        system.stateVersion = "24.05";
      }
    )
  ];
}
