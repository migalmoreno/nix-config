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
          ../../profiles/server.nix
          ./cgit.nix
          ./filestash.nix
          ./forgejo.nix
          ./homepage.nix
          ./irc.nix
          ./nixarr.nix
          ./searxng.nix
          ./syncthing.nix
          ./tubo.nix
        ];
        profiles.tailscale.enable = true;
        hardware = {
          enableRedistributableFirmware = true;
          raspberry-pi."4" = {
            apply-overlays-dtmerge.enable = true;
            fkms-3d.enable = true;
          };
          graphics.enable = true;
        };
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
          secrets."hosts/auriga/pass-web/admin_password" = { };
          secrets."hosts/auriga/pass-web/secret_key" = { };
          templates."homepage.env".content = "HOMEPAGE_VAR_GRAFANA_PASSWORD=${
            config.sops.placeholder."hosts/auriga/grafana/password"
          }";
          templates."pass-web.env".content = ''
            ADMIN_PASSWORD=${config.sops.placeholder."hosts/auriga/pass-web/admin_password"}
            JWT_SECRET_KEY=${config.sops.placeholder."hosts/auriga/pass-web/secret_key"}
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
        virtualisation.containers.registries.insecure = [ "auriga:8084" ];
        virtualisation.oci-containers.containers = {
          pass-web-frontend = {
            image = "auriga:8084/migalmoreno/pass-web-frontend";
            ports = [ "8085:80" ];
            extraOptions = [ "--pull=newer" ];
          };
          pass-web-backend = {
            image = "auriga:8084/migalmoreno/pass-web-backend";
            ports = [ "8001:8000" ];
            extraOptions = [ "--pull=newer" ];
            environment = {
              ADMIN_USERNAME = "migalmoreno";
            };
            volumes = [
              "/var/lib/syncthing/password-store:/home/user/.password-store:ro"
              "/var/local/data/pass-web/gnupg:/tmp/host-gnupg:ro"
            ];
            environmentFiles = [ config.sops.templates."pass-web.env".path ];
          };
        };
        profiles.homepage.services."Media and Storage" = [
          {
            "Password Store" = {
              icon = "mdi-lock";
              href = "http://${config.networking.hostName}:8085";
            };
          }
        ];
        system.stateVersion = "24.05";
      }
    )
  ];
}
