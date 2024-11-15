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
            nginx.enable = true;
          };
        };
        nixarr = {
          enable = true;
          jellyfin.enable = true;
          radarr.enable = true;
          sonarr.enable = true;
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
        services.soju = {
          enable = true;
          listen = [ "irc+insecure://" ];
        };
        networking.firewall.allowedTCPPorts = [ 6667 ];
        services.gitolite = {
          enable = true;
          user = "git";
          group = "git";
          adminPubkey = pkgs.secrets.personal.publicSshKey;
          extraGitoliteRc = ''
            $RC{UMASK} = 0027;
            $RC{GIT_CONFIG_KEYS} = "gitweb\..*";
          '';
        };
        services.cgit."git.migalmoreno.com" = {
          enable = true;
          scanPath = "/var/lib/gitolite/repositories";
          settings = {
            project-list = "/var/lib/gitolite/projects.list";
            section-from-path = true;
            repository-directory = "/var/lib/gitolite/repositories";
            repository-sort = "name";
            case-sensitive-sort = false;
            root-desc = "Miguel Ángel Moreno's Git repositories";
            enable-git-config = true;
            enable-index-links = false;
            enable-index-owner = false;
            enable-commit-graph = true;
            enable-log-filecount = true;
            enable-log-linecount = true;
            readme = ":README";
            remove-suffix = true;
            clone-url = "https://git.migalmoreno.com/$CGIT_REPO_URL";
            about-filter = "${
              config.services.cgit."git.migalmoreno.com".package
            }/lib/cgit/filters/about-formatting.sh";
            source-filter = "${
              config.services.cgit."git.migalmoreno.com".package
            }/lib/cgit/filters/syntax-highlighting.py";
          };
          extraConfig = ''
            readme=:README.md
            readme=:README.org
          '';
        };
        users.users.${config.services.cgit."git.migalmoreno.com".user} = {
          extraGroups = [ "git" ];
        };
        virtualisation.oci-containers.containers = {
          tubo = {
            image = "migalmoreno/tubo";
            ports = [ "3000:3000" ];
            extraOptions = [ "--network=host" ];
          };
          whoogle-search = {
            image = "benbusby/whoogle-search";
            ports = [ "5000:5000" ];
            extraOptions = [ "--network=host" ];
            environment = {
              WHOOGLE_MINIMAL = "1";
              WHOOGLE_CONFIG_VIEW_IMAGE = "1";
              WHOOGLE_RESULTS_PER_PAGE = "50";
              WHOOGLE_CONFIG_SEARCH_LANGUAGE = "lang_en";
            };
          };
        };
        services.nginx.virtualHosts."_" = {
          listen = [
            {
              port = 8080;
              addr = "0.0.0.0";
            }
          ];
          root = "/srv/http/migalmoreno.com";
          extraConfig = ''
            error_page 404 = /404.html;
          '';
          locations."/" = {
            extraConfig = ''
              try_files $uri $uri/ $uri/index.html $uri;
            '';
          };
        };
        nix.gc = {
          automatic = true;
          options = "--delete-older-than 30d";
        };
        system.stateVersion = "24.05";
      }
    )
  ];
}
