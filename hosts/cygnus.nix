{ inputs, ... }:

with inputs;

nixpkgs.lib.nixosSystem {
  system = "x86_64-linux";
  modules = [
    home-manager.nixosModules.home-manager
    (
      {
        config,
        lib,
        pkgs,
        ...
      }:
      {
        user = "deneb";
        networking.hostName = "cygnus";
        networking.firewall.enable = true;
        time.timeZone = "Europe/Madrid";
        boot = {
          initrd.availableKernelModules = [
            "ahci"
            "xhci_pci"
            "virtio_pci"
            "sr_mod"
            "virtio_blk"
          ];
          loader.efi.canTouchEfiVariables = false;
          loader.grub = {
            enable = true;
            efiInstallAsRemovable = true;
            efiSupport = true;
            devices = [ "nodev" ];
          };
        };
        zramSwap.enable = true;
        environment.systemPackages = with pkgs; [
          curl
          git
          rsync
        ];
        users.users.${config.user} = {
          isNormalUser = true;
          extraGroups = [ "wheel" ];
        };
        users.users.root = {
          openssh.authorizedKeys.keys = [
            config.secrets.personal.publicSshKey
            config.secrets.work.publicSshKey
          ];
        };
        fileSystems = {
          "/" = {
            device = "/dev/disk/by-uuid/b84be386-e9c1-463f-a7f8-6e076d1c1bb0";
            fsType = "ext4";
          };
          "/boot" = {
            device = "/dev/disk/by-uuid/D44D-8B43";
            fsType = "vfat";
          };
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
        services.nginx = {
          appendHttpConfig = ''
            limit_req_zone $binary_remote_addr zone=ip:20m rate=10r/s;
            limit_req_status 429;
          '';
          virtualHosts =
            let
              robotsTxt = {
                extraConfig = ''
                  rewrite ^/(.*)  $1;
                  return 200 "User-agent: *\nDisallow: /";
                '';
              };
            in
            {
              "migalmoreno.com" = {
                enableACME = true;
                forceSSL = true;
                root = "/srv/http/migalmoreno.com";
                extraConfig = ''
                  error_page 404 = /404.html;
                '';
                locations."/robots.txt" = robotsTxt;
              };
              "git.migalmoreno.com" = {
                enableACME = true;
                forceSSL = true;
                locations."/" = {
                  proxyPass = "http://auriga:80";
                };
                locations."/robots.txt" = robotsTxt;
              };
              "whoogle.migalmoreno.com" = {
                enableACME = true;
                forceSSL = true;
                locations."/" = {
                  proxyPass = "http://localhost:5000";
                };
                locations."/robots.txt" = robotsTxt;
              };
              "jellyfin.migalmoreno.com" = {
                enableACME = true;
                forceSSL = true;
                locations."/" = {
                  proxyPass = "http://auriga:8096";
                };
                locations."/robots.txt" = robotsTxt;
              };
              "jellyseerr.migalmoreno.com" = {
                enableACME = true;
                forceSSL = true;
                locations."/" = {
                  proxyPass = "http://auriga:5055";
                };
                locations."/robots.txt" = robotsTxt;
              };
              "tubo.migalmoreno.com" = {
                enableACME = true;
                forceSSL = true;
                locations."/" = {
                  return = "301 https://tubo.media$request_uri";
                };
                locations."/robots.txt" = robotsTxt;
              };
              "tubo.media" = {
                enableACME = true;
                forceSSL = true;
                locations."/" = {
                  proxyPass = "http://localhost:3000";
                  extraConfig = ''
                    limit_req zone=ip burst=20 nodelay;
                  '';
                };
                locations."/robots.txt" = robotsTxt;
              };
            };
        };
      }
    )
    ../modules/home.nix
    ../modules/networking
    ../modules/networking/ssh.nix
    ../modules/networking/tailscale.nix
    ../modules/nginx.nix
    ../modules/nix.nix
    ../modules/secrets.nix
  ];
}
