{ inputs, overlays, ... }:

with inputs;

nixpkgs.lib.nixosSystem {
  system = "x86_64-linux";
  modules = [
    home-manager.nixosModules.home-manager
    ordenada.nixosModules.ordenada
    (
      {
        config,
        lib,
        pkgs,
        ...
      }:
      {
        networking.hostName = "cygnus";
        networking.firewall.enable = true;
        time.timeZone = "Europe/Madrid";
        nixpkgs.overlays = overlays;
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
        ordenada = {
          users.deneb = { };
          features = {
            userInfo.username = "deneb";
            home.enable = true;
            networking.enable = true;
            ssh = {
              enable = true;
              rootAuthorizedKeys = [
                pkgs.secrets.personal.publicSshKey
                pkgs.secrets.work.publicSshKey
              ];
            };
            tailscale.enable = true;
            nginx.enable = true;
            nix.enable = true;
          };
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
              crawlersBlock = ''
                if ($http_user_agent ~* (Bytespider|Amazonbot|MJ12bot|DotBot|FriendlyCrawler)) {
                  return 403;
                }
              '';
            in
            lib.mkMerge [
              {
                "migalmoreno.com" = {
                  enableACME = true;
                  forceSSL = true;
                  root = "/srv/http/migalmoreno.com";
                  extraConfig = ''
                    error_page 404 = /404.html;
                    ${crawlersBlock}
                  '';
                  locations."/robots.txt" = robotsTxt;
                };
                "tubo.migalmoreno.com" = {
                  enableACME = true;
                  forceSSL = true;
                  extraConfig = crawlersBlock;
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
                      ${crawlersBlock}
                    '';
                  };
                  locations."/robots.txt" = robotsTxt;
                };
              }
              (lib.mkMerge (
                lib.attrsets.mapAttrsToList
                  (name: value: {
                    ${name} = {
                      enableACME = true;
                      forceSSL = true;
                      extraConfig = crawlersBlock;
                      locations."/" = {
                        proxyPass = value;
                      };
                      locations."/robots.txt" = robotsTxt;
                    };
                  })
                  {
                    "whoogle.migalmoreno.com" = "http://auriga:5000";
                    "git.migalmoreno.com" = "http://auriga:80";
                    "jellyseerr.migalmoreno.com" = "http://auriga:5055";
                    "jellyfin.migalmoreno.com" = "http://auriga:8096";
                  }
              ))
            ];
        };
        security.acme = {
          acceptTerms = true;
          defaults.email = pkgs.secrets.personal.email;
        };
      }
    )
  ];
}
