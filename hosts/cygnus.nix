{ inputs, overlays, ... }:

with inputs;

nixpkgs.lib.nixosSystem {
  system = "x86_64-linux";
  modules = [
    home-manager.nixosModules.home-manager
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
        networking.firewall.allowedTCPPorts = [
          80
          443
        ];
        services.nginx = {
          enable = true;
          enableReload = true;
          recommendedOptimisation = true;
          recommendedGzipSettings = true;
          recommendedProxySettings = true;
          recommendedTlsSettings = true;
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
              botNames = [
                "Bytespider"
                "AmazonBot"
                "MJ12bot"
                "DotBot"
                "FriendlyCrawler"
                "GPTBot"
                "AhrefsBot"
                "bingbot"
                "ClaudeBot"
                "Googlebot"
              ];
              crawlersBlock = ''
                if ($http_user_agent ~* (${lib.concatStringsSep "|" botNames})) {
                  return 403;
                }
              '';
            in
            {
              "migalmoreno.com" = {
                enableACME = true;
                forceSSL = true;
                root = "/srv/http/migalmoreno.com";
                extraConfig = ''
                  error_page 404 = /404.html;
                  ${crawlersBlock}
                '';
              };
              "git.migalmoreno.com" = {
                enableACME = true;
                forceSSL = true;
                extraConfig = crawlersBlock;
                locations = {
                  "/" = {
                    proxyPass = "http://localhost:4040";
                    extraConfig = ''
                      limit_req zone=ip burst=5 nodelay;
                    '';
                  };
                  "/robots.txt" = robotsTxt;
                };
              };
              "tubo.media" = {
                enableACME = true;
                forceSSL = true;
                extraConfig = crawlersBlock;
                locations = {
                  "/" = {
                    proxyPass = "http://localhost:3000";
                    extraConfig = ''
                      limit_req zone=ip burst=20 nodelay;
                    '';
                  };
                  "/robots.txt" = robotsTxt;
                };
              };
            };
        };
        security.acme = {
          acceptTerms = true;
          defaults.email = pkgs.secrets.personal.email;
        };
        system.stateVersion = "24.11";
      }
    )
  ];
}
