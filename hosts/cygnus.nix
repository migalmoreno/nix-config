{ inputs, overlays, ... }:

inputs.nixpkgs.lib.nixosSystem {
  system = "x86_64-linux";
  modules = [
    (
      {
        config,
        lib,
        pkgs,
        ...
      }:
      {
        imports = with inputs; [
          sops-nix.nixosModules.sops
          ../profiles/cgit.nix
          ../profiles/nix.nix
          ../profiles/tailscale.nix
          ../services/prosody.nix
          ../services/goaccess.nix
        ];
        disabledModules = [ "services/networking/prosody.nix" ];
        networking = {
          hostName = "cygnus";
          useDHCP = false;
          firewall = {
            enable = true;
            allowedTCPPorts = [
              22
              80
              443
            ];
          };
          networkmanager.enable = true;
        };
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
        services.openssh = {
          enable = true;
          settings = {
            PasswordAuthentication = false;
            KbdInteractiveAuthentication = false;
            PermitRootLogin = "yes";
          };
        };
        users.users = {
          root.openssh.authorizedKeys.keys = [
            pkgs.secrets.personal.publicSshKey
            pkgs.secrets.work.publicSshKey
          ];
          deneb = {
            isNormalUser = true;
            extraGroups = [ "wheel" ];
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
          tubo-frontend = {
            image = "migalmoreno/tubo-frontend";
            ports = [ "8080:8080" ];
            environment = {
              BACKEND_URL = "https://api.tubo.media";
            };
            extraOptions = [ "--pull=newer" ];
          };
          tubo-backend = {
            image = "migalmoreno/tubo-backend";
            ports = [ "3000:3000" ];
            extraOptions = [
              "--network=container:gluetun"
              "--pull=newer"
            ];
            dependsOn = [
              "gluetun"
              "tubo-db"
            ];
            environmentFiles = [ config.sops.templates."tubo-backend.env".path ];
          };
          tubo-bg-helper = {
            image = "migalmoreno/tubo-bg-helper";
            ports = [ "3005:3005" ];
            extraOptions = [
              "--network=container:gluetun"
              "--pull=newer"
            ];
            dependsOn = [ "gluetun" ];
          };
          tubo-db = {
            image = "postgres:16-alpine";
            extraOptions = [
              "--network=container:gluetun"
            ];
            dependsOn = [ "gluetun" ];
            volumes = [ "/var/local/db/tubo:/var/lib/postgresql/data" ];
            environmentFiles = [ config.sops.templates."tubo-db.env".path ];
          };
          gluetun = {
            image = "qmcgaw/gluetun";
            extraOptions = [
              "--cap-add=NET_ADMIN"
              "--device=/dev/net/tun:/dev/net/tun"
            ];
            ports = [
              "3000:3000"
              "3005:3005"
            ];
            environmentFiles = [ config.sops.templates."gluetun.env".path ];
          };
        };
        sops = {
          defaultSopsFile = ../secrets.yaml;
          age.sshKeyPaths = [ "/etc/ssh/ssh_host_ed25519_key" ];
          secrets = {
            "hosts/cygnus/gluetun/config" = { };
            "hosts/cygnus/tubo/db/password" = { };
          };
          templates."gluetun.env".content = config.sops.placeholder."hosts/cygnus/gluetun/config";
          templates."tubo-backend.env".content = ''
            DB_HOST=tubo-db
            DB_NAME=tubo
            DB_USER=tubo
            DB_PASSWORD=${config.sops.placeholder."hosts/cygnus/tubo/db/password"}
          '';
          templates."tubo-db.env".content = ''
            POSTGRES_DB=tubo
            POSTGRES_USER=tubo
            POSTGRES_PASSWORD=${config.sops.placeholder."hosts/cygnus/tubo/db/password"}
          '';
        };
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
              crawlersBlock = builtins.readFile (
                pkgs.fetchurl {
                  url = "https://raw.githubusercontent.com/ai-robots-txt/ai.robots.txt/refs/heads/main/nginx-block-ai-bots.conf";
                  sha256 = "sha256-06Md2MoG+xYQKFtgFlu8qNDGG/f0XeEPyxvE24ZbO/c=";
                }
              );
            in
            {
              "migalmoreno.com" = {
                enableACME = true;
                forceSSL = true;
                root = "/srv/http/migalmoreno.com";
                extraConfig = ''
                  error_page 404 = /404.html;
                  limit_req zone=ip burst=5 nodelay;
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
                    proxyPass = "http://localhost:8080";
                    extraConfig = ''
                      limit_req zone=ip burst=20 nodelay;
                    '';
                  };
                  "/robots.txt" = robotsTxt;
                };
              };
              "api.tubo.media" = {
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
