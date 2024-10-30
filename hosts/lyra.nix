{ inputs, overlays, ... }:

with inputs;

nixpkgs.lib.nixosSystem {
  system = "x86_64-linux";
  modules = [
    nur.nixosModules.nur
    home-manager.nixosModules.home-manager
    sops-nix.nixosModules.sops
    ordenada.nixosModules.ordenada
    ../presets/development.nix
    (
      {
        lib,
        pkgs,
        config,
        ...
      }:
      {
        networking.hostName = "lyra";
        networking.firewall.enable = false;
        time.timeZone = "Europe/Madrid";
        hardware.firmware = [ pkgs.sof-firmware ];
        hardware.enableRedistributableFirmware = true;
        hardware.graphics.enable = true;
        nixpkgs.overlays = overlays;
        environment.systemPackages = with pkgs; [
          emacs
          git
          git-agecrypt
        ];
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
        ordenada = {
          users.vega = { };
          features = {
            userInfo.username = "vega";
            git = {
              signingKey = "5F23F458";
              signCommits = true;
            };
            gnupg = {
              enable = true;
              sshKeys = [ "D6B4894600BB392AB2AEDE499CBBCF3E0620B7F6" ];
            };
            password-store.enable = true;
            sway = {
              autoStartTty = "/dev/tty1";
              modifier = "Mod4";
            };
            kanshi = {
              enable = true;
              settings = [
                {
                  profile.name = "headless";
                  profile.outputs = [
                    {
                      criteria = "eDP-1";
                      status = "enable";
                    }
                  ];
                }
                {
                  profile.name = "single-left";
                  profile.outputs = [
                    {
                      criteria = "eDP-1";
                      status = "disable";
                    }
                    {
                      criteria = "HDMI-A-1";
                      status = "enable";
                    }
                  ];
                }
                {
                  profile.name = "single-right";
                  profile.outputs = [
                    {
                      criteria = "eDP-1";
                      status = "disable";
                    }
                    {
                      criteria = "DP-2";
                      status = "enable";
                    }
                  ];
                }
                {
                  profile.name = "multi";
                  profile.outputs = [
                    {
                      criteria = "eDP-1";
                      status = "disable";
                    }
                    {
                      criteria = "HDMI-A-1";
                      mode = "1920x1080";
                      position = "0,0";
                    }
                    {
                      criteria = "DP-2";
                      mode = "1920x1080";
                      position = "1920,0";
                    }
                  ];
                }
              ];
            };
            emacs = {
              org-roam = {
                captureTemplates = [
                  ''
                    ("w" "work" plain "%?"
                     :if-new (file+head "work/%<%Y%m%d%H%M%S>-''${slug}.org"
                                        "#+title: ''${title}\n#+filetags: :''${Topic}:\n")
                     :unnarrowed t)
                  ''
                  ''
                    ("p" "personal" plain "%?"
                     :if-new (file+head "personal/%<%Y%m%d%H%M%S>-''${slug}.org"
                                        "#+title: ''${title}\n#+filetags: :''${Topic}:\n")
                     :unnarrowed t)
                  ''
                ];
                dailiesDirectory = "./";
                dailiesCaptureTemplates = [
                  ''
                    ("w" "work" entry
                     "* %?"
                     :if-new (file+head "work/daily/%<%Y-%m-%d>.org"
                                        "#+title: %<%Y-%m-%d>\n"))
                  ''
                  ''
                    ("p" "personal" entry
                     "* %?"
                     :if-new (file+head "personal/daily/%<%Y-%m-%d>.org"
                                        "#+title: %<%Y-%m-%d>\n"))
                  ''
                ];
              };
            };
            networking.enable = true;
            tailscale.enable = true;
            qemu.enable = true;
          };
        };
        services.guix.enable = true;
        sops = {
          defaultSopsFile = ../secrets.yaml;
          age.sshKeyPaths = [ "${config.home-manager.users.vega.home.homeDirectory}/.ssh/id_ed25519" ];
          secrets = {
            "hosts/lyra/syncthing/key".owner = "vega";
            "hosts/lyra/syncthing/cert".owner = "vega";
          };
        };
        systemd.services.syncthing.environment.STNODEFAULTFOLDER = "true";
        services.syncthing = with config.home-manager.users.vega; {
          enable = true;
          user = "vega";
          key = config.sops.secrets."hosts/lyra/syncthing/key".path;
          cert = config.sops.secrets."hosts/lyra/syncthing/cert".path;
          dataDir = "${xdg.dataHome}/syncthing";
          configDir = "${xdg.configHome}/syncthing";
          overrideDevices = true;
          overrideFolders = true;
          settings = {
            devices = {
              auriga.id = "FZVCCHW-DF2SVCK-6NFAH3K-A6TTZ6S-D44H3NZ-SHSC7FV-EQL3R2S-J2CR3QZ";
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
        virtualisation.oci-containers.containers = {
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
        system.stateVersion = "24.05";
      }
    )
  ];
}
