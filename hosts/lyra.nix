{ inputs, overlays, ... }:

with inputs;

nixpkgs.lib.nixosSystem {
  system = "x86_64-linux";
  modules = [
    sops-nix.nixosModules.sops
    ordenada.nixosModules.ordenada
    ../profiles/development.nix
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
        hardware.keyboard.qmk.enable = true;
        services.udev.packages = with pkgs; [
          via
          vial
        ];
        nixpkgs.overlays = overlays;
        environment.systemPackages = with pkgs; [
          emacs
          git
          git-agecrypt
          via
          vial
          podman-compose
          (texlive.combine {
            inherit (pkgs.texlive)
              scheme-basic
              wrapfig
              capt-of
              hyperref
              ec
              geometry
              xcolor
              ulem
              preview
              amsfonts
              etoolbox
              grfext
              natbib
              titling
              titlesec
              fontspec
              enumitem
              plex
              xkeyval
              fontaxes
              metafont
              ;
          })
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
          features = with config.ordenada.features; {
            userInfo = {
              username = "vega";
              gpgPrimaryKey = "5F23F458";
            };
            git.signCommits = true;
            gnupg = {
              enable = true;
              sshKeys = [ "D6B4894600BB392AB2AEDE499CBBCF3E0620B7F6" ];
            };
            password-store.enable = true;
            irc.accounts.soju.network = "auriga";
            mail = {
              enable = true;
              accounts = {
                personal = {
                  primary = true;
                  fqda = "mail@migalmoreno.com";
                  extraConfig = {
                    imap = {
                      host = "mail.gandi.net";
                      port = 993;
                    };
                    smtp = {
                      host = "mail.gandi.net";
                      port = 465;
                    };
                  };
                };
              };
            };
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
              gnus = {
                enable = true;
                topicGroups = {
                  "Personal" = [
                    "nnmaildir+personal:Inbox"
                    "nnmaildir+personal:Drafts"
                    "nnmaildir+personal:Sent"
                    "nnmaildir+personal:Archive"
                    "nnmaildir+personal:Junk"
                    "nnmaildir+personal:Trash"
                  ];
                  "Bug Trackers" = [
                    "nntp+gwene:gmane.comp.gnu.guix.bugs"
                    "nntp+gwene:gmane.comp.gnu.guix.patches"
                  ];
                  "News" = [
                    "nntp+gwene:gwene.rs.lobste"
                    "nntp+gwene:gwene.org.hnrss.newest.points"
                    "nntp+gwene:gwene.net.lwn.headlines.newrss"
                  ];
                  "Inbox" = [ ];
                  "Gnus" = [ ];
                };
                topicTopology = [
                  ''
                    ("Gnus" visible)
                  ''
                  ''
                    (("Inbox" visible)
                     (("Personal" visible nil)))
                  ''
                  ''
                    (("Bug Trackers" visible nil))
                  ''
                  ''
                    (("News" visible nil))
                  ''
                ];
                messageArchiveMethod = [
                  "nnmaildir"
                  "personal"
                ];
                messageArchiveGroup = [
                  ''
                    (".*" "Sent")
                  ''
                ];
                groupParameters = {
                  "^nnmaildir" = {
                    "gcc-self" = "nnmaildir+personal:Sent";
                    "display" = 1000;
                  };
                  "^nntp" = {
                    "display" = 1000;
                  };
                };
                postingStyles = with userInfo; [
                  ''
                    ((header "cc" ".*@debbugs.gnu.org")
                     (To ordenada-gnus-get-article-participants)
                     (cc nil))
                  ''
                  ''
                    ((header "to" ".*@lists.sr.ht")
                     (To ordenada-gnus-get-article-participants)
                     (cc "${email}"))
                  ''
                  ''
                    ("^nntp.+:"
                     (To ordenada-gnus-get-article-participants)
                     (cc "${email}"))
                  ''
                ];
              };
              message.enable = true;
              org-roam = {
                captureTemplates = [
                  ''
                    ("b" "public" plain "%?"
                     :if-new (file+head "public/%<%Y%m%d%H%M%S>-''${slug}.org"
                                        "#+title: ''${title}\n#+filetags: :''${Topic}:\n")
                     :unnarrowed t)
                  ''
                  ''
                    ("t" "private" plain "%?"
                     :if-new (file+head "private/%<%Y%m%d%H%M%S>-''${slug}.org"
                                        "#+title: ''${title}\n#+filetags: :''${Topic}:\n")
                     :unnarrowed t)
                  ''
                ];
                dailiesDirectory = "./";
                dailiesCaptureTemplates = [
                  ''
                    ("b" "public" entry
                     "* %?"
                     :if-new (file+head "public/daily/%<%Y-%m-%d>.org"
                                        "#+title: %<%Y-%m-%d>\n"))
                  ''
                  ''
                    ("t" "private" entry
                     "* %?"
                     :if-new (file+head "private/daily/%<%Y-%m-%d>.org"
                                        "#+title: %<%Y-%m-%d>\n"))
                  ''
                ];
              };
            };
            networking.enable = true;
            tailscale.enable = true;
            qemu.enable = false;
          };
        };
        sops = {
          defaultSopsFile = ../secrets.yaml;
          age.sshKeyPaths = [ "${config.home-manager.users.vega.home.homeDirectory}/.ssh/id_ed25519" ];
          secrets = {
            "hosts/lyra/syncthing/key".owner = "vega";
            "hosts/lyra/syncthing/cert".owner = "vega";
          };
        };
        virtualisation.podman = {
          enable = true;
          autoPrune = {
            enable = true;
            flags = [ "--all" ];
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
            };
            folders = {
              documents = {
                path = "~/documents";
                devices = [ "auriga" ];
              };
              pictures = {
                path = "~/pictures";
                devices = [ "auriga" ];
              };
              notes = {
                path = "~/notes";
                devices = [ "auriga" ];
              };
              videos = {
                path = "~/videos";
                devices = [ "auriga" ];
              };
              work-projects = {
                path = "~/src/work";
                devices = [ "orion" ];
              };
              public-notes = {
                path = "~/notes/public";
                devices = [ "orion" ];
              };
              work-documents = {
                path = "~/documents/work";
                devices = [ "orion" ];
              };
            };
          };
        };
        system.stateVersion = "24.05";
      }
    )
  ];
}
