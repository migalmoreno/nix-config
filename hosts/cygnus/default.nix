{ inputs, overlays, ... }:

inputs.nixpkgs.lib.nixosSystem {
  system = "x86_64-linux";
  specialArgs = { inherit inputs; };
  modules = [
    (
      { config, pkgs, ... }:
      {
        imports = [
          ./cgit.nix
          ./goaccess.nix
          ./tubo.nix
          ./xmpp.nix
          ../../profiles/server.nix
        ];
        profiles.tailscale.enable = false;
        nixpkgs = { inherit overlays; };
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
        sops.age.sshKeyPaths = [ "/etc/ssh/ssh_host_ed25519_key" ];
        services.nginx = {
          recommendedTlsSettings = true;
          appendHttpConfig = ''
            limit_req_zone $binary_remote_addr zone=ip:20m rate=10r/s;
            limit_req_status 429;
          '';
          virtualHosts = {
            "migalmoreno.com" = {
              enableACME = true;
              forceSSL = true;
              root = "/srv/http/migalmoreno.com";
              extraConfig = ''
                error_page 404 = /404.html;
                limit_req zone=ip burst=5 nodelay;
                ${config.profiles.nginx.globals.crawlersBlock}
              '';
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
