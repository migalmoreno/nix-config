{ inputs, pkgs, ... }:
{
  imports = [ inputs.nixos-wsl.nixosModules.default ];
  wsl = {
    enable = true;
    defaultUser = "saiph";
    interop.register = true;
    wslConf = {
      network.generateResolvConf = true;
      network.generateHosts = true;
      user.default = "saiph";
      wsl2.memory = "24GB";
    };
  };
  systemd.services.wsl-vpnkit = {
    enable = true;
    description = "wsl-vpnkit";
    wantedBy = [ "multi-user.target" ];
    after = [ "network.target" ];
    serviceConfig = {
      ExecStart = "${pkgs.wsl-vpnkit}/bin/wsl-vpnkit";
      Restart = "on-failure";
      KillMode = "mixed";
    };
  };
  home-manager.users.saiph.systemd.user.services.wsl-clipboard = {
    Unit = {
      Description = "WSL clipboard sharing";
      After = [ "sway-session.target" ];
    };
    Install = {
      WantedBy = [ "default.target" ];
    };
    Service = {
      Type = "exec";
      ExecStart = "${pkgs.wl-clipboard}/bin/wl-paste --watch /mnt/c/Windows/System32/clip.exe";
      Restart = "on-failure";
    };
  };
  security.pki.certificateFiles = [ ../../secrets/ca-bundle.crt ];
  virtualisation.docker = {
    daemon.settings = {
      bip = "10.2.20.10/16";
      dns = pkgs.secrets.work.dnsAddresses;
      dns-search = [ (builtins.elemAt pkgs.secrets.work.domains 0) ];
      live-restore = false;
      default-address-pools = [
        {
          base = "10.1.0.0/16";
          size = 24;
        }
      ];
    };
  };
}
