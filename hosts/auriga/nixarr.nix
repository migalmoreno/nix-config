{
  config,
  inputs,
  lib,
  ...
}:

{
  imports = [ inputs.nixarr.nixosModules.default ];
  nixarr = {
    enable = true;
    jellyfin.enable = true;
    jellyseerr.enable = true;
    lidarr.enable = true;
    readarr.enable = true;
    radarr.enable = true;
    bazarr.enable = true;
    sabnzbd = {
      enable = true;
      guiPort = 8081;
    };
    sonarr.enable = true;
    prowlarr.enable = true;
    transmission = {
      enable = true;
      extraSettings = {
        rpc-host-whitelist-enabled = false;
        rpc-whitelist-enabled = false;
      };
    };
  };
  boot = {
    kernelModules = [ "v4l2loopback" ];
    extraModulePackages = with config.boot.kernelPackages; [ v4l2loopback ];
  };
  users.users.${config.util-nixarr.globals.radarr.user}.extraGroups = [ "video" ];
  systemd.tmpfiles.settings."10-radarr".${config.services.radarr.dataDir}.d = lib.mkForce {
    inherit (config.services.radarr) user group;
    mode = "0775";
  };
  sops.secrets = {
    "hosts/auriga/radarr/api_key" = { };
    "hosts/auriga/sonarr/api_key" = { };
    "hosts/auriga/lidarr/api_key" = { };
    "hosts/auriga/readarr/api_key" = { };
    "hosts/auriga/prowlarr/api_key" = { };
    "hosts/auriga/bazarr/api_key" = { };
    "hosts/auriga/jellyfin/api_key" = { };
    "hosts/auriga/jellyseerr/api_key" = { };
  };
}
