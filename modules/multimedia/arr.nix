{
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
}
