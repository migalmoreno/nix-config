{ config, pkgs, ... }:

{
  services.soju = {
    enable = true;
    listen = [
      "irc+insecure://"
      "ws+insecure://0.0.0.0:8080"
    ];
    httpOrigins = [ "*" ];
  };
  networking.firewall.allowedTCPPorts = [ 6667 ];
  services.nginx.virtualHosts."irc.auriga" = {
    root = "${pkgs.gamja}";
    locations."= /config.json".extraConfig = ''
      alias ${pkgs.writeText "gamja-config.json" ''
        {
          "server": {
            "url": "http://auriga:8080",
            "autojoin": [],
            "auth": "optional",
            "nick": "migalmoreno",
            "autoconnect": false,
            "ping": 0
          }
        }
      ''};
    '';
    listen = [
      {
        addr = "0.0.0.0";
        port = 4800;
      }
    ];
  };
  profiles.homepage.services."Communication" = [
    {
      "Gamja" = {
        icon = "irc";
        href = "http://${config.networking.hostName}:4800";
      };
    }
  ];
}
