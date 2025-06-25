{ config, ... }:

{
  imports = [ ../../profiles/cgit.nix ];
  services.anubis.instances."git.migalmoreno.com" = {
    enable = true;
    settings = {
      TARGET = "http://localhost:4040";
    };
  };
  services.nginx.virtualHosts."git.migalmoreno.com" = with config.profiles.nginx.globals; {
    enableACME = true;
    forceSSL = true;
    extraConfig = crawlersBlock;
    locations = {
      "/" = {
        proxyPass = "http://unix:${config.services.anubis.instances."git.migalmoreno.com".settings.BIND}";
        extraConfig = ''
          limit_req zone=ip burst=5 nodelay;
        '';
      };
      "/robots.txt" = robotsTxt;
    };
  };
}
