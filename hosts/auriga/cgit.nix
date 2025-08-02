{ config, ... }:

{
  imports = [ ../../profiles/cgit.nix ];
  services.nginx.virtualHosts."git.migalmoreno.com" = {
    listen = [
      {
        addr = "0.0.0.0";
        port = 4040;
      }
    ];
  };
  profiles.homepage.services."Media and Storage" = [
    {
      "Cgit" = {
        icon = "git";
        href = "http://${config.networking.hostName}:4040";
      };
    }
  ];
  services.cgit."git.migalmoreno.com".settings.clone-url =
    "http://${config.networking.hostName}:4040/$CGIT_REPO_URL";
}
