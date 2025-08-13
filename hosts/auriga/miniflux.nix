{ config, ... }:

let
  port = 8086;
in
{
  services.miniflux = {
    enable = true;
    config = {
      LISTEN_ADDR = "0.0.0.0:${toString port}";
      CREATE_ADMIN = 1;
    };
    adminCredentialsFile = config.sops.templates."miniflux.env".path;
  };
  sops = {
    secrets."hosts/auriga/miniflux/admin_password" = { };
    templates."miniflux.env".content = ''
      ADMIN_USERNAME=admin
      ADMIN_PASSWORD=${config.sops.placeholder."hosts/auriga/miniflux/admin_password"} 
    '';
  };
  profiles.homepage.services."Media and Storage" = [
    {
      "Miniflux" = {
        icon = "miniflux";
        href = "http://${config.networking.hostName}:${toString port}";
      };
    }
  ];
}
