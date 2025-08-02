{ config, inputs, ... }:

{
  imports = [ inputs.filestash-nix.nixosModules.filestash ];
  services.filestash = {
    enable = true;
    settings = {
      general = {
        port = 8334;
        fork_button = false;
        display_hidden = true;
        filepage_default_view = "list";
        filepage_default_sort = "name";
      };
      auth.admin_file = config.sops.secrets."hosts/auriga/filestash/admin_password".path;
      connections = [
        {
          label = "webdav";
          type = "webdav";
          url = with config.services.webdav.settings; "http://${address}:${toString port}";
          username = "";
          password = "";
        }
      ];
    };
  };
  services.webdav = {
    enable = true;
    user = "syncthing";
    settings = {
      address = "0.0.0.0";
      port = 6065;
      directory = "/var/lib/syncthing";
      permissions = "CRUD";
      debug = true;
      cors = {
        enabled = true;
        credentials = true;
        exposed_headers = [
          "Content-Length"
          "Content-Range"
          "Origin"
          "X-Requested-With"
          "Content-Type"
          "Accept"
        ];
      };
    };
  };
  sops.secrets."hosts/auriga/filestash/admin_password" = {
    owner = config.services.filestash.user;
  };
  profiles.homepage.services."Media and Storage" = [
    {
      "Filestash" = {
        icon = "filestash";
        href = "http://${config.networking.hostName}:${toString config.services.filestash.settings.general.port}";
      };
    }
  ];
}
