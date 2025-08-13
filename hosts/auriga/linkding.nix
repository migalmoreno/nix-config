{ config, ... }:

let
  port = 8089;
in
{
  sops = {
    secrets."hosts/auriga/linkding/db_password" = { };
    secrets."hosts/auriga/linkding/admin_password" = { };
    templates = {
      "linkding.env".content = ''
        LD_DB_ENGINE=postgres
        LD_DB_HOST=linkding-db
        LD_DB_PASSWORD=${config.sops.placeholder."hosts/auriga/linkding/db_password"}
        LD_SUPERUSER_NAME=admin
        LD_SUPERUSER_PASSWORD=${config.sops.placeholder."hosts/auriga/linkding/admin_password"}
      '';
      "linkding-db.env".content = ''
        POSTGRES_DB=linkding
        POSTGRES_USER=linkding
        POSTGRES_PASSWORD=${config.sops.placeholder."hosts/auriga/linkding/db_password"}
      '';
    };
  };
  virtualisation.oci-containers.containers = {
    linkding = {
      image = "sissbruecker/linkding";
      ports = [ "${toString port}:9090" ];
      volumes = [ "/var/local/data/linkding:/etc/linkding/data" ];
      environmentFiles = [ config.sops.templates."linkding.env".path ];
    };
    linkding-db = {
      image = "postgres:16-alpine";
      volumes = [ "/var/local/db/linkding:/var/lib/postgresql/data" ];
      environmentFiles = [ config.sops.templates."linkding-db.env".path ];
    };
  };
  systemd.tmpfiles.settings = {
    "10-linkding"."/var/local/data/linkding".d = {
      user = "root";
      group = "root";
      mode = "0755";
    };
    "20-linkding"."/var/local/db/linkding".d = {
      user = "root";
      group = "root";
      mode = "0755";
    };
  };
  profiles.homepage.services."Media and Storage" = [
    {
      "Linkding" = {
        icon = "linkding";
        href = "http://${config.networking.hostName}:${toString port}";
      };
    }
  ];
}
