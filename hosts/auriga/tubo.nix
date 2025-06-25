{ config, pkgs, ... }:

{
  sops = {
    secrets."hosts/auriga/tubo/db/password" = { };
    templates = {
      "tubo-backend.env".content = ''
        DB_HOST=tubo-db
        DB_NAME=tubo
        DB_USER=tubo
        DB_PASSWORD=${config.sops.placeholder."hosts/auriga/tubo/db/password"}
      '';
      "tubo-db.env".content = ''
        POSTGRES_DB=tubo
        POSTGRES_USER=tubo
        POSTGRES_PASSWORD=${config.sops.placeholder."hosts/auriga/tubo/db/password"}
      '';
    };
  };
  services.resolved.enable = true;
  virtualisation.podman.defaultNetwork.settings.dns_enabled = true;
  virtualisation.oci-containers.containers = {
    tubo-frontend = {
      image = "migalmoreno/tubo-frontend";
      ports = [ "8083:8080" ];
      environment = {
        BACKEND_URL = "http://auriga:3000";
      };
      extraOptions = [ "--pull=newer" ];
    };
    tubo-backend = {
      image = "migalmoreno/tubo-backend";
      ports = [ "3000:3000" ];
      extraOptions = [ "--pull=newer" ];
      dependsOn = [ "tubo-db" ];
      environmentFiles = [ config.sops.templates."tubo-backend.env".path ];
    };
    tubo-bg-helper = {
      image = "migalmoreno/tubo-bg-helper";
      ports = [ "3005:3005" ];
      extraOptions = [ "--pull=newer" ];
    };
    tubo-db = {
      image = "postgres:16-alpine";
      ports = [ "5432:5432" ];
      volumes = [ "/var/local/db/tubo:/var/lib/postgresql/data" ];
      environmentFiles = [ config.sops.templates."tubo-db.env".path ];
    };
  };
  systemd.services.pgweb = {
    enable = false;
    description = "pgweb";
    wantedBy = [ "multi-user.target" ];
    after = [ "network.target" ];
    serviceConfig = {
      ExecStart = "${pkgs.pgweb}/bin/pgweb";
      Restart = "on-failure";
      KillMode = "mixed";
    };
    environment.PGWEB_DATABASE_URL = "postgres://tubo:tubo@localhost:5432/tubo";
  };
  networking.firewall.allowedTCPPorts = [ 3000 ];
}
