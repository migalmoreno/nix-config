{ config, ... }:

{
  sops = {
    secrets = {
      "hosts/cygnus/tubo/db/password" = { };
    };
    templates = {
      "tubo-db.env".content = ''
        POSTGRES_DB=tubo
        POSTGRES_USER=tubo
        POSTGRES_PASSWORD=${config.sops.placeholder."hosts/cygnus/tubo/db/password"}
      '';
    };
  };
  services.resolved.enable = true;
  virtualisation.podman.defaultNetwork.settings.dns_enabled = true;
  virtualisation.oci-containers.containers = {
    tubo-db = {
      image = "postgres:16-alpine";
      volumes = [ "/var/local/db/tubo:/var/lib/postgresql/data" ];
      environmentFiles = [ config.sops.templates."tubo-db.env".path ];
    };
  };
}
