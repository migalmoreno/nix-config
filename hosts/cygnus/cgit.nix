{ config, lib, ... }:

let
  cgitrcLine =
    name: value:
    "${name}=${
      if value == true then
        "1"
      else if value == false then
        "0"
      else
        toString value
    }";
  cgitrcEntry =
    name: value: if lib.isList value then map (cgitrcLine name) value else [ (cgitrcLine name value) ];
in
{
  imports = [ ../../profiles/cgit.nix ];
  environment.etc."cgitrc".text = with config.services.cgit."git.migalmoreno.com"; ''
    # global settings
    ${lib.concatStringsSep "\n" (
      lib.flatten (lib.mapAttrsToList cgitrcEntry ({ virtual-root = nginx.location; } // settings))
    )}
    ${lib.optionalString (scanPath != null) (cgitrcLine "scan-path" scanPath)}

    # repository settings
    ${lib.concatStrings (
      lib.mapAttrsToList (url: settings: ''
        ${cgitrcLine "repo.url" url}
        ${lib.concatStringsSep "\n" (lib.mapAttrsToList (name: cgitrcLine "repo.${name}") settings)}
      '') repos
    )}

    # extra config
    ${extraConfig}
  '';
  services.anubis.instances."cgit" = {
    user = "cgit";
    group = "nginx";
    botPolicy = {
      bots = [
        {
          name = "cgit-expensive";
          path__regex = "^/.+/(refs|log|tree|commit|diff)/.*$";
          action = "CHALLENGE";
        }
      ];
      dnsbl = true;
    };
    settings = {
      TARGET = "unix:///run/uwsgi/http_cgit.sock";
    };
  };
  services.uwsgi = {
    enable = true;
    user = "cgit";
    group = "nginx";
    plugins = [
      "cgi"
    ];
    instance = {
      type = "emperor";
      vassals.cgit = {
        chmod-socket = 660;
        type = "normal";
        master = "true";
        logger = "syslog";
        socket = "/run/uwsgi/uwsgi_cgit.sock";
        http-socket = "/run/uwsgi/http_cgit.sock";
        http-modifier1 = 9;
        http-socket-modifier1 = 9;
        procname-master = "uwsgi cgit";
        processes = 1;
        threads = 2;
        plugins = [ "cgi" ];
        cgi = "${config.services.cgit."git.migalmoreno.com".package}/cgit/cgit.cgi";
      };
    };
  };
  services.cgit."git.migalmoreno.com".settings.clone-url =
    "https://git.migalmoreno.com/$CGIT_REPO_URL";
  services.nginx.virtualHosts."git.migalmoreno.com" = with config.profiles.nginx.globals; {
    enableACME = true;
    forceSSL = true;
    extraConfig = crawlersBlock;
    locations = {
      "/" = lib.mkForce {
        proxyPass = "http://unix:${config.services.anubis.instances."cgit".settings.BIND}";
        extraConfig = ''
          proxy_set_header Host $host;
          proxy_set_header X-Forwarded-Proto $scheme;
          proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
          proxy_set_header X-Real-Ip $remote_addr;

          limit_req zone=ip burst=5 nodelay;
          include ${config.services.nginx.package}/conf/uwsgi_params;
          uwsgi_param HTTP_HOST $host;
          uwsgi_modifier1 9;
          uwsgi_pass unix:/run/uwsgi/uwsgi_cgit.sock;
        '';
      };
      "/robots.txt" = robotsTxt;
      "~ /.+/(info/refs|git-upload-pack)" = lib.mkForce { };
    };
  };
}
