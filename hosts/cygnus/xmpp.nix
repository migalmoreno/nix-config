{
  config,
  pkgs,
  ...
}:

{
  imports = [ ../../services/prosody.nix ];
  disabledModules = [ "services/networking/prosody.nix" ];
  services.prosody = {
    enable = true;
    environmentFile = config.sops.templates."prosody.env".path;
    openFirewall = true;
    modules = {
      "websocket" = true;
    };
    package = pkgs.prosody.override {
      withCommunityModules = [
        "privilege"
      ];
    };
    settings = {
      admins = [ "migalmoreno@migalmoreno.com" ];
      allow_registration = false;
      consider_websocket_secure = true;
    };
    virtualHosts."migalmoreno.com" = {
      useACMEHost = "migalmoreno.com";
      settings = {
        privileged_entities = {
          "slidge-whatsapp.migalmoreno.com" = {
            roster = "both";
            message = "outgoing";
            iq = {
              "http://jabber.org/protocol/pubsub" = "both";
              "http://jabber.org/protocol/pubsub#owner" = "set";
            };
          };
        };
      };
    };
    components = {
      "conference.migalmoreno.com" = {
        module = "muc";
        settings = {
          modules_enabled = [ "vcard_muc" ];
        };
      };
      "upload.migalmoreno.com" = {
        module = "http_upload";
      };
      "slidge-whatsapp.migalmoreno.com" = {
        settings = {
          component_secret = "ENV_SLIDGE_WHATSAPP_SECRET";
          modules_enabled = [ "privilege" ];
        };
      };
    };
  };
  users.users.${config.services.prosody.user}.extraGroups = [
    config.services.nginx.group
    "acme"
  ];
  virtualisation.oci-containers.containers.slidge-whatsapp = {
    image = "codeberg.org/slidge/slidge-whatsapp:main-amd64";
    volumes = [
      "/var/lib/slidge:/var/lib/slidge"
      "${with config.sops.templates."slidge-whatsapp.ini"; "${path}:${path}"}"
    ];
    extraOptions = [
      "--network=host"
    ];
    cmd = [
      "--config=${config.sops.templates."slidge-whatsapp.ini".path}"
    ];
  };
  systemd.tmpfiles.settings."10-slidge"."/var/lib/slidge".d = {
    user = "root";
    group = "root";
    mode = "0777";
  };
  services.nginx.virtualHosts."chat.migalmoreno.com".extraConfig = ''
    client_max_body_size 0;
    proxy_headers_hash_max_size 1024;
    proxy_headers_hash_bucket_size 128;
  '';
  services.movim = {
    enable = true;
    port = 8084;
    debug = false;
    verbose = false;
    domain = "chat.migalmoreno.com";
    podConfig = {
      chatonly = true;
      disableregistration = true;
      xmppdomain = "migalmoreno.com";
    };
    nginx = {
      enableACME = true;
      forceSSL = true;
    };
    phpCfg = {
      "memory_limit" = "2048M";
      "upload_max_filesize" = "2048M";
      "post_max_size" = "2048M";
      "opcache.memory_consumption" = 2048;
    };
    poolConfig = {
      "pm" = "ondemand";
      "php_admin_value[error_log]" = "stderr";
      "php_admin_flag[log_errors]" = true;
      "catch_workers_output" = true;
      "pm.max_children" = 40;
      "pm.start_servers" = 10;
      "pm.min_spare_servers" = 5;
      "pm.max_spare_servers" = 20;
      "pm.max_requests" = 500;
      "php_admin_value[memory_limit]" = "2048M";
    };
  };
  services.nginx.virtualHosts."migalmoreno.com".locations = {
    "^~ /slidge/" = {
      extraConfig = ''
        alias /var/lib/slidge/attachments/;
      '';
    };
  };
  security.acme.certs."migalmoreno.com".extraDomainNames = [
    "conference.migalmoreno.com"
    "upload.migalmoreno.com"
    "slidge-whatsapp.migalmoreno.com"
  ];
  sops = {
    secrets."hosts/cygnus/slidge-whatsapp/secret" = { };
    templates = {
      "prosody.env".content = ''
        SLIDGE_WHATSAPP_SECRET=${config.sops.placeholder."hosts/cygnus/slidge-whatsapp/secret"}
      '';
      "slidge-whatsapp.ini" = {
        content = ''
          secret=${config.sops.placeholder."hosts/cygnus/slidge-whatsapp/secret"}
          jid=slidge-whatsapp.migalmoreno.com
          no-upload-path=/var/lib/slidge/attachments
          no-upload-file-read-others=true
          no-upload-url-prefix=https://migalmoreno.com/slidge
          user-jid-validator=.*@migalmoreno.com
        '';
        mode = "0755";
      };
    };
  };
}
