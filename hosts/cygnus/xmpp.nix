{ config, pkgs, ... }:

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
    image = "codeberg.org/slidge/slidge-whatsapp";
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
  services.movim = {
    enable = true;
    port = 8084;
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
