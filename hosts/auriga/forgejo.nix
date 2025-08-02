{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.services.forgejo;
  adminCmd = "${lib.getExe cfg.package} admin user";
  user = "migalmoreno";
  pwd = config.sops.secrets."hosts/auriga/forgejo/admin_password";
in
{
  sops = {
    secrets = {
      "hosts/auriga/forgejo/admin_password".owner = "forgejo";
      "hosts/auriga/forgejo/api_token" = { };
    };
    templates."homepage.env".content = "HOMEPAGE_VAR_FORGEJO_API_TOKEN=${
      config.sops.placeholder."hosts/auriga/forgejo/api_token"
    }";
  };
  systemd.services.forgejo.preStart = ''
    ${adminCmd} create --admin --email "root@localhost" --username ${user} --password "$(tr -d '\n' < ${pwd.path})" || true
  '';
  networking.firewall.allowedTCPPorts = [ 8084 ];
  services.forgejo = {
    enable = true;
    database.type = "postgres";
    settings = {
      server = {
        HTTP_PORT = 8084;
        ROOT_URL = "http://${config.networking.hostName}:8084/";
        LANDING_PAGE = "explore";
      };
      service.DISABLE_REGISTRATION = true;
    };
  };
  environment.systemPackages =
    let
      forgejo-cli = pkgs.writeScriptBin "forgejo-cli" ''
        #!${pkgs.runtimeShell}
        cd ${cfg.stateDir}
        sudo=exec
        if [[ "$USER" != forgejo ]]; then
          sudo='exec /run/wrappers/bin/sudo -u ${cfg.user} -g ${cfg.group} --preserve-env=GITEA_WORK_DIR --preserve-env=GITEA_CUSTOM'
        fi
        # Note that these variable names will change
        export GITEA_WORK_DIR=${cfg.stateDir}
        export GITEA_CUSTOM=${cfg.customDir}
        $sudo ${lib.getExe cfg.package} "$@"
      '';
    in
    [ forgejo-cli ];
  profiles.homepage.services."Media and Storage" = [
    {
      "Forgejo" = {
        icon = "forgejo";
        href = "http://${config.networking.hostName}:${toString config.services.forgejo.settings.server.HTTP_PORT}";
        widget = {
          type = "gitea";
          url = "http://${config.networking.hostName}:${toString config.services.forgejo.settings.server.HTTP_PORT}";
          key = "{{HOMEPAGE_VAR_FORGEJO_API_TOKEN}}";
        };
      };
    }
  ];
}
