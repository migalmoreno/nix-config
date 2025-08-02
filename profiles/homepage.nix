{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib) mkOption types;
  cfg = config.profiles.homepage;
  yamlFormat = pkgs.formats.yaml { };
in
{
  options.profiles.homepage = {
    services = mkOption {
      type = types.attrsOf yamlFormat.type;
    };
    widgets = mkOption {
      type = types.listOf yamlFormat.type;
    };
    environmentFile = mkOption {
      type = types.path;
    };
    layout = mkOption {
      type = types.listOf yamlFormat.type;
    };
  };
  config = {
    services.homepage-dashboard = {
      enable = true;
      allowedHosts = "*";
      environmentFile = cfg.environmentFile;
      widgets = [
        {
          resources = {
            cpu = true;
            memory = true;
            disk = "/";
            cputemp = true;
            uptime = true;
            expanded = true;
          };
        }
      ] ++ cfg.widgets;
      docker = {
        podman = {
          socket = "/var/run/podman/podman.sock";
        };
      };
      settings = {
        target = "_self";
        headerStyle = "boxedWidgets";
        statusStyle = "dot";
        color = "slate";
        disableCollapse = true;
        hideVersion = true;
        disableUpdateCheck = true;
        layout = cfg.layout;
      };
      services = lib.mapAttrsToList (category: widgets: {
        ${category} = widgets;
      }) cfg.services;
    };
    systemd.services.homepage-dashboard.serviceConfig.Group = "podman";
  };
}
