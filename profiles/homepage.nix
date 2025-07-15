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
  serviceType = types.submodule {
    options = {
      widgets = mkOption { type = types.listOf yamlFormat.type; };
      layout = mkOption { type = types.attrsOf yamlFormat.type; };
    };
  };
in
{
  options.profiles.homepage = {
    services = mkOption {
      type = types.attrsOf serviceType;
      default = { };
    };
    widgets = mkOption {
      type = types.listOf yamlFormat.type;
    };
    environmentFile = mkOption {
      type = types.path;
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
        auriga-podman = {
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
        layout = lib.mapAttrs (_: settings: settings.layout) cfg.services;
      };
      services = lib.mapAttrsToList (category: settings: {
        ${category} = settings.widgets;
      }) cfg.services;
    };
    systemd.services.homepage-dashboard.serviceConfig.Group = "podman";
  };
}
