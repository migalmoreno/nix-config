{ config, ... }:

{
  imports = [ ../../profiles/syncthing.nix ];
  sops.secrets = {
    "hosts/orion/syncthing/key".owner = "saiph";
    "hosts/orion/syncthing/cert".owner = "saiph";
  };
  services.syncthing = with config.home-manager.users.saiph; {
    user = "saiph";
    key = config.sops.secrets."hosts/orion/syncthing/key".path;
    cert = config.sops.secrets."hosts/orion/syncthing/cert".path;
    dataDir = "${xdg.dataHome}/syncthing";
    configDir = "${xdg.configHome}/syncthing";
    settings = {
      devices.lyra.id = "M4GMJIA-KU75HGM-BTXRUNS-MVXZQFD-N2YG5KQ-6V2RQHZ-CNORKP5-H2WN6AP";
      folders = {
        work-projects = {
          path = "~/src/work";
          devices = [ "lyra" ];
        };
        public-notes = {
          path = "~/notes";
          devices = [ "lyra" ];
        };
        work-documents = {
          path = "~/documents";
          devices = [ "lyra" ];
        };
      };
    };
  };
}
