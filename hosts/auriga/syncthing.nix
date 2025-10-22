{ config, ... }:

{
  imports = [ ../../profiles/syncthing.nix ];
  sops.secrets = {
    "hosts/auriga/syncthing/key" = { };
    "hosts/auriga/syncthing/cert" = { };
  };
  services.syncthing = {
    key = config.sops.secrets."hosts/auriga/syncthing/key".path;
    cert = config.sops.secrets."hosts/auriga/syncthing/cert".path;
    guiAddress = "0.0.0.0:8384";
    settings = {
      devices.lyra.id = "M4GMJIA-KU75HGM-BTXRUNS-MVXZQFD-N2YG5KQ-6V2RQHZ-CNORKP5-H2WN6AP";
      folders = {
        documents = {
          path = "~/documents";
          devices = [ "lyra" ];
        };
        pictures = {
          path = "~/pictures";
          devices = [ "lyra" ];
        };
        notes = {
          path = "~/notes";
          devices = [ "lyra" ];
        };
        videos = {
          path = "~/videos";
          devices = [ "lyra" ];
        };
        password-store = {
          path = "~/password-store";
          devices = [ "lyra" ];
        };
        projects = {
          path = "~/projects";
          devices = [ "lyra" ];
        };
      };
    };
  };
  profiles.homepage.services."Media and Storage" = [
    {
      "Syncthing" = {
        icon = "syncthing";
        href = "http://${config.networking.hostName}:8384";
      };
    }
  ];
}
