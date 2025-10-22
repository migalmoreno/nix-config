{ config, ... }:

{
  imports = [ ../../profiles/syncthing.nix ];
  sops = {
    age.sshKeyPaths = [ "${config.home-manager.users.vega.home.homeDirectory}/.ssh/id_ed25519" ];
    secrets = {
      "hosts/lyra/syncthing/key".owner = "vega";
      "hosts/lyra/syncthing/cert".owner = "vega";
    };
  };
  services.syncthing = with config.home-manager.users.vega; {
    user = "vega";
    key = config.sops.secrets."hosts/lyra/syncthing/key".path;
    cert = config.sops.secrets."hosts/lyra/syncthing/cert".path;
    dataDir = "${xdg.dataHome}/syncthing";
    configDir = "${xdg.configHome}/syncthing";
    settings = {
      devices = {
        auriga.id = "FZVCCHW-DF2SVCK-6NFAH3K-A6TTZ6S-D44H3NZ-SHSC7FV-EQL3R2S-J2CR3QZ";
        orion.id = "JHG7EZC-D52KXLP-AN45CEX-ADKNSST-J4R3XDF-NDI26JH-JIJYZJ5-AEJHFQO";
      };
      folders = {
        password-store = {
          path = "~/.local/state/password-store";
          devices = [ "auriga" ];
        };
        documents = {
          path = "~/documents";
          devices = [ "auriga" ];
        };
        pictures = {
          path = "~/pictures";
          devices = [ "auriga" ];
        };
        notes = {
          path = "~/notes";
          devices = [ "auriga" ];
        };
        videos = {
          path = "~/videos";
          devices = [ "auriga" ];
        };
        projects = {
          path = "~/src/projects";
          devices = [ "auriga" ];
        };
        work-projects = {
          path = "~/src/work";
          devices = [ "orion" ];
        };
        public-notes = {
          path = "~/notes/public";
          devices = [ "orion" ];
        };
        work-documents = {
          path = "~/documents/work";
          devices = [ "orion" ];
        };
      };
    };
  };
}
