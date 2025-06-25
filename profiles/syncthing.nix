{
  systemd.services.syncthing.environment.STNODEFAULTFOLDER = "true";
  services.syncthing = {
    enable = true;
    overrideDevices = true;
    overrideFolders = true;
  };
}
