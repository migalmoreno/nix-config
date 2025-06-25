{
  imports = [ ../../profiles/cgit.nix ];
  services.nginx.virtualHosts."git.migalmoreno.com" = {
    listen = [
      {
        addr = "0.0.0.0";
        port = 4040;
      }
    ];
  };
}
