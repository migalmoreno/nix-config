{ config, lib, ... }:

{
  sops = {
    secrets."hosts/auriga/searxng/secret_key" = { };
    templates."searxng.env".content = ''
      SEARXNG_SECRET=${config.sops.placeholder."hosts/auriga/searxng/secret_key"}
    '';
  };
  services.searx = {
    enable = true;
    redisCreateLocally = true;
    settings = {
      general = {
        debug = false;
      };
      server = {
        port = 8888;
        bind_address = "0.0.0.0";
      };
      search = {
        favicon_resolver = "google";
        autocomplete = "google";
        autocomplete_min = 2;
        method = "GET";
      };
      ui = {
        infinite_scroll = true;
        default_theme = "simple";
      };
      categories_as_tabs = {
        general = { };
        images = { };
        videos = { };
        news = { };
      };
      engines = lib.mapAttrsToList (name: value: { inherit name; } // value) {
        "duckduckgo".disabled = true;
        "qwant".disabled = true;
        "startpage".disabled = false;
        "piped".disabled = true;
        "vimeo".disabled = true;
        "dailymotion".disabled = true;
        "library genesis".disabled = false;
        "reddit".disabled = false;
      };
    };
    environmentFile = config.sops.templates."searxng.env".path;
  };
}
