{ lib, pkgs, ... }:

{
  options.profiles.nginx.globals = lib.mkOption {
    type = lib.types.attrs;
    default = { };
  };
  config = {
    profiles.nginx.globals = rec {
      crawlersBlock = builtins.readFile (
        pkgs.fetchurl {
          url = "https://raw.githubusercontent.com/ai-robots-txt/ai.robots.txt/refs/heads/main/nginx-block-ai-bots.conf";
          sha256 = "sha256-06Md2MoG+xYQKFtgFlu8qNDGG/f0XeEPyxvE24ZbO/c=";
        }
      );
      robotsTxt = {
        extraConfig = ''
          rewrite ^/(.*)  $1;
          return 200 "User-agent: *\nDisallow: /";
        '';
      };
    };
    services.nginx = {
      enable = true;
      enableReload = true;
      recommendedOptimisation = true;
      recommendedGzipSettings = true;
      recommendedProxySettings = true;
    };
  };
}
