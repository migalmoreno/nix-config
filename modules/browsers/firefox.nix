{ config, lib, pkgs, ... }:

{
  home-manager.users.${config.user} = {
    programs.firefox = {
      enable = true;
      policies = {
        FirefoxHome = {
          Search = true;
          TopSites = true;
          SponsoredTopSites = false;
          Highlights = false;
          Pocket = false;
          SponsoredPocket = false;
          Snippets = false;
          Locked = true;
        };
        FirefoxSuggest = {
          WebSuggestions = false;
          SponsoredSuggestions = false;
          ImproveSuggest = false;
          Locked = true;
        };
        SearchEngines = {
          PreventInstalls = true;
          Add = [
            {
              Name = "Whoogle";
              URLTemplate = "https://whoogle.${config.secrets.personal.domain}/search?q={searchTerms}";
              Method = "GET";
            }
          ];
          Remove = [
            "Amazon.com"
            "Bing"
            "Google"
          ];
          Default = "Whoogle";
        };
      };
      profiles = {
        default = {
          id = 0;
          name = "default";
          isDefault = true;
          settings = {
            "browser.aboutConfig.showWarning" = false;
            "browser.aboutwelcome.enabled" = false;
            "browser.formfill.enable" = false;
            "browser.sessionstore" = false;
            "browser.sessionstore.max_tabs_undo" = 0;
            "browser.sessionstore.resume_session_once" = true;
            "browser.topsites.contile.enabled" = false;
            "browser.translations.enable" = false;
            "browser.urlbar.maxRichResults" = 0;
            "browser.urlbar.suggest.addons" = false;
            "browser.urlbar.suggest.engines" = false;
            "browser.urlbar.suggest.history" = false;
            "browser.urlbar.suggest.recentsearches" = false;
            "browser.urlbar.suggest.searches" = false;
            "browser.urlbar.suggest.topsites" = false;
            "extensions.autoDisableScopes" = 0;
            "extensions.enabledScopes" = 15;
            "general.smoothScroll" = false;
            "network.protocol-handler.external.mailto" = false;
            "places.history.enabled" = false;
            "signon.rememberSignons" = false;
            "trailhead.firstrun.branches" = "nofirstrun-empty";
          };
          extensions = with config.nur.repos.rycee.firefox-addons; [
            ublock-origin
            redirector
          ];
        };
      };
    };
  };
}
