{
  services.whoogle-search = {
    enable = true;
    listenAddress = "0.0.0.0";
    extraEnv = {
      WHOOGLE_CONFIG_SEARCH_LANGUAGE = "lang_en";
      WHOOGLE_CONFIG_VIEW_IMAGE = "1";
      WHOOGLE_MINIMAL = "1";
      WHOOGLE_RESULTS_PER_PAGE = "50";
      WHOOGLE_SHOW_FAVICONS = "0";
    };
  };
}
