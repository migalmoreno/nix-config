{ config, ... }:

{
  services.nginx = {
    enable = true;
    enableReload = true;
    recommendedOptimisation = true;
    recommendedGzipSettings = true;
    recommendedProxySettings = true;
    recommendedTlsSettings = true;
  };
  networking.firewall.allowedTCPPorts = [
    80
    443
  ];
  security.acme = {
    acceptTerms = true;
    defaults.email = config.secrets.personal.email;
  };
}
