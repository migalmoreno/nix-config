{ config, ... }:

{
  imports = [ ../../services/goaccess.nix ];
  services.goaccess = {
    enable = true;
    logFilePath = "/var/log/nginx/access.log";
    logFileFormat = "COMBINED";
    serverHost = "cygnus";
    serverPort = 8081;
    enableNginx = true;
  };
  users.users.${config.services.goaccess.user}.extraGroups = [ config.services.nginx.group ];
  services.nginx.commonHttpConfig = ''
    log_format vcombined '$host:$server_port '
                         '$remote_addr $remote_user [$time_local] '
                         '"$request" $status $body_bytes_sent '
                         '"$http_referer" '
                         '"$http_user_agent"';
    access_log /var/log/nginx/access.log vcombined;
  '';
}
