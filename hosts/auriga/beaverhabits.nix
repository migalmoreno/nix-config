{ config, ... }:

{
  virtualisation.oci-containers.containers.beaverhabits = {
    image = "daya0576/beaverhabits";
    ports = [ "8087:8080" ];
    volumes = [ "/var/local/data/beaverhabits:/app/.user" ];
    environment = {
      HABITS_STORAGE = "DATABASE";
      INDEX_SHOW_HABIT_COUNT = "true";
      INDEX_HABIT_NAME_COLUMNS = "7";
      INDEX_HABIT_DATE_COLUMNS = "7";
      TRUSTED_LOCAL_EMAIL = "mail@migalmoreno.com";
    };
  };
  systemd.tmpfiles.settings."10-beaverhabits"."/var/local/data/beaverhabits".d = {
    user = "nobody";
    group = "root";
    mode = "0755";
  };
  profiles.homepage.services."Media and Storage" = [
    {
      "Beaver Habit Tracker" = {
        icon = "beaver-habit-tracker";
        href = "http://${config.networking.hostName}:8087";
      };
    }
  ];
}
