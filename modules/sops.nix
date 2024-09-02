{ config, ... }:

{
  sops = {
    defaultSopsFile = ../secrets.yaml;
    defaultSopsFormat = "yaml";
    age.sshKeyPaths = [ "/home/${config.user}/.ssh/id_ed25519" ];
  };
}
