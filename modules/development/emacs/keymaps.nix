{
  config,
  lib,
  pkgs,
  ...
}:

let
  utils = import ../../../utils.nix { inherit config lib pkgs; };
in
{
  home-manager.users.${config.user} = {
    programs.emacs = utils.emacsPkg {
      name = "nrde-keymaps";
      description = "nrde keymaps";
      require = true;
      code = ''
        (defvar nrde-app-map nil "Prefix keymap for applications.")
        (define-prefix-command 'nrde-app-map nil)
        (defvar nrde-toggle-map nil
          "Prefix keymap for binding various minor modes for toggling functionality.")
        (define-prefix-command 'nrde-toggle-map nil)
        (define-key mode-specific-map (kbd "a") '("applications" . nrde-app-map))
        (define-key mode-specific-map (kbd "t") '("toggles" . nrde-toggle-map))
        (define-key nrde-toggle-map "f" 'display-fill-column-indicator-mode)
      '';
    };
  };
}
