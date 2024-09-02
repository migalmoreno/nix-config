{ config, pkgs, ... }:

{
  home-manager.users.${config.user} = {
    home.packages = with pkgs; [
      nil
      nixfmt-rfc-style
    ];
    programs.emacs = {
      extraPackages = epkgs: [ epkgs.nix-mode ];
      extraConfig = ''
(add-hook 'nix-mode-hook #'eglot-ensure)
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs '(nix-mode . ("nil"))))
'';
    };
  };
}
