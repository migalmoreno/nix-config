{ config, pkgs, lib, ... }:

{
  home-manager.users.${config.user} =
    let aspell = (pkgs.aspellWithDicts (dicts: with dicts; [en es]));
    in {
      home.packages = [aspell];
      programs.emacs = {
        extraConfig = ''
(mapcar (lambda (hook)
          (add-hook hook 'flyspell-mode))
        '(org-mode-hook message-mode-hook bibtext-mode-hook))
(mapcar (lambda (hook)
          (add-hook hook 'flyspell-prog-mode))
        '())
(with-eval-after-load 'ispell
  (setq ispell-program-name "${aspell}/bin/aspell")
  (setq ispell-dictionary "en_US"))
(with-eval-after-load 'flyspell
  (setq flyspell-issue-welcome-flag nil)
  (setq flyspell-issue-message-flag nil))
(with-eval-after-load 'dictionary
  (setq dictionary-server "dict.org"))
(with-eval-after-load 'nrde-keymaps
  (define-key nrde-app-map (kbd "d") 'dictionary-search))
'';
      };
    };
}
