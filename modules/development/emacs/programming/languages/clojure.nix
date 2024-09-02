{ config, lib, pkgs, ... }:

{
  home-manager.users.${config.user} = {
    home.sessionVariables.LEIN_HOME = "${config.home-manager.users.${config.user}.xdg.dataHome}/lein";
    home.packages = with pkgs; [
      clj-kondo
      cljfmt
      clojure
      jdk
      leiningen
    ];
    programs.emacs = {
      extraPackages = epkgs: with epkgs; [
        cider
        clojure-mode
        jarchive
        html-to-hiccup
        clj-deps-new
      ];
      extraConfig = ''
(defun nrde-clojure--disable-eglot-parts ()
  (setq-local eglot-stay-out-of '(flymake eldoc)))

(add-hook 'after-init-hook 'jarchive-setup)

(with-eval-after-load 'cider
  (setq cider-allow-jack-in-without-project t)
  (setq cider-use-xref nil)
  (setq cider-auto-select-error-buffer nil)
  (setq cider-inspector-auto-select-buffer nil)
  (setq cider-auto-select-test-report-buffer nil)
  (setq cider-print-options '(("right-margin" 70) ("length" 50)))
  (setq cider-doc-auto-select-buffer nil))

(with-eval-after-load 'cider-repl
  (define-key cider-repl-mode-map (kbd "C-M-q") 'indent-sexp)
  (setq cider-repl-pop-to-buffer-on-connect 'display-only)
  (setq cider-repl-display-help-banner nil))

(with-eval-after-load 'nrde-keymaps
  (define-key nrde-app-map (kbd "J") 'clj-deps-new))

(with-eval-after-load 'org
  (add-to-list 'org-structure-template-alist
               '("clj" . "src clojure")))

(with-eval-after-load 'ob-core
  (require 'ob-clojure)
  (require 'ob-java)
  (setq org-babel-default-header-args:clojure
        '((:results . "scalar")
          (:session . ""))))

(with-eval-after-load 'ob-clojure
  (setq org-babel-clojure-backend 'cider))

(add-hook 'clojure-mode-hook 'nrde-clojure--disable-eglot-parts)
(with-eval-after-load 'clojure-mode
  (setq clojure-align-forms-automatically t))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(((clojure-mode :language-id "clojure")
                  (clojurec-mode :language-id "clojure")
                  (clojurescript-mode :language-id "clojurescript"))
                 . ("${pkgs.clojure-lsp}/bin/clojure-lsp"))))
'';
    };
  };
}
