{ config, lib, pkgs, ... }:

{
  home-manager.users.${config.user} = {
    programs.emacs = {
      extraPackages = epkgs: with epkgs; [
        org-roam
      ];
      extraConfig = ''
(eval-when-compile
  (let ((org-roam-v2-ack t))
    (require 'org-roam)))
(setq org-roam-v2-ack t)
(setq org-roam-completion-everywhere t)
(setq org-roam-directory "~/notes")
(autoload 'org-roam-db-autosync-enable "org-roam")
(with-eval-after-load 'org-roam
  (setq org-roam-db-location
        (concat (or (getenv "XDG_CACHE_HOME") "~/.cache")
                "/emacs/org-roam.db"))
  (org-roam-db-autosync-enable))
(let ((map mode-specific-map))
  (define-key map (kbd "n n") 'org-roam-buffer-toggle)
  (define-key map (kbd "n f") 'org-roam-node-find)
  (define-key map (kbd "n i") 'org-roam-node-insert)
  (define-key map (kbd "n r") 'org-roam-ref-find)
  (define-key map (kbd "n C") 'org-roam-capture))

(defun rde-org-roam-open-ref ()
  "Prompt you for a list of all ROAM_REFS in the current buffer."
  (interactive)
  (when (derived-mode-p 'org-mode)
    (if-let* ((refs (org-property-values "ROAM_REFS"))
              (choices (mapcar
                        (lambda (x)
                          (org-unbracket-string "[[" "]]" x))
                        (split-string
                         (car (org-property-values "ROAM_REFS"))
                         " ")))
              (node-ref (completing-read
                         "Refs: "
                         (lambda (string pred action)
                           (if (eq action 'metadata)
                               `(metadata
                                 (category . org-roam-ref)
                                 ,(cons 'display-sort-function
                                        'identity))
                             (complete-with-action
                              action choices string pred)))
                         nil 'require-match)))
        node-ref
      (error "No roam refs in this node"))))

(with-eval-after-load 'org
  (let ((map org-mode-map))
    (define-key map (kbd "C-TAB") 'completion-at-point)
    (define-key map (kbd "C-c r r") 'org-roam-ref-add)
    (define-key map (kbd "C-c r R") 'org-roam-ref-remove)
    (define-key map (kbd "C-c r f") 'org-roam-ref-find)
    (define-key map (kbd "C-c r t") 'org-roam-tag-add)
    (define-key map (kbd "C-c r T") 'org-roam-tag-remove)
    (define-key map (kbd "C-c r a") 'org-roam-alias-add)
    (define-key map (kbd "C-c r A") 'org-roam-alias-remove)
    (define-key map (kbd "C-c r O") 'rde-org-roam-open-ref)))

(with-eval-after-load 'window
  (add-to-list 'display-buffer-alist
               `(,(rx "*org-roam*")
                 display-buffer-same-window)))

(autoload  'org-roam-dailies-map "org-roam-dailies" "" nil 'keymap)
(define-key mode-specific-map (kbd "d") 'org-roam-dailies-map)
(with-eval-after-load 'org-roam-dailies
  (setq org-roam-dailies-directory "daily/"))
'';
    };
  };
}
