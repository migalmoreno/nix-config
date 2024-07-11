{ config, lib, pkgs, ... }:

{
  home-manager.users.${config.user} = {
    programs.emacs = {
      extraConfig = ''
(eval-when-compile
  (require 'project)
  (require 'xdg))
(defgroup nrde-project nil
  "Custom `project.el' enhancements."
  :group 'nrde)
(defcustom nrde-project-dominating-files '()
  "List of root files that indicate a directory is a project."
  :group 'nrde-project
  :type '(repeat string))

(defun nrde-compilation-buffer-name (mode)
  "Returns the result of `project-prefixed-buffer-name' if inside
project and `compilation--default-buffer-name' if not."
    (if (project-current)
        (project-prefixed-buffer-name mode)
        (compilation--default-buffer-name mode)))

(defun nrde-project-custom-root (dir)
  "Search in project's DIR for a set of project dominating files."
  (let* ((files nrde-project-dominating-files)
         (root (cl-find-if (lambda (file)
                             (locate-dominating-file dir file))
                           files)))
    (when root
      (cons 'explicit (locate-dominating-file dir root)))))

(cl-defmethod project-root ((project (head explicit)))
  "Determine the PROJECT root."
  (cdr project))

(defun nrde-project-compile (&optional comint)
  "Compile current project and choose if buffer will be in COMINT mode."
  (interactive "P")
  (let ((default-directory (project-root (project-current t)))
        (compilation-buffer-name-function
         (or project-compilation-buffer-name-function
             compilation-buffer-name-function)))
    (call-interactively 'compile nil (and comint (vector (list 4))))))

(setq nrde-project-dominating-files '(".project.el" ".dir-locals.el" ".gitignore"))
(add-hook 'project-find-functions 'nrde-project-custom-root)
(add-hook 'project-find-functions 'project-try-vc)
(advice-add 'project-compile :override 'nrde-project-compile)
(with-eval-after-load 'project
  (define-key project-prefix-map "F" 'consult-find)
  (define-key project-prefix-map "R" 'consult-ripgrep)
  (with-eval-after-load 'consult
    (setq consult-project-root-function
          (lambda ()
            (when-let (project (project-current))
              (car (project-roots project))))))
  (setq project-switch-use-entire-map t)
  (setq project-list-file (expand-file-name "emacs/projects" (xdg-cache-home)))
  (setq compilation-buffer-name-function 'nrde-compilation-buffer-name)
  (setq project-compilation-buffer-name-function 'nrde-compilation-buffer-name))
'';
    };
  };
}
