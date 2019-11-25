;;; packages --- dependencies
;;; Commentary:

;;; Code:
(require 'package)

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(defvar my-packages '(
                      company
                      company-terraform
                      crux
                      dumb-jump
                      elpy
                      exec-path-from-shell
                      flycheck
                      helm-projectile
                      importmagic
                      jinja2-mode
                      json-mode
                      magit
                      material-theme
                      multiple-cursors
                      neotree
                      powerline
                      projectile
                      racer
                      rainbow-delimiters
                      rust-mode
                      terraform-mode
                      toml-mode
                      writegood-mode
                      yaml-mode
                      helm
                      ))

(dolist (p my-packages)
  (unless (package-installed-p p)
    (package-refresh-contents)
    (package-install p))
  (add-to-list 'package-selected-packages p))
;;; packages.el ends here
