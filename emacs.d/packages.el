;;; packages --- dependencies
;;; Commentary:

;;; Code:

(setq package-archives '(("melpa" . "https://melpa.org/packages/")))
;; This is only needed once, near the top of the file
(eval-when-compile
  (require 'use-package))

(use-package lsp-mode
  :config
  (add-hook 'js-mode-hook #'lsp)
  (add-hook 'python-mode-hook #'lsp)
  (add-hook 'rust-mode-hook #'lsp))

(use-package lsp-company)
(use-package lsp-helm)
(use-package material-theme)
(use-package multiple-cursors)
(use-package neotree)
(use-package powerline)
(use-package projectile)
(use-package flycheck)
(use-package rainbow-delimiters)
(use-package magit)




(use-package toml-mode)
(use-package rust-mode)
(use-package terraform-mode)
(use-package company-terraform)
(use-package jinja2-mode)
(use-package json-mode)


(use-package yaml-mode)



;;; packages.el ends here
