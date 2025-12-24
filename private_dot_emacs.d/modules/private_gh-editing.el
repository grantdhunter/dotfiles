;;; gh-editing.el --- Editing enhancements -*- lexical-binding: t; -*-

;;; Commentary:
;; Multiple cursors, undo-tree, whitespace, and other editing improvements.

;;; Code:

;; Multiple cursors
(use-package multiple-cursors
  :bind (("C-}" . mc/mark-next-like-this)
         ("C-{" . mc/mark-previous-like-this)
         ("C-|" . mc/mark-all-like-this)))

;; Crux - useful utilities
(use-package crux
  :bind (("C-c r" . crux-rename-file-and-buffer)))

;; Whitespace cleanup on save
(use-package whitespace
  :hook (before-save-hook . delete-trailing-whitespace))

;; Indent guides
(use-package indent-bars
  :hook ((prog-mode yaml-ts-mode) . indent-bars-mode)
  :config
  (require 'indent-bars-ts)
  :custom
  (indent-bars-treesit-support t)
  (indent-bars-width-frac 0.05)
  (indent-bars-pad-frac 0.4)
  (indent-bars-pattern "  .  .")
  (indent-bars-treesit-ignore-blank-lines-types '("module")))

;; Undo tree
(use-package undo-tree
  :config (global-undo-tree-mode))

;; Spellcheck
(use-package flyspell
  :hook ((text-mode . flyspell-mode)
         (prog-mode . flyspell-prog-mode)))

;; Rainbow delimiters for programming
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Window navigation
(global-set-key (kbd "C-x O") 'previous-multiframe-window)

;; Enable downcase-region
(put 'downcase-region 'disabled nil)

(provide 'gh-editing)
;;; gh-editing.el ends here
