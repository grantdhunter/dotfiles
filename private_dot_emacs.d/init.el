;;; init.el --- Grant Hunter's Emacs Configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Modular Emacs configuration.
;; Each module in the `modules/' directory handles a specific aspect of the setup.

;;; Code:

;; Add modules directory to load path
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

;; Load configuration modules
(require 'gh-core)        ; Core settings, defaults, performance
(require 'gh-packages)    ; Package management (straight.el)
(require 'gh-ui)          ; Theme, fonts, icons
(require 'gh-completion)  ; Vertico, Corfu, Consult, Marginalia
(require 'gh-editing)     ; Multiple cursors, undo-tree, whitespace
(require 'gh-tools)       ; Dired, terminal, Kubernetes, AI tools
(require 'gh-programming) ; Eglot, Tree-sitter, Flycheck, Magit
(require 'gh-languages)   ; Python, TypeScript, Go, YAML, etc.
(require 'gh-org)         ; Org-mode configuration

;;; init.el ends here
