;;; gh-tools.el --- Tools and utilities -*- lexical-binding: t; -*-

;;; Commentary:
;; Dired, terminal, Kubernetes, AI assistants, and other tools.

;;; Code:

;;; Dired - file manager
(use-package dired
  :ensure nil
  :straight (:type built-in)
  :commands (dired)
  :hook
  ((dired-mode . dired-hide-details-mode)
   (dired-mode . hl-line-mode))
  :config
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq delete-by-moving-to-trash t)
  (setq dired-dwim-target t))

(use-package dired-subtree
  :after dired
  :bind
  (:map dired-mode-map
        ("<tab>" . dired-subtree-toggle)
        ("TAB" . dired-subtree-toggle)
        ("<backtab>" . dired-subtree-remove)
        ("S-TAB" . dired-subtree-remove))
  :config
  (setq dired-subtree-use-backgrounds nil))

;;; Terminal - Eat
(use-package eat
  :straight (:type git
                   :host codeberg
                   :repo "akib/emacs-eat"
                   :files ("*.el" ("term" "term/*.el") "*.texi"
                           "*.ti" ("terminfo/e" "terminfo/e/*")
                           ("terminfo/65" "terminfo/65/*")
                           ("integration" "integration/*")
                           (:exclude ".dir-locals.el" "*-tests.el"))))

;;; Kubernetes
(use-package kubernetes
  :commands (kubernetes-overview)
  :config
  (setq kubernetes-poll-frequency 3600
        kubernetes-redraw-frequency 3600))

;;; AI assistants
(use-package shell-maker
  :ensure t)

(use-package acp
  :straight (:type git
                   :host github
                   :repo "xenodium/acp.el"))

(use-package agent-shell
  :straight (:type git
                   :host github
                   :repo "xenodium/agent-shell")
  :config
  (setq agent-shell-anthropic-authentication
        (agent-shell-anthropic-make-authentication :login t))
  (setq agent-shell-anthropic-claude-environment
        (agent-shell-make-environment-variables
         "ANTHROPIC_MODEL" "opus"
         "ANTHROPIC_SMALL_FAST_MODEL" "sonnet")))

(use-package claude-code-ide
  :straight (:type git :host github :repo "manzaltu/claude-code-ide.el")
  :bind ("C-c C-'" . claude-code-ide-menu)
  :config
  (claude-code-ide-emacs-tools-setup)
  (setq claude-code-ide-terminal-backend 'eat))

(provide 'gh-tools)
;;; gh-tools.el ends here
