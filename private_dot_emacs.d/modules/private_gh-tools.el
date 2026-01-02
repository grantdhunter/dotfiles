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

;;; Project
(use-package projectile)

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
    :ensure-system-package
    ;; Add agent installation configs here
    ((claude . "curl -fsSL https://claude.ai/install.sh | bash")
     (claude-code-acp . "npm install -g @zed-industries/claude-code-acp"))
    :custom
    (agent-shell-file-completion-enabled t)
    (agent-shell-preferred-agent-config (agent-shell-anthropic-make-claude-code-config))
    (agent-shell-anthropic-authentication
     (agent-shell-anthropic-make-authentication :login t))
    (agent-shell-display-action '((display-buffer-in-side-window)
                                 (side . right)
                                 (slot . 0)
                                 (window-width . 0.4)
                                 (dedicated . t)
                                 (window-parameters . ((no-delete-other-windows . t))))))

(use-package claude-code-ide
  :straight (:type git :host github :repo "manzaltu/claude-code-ide.el")
  :bind ("C-c C-'" . claude-code-ide-menu)
  :config
  (claude-code-ide-emacs-tools-setup)
  (setq claude-code-ide-terminal-backend 'eat))

    (provide 'gh-tools)
;;; gh-tools.el ends here
