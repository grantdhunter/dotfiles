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
(use-package shell-maker)

(use-package acp
  :straight (:type git
                   :host github
                   :repo "xenodium/acp.el"))


(use-package agent-shell
    :custom
    (agent-shell-file-completion-enabled t)
    (agent-shell-preferred-agent-config (agent-shell-mistral-make-config))

    (agent-shell-anthropic-authentication
     (agent-shell-anthropic-make-authentication :login t))

    (agent-shell-mistral-authentication
     (agent-shell-mistral-make-authentication
      :api-key (lambda ()
                 (string-trim
                    (shell-command-to-string "source ~/.vibe/.env; echo $MISTRAL_API_KEY")))))


     (agent-shell-display-action '((display-buffer-in-side-window)
                                 (side . right)
                                 (slot . 0)
                                 (window-width . 0.4)
                                 (dedicated . t)
                                 (window-parameters . ((no-delete-other-windows . t))))))



    (provide 'gh-tools)
;;; gh-tools.el ends here
