;;; gh-programming.el --- Programming support -*- lexical-binding: t; -*-

;;; Commentary:
;; Eglot, Tree-sitter, Flycheck, and general programming configuration.

;;; Code:

;; Flycheck - syntax checking
(use-package flycheck
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package consult-flycheck)

;; Magit - Git interface
(use-package magit
  :bind (("C-x g" . magit-status))
  :custom (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;;; git diff
(use-package difftastic
  :after magit
  :straight (:type git
                   :host github
                   :repo "emacsmirror/difftastic")
  :config
  (transient-append-suffix 'magit-diff '(-1 -1)
    [("D" "Difftastic diff (dwim)" difftastic-magit-diff)
     ("S" "Difftastic show" difftastic-magit-show)])
  )

;; LSP Mode - LSP client
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
   (defun gh/lsp-mode-setup-completion()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless))) ;; Configure orderless

   (defun lsp-ui-sideline--compute-height ()
     1)


  :hook ((js-ts-mode . lsp-deferred)
         (typescript-ts-mode . lsp-deferred)
         (tsx-ts-mode . lsp-deferred)
         (python-ts-mode . lsp-deferred)
         (yaml-ts-mode . lsp-deferred)
         (go-ts-mode . lsp-deferred)
         (lsp-completion-mode . gh/lsp-mode-setup-completion))
  :config
  (add-to-list 'lsp-file-watch-ignored-directories "\\.pyenv\\/")
  :custom
  (lsp-keymap-prefix "C-c l")
  (lsp-idle-delay 0.5)
  (lsp-enable-file-watchers t)
  (lsp-file-watch-threshold 2000)
  (lsp-enable-folding nil)
  (lsp-enable-links nil)
  (lsp-enable-snippet nil)
  (lsp-completion-provider :none)
  (lsp-treemacs-sync-mode 1))

;; LSP UI - UI enhancements for LSP
(use-package lsp-ui
  :commands lsp-ui-mode
  :custom
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-position 'at-point)
  (lsp-ui-sideline-enable t)
  (lsp-ui-sideline-show-diagnostics t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-sideline-show-code-actions t)
  :config (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references))

;; LSP consult
(use-package consult-lsp
  :config (define-key lsp-mode-map [remap xref-find-apropos] #'consult-lsp-symbols))

;; LSP Treemacs integration
(use-package lsp-treemacs
  :after lsp-mode
  :commands lsp-treemacs-errors-list)


;; DAP mode - debugger
(use-package dap-mode
  :config
  (setq dap-auto-configure-features '(sessions locals controls tooltip))
  (require 'dap-python)
  (setq dap-python-debugger 'debugpy)
  :hook ((python-mode . dap-mode)
         (python-mode . dap-ui-mode)))

;; Tree-sitter configuration
(use-package treesit
  :straight (:type built-in)
  :preface
  (defun gh-setup-install-grammars ()
    "Install Tree-sitter grammars if they are absent."
    (interactive)
    (dolist (grammar
             '((css . ("https://github.com/tree-sitter/tree-sitter-css" "v0.20.0"))
               (go . ("https://github.com/tree-sitter/tree-sitter-go" "v0.20.0"))
               (html . ("https://github.com/tree-sitter/tree-sitter-html" "v0.20.1"))
               (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "v0.20.1" "src"))
               (json . ("https://github.com/tree-sitter/tree-sitter-json" "v0.20.2"))
               (markdown . ("https://github.com/ikatyang/tree-sitter-markdown" "v0.7.1"))
               (python . ("https://github.com/tree-sitter/tree-sitter-python" "v0.20.4"))
               (rust . ("https://github.com/tree-sitter/tree-sitter-rust" "v0.21.2"))
               (toml . ("https://github.com/tree-sitter/tree-sitter-toml" "v0.5.1"))
               (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "tsx/src"))
               (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "typescript/src"))
               (yaml . ("https://github.com/ikatyang/tree-sitter-yaml" "v0.5.0"))
               (turtle "https://github.com/GordianDziwis/tree-sitter-turtle")))
      (add-to-list 'treesit-language-source-alist grammar)
      (unless (treesit-language-available-p (car grammar))
        (treesit-install-language-grammar (car grammar)))))

  (dolist (mapping
           '((python-mode . python-ts-mode)
             (typescript-mode . typescript-ts-mode)
             (tsx-mode . tsx-ts-mode)
             (js2-mode . js-ts-mode)
             (bash-mode . bash-ts-mode)
             (conf-toml-mode . toml-ts-mode)
             (go-mode . go-ts-mode)
             (css-mode . css-ts-mode)
             (json-mode . json-ts-mode)
             (js-json-mode . json-ts-mode)
             (yaml-mode . yaml-ts-mode)))
    (add-to-list 'major-mode-remap-alist mapping))
  :config
  (gh-setup-install-grammars)
  (setq typescript-ts-mode-indent-offset 2)
  (setq treesit-font-lock-level 2)

  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode)))

  ;; Combobulate for structured editing
(use-package combobulate
  :straight (:type git :repo "mickeynp/combobulate" :nonrecursive t)
  :custom
  (combobulate-key-prefix "C-c o")
  :hook ((prog-mode . combobulate-mode)))

(use-package treesit-fold
  :straight (treesit-fold :type git :host github :repo "emacs-tree-sitter/treesit-fold"))
(use-package treesit-fold-indicators
:straight (treesit-fold-indicators :type git :host github :repo "emacs-tree-sitter/treesit-fold"))
(provide 'gh-programming)
;;; gh-programming.el ends here
