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

;; Eglot - LSP client
(use-package eglot
  :ensure nil
  :straight (:type built-in)
  :hook ((js-ts-mode . eglot-ensure)
         (typescript-ts-mode . eglot-ensure)
         (tsx-ts-mode . eglot-ensure)
         (python-ts-mode . eglot-ensure)
         (yaml-ts-mode . eglot-ensure))
  :custom
  (eglot-autoshutdown t)
  (eglot-send-changes-idle-time 0.5)
  :config
  (add-to-list 'eglot-server-programs
               '(python-ts-mode . ("pyrefly-language-server")))
  (add-to-list 'eglot-server-programs
               '(yaml-ts-mode . ("yaml-language-server" "--stdio"))))

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
               (yaml . ("https://github.com/ikatyang/tree-sitter-yaml" "v0.5.0"))))
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
             (js-json-mode . json-ts-mode)))
    (add-to-list 'major-mode-remap-alist mapping))
  :config
  (gh-setup-install-grammars)
  (setq typescript-ts-mode-indent-offset 2)
  (setq treesit-font-lock-level 2)
  (push '(tsx-ts-mode . typescript-ts-mode-indent-offset) lsp--formatting-indent-alist)
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))

  ;; Combobulate for structured editing
  (use-package combobulate
    :straight (:type git :repo "mickeynp/combobulate" :nonrecursive t)
    :custom
    (combobulate-key-prefix "C-c o")
    :hook ((prog-mode . combobulate-mode))))

(provide 'gh-programming)
;;; gh-programming.el ends here
