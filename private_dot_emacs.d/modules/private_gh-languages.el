;;; gh-languages.el --- Language-specific configurations -*- lexical-binding: t; -*-

;;; Commentary:
;; Python, JavaScript/TypeScript, Go, YAML, Markdown, and other language modes.

;;; Code:

;;; Python
(use-package python
  :after eglot
  :ensure flycheck
  :preface
  (defun eglot-format-buffer-on-save ()
    (add-hook 'before-save-hook #'eglot-format-buffer -10 t))
  (defun eglot-organize-imports ()
    (interactive)
    (eglot-code-actions nil nil "source.organizeImports" t))
  :hook ((python-ts-mode . eglot-format-buffer-on-save)
         (before-save . eglot-organize-imports)))

(use-package python-pytest)

(use-package uv-mode
  :hook (python-mode . uv-mode-auto-activate-hook))

(use-package lsp-pyright
  :after lsp-mode
  :custom (lsp-pyright-langserver-command "pyright")
  :hook (python-ts-mode . (lambda ()
                            (require 'lsp-pyright))))

(use-package lsp-python-ty
  :after lsp-mode
  :hook (python-ts-mode . (lambda ()
                            (require 'lsp-python-ty))))

;;; YAML
(use-package yaml-ts-mode
  :config
  (setq yaml-indent-offset 2)
  (setq lsp-yaml-format-enable nil))

;;; JavaScript/TypeScript
(use-package jest-test-mode
  :ensure t
  :commands jest-test-mode
  :hook (typescript-mode js-mode typescript-tsx-mode))

;;; Markdown
(use-package markdown-mode
  :mode ("README\\.md\\.'" . gfm-mode)
  :init (setq markdown-command "pandoc"))

(use-package impatient-mode)

;;; Docker
(use-package dockerfile-mode)

;;; Go
(use-package go-mode)

;;; Justfiles
(use-package just-mode)

(provide 'gh-languages)
;;; gh-languages.el ends here
