;;; gh-languages.el --- Language-specific configurations -*- lexical-binding: t; -*-

;;; Commentary:
;; Python, JavaScript/TypeScript, Go, YAML, Markdown, and other language modes.

;;; Code:

;;; Python
(use-package
  python
  :after lsp-mode
  :ensure flycheck
  :preface
  (defun lsp-fix-all ()
    (interactive)
    (condition-case nil
      (lsp-execute-code-action-by-kind "source.fixAll")
    (lsp-no-code-actions
     (when (called-interactively-p 'any)
       (lsp--info "source.fixAll action not available")))))
  :hook ((before-save . lsp-fix-all)
         (before-save . lsp-organize-imports)
         (before-save . lsp-format-buffer)))

(use-package python-pytest)

(use-package uv-mode
  :hook (python-ts-mode . uv-mode-auto-activate-hook))

(use-package
	lsp-pyright
	:after lsp-mode
	:custom (lsp-pyright-langserver-command "basedpyright")
  :hook (python-ts-mode . (lambda ()
                            (require 'lsp-pyright)
                            (lsp-deferred))))


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



;;; Docker
(use-package dockerfile-mode)

;;; Go
(use-package go-mode)

;;; Justfiles
(use-package just-mode)

(provide 'gh-languages)
;;; gh-languages.el ends here
