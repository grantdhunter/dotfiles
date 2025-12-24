;;; gh-completion.el --- Minibuffer and completion settings -*- lexical-binding: t; -*-

;;; Commentary:
;; Vertico, Corfu, Consult, Marginalia, and Orderless configuration.

;;; Code:

;; Vertico - vertical completion UI
(use-package vertico
  :init
  (vertico-mode))

;; Marginalia - rich annotations in minibuffer
(use-package marginalia
  :init (marginalia-mode)
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle)))

;; Orderless - flexible completion style
(use-package orderless
  :config
  (setq completion-styles '(orderless basic))
  (setq completion-category-defaults nil)
  (setq completion-category-overrides nil))

;; Savehist - persist minibuffer history
(use-package savehist
  :ensure nil
  :hook (after-init . savehist-mode))

;; Emacs minibuffer enhancements
(use-package emacs
  :custom
  (tab-always-indent 'complete)
  (enable-recursive-minibuffers t)
  (read-extended-command-predicate #'command-completion-default-include-p)
  :init
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode))

;; Corfu - in-buffer completion popup
(use-package corfu
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0)
  (corfu-auto-prefix 1)
  (corfu-cycle t)
  (corfu-min-width 1)
  :bind
  (:map corfu-map
        ("TAB" . corfu-next)
        ([tab] . corfu-next)
        ("S-TAB" . corfu-previous)
        ([backtab] . corfu-previous))
  :init
  (global-corfu-mode)
  (setq corfu-popupinfo-delay '(1.25 . 0.5))
  (corfu-popupinfo-mode 1)

  (with-eval-after-load 'savehist
    (corfu-history-mode 1)
    (add-to-list 'savehist-additional-variables 'corfu-history)))

;; Consult - enhanced search and navigation
(use-package consult
  :preface
  (defun consult-ripgrep-at-point ()
    (interactive)
    (consult-ripgrep nil (thing-at-point 'symbol)))

  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)
         ("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ("C-x r b" . consult-bookmark)
         ("C-x p b" . consult-project-buffer)
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)
         ("<help> a" . consult-apropos)
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flycheck)
         ("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("M-g o" . consult-outline)
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings (search-map)
         ("M-s d" . consult-find)
         ("M-s D" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep-at-point)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s m" . consult-multi-occur)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)
         ("M-s e" . consult-isearch-history)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)
         ("M-r" . consult-history))
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  (advice-add #'register-preview :override #'consult-register-window)
  (setq register-preview-delay 0.5)
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  :config
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   :preview-key '(:debounce 0.4 any))
  (setq consult-narrow-key "<"))

(provide 'gh-completion)
;;; gh-completion.el ends here
