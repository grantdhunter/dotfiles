(setq-default backup-directory-alist `((".*" . ,(concat user-emacs-directory "backups")))
              undo-tree-history-directory-alist `((".*" . ,(concat user-emacs-directory "undo")))
              confirm-kill-emacs 'y-or-n-p
              frame-title-format "%F %b "
              gc-cons-threshold (* (* 1
                                      128)
                                   1024
                                   1024) ;; 100mb
              indent-tabs-mode nil
              inhibit-splash-screen t
              inhibit-startup-message t
              lazy-highlight-cleanup nil
              make-backup-files nil
              read-process-output-max (* 1 (* 1024
                                              1024)) ;; 1mb
              ring-bell-function 'ignore
              uniquify-buffer-name-style 'forward

              vc-follow-symlinks t
              warning-minimum-level
              :error whitespace-line-column 500
              package-enable-at-startup nil)

(fset 'yes-or-no-p 'y-or-n-p)
(show-paren-mode)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(global-auto-revert-mode t)

(setq linum-format
      (lambda (line)
        (propertize (number-to-string (1- line)) 'face 'linum)))
(global-display-line-numbers-mode)
(column-number-mode)
(pixel-scroll-precision-mode)

;;keybindings
(global-set-key (kbd "C-x O") 'previous-multiframe-window)


(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))



;; Integrates `straight' directly into the `use-package' package through the
;; `:straight' expression.
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(use-package
  delight)

;; theme
(use-package
  material-theme
  :config
  (if (daemonp)
      (add-hook 'after-make-frame-functions
                (lambda (frame)
                  (select-frame frame)
                  (load-theme 'material t)
                  (set-frame-size (selected-frame) 150 48)))
    (load-theme 'material t)))


;; globally used packages
(use-package
  whitespace
  :config (add-hook 'before-save-hook 'delete-trailing-whitespace))

(use-package
  lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l" lsp-use-plists t)
  (defun my/lsp-mode-setup-completion()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless))) ;; Configure orderless
  :hook ((js-ts-mode . lsp-deferred)
         (typescript-ts-mode . lsp-deferred)
         (python-ts-mode . lsp-deferred)
         (sql-ts-mode . lsp-deferred)
         (lsp-completion-mode . my/lsp-mode-setup-completion))
  :commands (lsp lsp-deferred)
  :config (setq lsp-idle-delay 0.500)
  :custom
  (lsp-enable-folding nil)
  (lsp-enable-links nil)
  (lsp-enable-snippet nil)
  (lsp-completion-provider :none))

;; (add-hook 'hack-local-variables-hook
;;           (lambda () (when (derived-mode-p 'prog-mode) (lsp))))
(use-package
  lsp-ui
  :after lsp-mode
  :commands lsp-ui-mode
  :config (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references))

(use-package
  dap-mode
  :config
  (setq dap-auto-configure-features '(sessions locals controls tooltip)))


(use-package vertico
  :straight (:files (:defaults "extensions/*"))
  :init
  (vertico-mode)
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              )
  :custom (vertico-cycle t))

(use-package savehist
  :init
  (savehist-mode))

(use-package emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))

(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))


(defun consult-ripgrep-at-point ()
    (interactive)
    (consult-ripgrep nil (thing-at-point 'symbol)))
(use-package consult
  :bind  (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ("<help> a" . consult-apropos)            ;; orig. apropos-command
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flycheck)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
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
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  :custom
  (completion-in-region-function #'consult-completion-in-region)
  (consult-narrow-key "<")
  (consult-project-root-function #'projectile-project-root)
  ;; Provides consistent display for both `consult-register' and the register
  ;; preview when editing registers.
  (register-preview-delay 0)
  (register-preview-function #'consult-register-preview))


(use-package consult-lsp
  :config (define-key lsp-mode-map [remap xref-find-apropos] #'consult-lsp-symbols))

(use-package
  consult-flycheck)

(use-package
  corfu
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0)
  (corfu-auto-prefix 1)
  (corfu-cycle t)
  :bind
  (:map corfu-map
        ("TAB" . corfu-next)
        ([tab] . corfu-next)
        ("S-TAB" . corfu-previous)
        ([backtab] . corfu-previous))
  :init
  (global-corfu-mode))

(use-package
  multiple-cursors
  :bind(("C-}" . mc/mark-next-like-this)
        ("C-{" . mc/mark-previous-like-this)
        ("C-|" .  mc/mark-all-like-this)))

(use-package
  undo-tree
  :config (global-undo-tree-mode))


(use-package
  rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package
  crux
  :bind (("C-c r" . crux-rename-file-and-buffer)))

(use-package
  compat
  :straight (compat :host github :repo "emacs-compat/compat"))

(use-package
  magit
  :bind (("C-c m" . magit-status))
  :custom (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package
  flycheck)

(use-package treesit
  :straight nil
  :preface
  (defun mp-setup-install-grammars ()
    "Install Tree-sitter grammars if they are absent."
    (interactive)
    (dolist (grammar
             '((bash "https://github.com/tree-sitter/tree-sitter-bash")
     (cmake "https://github.com/uyha/tree-sitter-cmake")
     (css "https://github.com/tree-sitter/tree-sitter-css")
     (elisp "https://github.com/Wilfred/tree-sitter-elisp")
     (go "https://github.com/tree-sitter/tree-sitter-go")
     (html "https://github.com/tree-sitter/tree-sitter-html")
     (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
     (json "https://github.com/tree-sitter/tree-sitter-json")
     (make "https://github.com/alemuller/tree-sitter-make")
     (markdown "https://github.com/ikatyang/tree-sitter-markdown")
     (python "https://github.com/tree-sitter/tree-sitter-python")
     (toml "https://github.com/tree-sitter/tree-sitter-toml")
     (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
     (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
     (yaml "https://github.com/ikatyang/tree-sitter-yaml")))
      (add-to-list 'treesit-language-source-alist grammar)
      ;; Only install `grammar' if we don't already have it
      ;; installed. However, if you want to *update* a grammar then
      ;; this obviously prevents that from happening.
      (unless (treesit-language-available-p (car grammar))
        (treesit-install-language-grammar (car grammar)))))

  ;; Optional, but recommended. Tree-sitter enabled major modes are
  ;; distinct from their ordinary counterparts.
  ;;
  ;; You can remap major modes with `major-mode-remap-alist'. Note
  ;; that this does *not* extend to hooks! Make sure you migrate them
  ;; also
  (dolist (mapping '((python-mode . python-ts-mode)
                     (css-mode . css-ts-mode)
                     (typescript-mode . tsx-ts-mode)
                     (json-mode . json-ts-mode)
                     (js-mode . js-ts-mode)
                     (css-mode . css-ts-mode)
                     (yaml-mode . yaml-ts-mode)))
    (add-to-list 'major-mode-remap-alist mapping))

  :config
  (mp-setup-install-grammars)
  ;; Do not forget to customize Combobulate to your liking:
  ;;
  ;;  M-x customize-group RET combobulate RET
  ;;
  (use-package combobulate
    :preface
    ;; You can customize Combobulate's key prefix here.
    ;; Note that you may have to restart Emacs for this to take effect!
    (setq combobulate-key-prefix "C-c o")

    ;; Optional, but recommended.
    ;;
    ;; You can manually enable Combobulate with `M-x
    ;; combobulate-mode'.
    :hook ((python-ts-mode . combobulate-mode)
           (js-ts-mode . combobulate-mode)
           (css-ts-mode . combobulate-mode)
           (yaml-ts-mode . combobulate-mode)
           (json-ts-mode . combobulate-mode)
           (typescript-ts-mode . combobulate-mode)
           (tsx-ts-mode . combobulate-mode))
    ;; Amend this to the directory where you keep Combobulate's source
    ;; code.
    ))

;;python
(use-package
  python
  :after lsp-mode
  :ensure flycheck
  :preface (defun python-remove-unused-imports()
             "Remove unused imports and unused variables with autoflake."
             (interactive)
             (if (executable-find "autoflake")
                 (progn (shell-command (format "autoflake --remove-all-unused-imports -i %s"
                                               (shell-quote-argument (buffer-file-name))))
                        (revert-buffer t t t))
               (warn
                "[✗] python-mode: Cannot find autoflake executable."))))
(use-package
  blacken
  :after lsp-mode
  :hook (python-ts-mode . blacken-mode))

(use-package
  py-isort
  :hook (before-save . py-isort-before-save))

(use-package
  lsp-pyright
  :after lsp-mode
  :config (setq lsp-pyright-venv-path "~/.pyenv/version")
  :if (executable-find "pyright")
  ;; To properly load `lsp-pyright', the `require' instruction is important.
  :hook (python-ts-mode . (lambda ()
                         (require 'lsp-pyright))))


;; js/ts
(use-package
  prettier-js
  :after lsp-mode
  :hook ((js-mode . prettier-js-mode)
         (typescript-mode . prettier-js-mode))
  :config
  (setq typescript-indent-level 2)
  (require 'dap-node)
  (dap-node-setup))

(use-package
  typescript-mode
  :hook (before-save . (lambda () (lsp-organize-imports))))
(use-package
  jest-test-mode
  :straight '(jest-test-mode :type git :host github :repo "grantdhunter/jest-test-mode")
  :commands jest-test-mode
  :hook (typescript-mode))
;; java

(use-package
  lsp-java
  :config (add-hook 'java-mode-hook 'lsp-deferred))

;; misc langs
(use-package
  yaml-mode)
(use-package
  openapi-preview
  :straight  '(openapi-preview :type git :host github :repo "merrickluo/openapi-preview"))

(use-package
  dockerfile-mode)
;; org mode
(use-package
  org
  :mode (("\\.org$" . org-mode))
  :init
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((sql . t)
     (python . t)))

  (defun my-org-confirm-babel-evaluate (lang body)
    (not (member lang `("sql" "emacs-lisp" "python"))))  ;don't ask for ditaa
  (setq org-confirm-babel-evaluate #'my-org-confirm-babel-evaluate))


(setq org-directory (concat (getenv "HOME") "/notes/"))
(use-package
  org-roam
  :after org
  :init (setq org-roam-v2-ack t)
  :custom (org-roam-directory (file-truename org-directory))
  :bind (("C-c n f" . org-roam-node-find)
         ("C-c n r" . org-roam-node-random)
         (:map org-mode-map
               (("C-c n i" . org-roam-node-insert)
                ("C-c n o" . org-id-get-create)
                ("C-c n t" . org-roam-tag-add)
                ("C-c n a" . org-roam-alias-add)
                ("C-c n l" . org-roam-buffer-toggle))))
  :config
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode))

(use-package
  org-roam-ui
  :after org-roam
  :config (setq org-roam-ui-sync-theme t
                org-roam-ui-follow t
                org-roam-ui-update-on-save t
                org-roam-ui-open-on-start t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("f149d9986497e8877e0bd1981d1bef8c8a6d35be7d82cba193ad7e46f0989f6a" default))
 '(jest-test-command-string "yarn run %s jest %s %s")
 '(lsp-ui-doc-position 'bottom)
 '(lsp-ui-doc-show-with-mouse nil)
 '(lsp-ui-sideline-diagnostic-max-line-length 150)
 '(lsp-ui-sideline-diagnostic-max-lines 10)
 '(safe-local-variable-values
   '((eval let
           ((sdks-directory
             (concat
              (car
               (dir-locals-find-file default-directory))
              ".yarn/sdks/")))
           (set
            (make-local-variable 'flycheck-javascript-eslint-executable)
            (concat sdks-directory "eslint/bin/eslint.js"))
           (with-eval-after-load 'lsp-javascript
             (lsp-dependency 'typescript-language-server
                             (list :system
                                   (concat "/foo/.yarn/sdks/" "typescript-language-server/lib/cli.js")))
             (lsp-dependency 'typescript
                             (list :system
                                   (concat "/foo/.yarn/sdks/" "typescript/bin/tsserver")))))
     (eval let
           ((sdks-directory
             (concat
              (car
               (dir-locals-find-file default-directory))
              ".yarn/sdks/")))
           (remhash 'prettier format-all--executable-table)
           (puthash 'prettier
                    (concat sdks-directory "prettier/index.js")
                    format-all--executable-table)
           (set
            (make-local-variable 'flycheck-javascript-eslint-executable)
            (concat sdks-directory "eslint/bin/eslint.js"))
           (with-eval-after-load 'lsp-javascript
             (lsp-dependency 'typescript-language-server
                             (list :system
                                   (concat "/foo/.yarn/sdks/" "typescript-language-server/lib/cli.js")))
             (lsp-dependency 'typescript
                             (list :system
                                   (concat "/foo/.yarn/sdks/" "typescript/bin/tsserver")))))
     (lsp-enabled-clients ts-ls eslint)
     (eval setq lsp-clients-typescript-server-args
           `("--tsserver-path" ,(concat
                                 (car
                                  (dir-locals-find-file default-directory))
                                 ".yarn/sdks/typescript/bin/tsserver")
             "--stdio"))
     (eval message "in otto"))))
