(setq-default auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
              backup-directory-alist `((".*" . ,temporary-file-directory))
              undo-tree-history-directory-alist `((".*" . ,temporary-file-directory))
              confirm-kill-emacs 'y-or-n-p frame-title-format "%F %b " gc-cons-threshold (* (* 1
                                                                                               128)
                                                                                            1024
                                                                                            1024) ;; 100mb
              indent-tabs-mode nil inhibit-splash-screen t inhibit-startup-message t
              lazy-highlight-cleanup nil make-backup-files nil read-process-output-max (* 1 (* 1024
                                                                                               1024))
              ;; 1mb
              ring-bell-function 'ignore uniquify-buffer-name-style 'forward
              use-package-always-ensure t vc-follow-symlinks t warning-minimum-level
              :error whitespace-line-column 500)

(fset 'yes-or-no-p 'y-or-n-p)
(show-paren-mode)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(global-auto-revert-mode t)
(global-linum-mode)
(column-number-mode)

;;keybindings
(global-set-key (kbd "C-x p") 'previous-multiframe-window)



(setq package-archives '(("elpa" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
(defvar bootstrap-version)
(let ((bootstrap-file (expand-file-name "straight/repos/straight.el/bootstrap.el"
                                        user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer (url-retrieve-synchronously
                          "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
                          'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(require 'package)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))


;; Integrates `straight' directly into the `use-package' package through the
;; `:straight' expression.
(straight-use-package 'use-package)

(use-package
  delight)
(use-package
  material-theme)

(if (daemonp)
    (add-hook 'after-make-frame-functions (lambda (frame)
                                            (select-frame frame)
                                            (load-theme 'material t)
                                            (set-frame-size (selected-frame) 150 48)))
  (load-theme 'material t))

(use-package
  whitespace
  :config (add-hook 'before-save-hook 'delete-trailing-whitespace))


(use-package
  eldoc
  :delight)

(use-package
  password-store)
(setq auth-sources '(password-store))

(use-package
  all-the-icons
  :if (display-graphic-p))

(use-package
  yasnippet-snippets
  :after yasnippet
  :config (yasnippet-snippets-initialize))

(use-package
  yasnippet
  :delight yas-minor-mode
  :hook (yas-minor-mode . gh/disable-yas-if-no-snippets)
  :config (yas-global-mode)
  :preface (defun gh/disable-yas-if-no-snippets ()
             (when (and yas-minor-mode
                        (null (yas--get-snippet-tables)))
               (yas-minor-mode -1))))

(use-package
  lsp-mode
  :delight lsp-mode
  :init (setq lsp-keymap-prefix "C-c l" lsp-use-plists t)
  :hook ((js-mode . lsp-deferred)
         (rust-mode . lsp-deferred)
         (python-mode . lsp-deferred)
         (swift-mode . lsp-deferred)
         (sh-mode . lsp-deferred))
  :commands (lsp lsp-deferred)
  :config (setq lsp-idle-delay 0.500)
  :custom (lsp-enable-folding nil)
  (lsp-enable-links nil)
  (lsp-enable-snippet nil))

(use-package
  python
  :after lsp-mode
  :ensure flycheck
  :delight "Py"
  :preface (defun python-remove-unused-imports()
             "Remove unused imports and unused variables with autoflake."
             (interactive)
             (if (executable-find "autoflake")
                 (progn (shell-command (format "autoflake --remove-all-unused-imports -i %s"
                                               (shell-quote-argument (buffer-file-name))))
                        (revert-buffer t t t))
               (warn
                "[✗] python-mode: Cannot find autoflake executable.")))
  :custom (flycheck-pylintrc "~/.pylintrc")
  (flycheck-python-pylint-executable "/usr/bin/pylint"))

(use-package
  pyvenv
  :init (setenv "WORKON_HOME" "~/.pyenv/versions"))

(use-package
  lsp-pyright
  :after lsp-mode
  :config (setq lsp-pyright-venv-path "~/.pyenv/version")
  :if (executable-find "pyright")
  ;; To properly load `lsp-pyright', the `require' instruction is important.
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp-deferred))))
(use-package
  blacken
  :after lsp-mode
  :delight
  :hook (python-mode . blacken-mode))

(use-package
  py-isort
  :config (setq py-isort-options '("--src ."))
  :hook ((before-save . py-isort-before-save)
         (python-mode . pyvenv-mode)))

(use-package
  lsp-sourcekit
  :after lsp-mode
  :config (setq lsp-sourcekit-executable "/usr/local/bin/sourcekit-lsp"))
(use-package
  lsp-ui
  :after lsp-mode
  :commands lsp-ui-mode
  :config (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references))

(use-package
  company
  :after lsp-mode
  :delight
  :hook (after-init-hook . global-company-mode)
  :init (setq company-dabbrev-downcase nil company-idle-delay 0.0 company-minimum-prefix-length 1
              company-format-margin-function nil))

;; (use-package
;;   helm

;;   :delight :bind(("M-x" . helm-M-x)
;;                  ("C-x b" . helm-mini)
;;                  ("C-x C-b" . helm-buffers-list)
;;                  ("C-x C-f" . helm-find-files)
;;                  ("C-x C-r" . helm-recentf)
;;                  ("M-y" . helm-show-kill-ring))
;;   :config (require 'helm-config)
;;   (helm-mode 1)
;;   (setq helm-autoresize-max-height 20)
;;   (setq helm-autoresize-max-height 20)
;;   (helm-autoresize-mode 1)
;;   :commands helm-lsp-workspace-symbol)
;; (use-package
;;   helm-lsp
;;   :after lsp-mode
;;   :commands helm-lsp-workspace-symbol)

;; (use-package
;;   helm-projectile
;;   :after projectile
;;   :delight)

;; (use-package
;;   helm-swoop
;;   :bind(("C-s" . helm-swoop)
;;         ("M-i" . helm-swoop-back-to-last-point))
;;   :config (define-key helm-swoop-map (kbd "C-r") 'helm-previous-line)
;;   (define-key helm-swoop-map (kbd "C-s") 'helm-next-line)
;;   (define-key helm-multi-swoop-map (kbd "C-r") 'helm-previous-line)
;;   (define-key helm-multi-swoop-map (kbd "C-s") 'helm-next-line)
;;   (setq helm-swoop-split-window-function 'helm-default-display-buffer)
;;   (setq helm-swoop-pre-input-function (lambda () "")))

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
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package consult
  :after projectile
  :bind  (;; Related to the control commands.
          ("<help> a" . consult-apropos)
          ("C-x b" . consult-buffer)
          ("C-x M-:" . consult-complex-command)
          ("C-c k" . consult-kmacro)
          ;; Related to the navigation.
          ("M-g a" . consult-org-agenda)
          ("M-g e" . consult-error)
          ("M-g g" . consult-goto-line)
          ("M-g h" . consult-org-heading)
          ("M-g i" . consult-imenu)
          ("M-g k" . consult-global-mark)
          ("M-g l" . consult-line)
          ("M-g m" . consult-mark)
          ("M-g o" . consult-outline)
          ("M-g I" . consult-project-imenu)
          ;; Related to the search and selection.
          ("M-s G" . consult-git-grep)
          ("M-s g" . consult-grep)
          ("M-s k" . consult-keep-lines)
          ("M-s l" . consult-locate)
          ("M-s m" . consult-multi-occur)
          ("M-s r" . consult-ripgrep)
          ("M-s u" . consult-focus-lines)
          ("M-s f" . consult-find))
  :custom
  (completion-in-region-function #'consult-completion-in-region)
  (consult-narrow-key "<")
  (consult-project-root-function #'projectile-project-root)
  ;; Provides consistent display for both `consult-register' and the register
  ;; preview when editing registers.
  (register-preview-delay 0)
  (register-preview-function #'consult-register-preview))

(use-package marginalia
  :ensure t
  :config
  (marginalia-mode))

(use-package embark
  :ensure t

  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))


(use-package
  projectile
  :delight '(:eval (projectile-project-name))
  :custom
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-enable-caching t projectile-indexing-method 'alien)
  (projectile-switch-project-action #'projectile-dired)
  :config
  (setq projectile-globally-ignored-files (append '(".pyc" ".class" "~" "node_modules"
                                                    ".cache" "package.json"
                                                    "package-lock.json")
                                                  projectile-globally-ignored-files))
  (projectile-global-mode))



(use-package
  markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))
(use-package
  grip-mode)

(use-package
  org
  :bind(("C-c l" . org-store-link)
        ("C-c a" . org-agenda)
        ("C-c c" . org-capture))
  :ensure org-contrib
  :custom (setq org-default-notes-file (concat org-directory "/notes.org"))
  (org-todo-keywords '((sequence "TODO(t)" "STARTED(s)" "NEXT(n)" "SOMEDAY(.)" "WAITING(w)""|"
                                 "DONE(x!)" "CANCELLED(c@)"))))

(use-package
  toc-org
  :after org
  :hook (org-mode . toc-org-enable))

(use-package
  flyspell
  :ensure nil
  :delight
  :hook ((text-mode . flyspell-mode)
         (prog-mode . flyspell-prog-mode))
  :custom
  ;; Add correction to abbreviation table.
  (flyspell-abbrev-p t)
  (flyspell-default-dictionary "en_CA")
  (flyspell-issue-message-flag nil)
  (flyspell-issue-welcome-flag nil))

(use-package
  ispell
  :custom (ispell-hunspell-dict-paths-alist '(("en_CA" "/usr/share/hunspell/en_CA.aff")))
  (ispell-silently-savep t)
  :config (setenv "LANG" "en_CA")
  (setq ispell-program-name "hunspell")
  (setq ispell-local-dictionary-alist '(("en_CA" "[[:alpha:]]" "[^[:alpha:]]" "['’-]" t ("-d"
                                                                                         "en_CA" )
                                         nil utf-8))))
(use-package
  semantic
  :config (semantic-mode 1)
  (global-semantic-stickyfunc-mode 1))


;;(font-lock-add-keywords 'lisp-mode '(("\'.*\'" 0 'font-lock-single-quote-string-face t)))

(use-package
  multiple-cursors
  :bind(("C-}" . mc/mark-next-like-this)
        ("C-{" . mc/mark-previous-like-this)
        ("C-|" .  mc/mark-all-like-this)))

(use-package
  undo-tree
  :delight undo-tree-mode
  :config (global-undo-tree-mode))

(use-package
  ace-window
  :bind (("M-o" . ace-window)))

(use-package
  flycheck)
(use-package
  rainbow-delimiters
  :delight)
(use-package
  magit
  :bind (("C-c m" . magit-status))
  :custom (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package
  crux
  :bind (("C-c r" . crux-rename-file-and-buffer)))

(use-package
  diff-hl
  :config (global-diff-hl-mode))

(use-package
  toml-mode)
(use-package
  rust-mode)
(use-package
  terraform-mode)
(use-package
  company-terraform)
(use-package
  jinja2-mode)
(use-package
  json-mode)
(use-package
  json-snatcher)
(use-package
  yaml-mode)

(add-to-list 'auto-mode-alist '("\\.lisp\\'" . emacs-lisp-mode))
(require 'uniquify)


(add-hook 'eww-mode-hook 'scroll-lock-mode)

;;cleanup buffers
(setq clean-buffer-list-delay-special 900)
(defvar clean-buffer-list-timer nil)
(setq clean-buffer-list-timer (run-at-time t 7200 'clean-buffer-list))
;; kill everything, clean-buffer-list is very intelligent at not killing
;; unsaved buffer.
(setq clean-buffer-list-kill-regexps '("^.*$"))

(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)



;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flycheck-checker-error-threshold 1000)
 '(lsp-ui-doc-max-height 13)
 '(lsp-ui-doc-show-with-cursor nil)
 '(lsp-ui-doc-show-with-mouse nil)
 '(lsp-ui-doc-use-webkit nil)
 '(package-selected-packages
   '(password-store slack k8s-mode lsp-sourcekit swift-mode python-isort helm-rg python-black lsp-python-ms all-the-icons lsp-treemacs grip-mode helm-swoop diff-hl diff-hl-mode crux ace-window company-mode django-snippets company helm-lsp pyvenv json-snatcher package-lint helm-slime slime slime-company slime-repl-ansi-color helm-projectile web-mode py-autopep8 lsp-ui uniquify lsp-helm lsp-company yaml-mode use-package toml-mode rust-mode rainbow-delimiters projectile powerline multiple-cursors material-theme magit lsp-mode json-mode jinja2-mode gnu-elpa-keyring-update flycheck company-terraform))
 '(python-isort-arguments '("--stdout" "--atomic" "-"))
 '(warning-suppress-types '((use-package) (comp)))
 '(yas-global-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-tooltip ((t (:inherit default :background "#2a4937a43e51"))))
 '(company-tooltip-common ((t (:inherit font-lock-constant-face))))
 '(company-tooltip-scrollbar-thumb ((t (:background "#307f3fcf4778"))))
 '(company-tooltip-scrollbar-track ((t (:background "#3ad84d6d56b8"))))
 '(company-tooltip-selection ((t (:inherit font-lock-function-name-face)))))
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
