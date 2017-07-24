(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package async
	     :ensure t)
(use-package dired-async
	     :after async
	     :config
	     (dired-async-mode 1)
	     (async-bytecomp-package-mode 1)
	     )


(if (display-graphic-p)

    (if (string-equal system-type "darwin")
        (setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
        (load-theme 'ubuntu t)
    )
)
(setq inhibit-startup-screen t)

;;add ternjs to emacs
(add-to-list 'load-path "~/DevelopmentUtils/tern/emacs/")
(autoload 'tern-mode "tern.el" nil t)

;;auto load tern with .js files
(add-hook 'js-mode-hook (lambda () (tern-mode t)))
(global-linum-mode t)

;;highlight brackets
(show-paren-mode 1)
(setq show-paren-delay 0)

;;window management
(global-set-key (kbd "C-x -") 'shrink-window)
(global-set-key (kbd "C-x +") 'enlarge-window)

(global-set-key (kbd "C-x p") 'previous-multiframe-window)
(put 'downcase-region 'disabled nil)

;;undo
(global-set-key (kbd "C--") 'undo)

(global-set-key (kbd "s--") 'text-scale-decrease)
(global-set-key (kbd "s-=") 'text-scale-increase)

;;neotree
(use-package neotree
  :ensure t
  :bind("<f8>" .  neotree-toggle)
)


;;Rust
(setq racer-cmd "/usr/local/bin/racer")
(setq racer-rust-src-path "/Users/grant/.rust/src/")

(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)

(add-hook 'racer-mode-hook #'company-mode)
(add-hook 'rust-mode-hook #'rust-enable-format-on-save)

(add-hook 'rust-mode-hook 'cargo-minor-mode)

(add-hook 'rust-mode-hook
	  (lambda ()
               (local-set-key (kbd "C-c <tab>") #'rust-format-buffer)))

(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)

;;Python

;;autocomplete
(use-package company
  :ensure t  )
(setq company-tooltip-align-annotations t)

(add-hook 'after-init-hook 'global-company-mode)
(eval-after-load "company"
 '(add-to-list 'company-backends 'company-anaconda))


(setq company-dabbrev-downcase 0)
(setq company-idle-delay 0)
(setq company-minimum-prefix-length 2)

(use-package py-autopep8
  :ensure t)


;;white space
(require 'whitespace)
 (autoload 'whitespace-mode           "whitespace" "Toggle whitespace visualization."        t)

(setq-default indent-tabs-mode nil)


(use-package magit
             :ensure t)
(use-package helm
  :ensure t
  :diminish helm-mode
  :init (progn
          (require 'helm-config)
          (helm-mode)
          )
  :bind(("C-c h" . helm-mini)
         ("C-h a" . helm-apropos)
         ("C-x C-b" . helm-buffers-list)
         ("C-x b" . helm-buffers-list)
         ("M-y" . helm-show-kill-ring)
         ("M-x" . helm-M-x)
         ("C-x c o" . helm-occur)
         ("C-x c s" . helm-swoop)
         ("C-x c y" . helm-yas-complete)
         ("C-x c Y" . helm-yas-create-snippet-on-region)
         ("C-x c b" . my/helm-do-grep-book-notes)
         ("C-x c SPC" . helm-all-mark-rings))
 )


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("3d5307e5d6eb221ce17b0c952aa4cf65dbb3fa4a360e12a71e03aab78e0176c5" default)))
 '(package-selected-packages
   (quote
    (ubuntu-theme py-autopep8 hide-comnt terraform-mode helm-projectile 0blayout jinja2-mode company-anaconda yaml-mode markdown-mode markdown-mode+ markdownfmt swift-mode swift3-mode go-mode jq-mode web-beautify use-package tern-auto-complete rustfmt racer neotree magit json-reformat js2-highlight-vars jedi helm flymake-rust flycheck-rust csharp-mode company-jedi cargo))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-string-face ((t (:foreground "magenta")))))
