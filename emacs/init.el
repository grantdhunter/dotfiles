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
	     :config
	     (defun my/init-hook ()
	           "If the current buffer is 'emacs-init.org' the code-blocks
are tangled."
		   (when (equal (buffer-file-name) my-org-file)
		     (async-start
		      `(lambda ()
			 (require 'org)
			 (org-babel-tangle-file ,my-org-file))
		      (lambda (result)
			(message "Tangled file compiled.")))))
	     (add-hook 'after-save-hook 'my/init-hook))

(use-package dired-async
	     :after async
	     :config
	     (dired-async-mode 1)
	     (async-bytecomp-package-mode 1)
	     )




;;add ternjs to emacs
(add-to-list 'load-path "~/Development/util/tern/emacs/")
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

;;neotree
(global-set-key [f8] 'neotree-toggle)

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

;;(global-set-key (kbd "TAB") #'company-indent-or-complete-common)
(use-package company
  :ensure t  
  :bind("<C-tab>" . company-auto-complete))
(setq company-tooltip-align-annotations t)



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
 '(package-selected-packages
   (quote
    (swift-mode swift3-mode go-mode jq-mode web-beautify use-package tern-auto-complete rustfmt racer neotree magit json-reformat js2-highlight-vars jedi helm flymake-rust flycheck-rust csharp-mode company-jedi cargo))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
