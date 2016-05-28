(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("marmalade" . "https://marmalade-repo.org/packages/")
			 ("melpa" . "http://melpa.org/packages/")))


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

(global-set-key (kbd "TAB") #'company-indent-or-complete-common)
(setq company-tooltip-align-annotations t)



;;white space
(require 'whitespace)
 (autoload 'whitespace-mode           "whitespace" "Toggle whitespace visualization."        t)

(setq-default indent-tabs-mode nil)
