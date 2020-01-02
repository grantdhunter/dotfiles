(require 'uniquify)

(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(setq frame-title-format "%F %b ")
(setq uniquify-buffer-name-style 'forward)

(show-paren-mode)

(when window-system (progn
                     (set-frame-size (selected-frame) 100 30)
                     (menu-bar-mode -1)
                     (tool-bar-mode -1)
                     (scroll-bar-mode -1)
                     ))


(global-auto-revert-mode t)
(global-linum-mode)
(global-whitespace-mode)

(put 'narrow-to-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

(setq gc-cons-threshold 20000000)
(setq make-backup-files nil)
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(setq vc-follow-symlinks t)
(setq confirm-kill-emacs 'y-or-n-p)
(setq ring-bell-function 'ignore)
(setq lazy-highlight-cleanup nil)
(setq whitespace-line-column 500)

(setq-default indent-tabs-mode
              nil)

(fset 'yes-or-no-p 'y-or-n-p)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:height 120)))))

;; ITERM2 MOUSE SUPPORT
(unless window-system
  (require 'mouse)
  (xterm-mouse-mode t)
  (defun track-mouse (e))
  (setq mouse-sel-mode t)
)


(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;(desktop-save-mode t)

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))
;;add path
(add-to-list 'exec-path "/usr/local/bin")
(add-to-list 'exec-path "~/.local/bin")


`
