(require 'saveplace)
(require 'midnight)
(require 'helm-projectile)

(if (daemonp)
    (add-hook 'after-make-frame-functions
        (lambda (frame)
            (select-frame frame)
            (load-theme 'material t)))
    (load-theme 'material t))
;;(load-theme 'material t)

(setq-default save-place t)
(setq save-place-file "~/.emacs.d/saveplace")


(add-hook 'eww-mode-hook 'scroll-lock-mode)

;;cleanup buffers
(setq clean-buffer-list-delay-special 900)
(defvar clean-buffer-list-timer nil
  "Stores clean-buffer-list timer if there is one. You can disable clean-buffer-list by (cancel-timer clean-buffer-list-timer).")
(setq clean-buffer-list-timer (run-at-time t 7200 'clean-buffer-list))
;; kill everything, clean-buffer-list is very intelligent at not killing
;; unsaved buffer.
(setq clean-buffer-list-kill-regexps '("^.*$"))

;Enable windmove
(windmove-default-keybindings)

(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;;Enable powerline
(powerline-center-theme)
(setq powerline-default-separator 'slant)

(dumb-jump-mode)
(setq dumb-jump-selector 'helm)
(setq dumb-jump-prefer-searcher 'rg)

;; projectile
(helm-projectile-on)
(setq projectile-project-search-path '("~/Development/"))
