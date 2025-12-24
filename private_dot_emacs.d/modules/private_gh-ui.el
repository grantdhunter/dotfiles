;;; gh-ui.el --- UI and appearance settings -*- lexical-binding: t; -*-

;;; Commentary:
;; Theme, fonts, icons, and visual configuration.

;;; Code:

;; Fonts
(let ((mono-spaced-font "Monospace")
      (proportionately-spaced-font "Sans"))
  (set-face-attribute 'default nil :family mono-spaced-font :height 100)
  (set-face-attribute 'fixed-pitch nil :family mono-spaced-font :height 1.0)
  (set-face-attribute 'variable-pitch nil :family proportionately-spaced-font :height 1.0))

;; Line numbers and display
(show-paren-mode)
(setq linum-format
      (lambda (line)
        (propertize (number-to-string (1- line)) 'face 'linum)))
(global-display-line-numbers-mode)
(column-number-mode)
(pixel-scroll-precision-mode)

;; Theme
(use-package material-theme
  :config
  (if (daemonp)
      (add-hook 'after-make-frame-functions
                (lambda (frame)
                  (select-frame frame)
                  (load-theme 'material t)
                  (set-frame-size (selected-frame) 150 48)))
    (load-theme 'material t)))

;; Nerd icons
(use-package nerd-icons)

(use-package nerd-icons-completion
  :after marginalia
  :config
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(use-package nerd-icons-corfu
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package nerd-icons-dired
  :hook (dired-mode . nerd-icons-dired-mode))

(provide 'gh-ui)
;;; gh-ui.el ends here
