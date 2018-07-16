;Change title-bar text
(setq frame-title-format
      "%F %b ")

;Disable menu-bar
(menu-bar-mode -1)

;Disable tool-bar
(tool-bar-mode -1)

;Disable scroll-bar
(scroll-bar-mode -1)

;Activate material theme
(load-theme 'material
            t)

;Set font
(custom-set-faces
 '(default ((t (:height 120)))))

;line numbers
(global-linum-mode)

  ;; ITERM2 MOUSE SUPPORT
(unless window-system
  (require 'mouse)
  (xterm-mouse-mode t)
  (defun track-mouse (e))
  (setq mouse-sel-mode t)
  )
