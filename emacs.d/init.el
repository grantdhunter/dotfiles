;;; package --- Alain M. Lafon <alain@200ok.ch> Emacs config
;;; Commentary:
;;   - All of the configuration is within `configuration.org`
;;; Code:

(package-initialize)

;; This loads the actual configuration in literate org-mode elisp
(defun load-config()
  (load-file "~/.emacs.d/default.el")
  (load-file "~/.emacs.d/packages.el")
  (load-file "~/.emacs.d/custom.el")
  (load-file "~/.emacs.d/keyboard-bindings.el")
  (load-file "~/.emacs.d/autocomplete.el")
  (load-file "~/.emacs.d/python.el")
  (load-file "~/.emacs.d/rust.el")
 ;; (load-file "~/.emacs.d/lisp.el")
  )


(load-config)

;;; init.el ends here


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flycheck-checker-error-threshold 100000)
 '(package-selected-packages
   (quote
    (js-format elpy csv-mode markdown-mode helm-rg dumb-jump writegood-mode crux rainbow-delimiters powerline py-autopep8 csharp-mode toml-mode importmagic exec-path-from-shell flycheck helm-projectile projectile company racer company-terraform terraform-mode json-mode jinja2-mode yaml-mode rust-mode magit material-theme neotree multiple-cursors helm)))
 '(projectile-mode t nil (projectile)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:height 120)))))
(put 'upcase-region 'disabled nil)
