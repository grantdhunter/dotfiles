(setq auto-mode-alist (append '(("\\.sls$" . yaml-mode))
      auto-mode-alist))




(defvar dwlisp-mode-hook nil)
(add-to-list 'auto-mode-alist '("\\.lisp\\'" . dwlisp-mode))

(define-derived-mode dwlisp-mode
  lisp-mode "dwlisp"
  "major mode for dwlisp"
  (setq lisp-body-indent 2))
