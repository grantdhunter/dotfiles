(require 'semantic)

(global-company-mode)
;;Setup company-mode
(setq company-dabbrev-downcase nil)
(setq company-idle-delay 0)
(setq company-minimum-prefix-length 1)


(add-hook 'after-init-hook #'global-flycheck-mode)
(setq flycheck-flake8-maximum-line-length 500)

(semantic-mode 1)
(global-semantic-stickyfunc-mode 1)
