;Set up helm-mode
(require 'helm)
(helm-mode 1)
(helm-autoresize-mode 1)

(setq helm-split-window-in-side-p t)
(setq-default indent-tabs-mode nil)
(setq helm-grep-ag-command "rg --color=always --colors 'match:fg:black' --colors 'match:bg:yellow' --smart-case --no-heading --line-number %s %s %s")
(setq helm-grep-ag-pipe-cmd-switches '("--colors 'match:fg:black'" "--colors 'match:bg:yellow'"))
