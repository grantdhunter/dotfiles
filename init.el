(require 'package) ;; You might already have this line
(add-to-list 'package-archives
	                  '("melpa" . "https://melpa.org/packages/"))


(global-set-key [f8] 'neotree-toggle)

(global-set-key (kbd "C-x p") 'previous-multiframe-window)
(put 'downcase-region 'disabled nil)


(setq-default indent-tabs-mode nil)
