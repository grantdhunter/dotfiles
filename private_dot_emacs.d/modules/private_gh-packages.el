;;; gh-packages.el --- Package management with straight.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Bootstrap straight.el and configure use-package integration.

;;; Code:

;; Bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Integrate straight.el with use-package
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; Load Org early to prevent version mismatch
(straight-use-package 'org)
(require 'org)  ;; Force immediate loading

(provide 'gh-packages)
;;; gh-packages.el ends here
