;;; gh-core.el --- Core Emacs settings -*- lexical-binding: t; -*-

;;; Commentary:
;; Basic Emacs configuration: defaults, backups, and performance settings.

;;; Code:

;; Backup and autosave directories
(setq-default backup-directory-alist `((".*" . ,(concat user-emacs-directory "backups")))
              undo-tree-history-directory-alist `((".*" . ,(concat user-emacs-directory "undo")))
              auto-save-file-name-transforms `((".*" ,(concat user-emacs-directory "autosave") t))
              custom-file (locate-user-emacs-file "custom.el"))

;; General behavior
(setq-default confirm-kill-emacs 'y-or-n-p
              frame-title-format "%F %b "
              inhibit-splash-screen t
              inhibit-startup-message t
              make-backup-files nil
              create-lockfiles nil
              vc-follow-symlinks t
              ring-bell-function 'ignore
              uniquify-buffer-name-style 'forward)

;; Performance tuning
(setq-default gc-cons-threshold (* 128 1024 1024)  ; 128mb
              read-process-output-max (* 1024 1024)) ; 1mb

;; Editing defaults
(setq-default indent-tabs-mode nil
              tab-width 2)

;; Search
(setq-default lazy-highlight-cleanup nil)

;; Warnings
(setq-default warning-minimum-level :error
              whitespace-line-column 500)

;; Package and startup
(setq-default package-enable-at-startup nil
              browse-url-generic-program (executable-find "firefox"))

;; Load custom file if it exists
(load custom-file :no-error-if-file-is-missing)

;; Suppress warnings/compile-log buffers
(add-to-list 'display-buffer-alist
             '("\\`\\*\\(Warnings\\|Compile-Log\\)\\*\\'"
               (display-buffer-no-window)
               (allow-no-window . t)))

;; Yes/no shortcuts
(fset 'yes-or-no-p 'y-or-n-p)

;; Disable suspend
(global-unset-key (kbd "C-z"))

;; Auto-revert files when changed on disk
(global-auto-revert-mode t)

;; DWIM keyboard-quit
(defun prot/keyboard-quit-dwim ()
  "Do-What-I-Mean behaviour for a general `keyboard-quit'.

The generic `keyboard-quit' does not do the expected thing when
the minibuffer is open.  Whereas we want it to close the
minibuffer, even without explicitly focusing it.

The DWIM behaviour of this command is as follows:

- When the region is active, disable it.
- When a minibuffer is open, but not focused, close the minibuffer.
- When the Completions buffer is selected, close it.
- In every other case use the regular `keyboard-quit'."
  (interactive)
  (cond
   ((region-active-p)
    (keyboard-quit))
   ((derived-mode-p 'completion-list-mode)
    (delete-completion-window))
   ((> (minibuffer-depth) 0)
    (abort-recursive-edit))
   (t
    (keyboard-quit))))

(define-key global-map (kbd "C-g") #'prot/keyboard-quit-dwim)

(provide 'gh-core)
;;; gh-core.el ends here
