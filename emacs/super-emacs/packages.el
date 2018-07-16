;Create repositories cache, if required
(when (not package-archive-contents)
  (package-refresh-contents))

;Declare a list of required packages
(defvar super-emacs--required-packages
  '(helm
    multiple-cursors
    ace-jump-mode
    ace-window
    powerline
    buffer-move
    ztree
    undo-tree
    neotree
    material-theme
    dired-launch
    which-key
    meta-presenter
    myterminal-controls
    theme-looper
    magit
    rust-mode
    yaml-mode
    jinja2-mode
    json-mode
    terraform-mode
    company-terraform
    elpy
    racer
    company
    projectile
    helm-projectile
    flycheck
    ))

;Install required packages
(mapc (lambda (p)
        (package-install p))
      super-emacs--required-packages)


;Start undo-tree
(global-undo-tree-mode)

;Set hooks for dired-launch-mode
(add-hook 'dired-mode-hook
          'dired-launch-mode)

;Start which-key-mode
(which-key-mode)

;Setup company-mode
(setq company-dabbrev-downcase nil)
(setq company-idle-delay 0)
(setq company-minimum-prefix-length 2)

(global-company-mode)


;Set up ace-jump-mode
(autoload 'ace-jump-mode
  "ace-jump-mode"
  "Emacs quick move minor mode"
  t)
(autoload 'ace-jump-mode-pop-mark
  "ace-jump-mode"
  "Ace jump back:-"
  t)

;Enable powerline
(powerline-center-theme)
(setq powerline-default-separator
      'slant)


;Configure theme-looper
(theme-looper-set-theme-set '(deeper-blue
                              wheatgrass
                              wombat
                              material))
(theme-looper-set-customizations 'powerline-reset)

;Configure myterminal-controls
(myterminal-controls-set-controls-data
 '(("t" "Change color theme" theme-looper-enable-next-theme)
   ("r" "Reload file" super-emacs-reload-current-file)))

;Set up helm-mode
(helm-mode 1)
(helm-autoresize-mode 1)
(setq helm-split-window-in-side-p
      t)

;Set up racer for rust-mode
(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'rust-mode-hook #'eldoc-mode)
(add-hook 'racer-mode-hook #'company-mode)


;Projectile
(add-hook 'projectile-mode-hook (lambda () (
                                            (setq projectile-globally-ignored-files
                                                  (append '(
                                                            "#*#"
                                                            "*~"
                                                            ".DS_Store"
                                                            "*.gz"
                                                            "*.pyc"
                                                            "*.jar"
                                                            "*.tar.gz"
                                                            "*.tgz"
                                                            "*.zip"
                                                            "*.o"
                                                            )
                                                          projectile-globally-ignored-files))
                                            (setq projectile-globally-ignored-directories
                                                  (append '(
                                                            ".git"
                                                            ".svn"
                                                            ".idea"
                                                            ".vscode"
                                                            "node_modules"
                                                            )
                                                          projectile-globally-ignored-directories))
                                            )))


;;elpy
;; (when (require 'flycheck nil t)
;;   (setq elpy-default-minor-modes (delete 'flymake-mode elpy-default-minor-modes))
;;   (add-to-list 'elpy-default-minor-modes 'flycheck-mode))

;;flycheck
(setq flycheck-flake8-maximum-line-length 500)


(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

(require 'semantic)
(semantic-mode 1)
(global-semantic-stickyfunc-mode 1)
