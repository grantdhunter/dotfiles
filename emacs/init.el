;Record startup timestamp
(defvar super-emacs/invokation-time
  (current-time))

;Load configuration files
(load-file "~/.emacs.d/super-emacs/repositories.el")
(load-file "~/.emacs.d/super-emacs/packages.el")
(load-file "~/.emacs.d/super-emacs/interface.el")
(load-file "~/.emacs.d/super-emacs/misc.el")
(load-file "~/.emacs.d/super-emacs/key-bindings.el")
