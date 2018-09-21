(setq user-home-directory (getenv "HOME"))
(setq user-customizations-directory (concat user-emacs-directory "my-init/"))
(add-to-list 'load-path user-customizations-directory)
(load "setup")
(load "load-packages")
(load "customize-behavior")
(load "utils")
(load "keybindings")
(load custom-file 'noerror)
