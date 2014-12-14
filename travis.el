(setq user-emacs-directory (getenv "TRAVIS_BUILD_DIR"))
(print (concat "Using emacs directory: " user-emacs-directory))
(add-to-list 'load-path user-emacs-directory)

(add-to-list 'load-path (concat user-emacs-directory "config"))
(add-to-list 'load-path (concat user-emacs-directory "elisp"))
(add-to-list 'load-path (concat user-emacs-directory "config" "/eyecandy"))
(add-to-list 'load-path (concat user-emacs-directory "config" "/languages"))
(add-to-list 'load-path (concat user-emacs-directory "/usr/share/emacs/site-lisp"))

(print (concat "Load path: " user-emacs-directory))
(require 'init)
(batch-byte-compile)
