(setq user-emacs-directory (concat (getenv "TRAVIS_BUILD_DIR") "/"))
(print (concat "Using emacs directory: " user-emacs-directory))
(add-to-list 'load-path user-emacs-directory)

(print (concat "Load path: " user-emacs-directory))
(require 'init)
(batch-byte-compile)
