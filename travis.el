(setq user-emacs-directory (getenv "TRAVIS_BUILD_DIR"))

(add-to-list 'load-path user-emacs-directory)

(require 'init)
(batch-byte-compile)
