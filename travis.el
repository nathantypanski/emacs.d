(setq user-emacs-directory (getenv "TRAVIS_BUILD_DIR"))
(require 'init)
(batch-byte-compile)
