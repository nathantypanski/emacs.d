(setq user-emacs-directory (getenv "TRAVIS_BUILD_DIR"))
(require 'my-init)
(batch-byte-compile)
