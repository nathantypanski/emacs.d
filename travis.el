;; travis.el -*- lexical-binding: t; -*-
;;
;; Test my emacs configuration using Travis-CI.

;; Set the emacs directory using Travis environment variables to find
;; our repo.
(setq user-emacs-directory (concat (getenv "TRAVIS_BUILD_DIR") "/"))
(add-to-list 'load-path user-emacs-directory)

;; Load my config files.
(require 'init)

;; Running this tries to byte-compile everything in the home directory,
;; when executed with a noninteractive Emacs.
(batch-byte-compile)
