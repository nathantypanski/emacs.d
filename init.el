;; -*- lexical-binding: t; -*-

(add-to-list 'load-path (concat user-emacs-directory "config"))
(add-to-list 'load-path (concat user-emacs-directory "elisp"))
(add-to-list 'load-path (concat user-emacs-directory "config" "/languages"))
(add-to-list 'load-path (concat user-emacs-directory "pkg"))
;; (add-to-list 'load-path "/usr/share/emacs/site-lisp")

;; (setq package-archives '(("melpa" . "https://stable.melpa.org/packages/")
;;                          ("gnu" . "https://elpa.gnu.org/packages/")))



;; Prevent straight from cloning org.
(setq straight-built-in-pseudo-packages '(org eglot eldoc python))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(setq straight-use-package-by-default :t)
(straight-use-package 'use-package)

(require 'my-env)
(require 'my-core)
(require 'my-functions)
(require 'my-eyecandy)
(require 'my-keychain)
(require 'my-dired)
(require 'my-buffers)
  (when (getenv "TMUX")
(require 'my-tmux-frames))
(require 'my-elisp)
(require 'my-tags)
(require 'my-spelling)
(require 'my-finances)
(require 'my-completion)
(require 'my-projects)
(require 'my-ag)
(require 'my-interaction)
(require 'my-comint)
(require 'my-unbound-keys)
(require 'my-keys)
(require 'my-copy)
(require 'my-languages)
(require 'my-org)
(require 'my-filetypes)
(require 'my-flycheck)
(require 'my-term)
(require 'my-magit)
(require 'my-android)
(require 'my-eshell)
(require 'my-ielm)
(require 'my-package-list)
(require 'my-evil)
(require 'my-age)
(require 'my-gpt)
(require 'my-help)
(require 'my-god)
(require 'my-sessions)
(require 'my-leader-keys)

(after 'evil (my-tty-cursor-update))

(provide 'init)
