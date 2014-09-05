(add-to-list 'load-path (concat user-emacs-directory "config"))
(add-to-list 'load-path (concat user-emacs-directory "config"))
(add-to-list 'load-path (concat user-emacs-directory "elisp"))
(add-to-list 'load-path (concat user-emacs-directory "config" "/eyecandy"))
(add-to-list 'load-path (concat user-emacs-directory "config" "/languages"))
(setq package-archives '(("melpa" . "http://melpa.milkbox.net/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")))

;; (byte-recompile-directory "~/.emacs.d")
(require 'package)
(package-initialize)
(setq package-enable-at-startup nil)
(unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))

(require 'use-package)

(require 'my-functions)
(require 'my-core)
(require 'my-keychain)
(require 'my-dirs)
(require 'my-buffers)
(require 'my-imenu)
(require 'my-elisp)
(require 'my-gtags)
(require 'my-autocomplete)
(require 'my-projects)
(require 'my-interaction)
(require 'my-ibuffer)
(require 'my-helm)
(require 'my-code)
(require 'my-comint)
(require 'my-unbound-keys)
(require 'my-eyecandy)
(require 'my-shell)
(require 'my-filetypes)
(require 'my-term)
(require 'my-magit)
(require 'my-android)
(require 'my-eshell)
(require 'my-evil)
(require 'my-help)
(require 'my-god)
