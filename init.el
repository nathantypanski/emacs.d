(setq package-archives '(("melpa" . "http://melpa.milkbox.net/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")))
(require 'package)
(package-initialize)
(setq package-enable-at-startup nil)


;; init.el for this setup. Must use Emacs 24
(org-babel-load-file
(expand-file-name "emacs-init.org" user-emacs-directory))
