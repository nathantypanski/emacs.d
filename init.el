(add-to-list 'load-path (concat user-emacs-directory "config"))
(add-to-list 'load-path (concat user-emacs-directory "elisp"))
(setq package-archives '(("melpa" . "http://melpa.milkbox.net/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")))
;;(byte-recompile-directory "~/.emacs.d")
(require 'package)
(package-initialize)
(setq package-enable-at-startup nil)
(unless (package-installed-p 'use-package)
    (unless (assoc package package-archive-contents)
      (package-refresh-contents))
    (package-install 'use-package))
(require 'use-package)

(require 'my-globals)
(require 'my-functions)
(require 'my-dirs)
(require 'my-buffers)
(require 'my-imenu)
(require 'my-elisp)
(require 'my-c)
(require 'my-autocomplete)
(require 'my-org)
(require 'my-projects)
(require 'my-interaction)
(require 'my-scss)
(require 'my-latex)
(require 'my-markdown)
(require 'my-haskell)
(require 'my-python)
(require 'my-unbound-keys)
(require 'my-eyecandy)
(require 'my-shell)
(require 'my-filetypes)
(require 'my-term)
(require 'my-evil)
