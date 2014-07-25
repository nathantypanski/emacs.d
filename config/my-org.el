
(use-package org
  :commands (org-mode org-capture org-agenda orgtbl-mode)
  :init
  (progn
    (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
    (global-set-key (kbd "C-c c") 'org-capture)
    (global-set-key (kbd "C-c a") 'org-agenda)
    )
  :config
  (progn
    (setq org-default-notes-file "~/.notes.org" org-log-done t)
    (defface org-block-begin-line '((t ( org-meta-line :background "gray27" :overline "gray20" :underline "gray20" :height 0.8)))
      "Face used for the line delimiting the begin of source blocks.")

    (defface org-block-background
      '((t (:background "#FFFFEA")))
      "Face used for the source block background.")

    (defface org-block-end-line
      '((t ( org-meta-line :background "gray27" :overline "gray20" :underline "gray20" :height 0.8)))
      "Face used for the line delimiting the end of source blocks.")

    (setq org-src-fontify-natively t)
    (use-package ob
      :config
      (progn
	(org-babel-do-load-languages
	 'org-babel-load-languages
	 '((R . t)
	   (emacs-lisp . t)
	   (python . t)
	   ))
	)
      )
    )
  )

(provide 'my-org)
