(use-package org
  :commands (org-mode org-capture org-agenda orgtbl-mode)
  :init
  (progn
    (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
    (global-set-key (kbd "C-c c") 'org-capture)
    (global-set-key (kbd "C-c a") 'org-agenda))
  :config
  (progn
    (setq org-startup-folded nil)
    (setq org-default-notes-file (concat (getenv "HOME") "/notes/notes.org") org-log-done t)
    (defface org-block-begin-line '((t ( org-meta-line :background "gray27" :overline "gray20" :underline "gray20" :height 0.8)))
      "Face used for the line delimiting the begin of source blocks.")

    (defface org-block-background
      '((t (:background "#FFFFEA")))
      "Face used for the source block background.")

    (defface org-block-end-line
      '((t ( org-meta-line :background "gray27" :overline "gray20" :underline "gray20" :height 0.8)))
      "Face used for the line delimiting the end of source blocks.")

    ;; better default
    (add-hook 'org-mode-hook 'org-indent-mode)

    (setq org-agenda-files
          (mapcar (apply-partially #'my-home-path "notes")
                  '("agenda.org"
                    "todo/todo.org"
                    "todo/home.org")))

    (setq org-capture-templates
          `(("t" "Todo" entry
             (file+headline ,(my-home-path "notes/todo/todo.org") "Tasks")
             "* TODO %?\n  %U\n  %a")
            ("h" "Todo" entry
             (file+headline ,(my-home-path "notes/todo/home.org") "Home Tasks")
             "* TODO %?\n  %U\n  %a")
            ))

    (org-babel-do-load-languages
     'org-babel-load-languages
     '((R . t)
       (emacs-lisp . t)
       (python . t)))

    (setq org-src-fontify-natively t)))

(provide 'my-org)
