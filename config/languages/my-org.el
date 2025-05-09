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
          `(("t" "Tasks" entry
             (file+headline ,(my-home-path "notes/todo/todo.org") "Tasks")
             "* TODO %?\n  %U\n  %a")
            ("h" "Home Tasks" entry
             (file+headline ,(my-home-path "notes/todo/home.org") "Home Tasks")
             "* TODO %?\n  %u")))

    (org-babel-do-load-languages
     'org-babel-load-languages
     '((R . t)
       (emacs-lisp . t)
       (python . t)))

    (setq org-src-fontify-natively t)

    (setq org-agenda-prefix-format
      '((home  . "  %i %-12:c%?-12t% s")
        (todo    . "  %i %-12:c [%e] %b ")
        (tags    . "  %i %-12:c")
        (search  . "  %i %-12:c")))

    (with-eval-after-load 'evil
      (evil-define-key 'normal org-mode-map
        (kbd "<TAB>")     #'org-cycle
        (kbd "o a")       #'org-agenda
        (kbd "o T")       #'org-todo-list
        (kbd "o c")       #'org-capture
        (kbd "o d")       #'org-deadline
        (kbd "o s")       #'org-schedule
        (kbd "o p")       #'org-priority
        (kbd "o q")       #'org-set-tags-command
        (kbd "] ]")       #'org-next-visible-heading
        (kbd "[ [")       #'org-previous-visible-heading
        (kbd "o h")       #'org-insert-heading
        (kbd "o s")       #'org-insert-subheading
        (kbd "o <RET>")   #'org-insert-heading-after-current
        (kbd "o }")       #'org-do-demote
        (kbd "o {")       #'org-do-promote
        (kbd "o a")       #'org-agenda)
      )))

(provide 'my-org)
