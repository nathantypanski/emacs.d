;; my-org.el   -*- lexical-binding:t; -*-
;;
;; Customizations for org-mode.

(use-package org
  :commands (org-mode org-capture org-agenda orgtbl-mode)
  :ensure t
  :init
  (progn
    (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
    (global-set-key (kbd "C-c c") 'org-capture)
    (global-set-key (kbd "C-c a") 'org-agenda))
  :custom
  (org-startup-folded nil)
  (org-default-notes-file (concat (getenv "HOME") "/notes/notes.org") org-log-done t)
  (org-agenda-files
  (mapcar (apply-partially #'my-home-path "notes")
           '("agenda.org"
             "todo/todo.org"
             "todo/home.org"))
  (org-src-fontify-natively t)

  (org-agenda-prefix-format
        '((home  . "  %i %-12:c%?-12t% s")
          (todo    . "  %i %-12:c [%e] %b ")
          (tags    . "  %i %-12:c")
          (search  . "  %i %-12:c")))

  (org-capture-templates
   `(("t" "Tasks" entry
      (file+headline ,(my-home-path "notes/todo/todo.org") "Tasks")
      "* TODO %?\n  %U\n  %a")
     ("h" "Home Tasks" entry
      (file+headline ,(my-home-path "notes/todo/home.org") "Home Tasks")
      "* TODO %?\n  %u")))

  ;; don't kill my windows
   ;; Agenda views
  (org-agenda-window-setup       'current-window)
  (org-agenda-restore-windows-after-quit t)

   ;; Capture
  (org-capture-window-setup      'current-window)

   ;; Refile targets (C-c C-w)
  (org-refile-window-setup       'current-window)

   ;; Tag searches (C-c a m)
  (org-tags-view-window-setup    'current-window)

   ;; Search (C-c a /)
  (org-search-view-window-setup  'current-window)

  ;; (Optional) if you want also to restore windows after a capture or refile:
  (org-capture-restore-windows-after-quit    t)
  (org-refile-restore-windows-after-quit     t)
  :config
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

    (org-babel-do-load-languages
     'org-babel-load-languages
     '((R . t)
       (emacs-lisp . t)
       (python . t)))

    (with-eval-after-load 'evil
      (evil-define-key 'normal org-mode-map
        (kbd "<TAB>")     #'org-cycle
        (kbd "o [")       #'org-metaup
        (kbd "o ]")       #'org-metadown
        (kbd "[ [")       #'org-previous-visible-heading
        (kbd "] ]")       #'org-next-visible-heading
        (kbd "o h")       #'org-insert-heading
        (kbd "o s")       #'org-insert-subheading
        (kbd "o <RET>")   #'org-insert-heading-after-current
        (kbd "o }")       #'org-do-demote
        (kbd "o {")       #'org-do-promote
        (kbd "o d")       #'org-deadline
        (kbd "o s")       #'org-schedule
        (kbd "o p")       #'org-priority
        (kbd "o z")       #'org-add-note
        (kbd "o t")       #'org-set-tags-command
        (kbd "o q")       #'org-todo
        (kbd "o g")       #'org-open-at-point
        (kbd "o e")       #'org-set-effort
        (kbd "o O")       #'org-toggle-ordered-property
        (kbd "o B")       #'org-toggle-checkbox
        (kbd "o r")       #'org-refile
        (kbd "o C i")     #'org-clock-in
        (kbd "o C o")     #'org-clock-out
        (kbd "o C r")     #'org-clock-report
        (kbd "o v t")     #'org-tags-expand
        (kbd "M-<RET>")   #'org-insert-heading-respect-content
        (kbd "o i")       #'org-insert-todo-heading-respect-content
        ;; the following should mirror leader keys
        (kbd "o a")       #'org-agenda
        (kbd "o T")       #'org-todo-list
        (kbd "o c")       #'org-capture
        (kbd "o a")       #'org-agenda))))

(use-package org-roam
  :after (org age)
  :straight (org-roam :type git :host github :repo "org-roam/org-roam"
                      :files (:defaults "extensions/*"))
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.org\\.age" . org-mode)))

(use-package org-crypt
  ;;  builtin
  :straight nil
  :after (org)
  :demand t
  :custom
  (org-crypt-key nil)
  ;; tag entries with “crypt” to auto‐encrypt them
  (org-crypt-tag-matcher "crypt")
  ;; don’t leave an unencrypted copy on disk
  (org-crypt-disable-auto-save t)
  :init
  ;; hook in before-save so that tagged entries auto‐encrypt
  ;; (org-crypt-use-before-save-magic)
  :config

  (defun my-org-crypt-encrypt-entry (&rest _)
    (interactive)
    (let ((beg (point-at-bol))
          (end (save-excursion (org-end-of-subtree t))))
      (age-encrypt-region beg end)))

  (defun my-org-crypt-decrypt-entry (&rest _)
    (interactive)
    (let ((beg (point-at-bol))
          (end (save-excursion
                 (search-forward "-----END AGE ENCRYPTED")
                 (point))))
      (age-decrypt-region beg end)))

  ;;;; override the GPG calls to use age.el instead:
  ;;(defun my/org-crypt-encrypt-entry (&rest _args)
  ;;  "Encrypt current subtree with age.el."
  ;;  (let ((beg (point-at-bol))
  ;;        (end (save-excursion (org-end-of-subtree t))))
  ;;    (age-encrypt-region beg end)))
  ;;(defun my/org-crypt-decrypt-entry (&rest _args)
  ;;  "Decrypt an age-encrypted block at point."
  ;;  (let ((beg (point-at-bol))
  ;;        (end (save-excursion
  ;;               (search-forward "-----END AGE ENCRYPTED")
  ;;               (point))))
      ;; (age-decrypt-region beg end)))
  (advice-add 'org-crypt-encrypt-entry :override #'my-org-crypt-encrypt-entry)

  (advice-add 'org-crypt-decrypt-entry :override #'my-org-crypt-decrypt-entry))

(provide 'my-org)
