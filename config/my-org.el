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
  (org-src-fontify-natively t)
  (org-default-notes-file (concat (getenv "HOME") "/notes/notes.org"))
  (org-log-done t)
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
      (file+headline (my-home-path "notes/todo/todo.org") "Tasks")
      "* TODO %?\n  %U\n  %a")
     ("h" "Home Tasks" entry
      (file+headline (my-home-path "notes/todo/home.org") "Home Tasks")
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
       (python . t))))

  (defun my-org-tab-action ()
  "Indent or expand org-tempo template based on context."
  (interactive)
  (if (org-at-table-p)
      ;; If point is at a table, move to the next cell
      (org-table-next-field)
    ;; Else, try to expand a tempo template or indent
    (let ((pos (point)))
      (tempo-complete-tag)
      (when (= pos (point))
        (indent-for-tab-command)))))

  ;; Bind the function to TAB in Org mode
  (add-hook 'org-mode-hook
            (lambda ()
              (local-set-key (kbd "TAB") 'my-org-tab-action)))

  (require 'org-tempo)

  (tempo-define-template "org-src-block"
                         '((tempo-template-org-src))
                         "<ss"
                         "Insert a source code block in Org mode")


  (add-to-list 'org-structure-template-alist
               '("ss" . "src"))

  ;; Bind <TAB> to org-tempo-expand in Evil insert mode
  (general-define-key
   :states '(insert)
   :keymaps 'org-mode-map
   "TAB" 'tempo-complete-tag)

  (general-define-key
   :states '(normal)
   :keymaps 'org-mode-map
   "<TAB>"     #'org-cycle
   "o ["       #'org-metaup
   "o ]"       #'org-metadown
   "[ ["       #'org-previous-visible-heading
   "] ]"       #'org-next-visible-heading
   "o h"       #'org-insert-heading
   "o s"       #'org-insert-subheading
   "o <RET>"   #'org-insert-heading-after-current
   "o }"       #'org-do-demote
   "o {"       #'org-do-promote
   "o d"       #'org-deadline
   "o s"       #'org-schedule
   "o p"       #'org-priority
   "o z"       #'org-add-note
   "o t"       #'org-set-tags-command
   "o q"       #'org-todo
   "o g"       #'org-open-at-point
   "o e"       #'org-set-effort
   "o O"       #'org-toggle-ordered-property
   "o B"       #'org-toggle-checkbox
   "o r"       #'org-refile
   "o C i"     #'org-clock-in
   "o C o"     #'org-clock-out
   "o C r"     #'org-clock-report
   "o v t"     #'org-tags-expand
   "M-<RET>"   #'org-insert-heading-respect-content
   "o i"       #'org-insert-todo-heading-respect-content
   "o a"       #'org-agenda
   "o T"       #'org-todo-list
   "o c"       #'org-capture)
)

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
  (org-crypt-disable-auto-save t))

(provide 'my-org)
