;; my-org.el   -*- lexical-binding:t; -*-
;;
;; Customizations for org-mode.


;; TODO: root cause this:
;;
;; Error (wrong-number-of-arguments #<subr org-persist--find-index>
;; 2); continue? (y or n)

(use-package org
  :commands (org-mode org-capture org-agenda orgtbl-mode)
  :straight nil
  :hook ((org-mode . my-org-setup-keybindings)
         (org-mode . visual-line-mode))
  :init
  (global-set-key (kbd "C-c c") #'org-capture)
  (global-set-key (kbd "C-c a") #'org-agenda)

  (let* ((todo-file  (my-home-path "notes/todo/todo.org"))
         (home-file  (my-home-path "notes/todo/home.org"))
         (agenda-files (list todo-file home-file))
         (notes-file (concat (getenv "HOME") "/notes/notes.org")))
    (when (and (file-exists-p todo-file)
               (file-exists-p home-file))
      (setq org-agenda-files agenda-files)
      (setq org-capture-templates
            `(("t" "Tasks" entry
               (file+headline ,todo-file "Tasks")
               "* TODO %?\n  %U\n  %a")
              ("h" "Home Tasks" entry
               (file+headline ,home-file "Home Tasks")
               "* TODO %?\n  %u"))))

    (when (file-exists-p notes-file)
      (setq org-default-notes-file notes-file)))

  (defun my-org-flatten-headings (depth)
    "Convert all headings to level 2."
    (interactive "p")
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^\\*+" nil t)
        (replace-match "**")))
    (save-excursion
     (goto-char (point-min))
     (while (re-search-forward "^\\*[^\*]" nil t)
       (replace-match "**"))))

  (defun my-org-fontify-tool-blocks (limit)
    "Fontify #+begin_tool … #+end_tool like src blocks."
    (let ((case-fold-search t)
          found)
      (while (and (< (point) limit)
                  (re-search-forward "^[ \t]*#\\+begin_tool\\b.*$" limit t))
        (setq found t)
        (let* ((beg-line-b (match-beginning 0))
               (beg-line-e (match-end 0))
               (end-line-beg
                (save-excursion
                  (if (re-search-forward "^[ \t]*#\\+end_tool\\b.*$" nil t)
                      (match-beginning 0)
                    (point-max))))
               (end-line-end
                (save-excursion
                  (goto-char end-line-beg)
                  (line-end-position))))
          (put-text-property beg-line-b beg-line-e 'face 'org-block-begin-line)
          (put-text-property beg-line-e end-line-beg 'face 'org-block)
          (put-text-property end-line-beg end-line-end 'face 'org-block-end-line)
          (put-text-property beg-line-b end-line-end 'font-lock-multiline t)))
      found))
  (font-lock-add-keywords 'org-mode '((my-org-fontify-tool-blocks)) 'append)
  :custom
  (org-edit-src-content-indentation 0)
  (org-auto-align-tags nil)
  (org-tags-column 0)
  (org-catch-invisible-edits 'show-and-error)
  (org-special-ctrl-a/e t)
  (org-insert-heading-respect-content t)

  ;; Org styling, hide markup etc.
  (org-hide-emphasis-markers t)
  (org-pretty-entities t)
  (org-agenda-tags-column 0)
  (org-ellipsis "…")

  (org-startup-folded nil)
  (org-log-done t)
  (org-src-fontify-natively t)
  (org-agenda-prefix-format
   '((home  . "  %i %-12:c%?-12t% s")
     (todo    . "  %i %-12:c [%e] %b ")
     (tags    . "  %i %-12:c")
     (search  . "  %i %-12:c")))

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
  (defface org-block-begin-line '((t (:inherit org-meta-line :background "gray27" :overline "gray20" :underline "gray20" :height 0.8)))
    "Face used for the line delimiting the begin of source blocks.")

  (defface org-block-background
    '((t (:background "#FFFFEA")))
    "Face used for the source block background.")

  (defface org-block-end-line
    '((t (:inherit org-meta-line :background "gray27" :overline "gray20" :underline "gray20" :height 0.8)))
    "Face used for the line delimiting the end of source blocks.")

  ;; better default
  (add-hook 'org-mode-hook 'org-indent-mode)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((R . t)
     (emacs-lisp . t)
     (python . t)))

  (require 'org-tempo)

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

  (defun my-org-setup-keybindings ()
    "setup my keybindings for org-mode"
    (interactive)

    (general-define-key
     :states '(insert)
     :keymaps 'org-mode-map
     (kbd "<tab>")   #'my-org-tab-action
     (kbd "<RET>")   #'org-return)

    (general-define-key
     :states '(insert)
     :keymaps 'local
     "RET"             #'org-return
     (kbd "RET")       #'org-return
     (kbd "<return>")  #'org-return
     [return]          #'org-return)

    (general-define-key
     :states '(normal)
     :keymaps 'org-mode-map
     "SPC"             '(:ignore t :which-key "org")
     "SPC ["           #'org-metaup
     "SPC ]"           #'org-metadown
     "SPC j"           #'org-insert-subheading
     "SPC h"           #'org-insert-heading
     "SPC }"           #'org-do-demote
     "SPC {"           #'org-do-promote
     "SPC d"           #'org-deadline
     "SPC s"           #'org-schedule
     "SPC p"           #'org-priority
     "SPC z"           #'org-add-note
     "SPC t"           #'org-set-tags-command
     "SPC q"           #'org-todo
     "SPC o"           #'org-open-at-point
     "SPC e"           #'org-set-effort
     "SPC o"           #'org-toggle-ordered-property
     "SPC b"           #'org-toggle-checkbox
     "SPC r"           #'org-refile
     "SPC C i"         #'org-clock-in
     "SPC C o"         #'org-clock-out
     "SPC C r"         #'org-clock-report
     "SPC v t"         #'org-tags-expand
     "m-<ret>"         #'org-insert-heading-respect-content
     "SPC i"           #'org-insert-todo-heading-respect-content
     "SPC a"           #'org-agenda
     "SPC t"           #'org-todo-list
     "SPC c"           #'org-capture
     "SPC SPC RET"         #'org-insert-heading-after-current
     "SPC RET"         #'org-babel-execute-maybe
     "[ ["             #'org-previous-visible-heading
     "] ]"             #'org-next-visible-heading)))

(use-package org-roam
  :after (org age)
  :straight (:repo "org-roam/org-roam"
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

(use-package org-modern
  :ensure t
  :straight (:repo "minad/org-modern")
  :custom
  (org-modern-block-name t)
  :config
  (global-org-modern-mode)

  (defun my-setup-org-faces (&optional height)
    "Do the org face setup (code block style, etc.)."
    (interactive)
    (when (my-display-gui-p)
      ;; Spice things up a bit. Use a classic computing style font for the
      ;; src block headings.
      (let ((font-height (if height height 100)))
        ;; Spice things up a bit. Use a classic computing style font for the
        ;; src block headings.
        (set-face-attribute 'org-modern-block-name nil
                            :family "DepartureMono Nerd Font"
                            :height 100
                            :weight 'bold) ;; For the language identifier
        ;; src block headings.
        (set-face-attribute 'org-block nil
                            :family "DepartureMono Nerd Font"
                            :height font-height
                            :weight 'regular) ;; For the language identifier
        (set-face-attribute 'org-block-begin-line nil
                            :height font-height
                            :font "DepartureMono Nerd Font")
        (set-face-attribute 'org-block-end-line nil
                            :height font-height
                            :font "DepartureMono Nerd Font")
        ;; (set-face-attribute 'org-modern-block nil)

        (message "setting face height attribute to `%s'" font-height)
        (set-face-attribute 'org-block nil
                            :height font-height
                            :font "DepartureMono Nerd Font"
                            :weight 'regular)
        (set-face-attribute 'org-block-begin-line nil
                            :height font-height
                            :font "DepartureMono Nerd Font")
        (set-face-attribute 'org-block-end-line nil
                            :height font-height
                            :font "DepartureMono Nerd Font")
        (set-face-attribute 'org-modern-block-name nil
                            :family "DepartureMono Nerd Font"
                            :height font-height
                            :weight 'bold))
      ;; For each org buffer, force-load the new faces.
      (dolist (buf (buffer-list))
        (with-current-buffer buf
          (when (derived-mode-p 'org-mode)
        (font-lock-flush))))))

  (add-hook 'org-mode-hook 'my-setup-org-faces)
  )

(provide 'my-org)
