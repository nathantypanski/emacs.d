(after 'evil
    (defcustom evil-org-mode-hook nil
      "Mode hook for Evil-Org-mode, run after the mode was turned on."
      :group 'evil-org
      :type 'hook)

    (defcustom evil-org-load-hook nil
      "Hook that is run after evil-org-mode has been loaded."
      :group 'evil-org
      :type 'hook)

    (define-minor-mode evil-org-mode
      "Buffer local minor mode for evil-org"
      :init-value nil
      :mode org-mode
      :diminsh evil-org-mode
      :lighter " EvilOrg"
      :keymap (make-sparse-keymap) ; defines evil-org-mode-map
      :group 'evil-org
      :)
    (add-hook 'org-mode-hook 'evil-org-mode) ;; only load with org-mode

    (defun always-insert-item ()
      "Force insertion of org item"
      (if (not (org-in-item-p))
          (insert "\n- ")
        (org-insert-item))
      )

    (setq evil-auto-indent 1)

    (defun evil-org-eol-call (fun)
      "Go to end of line and call provided function"
      (end-of-line)
      (funcall fun)
      (evil-append nil)
      )

    (evil-define-key 'normal evil-org-mode-map (kbd "<") 'org-metaleft)
    (evil-define-key 'normal evil-org-mode-map (kbd ">") 'org-metaright)
    (evil-define-key 'normal evil-org-mode-map "gj" (if (fboundp 'org-forward-same-level)))
    ;; normal state shortcuts
    ;; (evil-define-key 'normal evil-org-mode-map
    ;;   "gh" 'outline-up-heading
      "gj" (if (fboundp 'org-forward-same-level) ;to be backward compatible with older org version
    ;;            'org-forward-same-level
    ;;          'org-forward-heading-same-level)
    ;;   "gk" (if (fboundp 'org-backward-same-level)
    ;;            'org-backward-same-level
    ;;          'org-backward-heading-same-level)
    ;;   "gl" 'outline-next-visible-heading
    ;;   "t" 'org-todo
    ;;   "T" '(lambda () (interactive) (evil-org-eol-call '(org-insert-todo-heading nil)))
    ;;   "H" 'org-beginning-of-line
    ;;   "L" 'org-end-of-line
    ;;   ";t" 'org-show-todo-tree
    ;;   "o" '(lambda () (interactive) (evil-org-eol-call 'always-insert-item))
    ;;   "O" '(lambda () (interactive) (evil-org-eol-call 'org-insert-heading))
    ;;   "$" 'org-end-of-line
    ;;   "^" 'org-beginning-of-line
    ;;   "<" 'org-metaleft
    ;;   ">" 'org-metaright
    ;;   "&" 'org-edit-src-code
    ;;   ";a" 'org-agenda
    ;;   "-" 'org-cycle-list-bullet
    ;;   (kbd "TAB") 'org-cycle)

    ;; normal & insert state shortcuts.
    ;; (mapc (lambda (state)
    ;;         (evil-define-key state evil-org-mode-map
    ;;           (kbd "M-l") 'org-metaright
    ;;           (kbd "M-h") 'org-metaleft
    ;;           (kbd "M-k") 'org-metaup
    ;;           (kbd "M-j") 'org-metadown
    ;;           (kbd "M-L") 'org-shiftmetaright
    ;;           (kbd "M-H") 'org-shiftmetaleft
    ;;           (kbd "M-K") 'org-shiftmetaup
    ;;           (kbd "M-J") 'org-shiftmetadown
    ;;           (kbd "M-o") '(lambda () (interactive)
    ;;                          (evil-org-eol-call
    ;;                           '(lambda()
    ;;                              (org-insert-heading)
    ;;                              (org-metaright))))
    ;;           (kbd "M-t") '(lambda () (interactive)
    ;;                          (evil-org-eol-call
    ;;                           '(lambda()
    ;;                              (org-insert-todo-heading nil)
    ;;                              (org-metaright))))
    ;;           ))
    ;;       '(normal insert))
    )
(provide 'my-evil-org-mode)

(run-hooks 'evil-org-load-hook)
