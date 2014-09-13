(after 'evil
  (defun my-package-list-get-q-key ()
    "Lookup the current meaning of `q` key in the active package-menu-mode"
    (interactive)
    (call-interactively (lookup-key package-menu-mode-map "q"))
    )
  (evil-set-initial-state 'package-menu-mode 'normal)
  (evil-define-key 'normal package-menu-mode-map
    ;; bound when package-menu is initialized
    ;; for clearing filters.
    "q" 'my-package-list-get-q-key
    "d" 'package-menu-mark-delete
    "i" 'package-menu-mark-install
    "U" 'package-menu-mark-upgrades
    "r" 'package-menu-refresh
    "K" 'package-menu-describe-package
    "u" 'package-menu-mark-unmark
    "\177" 'package-menu-backup-unmark
    "d" 'package-menu-mark-delete
    "i" 'package-menu-mark-install
    "U" 'package-menu-mark-upgrades
    "r" 'package-menu-refresh
    "/" 'package-menu-filter
    "~" 'package-menu-mark-obsolete-for-deletion
    "x" 'package-menu-execute
    "?" 'package-menu-quick-help
    )
  (evil-define-key 'motion package-menu-mode-map
    "q" 'my-package-list-get-q-key
    "h" 'evil-backward-char
    "j" 'evil-next-visual-line
    "k" 'evil-previous-visual-line
    "l" 'evil-forward-char
    )
  )

(provide 'my-package-list)
