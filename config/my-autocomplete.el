(use-package auto-complete
  :ensure auto-complete
  :diminish auto-complete-mode
  :commands global-auto-complete-mode
  :idle (global-auto-complete-mode)
  :config
  (progn
    (require 'auto-complete-config)
    ;; Needed for pretty completion menus
    (setq
        ac-auto-show-menu 1.0
        ac-auto-start 4
        ac-use-quick-help nil
        ac-comphist-file (concat user-emacs-directory ".cache/ac-comphist.dat")
        ac-quick-help-delay 0.3
        ac-quick-help-height 30

        ;; Ignore case when the input contains uppercase characters
        ac-ignore-case 'smart
        ac-show-menu-immediately-on-auto-complete t
        ac-use-fuzzy t
        )

    ;; Bind TAB for auto complete, but only when actually performing autocompletion
    (ac-set-trigger-key "TAB")

    (ac-config-default)

    (dolist (mode '(vimrc-mode))
      (add-to-list 'ac-modes mode))
    (after 'linum
      (ac-linum-workaround))

    ;; From AC manual:
    ;;
    ;; In this case, it is better that selecting candidates is enabled only when
    ;; completion menu is displayed so that the key input will not be taken as
    ;; much as possible. ac-menu-map is a keymap for completion on completion
    ;; menu which is enabled when ac-use-menu-map is t.
    (setq ac-use-menu-map t)
    (define-key ac-menu-map "\C-n" 'ac-next)
    (define-key ac-menu-map "\C-p" 'ac-previous)

    (use-package auto-complete-config
      :config
      (progn
        (ac-config-default)
        )
      )
    )
  )
(provide 'my-autocomplete)
