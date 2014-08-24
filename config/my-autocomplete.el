(use-package company
  :ensure company
  :init
  (progn
    (global-company-mode)
    )
  :config
  (progn
    ;; C-hjkl in company-mode
    (define-key company-active-map (kbd "C-h") 'company-show-doc-buffer)
    (define-key company-active-map (kbd "C-l") 'company-show-location)
    (define-key company-active-map (kbd "C-j") 'company-select-next)
    (define-key company-active-map (kbd "C-k") 'company-select-previous)
    (define-key company-active-map "\e\e\e" 'company-abort)
    (define-key company-active-map "\C-g" 'company-abort)
    (define-key company-active-map (kbd "<down>") 'company-select-next-or-abort)
    (define-key company-active-map (kbd "<up>") 'company-select-previous-or-abort)
    (define-key company-active-map [down-mouse-1] 'ignore)
    (define-key company-active-map [down-mouse-3] 'ignore)
    (define-key company-active-map [mouse-1] 'ignore)
    (define-key company-active-map [mouse-3] 'ignore)
    (define-key company-active-map [up-mouse-1] 'ignore)
    (define-key company-active-map [up-mouse-3] 'ignore)
    (define-key company-active-map [return] 'company-complete-selection)
    (define-key company-active-map (kbd "RET") 'company-complete-selection)
    (define-key company-active-map [tab] 'company-complete-common)
    (define-key company-active-map (kbd "TAB") 'company-complete-common)
    (define-key company-active-map (kbd "<f1>") 'company-show-doc-buffer)
    (define-key company-active-map "\C-w" 'company-show-location)
    (define-key company-active-map "\C-s" 'company-search-candidates)
    (define-key company-active-map "\C-\M-s" 'company-filter-candidates)
    )
  )
;; (use-package auto-complete
;;   :diminish auto-complete-mode
;;   :commands global-auto-complete-mode
;;   :idle (global-auto-complete-mode)
;;   :config
;;   (progn
;;     (require 'auto-complete-config)
;;     ;; Needed for pretty completion menus
;;     (setq
;;         ac-auto-show-menu 1.0
;;         ac-auto-start 1
;;         ac-use-quick-help nil
;;         ac-comphist-file (concat user-emacs-directory ".cache/ac-comphist.dat")
;;         ac-quick-help-delay 0.1
;;         ac-quick-help-height 30

;;         ;; Ignore case when the input contains uppercase characters
;;         ac-ignore-case 'smart
;;         ac-show-menu-immediately-on-auto-complete t
;;         ac-use-fuzzy t
;;         )
;;     ;; Bind TAB for auto complete, but only when actually performing autocompletion
;;     (ac-set-trigger-key "TAB")
;;     (ac-config-default)
;;     (dolist (mode '(vimrc-mode))
;;       (add-to-list 'ac-modes mode))
;;     (after 'linum
;;       (ac-linum-workaround))

;;     ;; From AC manual:
;;     ;;
;;     ;; In this case, it is better that selecting candidates is enabled only when
;;     ;; completion menu is displayed so that the key input will not be taken as
;;     ;; much as possible. ac-menu-map is a keymap for completion on completion
;;     ;; menu which is enabled when ac-use-menu-map is t.
;;     (setq ac-use-menu-map t)
;;     (define-key ac-menu-map "\C-n" 'ac-next)
;;     (define-key ac-menu-map "\C-p" 'ac-previous)
;;     )
;;   )

;; (use-package ac-etags
;;   :ensure ac-etags
;;   :init
;;   (progn
;;     (setq ac-etags-requires 1)
;;     (after 'etags
;;       (ac-etags-setup)
;;       )
;;     )
;;   )

(provide 'my-autocomplete)
