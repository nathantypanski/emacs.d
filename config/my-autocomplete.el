(use-package company
  :ensure company
  :init
  (progn
    (global-company-mode)
    )
  :config
  (progn
    (defun my-company-pass-key (arg)
      "Pass a key out of company-mode"
      (interactive "P")
      (company-abort)
      (kbd arg)
      )
    ;; C-hjkl in company-mode
    (define-key company-active-map (kbd "C-h") 'company-show-doc-buffer)
    (define-key company-active-map (kbd "C-l") 'company-show-location)
    (define-key company-active-map (kbd "C-j") 'company-select-next)
    (define-key company-active-map (kbd "C-k") 'company-select-previous)
    (define-key company-active-map (kbd "<down>") 'company-select-next-or-abort)
    (define-key company-active-map (kbd "<up>") 'company-select-previous-or-abort)
    (define-key company-active-map [down-mouse-1] 'ignore)
    (define-key company-active-map [down-mouse-3] 'ignore)
    (define-key company-active-map [mouse-1] 'ignore)
    (define-key company-active-map [mouse-3] 'ignore)
    (define-key company-active-map [up-mouse-1] 'ignore)
    (define-key company-active-map [up-mouse-3] 'ignore)
    (define-key company-active-map [return] 'company-abort)
    (define-key company-active-map (kbd "SPC") 'my-company-pass-key)
    (define-key company-active-map "\e\e\e" 'company-abort)
    (define-key company-active-map "\C-g" 'company-abort)
    (define-key company-active-map [tab] 'company-complete-selection)
    (define-key company-active-map (kbd "S-TAB") 'company-complete-common)
    (define-key company-active-map (kbd "<f1>") 'company-show-doc-buffer)
    (define-key company-active-map "\C-w" 'company-show-location)
    (define-key company-active-map "\C-s" 'company-search-candidates)
    (define-key company-active-map "\C-\M-s" 'company-filter-candidates)
    )
  )

(provide 'my-autocomplete)
