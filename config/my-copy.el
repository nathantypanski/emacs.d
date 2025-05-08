(when (getenv "WAYLAND_DISPLAY")
  ;; General-purpose clipboard copy function
  (defun my-wl-copy-region (start end)
    "Copy region from START to END to system clipboard using wl-copy."
    (interactive "r")
    (let ((text (buffer-substring-no-properties start end)))
      (with-temp-buffer
        (insert text)
        (call-process-region (point-min) (point-max) "wl-copy"))
      (message "Copied to clipboard via wl-copy")))

  ;; General-purpose paste function
  (defun my-wl-paste-insert ()
    "Insert system clipboard contents at point using wl-paste."
    (interactive)
    (let ((text (shell-command-to-string "wl-paste -n")))
      (insert text)))

  ;; Operator-compatible version of wl-copy (works with evil or M-x)
  (defun my-wl-copy-operator (beg end &optional _type)
    "Copy region to clipboard using wl-copy. Works with Evil and M-x."
    (interactive
     (if (use-region-p)
         (list (region-beginning) (region-end))
       (error "No active region")))
    (my-wl-copy-region beg end))

  ;; Optional global keybindings (non-Evil)
  (global-set-key (kbd "C-c w c") #'my-wl-copy-operator)
  (global-set-key (kbd "C-c w v") #'my-wl-paste-insert)

  ;; Evil-specific bindings
  (with-eval-after-load 'evil
    ;; Evil operator for visual mode: `yg`
    (evil-define-operator my-wl-copy-evil-operator (beg end type)
      "Evil operator wrapper for `my-wl-copy-operator` (use with `yg`)."
      (interactive "<R>")
      (my-wl-copy-operator beg end type))

    (defun my-wl-paste-evil ()
      "Paste clipboard contents at point using wl-paste, Evil-style."
      (interactive)
      (let ((text (string-trim (shell-command-to-string "wl-paste -n"))))
        (evil-with-undo
          (cond
           ((evil-visual-state-p)
            ;; Replace visual region
            (delete-region (region-beginning) (region-end))
            (evil-normal-state)
            (insert text))
           ((evil-normal-state-p)
            ;; Paste after point
            (forward-char)
            (insert text))
           (t
            ;; Fallback (e.g. insert or emacs state)
            (insert text))))))

    ;; Evil bindings
    (define-key evil-visual-state-map (kbd "yg") #'my-wl-copy-evil-operator)
    (define-key evil-normal-state-map (kbd "gP") #'my-wl-paste-evil)))

(provide 'my-copy)
