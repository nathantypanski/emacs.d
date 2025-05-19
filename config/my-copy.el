;; my-copy.el   -*- lexical-binding:t; -*-

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
      (insert text))))

(provide 'my-copy)
