;; my-copy.el   -*- lexical-binding:t; -*-

(defvar my-copy-command
  (cond
   ((eq system-type 'darwin) "pbcopy")
   ((eq system-type 'gnu/linux) "wl-copy"))
  "Command used for wm (i.e., outside-emacs) copies of text.")

(defvar my-paste-command
  (cond
   ((eq system-type 'darwin) "pbpaste")
   ((eq system-type 'gnu/linux) "wl-paste -n"))
  "Command used for pasting into emacs.")

;; General-purpose clipboard copy function
(defun my-wl-copy-region (start end)
  "Copy region from START to END to system clipboard using wl-copy."
  (interactive "r")
  (let ((text (buffer-substring-no-properties start end)))
    (with-temp-buffer
      (insert text)
      (call-process-region (point-min) (point-max) my-copy-command))
    (message "%s" "Copied to clipboard via 'my-copy-command'")))

;; General-purpose paste function
(defun my-wl-paste-insert ()
  "Insert system clipboard contents at point using wl-paste."
  (interactive)
  (let ((text (shell-command-to-string my-paste-command)))
    (insert text)))

(provide 'my-copy)
