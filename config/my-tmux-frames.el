;;; tmux-frames.el --- one Emacs server per tmux session  -*- lexical-binding:t; -*-


(require 'server)

;; 1.  Choose a socket name once, based on tmux session
(setq server-name
      (if (getenv "TMUX")
          (format "tmux-%s"
                  (string-trim
                   (shell-command-to-string
                    "tmux display-message -p '#S' 2>/dev/null")))
        "server"))

(unless (server-running-p) (server-start))

(defun my/tmux-new-frame (&optional buffer)
  "Open BUFFER in a new tmux window (as a TTY frame of this Emacs server),
   with correct shell quoting so the pane stays open."
  (interactive)
  (let* ((buf  (or buffer (current-buffer)))
         (lisp (format "(switch-to-buffer %S)" (buffer-name buf)))
         ;; Properly quote the Lisp string
         (ecmd (format "emacsclient -s %s -nw -c -e %s"
                       server-name
                       (shell-quote-argument lisp)))
         ;; Wrap entire command in a quoted string for sh -c '...'
         (cmd  (format "sh -c %s"
                       (shell-quote-argument
                        (concat ecmd " ; exec $SHELL -l")))))
    (message "Launching: %s" cmd)
    (start-process "tmux-frame" nil
                   "tmux" "new-window" "-n" "emacs" cmd)))

;; Handy alias for M‑x completion
(defalias 'my-tmux-new-frame #'my/tmux-new-frame)

(provide 'my-tmux-frames)
