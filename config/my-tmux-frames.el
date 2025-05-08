;;; tmux-frames.el --- one Emacs server per tmux session  -*- lexical-binding:t; -*-

(require 'server)

(setq server-name
      (if (getenv "TMUX")
          (format "tmux-%s"
                  (string-trim
                   (shell-command-to-string
                    "tmux display-message -p '#S' 2>/dev/null")))
        "server"))

(unless (server-running-p) (server-start))

(defun my/tmux-new-frame (&optional buffer)
  "Open BUFFER in a new tmux window (if inside tmux), or Foot if not.
Always connects to THIS Emacs’s actual server socket. Leaves a shell after."
  (interactive)
  (require 'server)
  (let* ((buf     (or buffer (current-buffer)))
         (lisp    (format "(switch-to-buffer %S)" (buffer-name buf)))
         (sock    (expand-file-name server-name server-socket-dir))
         (ecmd    (format "emacsclient --socket-name=%s -nw -c -e %s"
                          (shell-quote-argument sock)
                          (shell-quote-argument lisp)))
         (shell-cmd (format "sh -c %s"
                            (shell-quote-argument
                             (concat ecmd " ; exec $SHELL -l")))))
    (message "Launching: %s" shell-cmd)
    (if (getenv "TMUX")
        (start-process "tmux-frame" nil
                       "tmux" "new-window" "-n" "emacs" shell-cmd)
      (start-process "foot-frame" nil
                     "foot" "-e" "sh" "-c"
                     (concat ecmd " ; exec $SHELL -l")))))

;; Handy alias for M‑x completion
(defalias 'my-tmux-new-frame #'my/tmux-new-frame)

(provide 'my-tmux-frames)
