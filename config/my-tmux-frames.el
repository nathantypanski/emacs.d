;;; tmux-frames.el --- one Emacs server per tmux session  -*- lexical-binding:t; -*-

(require 'server)

(use-package emamux
  :straight t
  :demand
  :ensure
  :config
  (setq server-name
        (if (getenv "TMUX")
            (format "tmux-%s"
                    (string-trim
                     (shell-command-to-string
                      "tmux display-message -p '#S' 2>/dev/null")))
          "server"))

  (unless (server-running-p) (server-start))

  (defun my-tmux-new-frame (&optional buffer)
    "Open BUFFER in a new tmux window (inside tmux) or in Foot (outside).
Connects to this Emacs’s server socket and quits when you close the client."
    (interactive)
    (let* ((buf  (or buffer (current-buffer)))
           (lisp (format "(switch-to-buffer %S)" (buffer-name buf)))
           (sock (expand-file-name server-name server-socket-dir))
           ;; build args list instead of wrapping in `sh -c …`
           (args (list "--socket-name" sock "-nw" "-c" "-e" lisp)))
      (if (getenv "TMUX")
          ;; inside tmux: use Emamux to spawn a new window
          (apply #'emamux:tmux-run-command
                 nil "new-window" "-n" "emacs" "emacsclient" args)
        ;; outside tmux: spawn Foot directly on emacsclient
        (apply #'start-process
               "foot-frame" nil
               "foot" "-e" "emacsclient" args)))))

(provide 'my-tmux-frames)
