;;; tmux‑unique‑server.el  -*- lexical-binding:t; -*-

(defun my/spawn-detached-emacs (&optional file)
  "Launch a new, fully detached `emacs -nw' in its own tmux/Foot window.
If FILE is non‑nil (or given with a prefix argument), visit that file
immediately; otherwise show *scratch*."
  (interactive
   (list (when current-prefix-arg
           (read-file-name "Open file in new Emacs: "))))
  (let* ((filearg (if file (shell-quote-argument file) ""))   ; keep empty string if nil
         ;; 1. Command that *runs inside* the new window:
         ;;    script ⇒ new PTY, setsid ⇒ detach from parent TTY.
         (inner (format "script -q /dev/null setsid -f emacs -nw %s"
                        filearg))
         ;; 2. Append '; exec $SHELL -l' so the pane remains interactive.
         (cmd   (format "%s ; exec $SHELL -l" inner)))
    (if (getenv "TMUX")
        ;; tmux expects one quoted string
        (start-process "detached-emacs" nil
                       "tmux" "new-window" "-n" "emacs" "sh" "-c" cmd)
      ;; Foot path: argv items are separate
      (start-process "detached-emacs" nil
                     "foot" "-e" "sh" "-c" cmd))))

(defalias 'my-spawn-detached-emacs #'my/spawn-detached-emacs)

;; (defun my/ibuffer-raise-other-window ()
;;   "Display *Ibuffer* in the other window and select it.
;; If *Ibuffer* is already visible anywhere, just raise that window."
;;   (interactive)
;;   (if-let ((win (get-buffer-window "*Ibuffer*" t)))   ; t ⇒ search all frames :contentReference[oaicite:1]{index=1}
;;       (select-window win)                             ; make it current        :contentReference[oaicite:2]{index=2}
;;     (ibuffer-other-window)))                          ; built‑in command       :contentReference[oaicite:3]{index=3}

(defun my/ibuffer-raise-other-window ()
  (interactive)
  (let* ((buf (ibuffer-list-buffers))
         (win (get-buffer-window buf 0))) ; 0 ⇒ search *this* frame only :contentReference[oaicite:12]{index=12}
    (if win (select-window win) (ibuffer-other-window))))

(defalias 'my-ibuffer-raise-other-window #'my/ibuffer-raise-other-window)

(provide 'my-tmux-unique-server)

