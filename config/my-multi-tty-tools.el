;;; my-multi-tty-tools.el  -*- lexical-binding:t; -*-

;; 0.  One socket per Emacs + ignore SIGHUP

;; 1) Pick a unique name at startup
;; (setq server-name (format "o3-%d" (emacs-pid)))   ;; e.g. o3-10543

;; 2) Optional: pick a directory (else use default ~/.emacs.d/server/)
;; (setq server-socket-dir (expand-file-name "~/.cache/emacs/sockets"))

;; 3) Start (or reuse) the server AFTER both vars are set
(require 'server)
(unless (server-running-p) (server-start))        ;; creates the socket file

(defun my/new-tty-frame ()
  "Open current / Ibuffer‑highlighted buffer in a new tmux or Foot window.
The new frame shares THIS Emacs’s buffers and a login shell keeps the pane
alive (no more ‘unknown file attribute’ from zsh)."
  (interactive)
  (let* ((buf  (if (derived-mode-p 'ibuffer-mode)
                   (ibuffer-current-buffer)
                 (current-buffer)))
         (lisp  (format "(switch-to-buffer %S)" (buffer-name buf)))
         (ecmd  (list "emacsclient" "-s" server-name "-nw" "-c" "-e" lisp)))

    (if (getenv "TMUX")
        (let* ((cmd-string
                (string-join
                 (append ecmd '(";" "exec" "$SHELL" "-l")) " "))) ; keep shell
          (start-process
           "new-tty-frame" nil
           "tmux" "new-window" "-n" "emacs"
           cmd-string))
      ;; ---------------- foot path -----------------
      (apply #'start-process
             "new-tty-frame" nil
             "foot" "-e"
             (append ecmd '(";" "exec" "$SHELL" "-l"))))))

(defalias 'my-new-tty-frame #'my/new-tty-frame)

(defun my/open-buffer-in-tmux-or-foot ()
  "Open the current / Ibuffer‑highlighted buffer in:
   • a *new tmux window* if running inside tmux, or
   • a *new foot* terminal frame otherwise."
  (interactive)
  (let* ((buf  (if (derived-mode-p 'ibuffer-mode)
                   (ibuffer-current-buffer)     ; highlighted buffer
                 (current-buffer)))             ; normal buffer
         (file (buffer-file-name buf))
         (ecmd (if file                             ; emacsclient cmd
                   (format "emacsclient -nw -c -a '' %s"
                           (shell-quote-argument file))
                 (format "emacsclient -nw -c -n -a '' -e \"(switch-to-buffer \\\"%s\\\")\""
                         (buffer-name buf)))))
    (if (getenv "TMUX")                          ; inside tmux?
        ;; ---------- NEW WINDOW ----------
        (start-process-shell-command
         "tmux+emacsclient" nil
         (format "tmux new-window \"%s\"" ecmd)) ; new window ➜ run cmd
      ;; ---------- foot fallback ----------
      (start-process-shell-command
       "foot+emacsclient" nil
       (format "foot sh -c 'exec %s'" ecmd)))))

(defalias 'my-open-buffer-in-tmux-or-foot #'my/open-buffer-in-tmux-or-foot)

(defun my/new-shared-frame ()
  "Pop the current (or Ibuffer‑highlighted) buffer in a new tmux/foot window,
   attached to the one Emacs server."
  (interactive)
  (require 'server)                       ; ensure server-start ran
  (let* ((buf (if (derived-mode-p 'ibuffer-mode)
                  (ibuffer-current-buffer)
                (current-buffer)))
         (cmd (format "emacsclient -nw -c -e '(switch-to-buffer %S)'" (buffer-name buf))))
    (start-process
     "shared-frame" nil
     (if (getenv "TMUX") "tmux" "foot")
     (if (getenv "TMUX")
         "new-window" "-n" "emacs" "bash" "-lc"
         "-e" "bash" "-lc")
     (concat cmd " ; exec $SHELL -l"))))

(defalias 'my-new-shared-frame #'my/new-shared-frame)

(defun my/ibuffer-raise ()
  "Show *Ibuffer*: if already visible, just select that window.
Otherwise create/reuse a window for it and select that window."
  (interactive)
  (let* ((buf (ibuffer-list-buffers))               ; refresh & return buffer
         (win (get-buffer-window buf t)))           ; t ⇒ search all frames
    (if win
        (select-window win)                         ; raise existing window
      (pop-to-buffer                                ; else create/reuse & jump
       buf
       '((display-buffer-reuse-window               ; try to reuse first
         display-buffer-pop-up-window))))))         ; then pop a new one

;; Optional: dash‑style alias so both names work
(defalias 'my-ibuffer-raise #'my/ibuffer-raise)


(defun my/ibuffer-raise-other-window ()
  "Display *Ibuffer* in the other window and select it.
If *Ibuffer* is already visible anywhere, just raise that window."
  (interactive)
  (if-let ((win (get-buffer-window "*Ibuffer*" t)))   ; t ⇒ search all frames :contentReference[oaicite:1]{index=1}
      (select-window win)                             ; make it current        :contentReference[oaicite:2]{index=2}
    (ibuffer-other-window)))                          ; built‑in command       :contentReference[oaicite:3]{index=3}

(defalias 'my-ibuffer-raise-other-window #'my/ibuffer-raise-other-window)

(defun my/ibuffer-raise-other-window ()
  "Open Ibuffer ignoring saved groups, filters, predicates."
  (interactive)
  (let ((ibuffer-never-show-predicates nil)
        ;; same as C-u
        (current-prefix-arg '(4)))
    (call-interactively #'ibuffer)))
(defalias 'my-ibuffer-raise-other-window #'my/ibuffer-raise-other-window)

(provide 'my-multi-tty-tools)
;;; my-multi-tty-tools.el ends here

