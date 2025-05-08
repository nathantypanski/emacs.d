(require 'server)
(setq server-name (format "o3-%d" (emacs-pid)))   ;; e.g. o3-10543
(add-hook 'emacs-startup-hook
          (lambda () (set-process-sentinel (get-buffer-process "*scratch*") #'ignore))) ;; dummy to keep compiler quiet
(define-key global-map (kbd "C-x C-c")
  (lambda () (interactive) (save-buffers-kill-terminal)))
(setq inhibit-splash-screen t)
;; The line below really does the work: remove SIGHUP’s default “quit” action.
(set-process-sentinel (start-process "ignore-hup" nil "sh" "-c" "trap '' HUP; sleep infinity")
                      #'ignore)
(unless (server-running-p) (server-start))

(defun my/new-tty-frame ()
  "Open selected buffer in a new tmux or foot window; keep window open."
  (interactive)
  (let* ((buf  (if (derived-mode-p 'ibuffer-mode)
                   (ibuffer-current-buffer)
                 (current-buffer)))
         (ecmd (list "emacsclient" "-s" server-name "-nw" "-c"
                     "-e" (format "(switch-to-buffer %S)" (buffer-name buf)))))
    (if (getenv "TMUX")
        ;; --- tmux path ------------------------------------------------
        ;; tmux wants one command string, but we build it safely:
        (start-process "tty-frame" nil
                       "tmux" "new-window" "-n" "emacs"
                       (string-join
                        (append ecmd '(";" "$SHELL" "-l")) " "))
      ;; --- foot path -------------------------------------------------
      (apply #'start-process "tty-frame" nil
             "foot" "-e" (append ecmd '(";" "exec" "$SHELL" "-l"))))))

(defalias 'my-new-tty-frame #'my/new-tty-frame)

(defun my/ibuffer-raise-other-window ()
  "Show *Ibuffer* in the other window of THIS TTY frame."
  (interactive)
  (let* ((buf (ibuffer-list-buffers))
         (win (get-buffer-window buf 0))) ; 0 ⇒ search only this frame
    (if win
        (select-window win)
      (ibuffer-other-window))))

(defalias 'my-ibuffer-raise-other-window #'my/ibuffer-raise-other-window)

(defun my/ibuffer-show-all ()
  "Open Ibuffer with every buffer from this Emacs, ignoring filters
and never‑show predicates."
  (interactive)
  (let ((ibuffer-never-show-predicates nil)
        (current-prefix-arg '(4)))        ; same as `C-u`
    (call-interactively #'ibuffer)))

(defalias 'my-ibuffer-show-all #'my/ibuffer-show-all)

(provide 'my-multi-tty-tools)
