;; Basic copy-paste setup. From wiki.
(setq x-select-enable-clipboard t)
(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)

; Brilliant working copy-paste (even in Evil mode!) ripped from:
; http://hugoheden.wordpress.com/2009/03/08/copypaste-with-emacs-in-terminal/
(unless window-system
    ;; Callback for when user cuts
    (defun my-cut-function (text &optional push) ;; Insert text to temp-buffer, and "send" content to xsel stdin
      (with-temp-buffer
	(insert text)
	;; I prefer using the "clipboard" selection (the one the
	;; typically is used by c-c/c-v) before the primary selection
	;; (that uses mouse-select/middle-button-click)
        (if (string-equal (getenv "WAYLAND_DISPLAY") "wayland-0")
            (call-process-region (point-min) (point-max) "wl-copy")
          (call-process-region (point-min) (point-max) "xsel" nil 0 nil "--clipboard" "--input"))))

    ;; Call back for when user pastes
    (defun my-paste-function()
      ;; Find out what is current selection by xsel. If it is different
      ;; from the top of the kill-ring (car kill-ring), then return
      ;; it. Else, nil is returned, so whatever is in the top of the
      ;; kill-ring will be used.
      (let ((paste-output (if (string-equal (getenv "WAYLAND_DISPLAY") "wayland-0")
                             (shell-command-to-string "wl-paste")
                           (shell-command-to-string "xsel --clipboard --output"))))
	(unless (string= (car kill-ring) paste-output)
	  paste-output )))
    ;; Attach callbacks to hooks
    (setq interprogram-cut-function 'my-cut-function)

    (setq interprogram-paste-function 'my-paste-function)
    ;; Idea from
    ;; http://shreevatsa.wordpress.com/2006/10/22/emacs-copypaste-and-x/
    ;; http://www.mail-archive.com/help-gnu-emacs@gnu.org/msg03577.html
    ))
(when (getenv "WAYLAND_DISPLAY")
     ;; Callback for when user cuts
    (defun my-cut-function (text &optional push)
      ;; Insert text to temp-buffer, and "send" content to xsel stdin
      (with-temp-buffer
	(insert text)
	;; I prefer using the "clipboard" selection (the one the
	;; typically is used by c-c/c-v) before the primary selection
	;; (that uses mouse-select/middle-button-click)
        (call-process-region (point-min) (point-max) "wl-copy")))

    ;; Call back for when user pastes
    (defun my-paste-function()
      ;; Find out what is current selection by xsel. If it is different
      ;; from the top of the kill-ring (car kill-ring), then return
      ;; it. Else, nil is returned, so whatever is in the top of the
      ;; kill-ring will be used.
      (let ((paste-output (shell-command-to-string "wl-paste"))
	(unless (string= (car kill-ring) paste-output)
	  paste-output )))
    ;; Attach callbacks to hooks
    (setq interprogram-cut-function 'my-cut-function)
    (setq interprogram-paste-function 'my-paste-function)
    ;; Idea from
    ;; http://shreevatsa.wordpress.com/2006/10/22/emacs-copypaste-and-x/
    ;; http://www.mail-archive.com/help-gnu-emacs@gnu.org/msg03577.html
)

(provide 'my-copy)
