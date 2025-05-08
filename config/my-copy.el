(unless (getenv "EMACS_SYSTEM_CLIPBOARD")
  ;; references;
  ;;
  ;; http://hugoheden.wordpress.com/2009/03/08/copyp;; Idea from
  ;; http://shreevatsa.wordpress.com/2006/10/22/emacs-copypaste-and-x/
  ;; http://www.mail-archive.com/help-gnu-emacs@gnu.org/msg03577.htmlaste-with-emacs-in-terminal/

  ;; Callback for when user cuts
  (defun my-cut-function (text &optional push)
    (with-temp-buffer
      (insert text)
      ;; I prefer using the "clipboard" selection (the one the
      ;; typically is used by c-c/c-v) before the primary selection
      ;; (that uses mouse-select/middle-button-click)
      (call-process-region (point-min) (point-max) "wl-copy")))
  ;; write the equivalent my-copy-function
  (defun my-paste-function ()
    "Return current clipboard content if different from the top of the kill-ring."
    (interactive)
    (let ((paste-output (cond
                         ((getenv "WAYLAND_DISPLAY")
                          (shell-command-to-string "wl-paste"))
                         ((string-equal (getenv "DISPLAY") ":0")
                          (shell-command-to-string "xclip -selection clipboard -o"))
                         (t
                          (shell-command-to-string "pbpaste"))))) ; macOS
      (unless (string= (car kill-ring) (string-trim paste-output))
        (string-trim paste-output))))
  (setq interprogram-cut-function 'my-cut-function)
  (setq interprogram-paste-function 'my-paste-function))

(use-package simpleclip
  :ensure simpleclip
  :config
  (simpleclip-mode 1))

(provide 'my-copy)
