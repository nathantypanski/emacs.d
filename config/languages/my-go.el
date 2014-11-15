;; my-go.el
;;
;; Settings for the Go programming language.

(use-package go-mode
  :commands go-mode godoc
  :mode "\\.go\\'"
  :config
  (progn
    (defun my-godoc--buffer-sentinel (proc event)
      "Modified sentinel function run when godoc command completes.
Doesn't jump to buffer automatically. Enters help mode on buffer."
      (with-current-buffer (process-buffer proc)
        (cond ((string= event "finished\n")  ;; Successful exit.
               (goto-char (point-min))
               (view-mode 1)
               (help-mode)
               )
              ((/= (process-exit-status proc) 0)  ;; Error exit.
               (let ((output (buffer-string)))
                 (kill-buffer (current-buffer))
                 (message (concat "godoc: " output)))))))


        (defun my-jump-to-go-docs ()
          "Jump to a pane and do godoc"
          (interactive)
          (let ((query (thing-at-point 'word)))
            (if (set-process-sentinel
                 (start-process-shell-command "godoc" (godoc--get-buffer query)
                                              (concat "godoc " query))
                 'my-godoc--buffer-sentinel)
                nil)
            (let ((helpdoc (-first
                            (lambda (e) (string-match ".*godoc.*" (buffer-name e)))
                            (buffer-list))))
              (pop-to-buffer (buffer-name helpdoc)))))


        (evil-define-key 'normal go-mode-map (kbd "K") 'my-jump-to-go-docs)))


(provide 'my-go)
