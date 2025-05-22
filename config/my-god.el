;; my-god.el -*- lexical-binding: t; -*-
;;
;; God mode bindings for emacs.
;;
;; Here, we make god-mode into an Evil state that executes in place of having
;; a normal Emacs state in Evil. This is really the best of both worlds: the
;; easiest-on-your-fingers Emacs mappings combined with
;; even-easier-on-your-fingers Vim mappings.

(use-package god-mode
  :ensure god-mode
  :init
  (progn)
  :config
  (progn
    (after 'evil
      ;; from https://gist.github.com/gridaphobe/9765143#file-god-state-el-L1
      (evil-define-state god
        "God state."
        :tag " <G> "
        :message "-- GOD MODE --"
        :entry-hook (evil-god-start-hook)
        :exit-hook (evil-god-stop-hook)
        :input-method t
        :intercept-esc nil)

      (defun evil-god-start-hook ()
        (diminish 'god-local-mode)
        (god-local-mode 1))

      (defun evil-god-stop-hook ()
        (god-local-mode -1))

      (defvar evil-execute-in-god-state-buffer nil)

      (defun evil-stop-execute-in-god-state ()
        (when (and (not (eq this-command #'evil-execute-in-god-state))
                   (not (minibufferp)))
          (remove-hook 'post-command-hook 'evil-stop-execute-in-god-state)
          (when (buffer-live-p evil-execute-in-god-state-buffer)
            (with-current-buffer evil-execute-in-god-state-buffer
              (if (and (eq evil-previous-state 'visual)
                       (not (use-region-p)))
                  (progn
                    (evil-change-to-previous-state)
                    (evil-exit-visual-state))
                (evil-change-to-previous-state))))
          (setq evil-execute-in-god-state-buffer nil)))

      (evil-define-command evil-execute-in-god-state ()
        "Execute the next command in God state."
        (add-hook 'post-command-hook #'evil-stop-execute-in-god-state t)
        (setq evil-execute-in-god-state-buffer (current-buffer))
        (cond
         ((evil-visual-state-p)
          (let ((mrk (mark))
                (pnt (point)))
            (evil-god-state)
            (set-mar mrk)
            (goto-char pnt)))
         (t
          (evil-god-state)))
        (evil-echo "Switched to God state for the next command ..."))

      (evil-define-key 'normal global-map (kbd "\\") 'evil-execute-in-god-state))))
(provide 'my-god)
