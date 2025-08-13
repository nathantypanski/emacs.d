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
      (defun evil-god-start-hook ()
        "Hook called upon entry to `god-state'."
        (god-local-mode 1))

      (defun evil-god-stop-hook ()
        "Exit `god-state'."
        (god-local-mode -1))

      (evil-define-state god
        "God state."
        :tag " <G> "
        :message "-- GOD MODE --"
        :entry-hook (evil-god-start-hook)
        :exit-hook (evil-god-stop-hook)
        :input-method t
        :intercept-esc nil)

      (defvar evil-execute-in-god-state-buffer nil)
      (defvar my-evil-god-state-active nil)

      (evil-define-command my-evil-execute-in-god-state ()
        "Execute the next command in God state."
        (setq my-evil-god-state-active t)
        (add-hook 'post-command-hook #'my-evil-stop-execute-in-god-state t)

        ;; (when (eq evil-execute-in-god-state-buffer (current-buffer)))

        (setq evil-execute-in-god-state-buffer (current-buffer))
        (cond
         ((evil-visual-state-p)
          (let ((mrk (mark))
                (pnt (point)))
            (evil-god-state)
            (set-mark mrk)
            (goto-char pnt)))
         (t
          (evil-god-state)))
        (evil-echo "Switched to God state for the next command ..."))


      (defun my-evil-stop-execute-in-god-state ()
        "Finish executing in God state."
        (when (and my-evil-god-state-active
                   (not (eq this-command #'my-evil-execute-in-god-state))
                   (not (minibufferp)))
          (setq my-evil-god-state-active nil)
          (remove-hook 'post-command-hook 'my-evil-stop-execute-in-god-state)
          (when (buffer-live-p evil-execute-in-god-state-buffer)
            (with-current-buffer evil-execute-in-god-state-buffer
              (if (and (eq evil-previous-state 'visual)
                       (not (use-region-p)))
                  (progn
                    (evil-change-to-previous-state)
                    (evil-exit-visual-state))
                (evil-change-to-previous-state))))
          (setq evil-execute-in-god-state-buffer nil)
          (evil-echo "... returning from God.")))

      (defun my-return-from-god ()
        "Toggle between normal and god state."
        (interactive)
        (my-evil-stop-execute-in-god-state))

      (after 'general
        (general-define-key
         :states '(normal)
         :keymap '(global-map)
         (kbd "\\") 'my-evil-execute-in-god-state)

        (general-define-key
         :states '(god)
         :keymap '(global-map)
         ;; todo: won't toggle mid command
         (kbd "C-\\") 'my-return-from-god)
        ))))


(provide 'my-god)
