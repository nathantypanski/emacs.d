(require 'lispy)
(require 'evil)

(define-minor-mode evil-lispy-mode
  "Lispy-mode reinvented for compatibility with Evil."
  :init-value nil
  :mode org-mode
  :lighter " Evil-Lispy"
  :keymap (make-sparse-keymap) ; defines evil-org-mode-map
  :group 'evil-lispy
  :)

(defun evil-lispy-state-on-begin ()
  "Dummy function that runs on Lispy state begin.
Currently does nothing, but that may change later.")

(defun evil-cleanup-lispy-state ()
  "Dummy function that runs on Lispy state end.
Currently does nothing, but that may change later.")

(evil-define-state lispy-normal
  "Lispy state."
  :tag " <L> "
  :cursor evil-half-cursor
  :message "-- LISPY --"
  :entry-hook (evil-start-track-last-insertion)
  :exit-hook (evil-cleanup-lispy-state)
  :enable (normal))

;; TODO: staying away for now.
;; (add-hook 'emacs-lisp-mode 'evil-lispy-mode)



(evil-define-motion evil-lispy-forward (count)
  "Move forward list COUNT times or until error.
Return t if moved at least once.
Otherwise call `lispy-out-forward` and return nil."
  (interactive "p")
  (lispy-forward count))

(evil-define-motion evil-lispy-out-forward (count)
  "Move outside list forwards COUNT times.
Return nil on failure, t otherwise."
  (interactive "p")
  (lispy-out-forward count))

(evil-define-motion evil-lispy-out-backward (count)
  "Move outside list forwards COUNT times.
Return nil on failure, t otherwise."
  (interactive "p")
  (lispy-out-backward count))

(evil-define-motion evil-lispy-backward (count)
  "Move backward list COUNT times or until error.
If couldn't move backward at least once, move up backward and return nil."
  (interactive "p")
  (lispy-backward count))

(evil-define-motion evil-lispy-down (count)
  "Move down COUNT times inside current list."
  (interactive "p")
  (lispy-down count))

(evil-define-motion evil-lispy-up (count)
  "Move up COUNT times inside current list."
  (interactive "p")
  (lispy-up count))

(evil-define-motion evil-lispy-different ()
  "Switch to the different side of the current sexp."
  (interactive)
  (lispy-different))

(evil-define-operator evil-lispy-kill ()
  "Kill, keeping parens balanced."
  (interactive)
  (lispy-kill))

(evil-define-operator evil-lispy-delete (count)
  "Delete COUNT sexps."
  (interactive "p")
  (lispy-delete count))

( evil-lispy-mode-map 'nil)

(evil-define-key 'insert evil-lispy-mode-map (kbd "(") 'lispy-parens)
(evil-define-key 'insert evil-lispy-mode-map (kbd "\"") 'lispy-quotes)
(evil-define-key 'insert evil-lispy-mode-map (kbd " ") 'lispy-space)
(evil-define-key 'insert evil-lispy-mode-map (kbd ":") 'lispy-colon)
(evil-define-key 'insert evil-lispy-mode-map (kbd ")") 'lispy-out-forward-nostring)
(evil-define-key 'insert evil-lispy-mode-map (kbd "{") 'lispy-braces)
(evil-define-key 'insert evil-lispy-mode-map (kbd "}") 'lispy-braces)
(evil-define-key 'insert evil-lispy-mode-map (kbd "RET") 'lispy-newline-and-indent)
(evil-define-key 'lispy-normal evil-lispy-mode-map (kbd "d") 'evil-delete)
(evil-define-key 'lispy-normal evil-lispy-mode-map (kbd "D") 'evil-delete-line)

(evil-define-key 'motion evil-lispy-mode-map (kbd "l") 'evil-forward-char)
(evil-define-key 'motion evil-lispy-mode-map (kbd "h") 'evil-backward-char)
(evil-define-key 'motion evil-lispy-mode-map (kbd "%") 'evil-jump-item)
(evil-define-key 'motion evil-lispy-mode-map (kbd "L") 'evil-window-out-bottom)
(evil-define-key 'motion evil-lispy-mode-map (kbd "H") 'evil-window-top)
(evil-define-key 'motion evil-lispy-mode-map (kbd "k") 'evil-previous-visual-line)
(evil-define-key 'motion evil-lispy-mode-map (kbd "j") 'evil-next-visual-line)

(provide 'my-evil-lispy-mode)
