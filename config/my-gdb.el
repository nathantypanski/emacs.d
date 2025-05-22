;; my-gdb.el -*- lexical-binding: t; -*-
;;
;; GDB settings for my Emacs config.


(defvar gdb-locals-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map "q" 'kill-this-buffer)
    (define-key map "\t"
      (lambda ()
        (interactive)
        (gdb-set-window-buffer
         (gdb-get-buffer-create 'gdb-registers-buffer gdb-thread-number)
         t)))
    map))


(defun my-gdb-swap-locals-registers ()
  "Switch between locals and registers in GDB."
  (interactive)
  (gdb-set-window-buffer
   (gdb-get-buffer-create 'gdb-registers-buffer gdb-thread-number)
   t))


(evil-set-initial-state 'gdb-locals-mode 'normal)
(evil-set-initial-state 'gdb-registers-mode 'normal)


(evil-define-key normal 'gdb-locals-mode-map
  (kbd "q") 'kill-this-buffer
  (kbd "o") 'my-gdb-swap-locals-registers)


(evil-define-key normal 'gdb-registers-mode-map
  "\r" 'gdb-edit-register-value
  (kbd "q") 'kill-this-buffer
  (kbd "o") 'my-gdb-swap-locals-registers)
