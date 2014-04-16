;; Quiet the byte compiler
(defvar unbound-keys nil
"Used internally by `unbound-keys'.")

(defun unbound-keys (max)
"Return a list of unbound keystrokes of complexity no greater than MAX.
Keys are sorted by their complexity; `key-complexity' determines it."
(let (unbound-keys)
 (unbound-keys-1 max nil nil)
 (mapcar 'car (sort unbound-keys (lambda (k l) (< (cdr k) (cdr l)))))))

;; Adds to `unbound-keys'.
(defun unbound-keys-1 (max map pfx)
(dolist (base unbound-key-list)
 (dotimes (modi (lsh 1 (length unbound-modifiers)))
   (let ((key (list base)))
     (dotimes (j (length unbound-modifiers))
       (unless (zerop (logand modi (lsh 1 j)))
         (push (nth j unbound-modifiers) key)))
     (let ((total (vconcat pfx (list key))) comp)
       ;; Don't use things that get translated and bound.  This isn't
       ;; perfect: it assumes that the entire key sequence is translated.
       (unless (or (let ((trans (lookup-key function-key-map total)))
                     (and (vectorp trans) (key-binding trans)))
                   ;; Don't add `shift' to any graphic character; can't
                   ;; type it, or it's redundant.
                   (and (memq 'shift key) (integerp base)
                        (> base ?\ ) (<= base ?~))
                   ;; Don't add `control' when it generates another
                   ;; character we use:
                   (and (memq 'control key) (integerp base)
                        (< base ?`)
                        (memq (- base 64) unbound-key-list))
                   ;; Limit the total complexity:
                   (> (setq comp (key-complexity total)) max))
         (let ((res (if map (lookup-key map (vector key))
                      (key-binding (vector (if (cdr key) key (car key)))))))
           (cond ((keymapp res)
                  ;; Don't add anything after an ESC, to avoid Meta
                  ;; confusion.
                  (unless (eq base ?\e)
                    (unbound-keys-1 max res total)))
                 (res)
                 (t (push (cons total comp) unbound-keys))))))))))

;;;###autoload
(defun describe-unbound-keys (max)
"Display a list of unbound keystrokes of complexity no greater than MAX.
Keys are sorted by their complexity; `key-complexity' determines it."
(interactive "nMaximum key complexity: ")
(with-output-to-temp-buffer "*Unbound Keys*"
 (let ((keys (unbound-keys max)))
   (princ (format "%s unbound keys with complexity at most %s:\n"
                  (length keys) max))
   (princ (mapconcat 'key-description keys "\n")))))

;; Local variables:
;; indent-tabs-mode: nil
;; End:

;; unbound.el ends here

(provide 'my-unbound-keys)
