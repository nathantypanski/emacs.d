;; https://www.emacswiki.org/emacs/unbound.el

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

(defcustom unbound-modifiers '(control meta shift)
  "Modifiers to consider when searching for unbound keys."
  :type '(set (const control) (const meta) (const shift)
	      (const super) (const hyper) (const alt)))

(defvar unbound-key-list
  (let (keys)
    (dotimes (i (- ?\d ?\  -1))
      (push (+ i ?\ ) keys))
    (dotimes (i 12)
      (push (intern (format "f%s" (1+ i))) keys))
    (append '(?\t ?\r ?\e) (nreverse keys)
	    '(insert delete home end prior next up down left right)))
  "Keys to consider when searching for unbound keys.")

(defun key-complexity (key)
  "Return a complexity score for key sequence KEY.
Currently KEY must be of the [(control shift ?s) ...] format."
  (let ((ret 0))
    (dotimes (i (length key) ret)
      (setq ret (+ ret (* i 2) (key-complexity-1 (aref key i)))))))

;; This is somewhat biased for US keyboards.
(defun key-complexity-1 (key)           ; key:=(modifiers... key)
  (+ (if (memq 'control key) 1 0)
     (if (memq 'meta key) 2 0)
     (if (memq 'shift key) 3 0)
     (if (memq 'super key) 4 0)
     (if (memq 'hyper key) 4 0)
     (if (memq 'alt key) 3 0)
     (* 2 (1- (length key)))
     (progn
       (setq key (car (last key)))
       (if (integerp key)
	   (cond ((and (>= key ?a) (<= key ?z)) 0)
		 ((and (>= key ?A) (<= key ?Z)) 6) ; capitals are weird
		 ((and (>= key ?0) (<= key ?9)) 2)
		 ((memq key '(?\b ?\r ?\ )) 1)
		 ;; Unshifted punctuation (US keyboards)
		 ((memq key '(?` ?- ?= ?\t ?\[ ?\] ?\\ ?\; ?' ?, ?. ?/)) 3)
		 ;; Other letters -- presume that one's keyboard has them if
		 ;; we're going to consider binding them.
		 ((let (case-fold-search)
		    (string-match
		     "[016A]" (category-set-mnemonics
			       (char-category-set key)))) 2)
		 (t 5))
	 7))))

(provide 'my-unbound-keys)
