;;; gptel-menu-fix.el --- Fix for gptel-menu transient crashes -*- lexical-binding: t; -*-
;;
;; Attempts to resolve strange `keymapp 2' and similar errors in gptel.
;;
;; ===================================================================
;;
;;                         ABANDON ALL HOPE
;;                         YE WHO ENTER HERE
;;
;;                                     Dante
;;
;; ===================================================================

;; This fixes the "wrong-type-argument keymapp 2" error in gptel-menu

(require 'cl-lib)
(require 'transient)
(require 'gptel)

;; Track the real issue - when transient--transient-map becomes 2
(defvar gptel-menu-fix--debug nil
  "Enable detailed debugging of transient corruption.")

(defvar gptel-menu-fix--error-count 0
  "Count of errors caught and fixed.")

(defun gptel-menu-fix--check-maps ()
  "Check all transient maps for corruption."
  ;; Silently fix corrupted maps - now handles any number, not just 2
  (when (and (boundp 'transient--transient-map)
             (numberp transient--transient-map))
    (when gptel-menu-fix--debug
      (message "Fixing transient--transient-map: was %s" transient--transient-map))
    (setq transient--transient-map nil)
    (cl-incf gptel-menu-fix--error-count))
  (when (and (boundp 'transient--predicate-map)
             (numberp transient--predicate-map))
    (when gptel-menu-fix--debug
      (message "Fixing transient--predicate-map: was %s" transient--predicate-map))
    (setq transient--predicate-map nil)
    (cl-incf gptel-menu-fix--error-count)))

;; Override transient--make-redisplay-map to catch the issue
(defun gptel-menu-fix--make-redisplay-map-safe (orig-fun &rest args)
  "Wrapper around transient--make-redisplay-map to catch corruption."
  (gptel-menu-fix--check-maps)
  ;; Silently catch and fix the error
  (condition-case err
      (apply orig-fun args)
    (wrong-type-argument
     (if (numberp (nth 2 err))
         (progn
           (cl-incf gptel-menu-fix--error-count)
           (when gptel-menu-fix--debug
             (message "Fixed keymapp %s error #%d" (nth 2 err) gptel-menu-fix--error-count))
           (make-sparse-keymap))  ; Return empty keymap as fallback
       (signal (car err) (cdr err))))))

(advice-add 'transient--make-redisplay-map :around #'gptel-menu-fix--make-redisplay-map-safe)

;; Suppress terminal mouse escape sequence warnings
(with-eval-after-load 'transient
  ;; ESC [ < is terminal mouse tracking - ignore it
  (define-key transient-map (kbd "ESC [ <") 'ignore)
  (define-key transient-map (kbd "M-[ <") 'ignore)
  ;; Also ignore other common terminal escape sequences
  (define-key transient-map (kbd "ESC [ >") 'ignore)
  (define-key transient-map (kbd "M-[ >") 'ignore))

;; Watch for corruption of transient--transient-map
(add-variable-watcher
 'transient--transient-map
 (lambda (symbol newval operation where)
   (when (eq newval 2)
     (message "WARNING: Setting transient--transient-map to 2 from %s (op: %s)"
              where operation)
     (backtrace))))

;; Disable debugging by default (set to t to see messages)
(setq gptel-menu-fix--debug nil)

;; Status function to check if fix is working
(defun gptel-menu-fix-status ()
  "Show status of gptel-menu fix."
  (interactive)
  (message "gptel-menu fix: %d errors caught and fixed"
           gptel-menu-fix--error-count))

(provide 'gptel-menu-fix)
;;; gptel-menu-fix.el ends here
