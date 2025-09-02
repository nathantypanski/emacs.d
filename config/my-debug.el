;; my-debug.el -*- lexical-binding: t; -*-
;;
;; Debugging utilities for tracking transient keymap corruption

;; Skip native compilation for transient to avoid keymap corruption bug
(when (and (fboundp 'native-comp-available-p) (native-comp-available-p))
  (unless (boundp 'native-comp-skip-packages)
    (setq native-comp-skip-packages nil))
  (add-to-list 'native-comp-skip-packages "transient"))

(defun debug-transient-redisplay-map (orig-fun &rest args)
  "Debug wrapper for transient--make-redisplay-map"
  (message "transient--make-redisplay-map called with args: %S" args)
  (message "transient--prefix: %S" (when (boundp 'transient--prefix) transient--prefix))
  (message "transient--suffixes: %S" (when (boundp 'transient--suffixes) transient--suffixes))
  (condition-case err
      (apply orig-fun args)
    (error
     (message "ERROR in transient--make-redisplay-map: %S" err)
     (message "transient--layout: %S" (when (boundp 'transient--layout) transient--layout))
     (signal (car err) (cdr err)))))

(defun debug-transient-state ()
  "Log current transient state"
  (when (boundp 'transient--prefix)
    (message "transient state - prefix: %S, stack: %S"
             transient--prefix
             (when (boundp 'transient--stack) transient--stack))))

;; Debug logging disabled
;; (with-eval-after-load 'transient
;;   (advice-add 'transient--make-redisplay-map :around #'debug-transient-redisplay-map)
;;   (setq transient-show-common-commands nil)
;;   (message "Transient debugging enabled"))

(provide 'my-debug)
