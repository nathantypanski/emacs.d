;; my-woman.el
;;
;; Stuff for opening and reading manual entries.

(require 'helm-man)

(defun my-woman-entry ()
  "Jump to a manual entry at point, using helm for completion."
  (interactive)
  (helm-man-default-action (Man-default-man-entry)))

(provide 'my-woman)
