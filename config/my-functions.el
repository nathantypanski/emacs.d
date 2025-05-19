;; my-functions.el
;;
;; Helper functions that don't fit nicely anywhere else.


(defun my-minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))


(defun my-google ()
  "Google the selected region if any, display a query prompt otherwise."
  (interactive)
  (browse-url
   (concat
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q="
    (url-hexify-string (if mark-active
                           (buffer-substring (region-beginning) (region-end))
                         (read-string "Search Google: "))))))


(defun my-eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))


(defun my-terminal-config (&optional frame)
  "Establish settings for the current terminal."
  (if (not frame) ;; The initial call.
      (xterm-mouse-mode 1)
    ;; Otherwise called via after-make-frame-functions.
    (if xterm-mouse-mode
        ;; Re-initialise the mode in case of a new terminal.
        (xterm-mouse-mode 1))))


;; Evaluate both now (for non-daemon emacs) and upon frame creation
;; (for new terminals via emacsclient).
(my-terminal-config)
(add-hook 'after-make-frame-functions 'my-terminal-config)


;; Place custom settings in their own file.
(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file) (load custom-file))


;; From <http://en.wikipedia.org/wiki/User:Gwern/.emacs>, who took it from
;; Shan-leung Maverick:
;;
;; "Redefine the Home/End keys to (nearly) the same as visual studio
;; behavior... special home and end by Shan-leung Maverick WOO
;; <sw77@cornell.edu>"
;;
;; This is complex. In short, the 1st invocation of Home/End moves
;; to the beginning of the *text* line (ignoring prefixed whitespace); 2nd invocation moves
;; cursor to the beginning of the *absolute* line. Most of the time
;; this won't matter or even be noticeable, but when it does (in
;; comments, for example) it will be quite convenient.
(defun my-smart-home ()
  "Odd home to beginning of line, even home to beginning of text/code."
  (interactive)
  (if (bolp)
      (beginning-of-line-text)
    (beginning-of-line)))

(defun my-smart-end ()
  "Odd end to end of line, even end to begin of text/code."
  (interactive)
  (if (eq (point) (- (line-end-position) 1))
    (end-of-line-text)
    (end-of-line)))

(defun end-of-line-text ()
  "Move to end of current line and skip comments and trailing space.
Require `font-lock'."
  (interactive)
  (end-of-line)
  (let ((bol (line-beginning-position)))
    (unless (eq font-lock-comment-face (get-text-property bol 'face))
      (while (and (/= bol (point))
                  (eq font-lock-comment-face
                      (get-text-property (point) 'face)))
        (backward-char 1))
      (unless (= (point) bol)
        (forward-char 1) (skip-chars-backward " \t\n")))))


(defun my-increment-number-at-point ()
  (interactive)
  (skip-chars-backward "0123456789")
  (or (looking-at "[0123456789]+")
      (error "No number at point"))
  (replace-match (number-to-string (1+ (string-to-number (match-string 0))))))


;; jacked from cider-repl.el
(defun my-same-line (pos1 pos2)
  "Return t if buffer positions POS1 and POS2 are on the same line."
  (save-excursion (goto-char (min pos1 pos2))
                  (<= (max pos1 pos2) (line-end-position))))


(defun my-hop-around-buffers ()
  "Swap the current buffer with the previous one."
  (interactive)
  (switch-to-buffer (other-buffer "*Ibuffer*")))


(defun my-kill-other-buffer ()
  "Kill the buffer opposite the current one."
  (interactive)
  (kill-buffer
   (other-buffer (current-buffer) t)))


(defun my-is-this-line-empty ()
  "Returns t when the current line is empty or contains only whitespace."
  (interactive)
  (save-excursion
    (beginning-of-line)
    ;;;; old regex:
    ;; (looking-at "^\s*$")
    (looking-at "^[ \t]*$")))

(defalias 'my-current-line-is-empty #'my-is-this-line-empty)


(defun my-is-before-point-empty ()
  "Returns t if the current line up until point is empty, otherwise nil."
  (interactive)
  (let ((old-point (point)))
    (save-excursion
      (beginning-of-line)
      (s-blank? (s-trim (buffer-substring (point) old-point))))))


(defun my-which-column ()
  "Returns the column number of point."
  (interactive)
  (abs (- (save-excursion (beginning-of-line) (point)) (point))))


;; Ensure Emacs can find its own source code; see footnotes in
;; <http://nathantypanski.com/blog/2014-08-03-a-vim-like-emacs-config.html#stealing-ibuffers-keymap>
;; for an explanation.
(defvar my-default-source-directory
  "~/build/aur/emacs-git/src/emacs-git/src"
  "Default source directory for emacs source code.")

(defun my-set-source-directory (&optional directory)
  "Set the directory for Emacs source code from building."
  (interactive)
  (let* ((directory (if directory directory my-default-source-directory))
         (expanded (directory-file-name (expand-file-name directory)))
	 (try expanded) new)
    (if (file-exists-p expanded)
        (setq source-directory directory))))

(my-set-source-directory)

(defun my-list-bind-difference (a b)
  "Remove B from A."
  (cl-dolist (elem a)
    (setq a (delete elem a))))


(provide 'my-functions)
