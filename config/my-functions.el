;; my-functions.el -*- lexical-binding: t; -*-
;;
;; Helper functions that don't fit nicely anywhere else.

(use-package s
  :ensure t
  :straight t)

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

(defun my-sensible-to-indent-p ()
  "Non-nil only if current line appears to be under-indented."
  (when (derived-mode-p 'prog-mode)
    (let* ((handle (prepare-change-group (current-buffer)))
           (orig-col (current-column))
           (expected-col 0))
      (activate-change-group handle)
      (unwind-protect
          (progn
            (indent-according-to-mode)
            (setq expected-col (current-column)))
        (cancel-change-group handle))
      ;; Actually return the comparison!
      (< orig-col expected-col))))

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
  "Toggle between bol and indentation, ALWAYS go to column 0
regardless of horizontal scroll."
  (interactive)
  (let ((orig (point)))
    (goto-char (line-beginning-position))
    (when (= (point) orig)
      (back-to-indentation))))

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
    (unless (eq 'font-lock-comment-face (get-text-property bol 'face))
      (while (and (/= bol (point))
                  (eq 'font-lock-comment-face
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
         (expanded (directory-file-name (expand-file-name directory))))
    (if (file-exists-p expanded)
        (setq source-directory directory))))

(my-set-source-directory)

(defun my-what-line ()
  "Get the line, without printing the word =line= before it."
  (1+ (count-lines 1 (point))))

(defun my-where-beginning-of-visual-line ()
  "Calculate the difference between the beginning
of the current visual line and point."
  (interactive)
  (let ((old-point (point))
        (bovl (save-excursion (beginning-of-visual-line)
                              (point))))
    (- old-point bovl)))

(defun my-with-suppressed-capf (fn)
  "Temporarily restore the raw CAPF handler around FN."
  (let ((completion-in-region-function #'completion--in-region))
    (funcall fn)))

(defun my-doc-at-point (&optional pos)
  "Describe the thing at POS via man, slime, describe-symbol, or
woman, and select its window. With `C-u` prefix, prompt for a
position; otherwise use point."
    (interactive "d")  ; reads POS or prompts if prefix given
    (let* ((symbol (save-excursion
                     (goto-char pos)
                     (thing-at-point 'symbol t)))
           (man-spec (and symbol (concat "1 " symbol))))
      (cond
       ;; 1) Shell-script or interactive shell → pop man(1)
       ((and symbol
             (derived-mode-p 'sh-mode
                             'shell-mode 'eshell-mode
                             'term-mode  'comint-mode))
        (if (executable-find "man")
            (let ((buf (man man-spec)))
              (pop-to-buffer buf))
          (user-error "No ‘man’ executable found to look up %s" symbol)))

       ;; 2) Try elisp-slime-nav
       ((and (fboundp #'elisp-slime-nav-describe-elisp-thing-at-point)
             (bound-and-true-p elisp-slime-nav-mode))
        (elisp-slime-nav-describe-elisp-thing-at-point symbol))

       ;; 3) Try describe-symbol
       ((and symbol
             (or (fboundp (intern symbol))
                 (boundp (intern symbol))))
        (funcall #'describe-symbol (intern symbol)))

       ;; 4) Nothing found → friendly error
       (t
        (user-error "No documentation available for %s"
                    (or symbol "<nothing>"))))))

(provide 'my-functions)
