;; for `dotimes', `push' (Emacs 21)
(eval-when-compile (require 'cl))

(defmacro bind (&rest commands)
  "Convience macro which creates a lambda interactive command."
  `(lambda ()
     (interactive)
     ;; ',@' splices an evaluated value into the resulting list
     ;; That is, this will take a list and put it where this
     ;; Strange-looking construct is:
     ,@commands))

(defun my-minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))

(defun set-transparency (alpha)
  "Sets the transparency of the current frame."
  (interactive "nAlpha: ")
  (set-frame-parameter nil 'alpha alpha))

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

(defun my-rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

(defun my-delete-current-buffer-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))


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

(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

(defun my-unmount (drive)
  "Prompts the user for input and unmounts the given device."
  (interactive "sunmount: ")
  (shell-command
   (concat "/usr/bin/gksu /usr/bin/umount " (shell-quote-argument drive))))

;;"Redefine the Home/End keys to (nearly) the same as visual studio
;;behavior... special home and end by Shan-leung Maverick WOO
;;<sw77@cornell.edu>"
;;This is complex. In short, the 1st invocation of Home/End moves
;;to the beginning of the *text* line (ignoring prefixed whitespace); 2nd invocation moves
;;cursor to the beginning of the *absolute* line. Most of the time
;;this won't matter or even be noticeable, but when it does (in
;;comments, for example) it will be quite convenient.

(defun my-smart-home ()
  "Odd home to beginning of line, even home to beginning of
text/code."
  (interactive)
  (if (and (eq last-command 'my-smart-home)
           (/= (line-beginning-position) (point)))
      (beginning-of-line)
    (beginning-of-line-text)))

(defun my-smart-end ()
  "Odd end to end of line, even end to begin of text/code."
  (interactive)
  (if (and (eq last-command 'my-smart-end)
           (= (line-end-position) (point)))
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

(defun my-delete-word (arg)
  "Delete characters forward until encountering the end of a word.
With argument ARG, do this that many times."
  (interactive "p")
  (delete-region (point) (progn (forward-word arg) (point))))

(defun my-backward-delete-word (arg)
  "Delete characters backward until encountering the beginning of a word.
With argument ARG, do this that many times."
  (interactive "p")
  (my-delete-word (- arg)))

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

(defun my-is-this-line-empty ()
  "Returns t if the current line is empty. Otherwise nil."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (looking-at "^[ \t]*$")
    ))

(defun my-is-before-point-empty ()
  "Returns t if the current line up until point is empty, otherwise nil."
  (interactive)
  (let ((old-point (point)))
    (save-excursion
      (beginning-of-line)
      (s-blank? (s-trim (buffer-substring (point) old-point)))
      )
    )
  )

(defun my-which-column ()
  "Returns the column number of point."
  (interactive)
  (abs (- (save-excursion (beginning-of-line) (point)) (point))))

(provide 'my-functions)
