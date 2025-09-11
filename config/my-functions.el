;; my-functions.el -*- lexical-binding: t; -*-
;;
;; Helper functions that don't fit nicely anywhere else.

(require 'ansi-color)

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

(defun my-get-symbol-at-point (&optional pos)
  "Get the symbol at POS (or point)."
  (save-excursion
    (goto-char (or pos (point)))
    (thing-at-point 'symbol t)))

(defun my-shell-man-help (symbol)
  "Show man page for SYMBOL in shell/terminal modes.
Returns non-nil if successful."
  (when (and symbol
             (derived-mode-p 'sh-mode 'shell-mode 'eshell-mode
                             'term-mode 'comint-mode)
             (executable-find "man"))
    (man (concat "1 " symbol))
    t))

(defun my-elisp-slime-nav-help (symbol)
  "Try elisp-slime-nav for SYMBOL.
Returns non-nil if successful."
  (when (and symbol
             (fboundp 'elisp-slime-nav-describe-elisp-thing-at-point)
             (bound-and-true-p elisp-slime-nav-mode))
    (elisp-slime-nav-describe-elisp-thing-at-point symbol)
    t))

(defun my-describe-symbol-help (symbol)
  "Use `describe-symbol' for SYMBOL if it's a known function/variable.
Returns non-nil if successful."
  (when-let* ((symbol)
              (sym (intern-soft symbol))
              ((or (fboundp sym) (boundp sym))))
    (describe-symbol sym)
    t))

(defun my-doc-at-point (&optional pos)
  "Describe the thing at POS via various documentation methods.
Tries shell man pages, elisp-slime-nav, and describe-symbol in order."
  (interactive "d")
  (let* ((pos (or pos (point)))
         (symbol (my-get-symbol-at-point pos)))
    ;; Try documentation methods, suppressing display
    (let ((display-buffer-alist '((".*" . (display-buffer-no-window)))))
      (unless (or (my-shell-man-help symbol)
                  (my-elisp-slime-nav-help symbol)
                  (my-describe-symbol-help symbol))
        (user-error "No documentation available for %s"
                    (or symbol "<nothing>"))))
    ;; Then popup the result
    (my-popup-buffer "*Help*")))

(defun my-popup-buffer (buffer-or-name)
  "Display BUFFER-OR-NAME in a popup at the bottom with consistent styling.
Returns the window displaying the buffer, or nil if buffer doesn't exist."
   (interactive)
  (when-let ((buf (get-buffer buffer-or-name)))
    (let ((window (display-buffer
                   buf
                   '((display-buffer-below-selected)
                     (window-height . 0.15)
                     (window-parameters . ((transient . t)
                                          (no-delete-other-windows . t)))))))
      ;; Add convenient key bindings
      (with-current-buffer buf
        (local-set-key (kbd "q") 'quit-window)
        (local-set-key (kbd "<escape>") 'quit-window))
      window)))

(defun my-popup-command (command buffer-name)
  "Execute shell COMMAND and display output in BUFFER-NAME as popup."
   (interactive)
  (with-output-to-temp-buffer buffer-name
    (shell-command command buffer-name))
  (my-popup-buffer buffer-name))


(defun my-eval-region-and-insert (start end)
  "Evaluate region and insert the result after it."
  (interactive "r")
  (let ((result (eval (read (buffer-substring-no-properties start end)))))
    (goto-char end)
    (insert (format "\n;; => %s" result))))

(defun my-replace-mac-addresses ()
  "Replace MAC addresses in buffer or region with [mac]."
  (interactive)
  (let ((start (if (use-region-p) (region-beginning) (point-min)))
        (end (if (use-region-p) (region-end) (point-max))))
    (save-excursion
      (goto-char start)
      (while (re-search-forward "\\([a-z0-9]\\{2\\}:\\)\\{5\\}[a-z0-9]\\{2\\}" end t)
        (replace-match "[mac]")))))

(defun my-unique-append (default-list additional-items)
    "Extend DEFAULT-LIST with ADDITIONAL-ITEMS, removing duplicates."
    (seq-uniq (append default-list additional-items) #'equal))

(defun my-insert-shell-command-output (command)
  "Run `command', insert output at point."
  (interactive)
  (insert (shell-command-to-string command)))

(defmacro my-extend-custom-default (var-symbol additional-items)
  "Extend the default value of VAR-SYMBOL with ADDITIONAL-ITEMS and set it."
  `(setq ,var-symbol
         (if (boundp ',var-symbol)
             ;; originally, `default-value' was `(symbol-value ',var-symbol)'
             ;; but that didn't quite work right in initial tests.
             ;; (eval (car (get ',var-symbol 'standard-value)))
             (let ((default-value (symbol-value ',var-symbol)))
               (my-unique-append default-value ,additional-items))
           ;; If variable isn't bound yet, just use the additional items
           ,additional-items)))

(defun my-add-hook (hook function &optional append local)
  "Add `function' to `hook' idempotently. For `append' and `local',
see docs for `add-hook'."
  (remove-hook hook function local)
  (add-hook hook function append local))

(defun my-run-process-in-buffer-unsafe (cmd)
  "Run shell `cmd' asynchronously in a new buffer, showing live colored output."
  (interactive (list (read-shell-command "$ ")))
  (let* ((name-frag (string-trim (if (> (length cmd) 40)
                                     (concat (substring cmd 0 40) "…")
                                   cmd)))
         (buf (generate-new-buffer (format "*cmd:%s*" name-frag))))
    (with-current-buffer buf
      (setq-local buffer-read-only t)
      (setq-local truncate-lines t)
      (special-mode))                     ; gives you 'q' to quit, etc.
    (let ((proc (start-process-shell-command name-frag buf cmd)))
      ;; Filter to apply ANSI colors and append incrementally.
      (set-process-filter
       proc
       (lambda (p chunk)
         (when (buffer-live-p (process-buffer p))
           (with-current-buffer (process-buffer p)
             (let ((inhibit-read-only t))
               (goto-char (point-max))
               (insert (ansi-color-apply chunk)))))))
      ;; Sentinel to append exit status footer and lock buffer.
      (set-process-sentinel
       proc
       (lambda (p event)
         (when (memq (process-status p) '(exit signal))
           (with-current-buffer (process-buffer p)
             (let ((inhibit-read-only t))
               (goto-char (point-max))
               (insert (format "\n\n[%s at %s]\n"
                               (string-trim event)
                               (format-time-string "%Y-%m-%d %H:%M:%S"))))
             (read-only-mode 1))))))
    (pop-to-buffer buf)))

(defun my-compile-cmd-unsafe (cmd)
  "Run CMD under `compilation-mode` in a new buffer for error navigation."
  (interactive (list (read-shell-command "Compile cmd: ")))
  ;; `compilation-start` always creates/uses a buffer; it’s async by default.
  (let ((default-directory (or (when-let ((prj (ignore-errors (project-current))))
                                 (project-root prj))
                               default-directory)))
    (compilation-start cmd 'compilation-mode
                       (lambda (_) (format "*compile:%s*" cmd)))))
;; Tip: M-g n / M-g p to jump between errors; RET on a file:line opens it.

(defun my-run-prog-async-safe (prog args)
  "Run PROG with list ARGS asynchronously in a new buffer (no shell)."
  (interactive
   (let* ((prog (read-file-name "Program: " nil nil t))
          (args (split-string-and-unquote (read-string "Args: "))))
     (list prog args)))
  (let* ((name (format "%s %s" (file-name-nondirectory prog)
                       (mapconcat #'identity args " ")))
         (buf  (generate-new-buffer (format "*proc:%s*" name)))
         (proc (apply #'start-process name buf prog args)))
    (with-current-buffer buf (special-mode))
    (set-process-sentinel
     proc (lambda (p e)
            (when (memq (process-status p) '(exit signal))
              (with-current-buffer (process-buffer p)
                (let ((inhibit-read-only t))
                  (goto-char (point-max))
                  (insert (format "\n\n[%s]" (string-trim e))))
                (read-only-mode 1)))))
    (pop-to-buffer buf)))

(provide 'my-functions)
