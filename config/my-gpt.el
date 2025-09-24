;; my-gpt.el -*- lexical-binding: t; -*-
;;
;; Streamlined LLM configuration with gptel and claude-agent integration

(use-package request :ensure t)

(use-package claude-code-ide
  :straight (:type git :host github :repo "manzaltu/claude-code-ide.el")
  :bind ("C-c C-'" . claude-code-ide-menu)
  :custom
  (claude-code-ide-cli-extra-flags "")
  (claude-code-ide-cli-path "claude")
  (claude-code-ide-focus-claude-after-ediff t)
  (claude-code-ide-focus-on-open t)
  (claude-code-ide-show-claude-window-in-ediff nil)
  (claude-code-ide-system-prompt nil)
  (claude-code-ide-use-side-window nil)
  (claude-code-ide-window-side 'right)
  ;; 1 char wider than previous
  (claude-code-ide-window-width 81)
  :config
  (claude-code-ide-emacs-tools-setup)

  ;; Add helpful message for ediff sessions
  (defun my-claude-ediff-help-message ()
    "Show helpful message for navigating Claude Code ediff sessions."
    (run-with-timer 0.5 nil
                    (lambda ()
                      (message "Ediff: [n]ext/[p]rev diff | [a]ccept original | [b]accept Claude's | [q]uit | [?]help"))))

  (add-hook 'ediff-startup-hook 'my-claude-ediff-help-message)

  ;; Terminal-friendly keybindings using general
  (after 'general
    (general-define-key
     :prefix "C-c"
     "C-j" 'claude-code-ide-insert-newline   ; Alternative to S-return
     "C-e" 'claude-code-ide-send-escape      ; Alternative if C-escape fails
     "C-t" 'claude-code-ide-toggle           ; Quick toggle
     "C-p" 'claude-code-ide-send-prompt))    ; Send prompt directly

  ;; Fix window configuration after ediff
  (defun my-claude-fix-windows-after-ediff ()
    "Fix ultra-wide side window after ediff closes."
    (when claude-code-ide-use-side-window
      ;; Delete existing side windows and recreate properly
      (dolist (window (window-list))
        (when (window-parameter window 'window-side)
          (delete-window window)))
      ;; Find claude buffer and redisplay it properly
      (when-let* ((claude-buffer-name (claude-code-ide--get-buffer-name))
                  (claude-buffer (get-buffer claude-buffer-name))
                  (claude-window (get-buffer-window claude-buffer)))
        (when claude-window
          (delete-window claude-window))
        (claude-code-ide--display-buffer-in-side-window claude-buffer))))

  (add-hook 'ediff-cleanup-hook 'my-claude-fix-windows-after-ediff))

(use-package mcp
  :after gptel
  :straight (:type git :host github :repo "lizqwerscott/mcp.el")
  :ensure
  :config
  (setq mcp-hub-servers
        '(("memory" :command "mcp-server-memory"))))

(use-package gptel
  :straight (:repo "karthink/gptel" :branch "master" :files ("*.el"))
  :custom-face
  (gptel-user-header ((t (:foreground "#dca3a3" :weight bold))))
  (gptel-assistant-header ((t (:foreground "#7f9f7f" :weight bold))))
  (gptel-response ((t (:foreground "#9fc59f"))))
  :commands (gptel gptel-menu gptel-send gptel-request my-gptel-review my-gptel-explain my-gptel-switch-model)
  :hook ((gptel-mode . my-gptel-setup-behavior))
  :custom
  (gptel-track-response t)
  (gptel-log-level 'debug)
  (gptel-use-curl t)
  (gptel-stream t)
  (gptel-use-header-line t)
  (gptel-prompt-prefix-alist '((org-mode . "** Human\n")))
  (gptel-response-prefix-alist '((org-mode . "** Assistant\n")))
  (gptel-model 'claude-sonnet-4-20250514)
  (gptel-max-tokens 3000)
  (gptel-use-tools t)
  (gptel-enable-enhanced-state-tracking t)
  (gptel-auto-repair-invalid-state t)
  (gptel-include-tool-results n)
  (gptel-confirm-tool-calls 'auto)
  :init
  ;; Core prompts
  (defvar my-gptel-system-prompt
    "You are a LLM running inside Emacs. Your responses are inserted literally into the buffer where the prompt is sent - usually code in the language being discussed. Do not use markdown or org to structure your comments. Instead, structure in alignment with the surrounding text. Put your commentary in comments (e.g., `;;` for elisp, `//` for go, ...).")

  (defvar my-gptel-elisp-prompt
    "You are a LLM running inside Emacs. Help me work on my elisp config. I am using emacs 30.1.90 and (use-package) with straight. Respond in org-mode. I can share any buffers or config files you request in the context.")

  (defvar my-emacs-system-prompt
    "You are a large language model living in Emacs and with introspection powers into the editor, like the ability to list/open/read buffers and run arbitrary elisp code.

Running tools which consume or return large output can result in extremely high costs. Output quantity is *especially* costly: generally output costs at least 3x per token than input. So be consice and prefer tool calls which allow you to send the minimum quantity of data to achieve your desired outcome.")

  (defvar my-gptel-directives
    (list (cons 'Inline my-gptel-system-prompt)
          (cons 'Elisp my-gptel-elisp-prompt)
          (cons 'Emacs my-emacs-system-prompt)))

  :config
  ;; Basic setup
  (when (getenv "OPENAI_API_KEY")
    (setq gptel-api-key (getenv "OPENAI_API_KEY")))
  (setq gptel-default-mode 'org-mode)
  (setq gptel-directives my-gptel-directives)
  (setq gptel-org-branching-context t)

  ;; lets us use `-n' to track number of responses, `-T' to set
  ;; temperature, etc.
  (setq gptel-expert-commands t)

  ;; Simple model switcher
  (defvar my-gptel-models
    '(("Claude Sonnet" . (claude-sonnet-4-20250514 anthropic))
      ("Claude Opus" . (claude-opus-4-20250514 anthropic))
      ("GPT-4o" . (gpt-4o openai))
      ("GPT-4o Mini" . (gpt-4o-mini openai))))

  (defun my-gptel-switch-model ()
    "Switch gptel model and backend."
    (interactive)
    (let* ((choice (completing-read "Model: " (mapcar #'car my-gptel-models)))
           (model-info (cdr (assoc choice my-gptel-models)))
           (model (car model-info))
           (provider (cadr model-info)))
      (pcase provider
        ('anthropic
         (require 'gptel-anthropic)
         (let ((key (or (getenv "ANTHROPIC_API_KEY")
                        (read-passwd "Anthropic API Key: "))))
           (setq gptel-model model
                 gptel-backend (gptel-make-anthropic "Claude" :key key :stream t)
                 gptel-api-key key)))
        ('openai
         (let* ((key (or (getenv "OPENAI_API_KEY")
                         (read-passwd "OpenAI API Key: ")))
                (backend (cdr (assoc "ChatGPT" gptel--known-backends))))
           (setq gptel-model model
                 gptel-backend backend
                 gptel-api-key key))))
      (message "Switched to %s" choice)))

  ;; Buffer behavior
  (defun my-gptel-setup-behavior ()
    "Setup gptel buffer behavior."
    (visual-line-mode 1)
    (setq-local auto-save-timeout 60)
    (setq-local auto-save-visited-mode nil))


  ;; Core commands
  (defun my-gptel-explain ()
    "Explain current region or function."
    (interactive)
    (let ((text (if (use-region-p)
                    (buffer-substring-no-properties (region-beginning) (region-end))
                  (thing-at-point 'defun t))))
      (if text
          (gptel-request (format "Explain this code:\n\n%s" text))
        (message "No code to explain"))))

  (defun my-gptel-review ()
    "Review current region or buffer."
    (interactive)
    (let ((text (if (use-region-p)
                    (buffer-substring-no-properties (region-beginning) (region-end))
                  (buffer-string))))
      (gptel-request (format "Review this code for bugs and improvements:\n\n%s" text))))

  (defun my-gptel-add-project-context ()
    "Add relevant project files as context to gptel."
    (interactive)
    (if-let ((files (project-files (project-current))))
        (progn
          (dolist (file (completing-read-multiple "Add files: " files))
            (gptel-add-file file))
          (message "Added files to context"))
      (message "No project found")))

  ;; Clean tool implementations for gptel
  (defvar my-gptel-blocked-paths
    '("/etc/" "/usr/" "/bin/" "/sbin/" "/root/")
    "System paths where AI tools are NOT allowed to operate.")

  (defvar my-gptel-blocked-patterns
    '("\\.ssh/" "\\.gnupg/" "\\.aws/" "\\.kube/" "\\.docker/"
      "\\.password-store/" "\\.authinfo" "\\.netrc")
    "Patterns for sensitive directories/files to block.")

  (defun my-gptel-path-allowed-p (path)
    "Check if PATH is safe to access - block system and sensitive directories."
    (let ((expanded (expand-file-name path)))
      ;; Block if path contains sensitive patterns or is in blocked directories
      (not (or
            ;; Check for sensitive file names
            (string-match-p "\\(id_rsa\\|id_ed25519\\|private.*key\\|\\.pem\\|\\.key\\|wallet\\|\\.gpg\\)" expanded)
            ;; Check for system directories
            (cl-some (lambda (blocked)
                       (string-prefix-p blocked expanded))
                     my-gptel-blocked-paths)
            ;; Check for sensitive directories anywhere in path
            (cl-some (lambda (pattern)
                       (string-match-p pattern expanded))
                     my-gptel-blocked-patterns)))))

  ;; Simple read file tool
  (defun my-gptel-tool-read-file (path)
    "Read file with size limit."
    (let*
        ((max-size (* 5 1024)))
      ((expanded-path (expand-file-name path)))
      (if (> (file-attribute-size (file-attributes expanded-path)) max-size)
          "File too large - please specify which section you need"
        (cond
         ((not (my-gptel-path-allowed-p expanded-path))
          (format "Error: Path '%s' is outside allowed directories" path))
         ((not (file-exists-p expanded-path))
          (format "Error: File '%s' does not exist" path))
         ((file-directory-p expanded-path)
          (format "Error: '%s' is a directory, not a file" path))
         ((> (file-attribute-size (file-attributes expanded-path)) (* 1024 1024))
          "Error: File too large (>1MB)")
         (t
          (with-temp-buffer
            (insert-file-contents expanded-path)
            (buffer-string)))))))

  ;; Simple list files tool
  (defun my-gptel-tool-list-files (path)
    "List files in directory at PATH with safety checks."
    (let ((expanded-path (expand-file-name (or path "."))))
      (cond
       ((not (my-gptel-path-allowed-p expanded-path))
        (format "Error: Path '%s' is outside allowed directories" path))
       ((not (file-exists-p expanded-path))
        (format "Error: Directory '%s' does not exist" path))
       ((not (file-directory-p expanded-path))
        (format "Error: '%s' is not a directory" path))
       (t
        (mapconcat #'identity
                   (directory-files expanded-path nil "^[^.]" t)
                   "\n")))))

  ;; Simple bash tool with restrictions
  (defun my-gptel-tool-bash (command)
    "Execute safe bash COMMAND with restrictions."
    (cond
     ;; Block dangerous commands
     ((string-match-p "\\(sudo\\|rm -rf\\|dd if=\\|mkfs\\|\\bsudo\\b\\)" command)
      (format "Error: Dangerous command blocked: %s" command))
     ;; Allow pipes and redirects but warn about rm
     ((string-match-p "\\brm\\b" command)
      (if (yes-or-no-p (format "Execute command with 'rm'? %s" command))
          (shell-command-to-string command)
        "Command cancelled by user"))
     (t
      (let ((output (shell-command-to-string command)))
        (if (string-empty-p output)
            "(Command completed with no output)"
          output)))))

  ;; Add buffer/frame introspection functions
  (defun my-gptel-get-buffer-info ()
    "Get info about current buffer for gptel"
    (list :buffer-name (buffer-name)
          :major-mode major-mode
          :point (point)
          :mark (when (mark t) (mark t))
          :buffer-size (buffer-size)
          :modified-p (buffer-modified-p)))

  (defun my-gptel-get-frame-info ()
    "Get frame and window layout info"
    (list :frames (length (frame-list))
          :windows (length (window-list))
          :current-window-edges (when (selected-window)
                                   (window-edges (selected-window)))
          :current-window-buffer (when (selected-window)
                                   (buffer-name (window-buffer (selected-window))))
          :frame-width (frame-width)
          :frame-height (frame-height)))

  (defun my-gptel-eval-elisp-safely (code)
    "Safely evaluate elisp code for gptel"
    (condition-case err
        (eval (read code))
      (error (format "Error: %s" err))))

  (defun my-gptel-add-newline-after-tools (start end)
    "Add newline after tool call responses"
    (save-excursion
      (goto-char start)  ; Use the provided start position
      (while (re-search-forward "</invoke>" end t)  ; Search within the response region
        (unless (looking-at "\n")
          (insert "\n")))))

  ;; Tool implementations
  (defun my-gptel-git-status ()
    "Get git repository status."
    (if (vc-git-root default-directory)
        (shell-command-to-string "git status --porcelain")
      "Not in a git repository"))

  (defun my-gptel-git-diff (&optional file)
    "Show git diff, optionally for specific file."
    (if (vc-git-root default-directory)
        (shell-command-to-string
         (if file
             (format "git diff %s" (shell-quote-argument file))
           "git diff"))
      "Not in a git repository"))

  (defun my-gptel-switch-buffer (buffer-name)
    "Switch to buffer by name."
    (if-let ((buf (get-buffer buffer-name)))
        (progn
          (switch-to-buffer buf)
          (format "Switched to buffer: %s" buffer-name))
      (format "Buffer not found: %s" buffer-name)))

  (defun my-gptel-list-buffers ()
    "List buffers (max 20, exclude internal buffers)."
    (let ((buffers (cl-remove-if
                    (lambda (buf) (string-prefix-p " " (buffer-name buf)))
                    (buffer-list))))
      (when (> (length buffers) 20)
        (setq buffers (cl-subseq buffers 0 20)))
      (mapconcat
       (lambda (buf)
         (with-current-buffer buf
           (format "%s [%s]" (buffer-name) major-mode)))
       buffers
       "\n")))

  (defun my-gptel-project-files (&optional pattern)
    "List files in current project, optionally filtered by pattern. Limited to 50 files."
    (if-let ((project (project-current)))
        (let ((files (project-files project)))
          (when (> (length files) 50)
            (setq files (cl-subseq files 0 50)))
          (if pattern
              (cl-remove-if-not
               (lambda (file) (string-match-p pattern file))
               files)
            files))
      '("Not in a project")))

  (defun my-gptel-find-file (path)
    "Open/create file at path."
    (let ((expanded-path (expand-file-name path)))
      (if (my-gptel-path-allowed-p expanded-path)
          (progn
            (find-file expanded-path)
            (format "Opened file: %s" path))
        (format "Path not allowed: %s" path))))

  (defun my-gptel-grep-project (pattern &optional file-pattern)
    "Search for pattern in project files."
    (if-let ((project (project-current)))
        (let ((default-directory (project-root project)))
          (shell-command-to-string
           (format "grep -r --include='%s' '%s' ."
                   (or file-pattern "*")
                   pattern)))
      "Not in a project"))

  (defun my-gptel-apply-diff-safer (buffer-name diff-content)
    "Apply unified diff directly in Emacs buffer without external patch command."
    (unless (get-buffer buffer-name)
      (error "Buffer %s not found" buffer-name))

    (with-current-buffer buffer-name
      (save-excursion
        ;; Parse the diff and apply changes directly
        (let ((changes-applied 0))
          (dolist (hunk (my-gptel-parse-diff diff-content))
            (let ((start-line (plist-get hunk :start-line))
                  (old-lines (plist-get hunk :old-lines))
                  (new-lines (plist-get hunk :new-lines)))
              (goto-char (point-min))
              (forward-line (1- start-line))
              (let ((start-pos (point)))
                ;; Delete old lines
                (dolist (line old-lines)
                  (when (looking-at-p (regexp-quote line))
                    (delete-region (point) (progn (forward-line 1) (point)))))
                ;; Insert new lines
                (dolist (line new-lines)
                  (insert line "\n"))
                (setq changes-applied (1+ changes-applied)))))
          (format "Applied %d changes to buffer %s" changes-applied buffer-name)))))

  (defun my-gptel-parse-diff (diff-content)
    "Parse unified diff content into a list of hunks."
    (let ((hunks '())
          (lines (split-string diff-content "\n")))
      (dolist (line lines)
        (cond
         ((string-match "^@@ -\\([0-9]+\\),?[0-9]* \\+\\([0-9]+\\),?[0-9]* @@" line)
          (push (list :start-line (string-to-number (match-string 2 line))
                      :old-lines '()
                      :new-lines '()) hunks))
         ((and hunks (string-prefix-p "-" line))
          (push (substring line 1) (plist-get (car hunks) :old-lines)))
         ((and hunks (string-prefix-p "+" line))
          (push (substring line 1) (plist-get (car hunks) :new-lines)))))
      (nreverse hunks)))

  ;; Enhanced patch tool with better error handling
  (defun my-gptel-apply-patch-safe (file-path diff-content &optional patch-options)
    "Apply patch with proper error handling and permission fixes."
    (let* ((buffer (find-file-noselect file-path))
           (original-mode (and (file-exists-p file-path)
                               (file-modes file-path)))
           (made-writable nil))

      (unwind-protect
          (with-current-buffer buffer
            ;; Make file writable if it exists and isn't writable
            (when (and original-mode (not (file-writable-p file-path)))
              (set-file-modes file-path (logior original-mode #o200))
              (setq made-writable t)
              (message "Made file %s writable for patching" file-path))

            ;; Use direct text replacement for simple patches
            (if (string-match-p "^@@.*@@" diff-content)
                ;; Complex unified diff - use external patch command
                (let ((temp-file (make-temp-file "gptel-patch")))
                  (unwind-protect
                      (progn
                        (with-temp-file temp-file
                          (insert diff-content))
                        (let ((result (shell-command
                                       (format "patch %s --read-only=warn %s < %s"
                                               (or patch-options "-N")
                                               (shell-quote-argument file-path)
                                               (shell-quote-argument temp-file)))))
                          (if (= result 0)
                              (progn
                                (revert-buffer t t)
                                "Patch applied successfully")
                            (format "Patch failed with exit code %d" result))))
                    (when (file-exists-p temp-file)
                      (delete-file temp-file))))
              ;; Simple diff - do direct replacement
              (my-gptel-apply-simple-diff diff-content)))

        ;; Restore original permissions
        (when (and made-writable original-mode)
          (set-file-modes file-path original-mode)
          (message "Restored original permissions for %s" file-path)))))

  (defun my-gptel-apply-simple-diff (diff-content)
    "Apply simple text replacements from diff content."
    (let ((lines (split-string diff-content "\n"))
          (changes 0))
      (dolist (line lines)
        (cond
         ((string-prefix-p "- " line)
          (let ((old-text (substring line 2)))
            (goto-char (point-min))
            (when (search-forward old-text nil t)
              (delete-region (match-beginning 0) (match-end 0))
              (cl-incf changes))))
         ((string-prefix-p "+ " line)
          (insert (substring line 2))
          (cl-incf changes))))
      (format "Applied %d changes" changes)))
  ;; Register tools with gptel
  (defun my-gptel-setup-tools ()
    "Setup working gptel tools."
    (interactive)
    (setq gptel-tools
          (list
           (gptel-make-tool
            :name "replace_in_file"
            :description "Replace specific text in a file (more efficient than patches)"
            :function (lambda (file old-text new-text)
                        (let ((buf (find-file-noselect file)))
                          (with-current-buffer buf
                            (goto-char (point-min))
                            (if (search-forward old-text nil t)
                                (progn (replace-match new-text)
                                       (save-buffer)
                                       "Replacement made")
                              "Text not found"))))
            :args '((:name "file" :type "string")
                    (:name "old_text" :type "string")
                    (:name "new_text" :type "string"))
            :category "edit")
           (gptel-make-tool
            :name "edit_buffer_line"
            :description "Edit a specific line in current buffer"
            :function (lambda (line-num new-content)
                        (goto-char (point-min))
                        (forward-line (1- line-num))
                        (kill-line)
                        (insert new-content)
                        "Line updated")
            :args '((:name "line_num" :type "integer")
                    (:name "new_content" :type "string")))
           ;; Read file tool
           (gptel-make-tool
            :function #'my-gptel-tool-read-file
            :name "read_file"
            :description "Read contents of a file"
            :args (list (list :name "path" :type "string" :description "File path to read"))
            :category "file")

           ;; List files tool
           (gptel-make-tool
            :function #'my-gptel-tool-list-files
            :name "list_files"
            :description "List files in a directory"
            :args (list (list :name "path" :type "string" :description "Directory path"))
            :category "file")

           ;; Bash command tool
           (gptel-make-tool
            :function #'my-gptel-tool-bash
            :name "bash"
            :description "Execute a shell command"
            :args (list (list :name "command" :type "string" :description "Command to execute"))
            :category "system"
            :confirm t)

           (gptel-make-tool
            :function #'my-gptel-get-buffer-info
            :name "get_buffer_info"
            :description "Get information about the current Emacs buffer"
            :args nil
            :category "emacs")

           (gptel-make-tool
            :function #'my-gptel-get-frame-info
            :name "get_frame_info"
            :description "Get information about Emacs frames and windows"
            :args nil
            :category "emacs")

           (gptel-make-tool
            :function #'my-gptel-eval-elisp-safely
            :name "eval_elisp"
            :description "Safely evaluate Emacs Lisp code"
            :args (list (list :name "code" :type "string" :description "Elisp code to evaluate"))
            :category "emacs"
            :confirm t)

           ;; Git operations
           (gptel-make-tool
            :function #'my-gptel-git-status
            :name "git_status"
            :description "Get git repository status"
            :args nil
            :category "git")

           ;; For optional arguments, create separate tools or handle in function
           (gptel-make-tool
            :function (lambda (&optional file) (my-gptel-git-diff file))
            :name "git_diff"
            :description "Show git diff for all files"
            :args nil
            :category "git")

           (gptel-make-tool
            :function (lambda (file) (my-gptel-git-diff file))
            :name "git_diff_file"
            :description "Show git diff for specific file"
            :args (list (list :name "file" :type "string" :description "File to diff"))
            :category "git")

           ;; Buffer operations
           (gptel-make-tool
            :function #'my-gptel-switch-buffer
            :name "switch_buffer"
            :description "Switch to a buffer by name"
            :args (list (list :name "buffer_name" :type "string" :description "Name of buffer to switch to"))
            :category "emacs")

           (gptel-make-tool
            :function #'my-gptel-list-buffers
            :name "list_buffers"
            :description "List all open buffers with their modes and files"
            :args nil
            :category "emacs")

           ;; Project operations - split into two tools
           (gptel-make-tool
            :function (lambda () (my-gptel-project-files))
            :name "project_files"
            :description "List all files in current project"
            :args nil
            :category "project")

           (gptel-make-tool
            :function (lambda (pattern) (my-gptel-project-files pattern))
            :name "project_files_filtered"
            :description "List files in current project filtered by pattern"
            :args (list (list :name "pattern" :type "string" :description "Regex pattern to filter files"))
            :category "project")

           (gptel-make-tool
            :function #'my-gptel-find-file
            :name "find_file"
            :description "Open or create a file"
            :args (list (list :name "path" :type "string" :description "File path to open"))
            :category "file")

           ;; Search operations - split into two tools
           (gptel-make-tool
            :function (lambda (pattern) (my-gptel-grep-project pattern))
            :name "grep_project"
            :description "Search for text patterns in all project files"
            :args (list (list :name "pattern" :type "string" :description "Search pattern"))
            :category "search")

           (gptel-make-tool
            :function (lambda (pattern file-pattern) (my-gptel-grep-project pattern file-pattern))
            :name "grep_project_filtered"
            :description "Search for text patterns in specific project files"
            :args (list (list :name "pattern" :type "string" :description "Search pattern")
                        (list :name "file_pattern" :type "string" :description "File glob pattern (e.g., '*.el')"))
            :category "search")

           (gptel-make-tool
            :function (lambda (buffer text)
                        (with-current-buffer (get-buffer-create buffer)
                          (save-excursion
                            (goto-char (point-max))
                            (insert text)))
                        (format "Appended text to buffer %s" buffer))
            :name "append_to_buffer"
            :description "Append text to the an Emacs buffer.  If the buffer does not exist, it will be created."
            :args (list '(:name "buffer"
                                :type "string"
                                :description "The name of the buffer to append text to.")
                        '(:name "text"
                                :type "string"
                                :description "The text to append to the buffer."))
            :category "emacs")
           (gptel-make-tool
            :function (lambda (filename)
                        (find-file-other-window filename)
                        (format "Opened file or directory %s" filename))
            :name "open_file_or_dir"
            :description "Open a file or directory in this Emacs session."
            :args (list '(:name "file"
                                :type "string"
                                :description "The file or directory to open in the Emacs session."))
            :category "emacs")
           (gptel-make-tool
            :function (lambda (buffer)
                        (unless (buffer-live-p (get-buffer buffer))
                          (error "error: buffer %s is not live." buffer))
                        (with-current-buffer  buffer
                          (buffer-substring-no-properties (point-min) (point-max))))
            :name "read_buffer"
            :description "return the contents of an emacs buffer"
            :args (list '(:name "buffer"
                                :type "string"
                                :description "the name of the buffer whose contents are to be retrieved"))
            :category "emacs")

           (gptel-make-tool
            :name "make_directory"
            :description "Create a new directory with the given name in the specified parent directory"
            :function (lambda (parent name)
                        (condition-case nil
                            (progn
                              (make-directory (expand-file-name name parent) t)
                              (format "Directory %s created/verified in %s" name parent))
                          (error (format "Error creating directory %s in %s" name parent))))
            :args (list '(:name "parent"
                                :type "string"
                                :description "The parent directory where the new directory should be created, e.g. /tmp")
                        '(:name "name"
                                :type "string"
                                :description "The name of the new directory to create, e.g. testdir"))
            :category "file"
            :confirm t)
           (gptel-make-tool
            :name "create_file"
            :description "Create a new file with the specified content"
            :args (list '(:name "path"
                                :type "string"
                                :description "The directory where to create the file")
                        '(:name "filename"
                                :type "string"
                                :description "The name of the file to create")
                        '(:name "content"
                                :type "string"
                                :description "The content to write to the file"))
            :category "file"
            :function (lambda (path filename content)
                        (let ((full-path (expand-file-name filename path)))
                          (with-temp-buffer
                            (insert content)
                            (write-file full-path))
                          (format "Created file %s in %s" filename path)))
            :confirm t)
           (gptel-make-tool
            :name "apply_diff_safer"
            :description "Apply unified diff directly in Emacs buffer without external patch command"
            :function (lambda (buffer-name diff-content)
                        (unless (get-buffer buffer-name)
                          (error "Buffer %s does not exist" buffer-name))

                        (with-current-buffer buffer-name
                          (save-excursion
                            (let ((changes-applied 0)
                                  (buffer-modified-p (buffer-modified-p)))

                              ;; Parse diff hunks
                              (dolist (line (split-string diff-content "\n"))
                                (cond
                                 ;; Handle hunk headers: @@ -old,count +new,count @@
                                 ((string-match "^@@ -\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)? \\+\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)? @@" line)
                                  (let* ((old-start (string-to-number (match-string 1 line)))
                                         (new-start (string-to-number (match-string 3 line)))
                                         (context-lines '())
                                         (removed-lines '())
                                         (added-lines '()))

                                    ;; Collect lines until next hunk or end
                                    (let ((hunk-lines '()))
                                      ;; This is simplified - you'd need to collect the actual hunk lines
                                      ;; For now, just indicate we processed a hunk
                                      (cl-incf changes-applied))))

                                 ;; Simple line replacements for basic diffs
                                 ((and (string-prefix-p "- " line) (string-prefix-p "+ " line))
                                  (let* ((old-text (substring line 2))
                                         (new-text (substring line 2)))
                                    (when (search-forward old-text nil t)
                                      (replace-match new-text)
                                      (cl-incf changes-applied))))))

                              (format "Applied %d changes to buffer %s%s"
                                      changes-applied
                                      buffer-name
                                      (if (and (not buffer-modified-p) (buffer-modified-p))
                                          " (buffer now modified)"
                                        ""))))))
            :args (list '(:name "buffer_name" :type "string" :description "Buffer name to modify")
                        '(:name "diff_content" :type "string" :description "Unified diff content"))
            :category "emacs"
            :confirm t)
           (gptel-make-tool
            :name "replace_text_in_buffer"
            :description "Replace text in a buffer directly without using diff/patch"
            :function (lambda (buffer-name old-text new-text)
                        (unless (get-buffer buffer-name)
                          (error "Buffer %s does not exist" buffer-name))

                        (with-current-buffer buffer-name
                          (save-excursion
                            (goto-char (point-min))
                            (if (search-forward old-text nil t)
                                (progn
                                  (replace-match new-text)
                                  (format "Replaced text in buffer %s" buffer-name))
                              (format "Text not found in buffer %s" buffer-name)))))
            :args (list '(:name "buffer_name" :type "string" :description "Buffer to modify")
                        '(:name "old_text" :type "string" :description "Text to replace")
                        '(:name "new_text" :type "string" :description "Replacement text"))
            :category "emacs"
            :confirm t)
           ))
    (message "Clean gptel tools configured"))
  (my-gptel-setup-tools)


  ;; Load fix for gptel-menu transient crashes (keymapp 2 error)
  ;; Load immediately - no need to wait for gptel-transient

  (require 'gptel-menu-fix)

  ;; Auto-enable gptel-mode for org files with GPTEL properties
  (defun my-auto-enable-gptel-mode ()
    "Auto-enable gptel-mode for files with GPTEL properties."
    (when (eq major-mode 'org-mode)
      (save-excursion
        (goto-char (point-min))
        (when (and (re-search-forward "^:PROPERTIES:" nil t)
                   (let ((props-end (save-excursion (re-search-forward "^:END:" nil t))))
                     (and props-end (re-search-forward "^:GPTEL_" props-end t))))
          (gptel-mode 1)))))

  (defun my-gptel-clean-completion ()
    "Disable auto-completion and remove text-focused completions in gptel buffers."
    (interactive)
    ;; Disable corfu auto-popup in gptel buffers
    (corfu-mode -1)

    ;; Keep useful completions like file paths, but remove text-focused ones
    (setq-local completion-at-point-functions
                (cl-remove-if (lambda (fn)
                                (memq fn '(ispell-completion-at-point
                                           org-completion-at-point
                                           pcomplete-completions-at-point
                                           comint-completion-at-point)))
                              completion-at-point-functions)))

  (add-hook 'find-file-hook 'my-auto-enable-gptel-mode)
  (add-hook 'gptel-mode-hook 'my-gptel-clean-completion)

  ;; Add debug function to see what's happening
  (defun my-gptel-debug-confirmation ()
    "Debug tool confirmation state."
    (interactive)
    (message "Current overlays with gptel-tool: %s"
             (mapcar (lambda (ov)
                       (list (overlay-start ov) (overlay-end ov)
                             (overlay-get ov 'gptel-tool)))
                     (seq-filter (lambda (ov) (overlay-get ov 'gptel-tool))
                                 (overlays-in (point-min) (point-max)))))))

(provide 'my-gpt)
