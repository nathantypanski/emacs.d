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

  ;; REMOVED: fix red font hangover from ediff sessions
  ;; The my-claude-reset-faces-after-ediff function was causing intermittent
  ;; syntax highlighting failures by aggressively clearing face remappings.
  ;; Modern ediff should handle cleanup properly without custom intervention.

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

(use-package gptel
  :straight (:repo "karthink/gptel" :branch "state-tracking" :files ("*.el"))
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
  (gptel-include-tool-results t)
  (gptel-confirm-tool-calls t)
  :init
  ;; Core prompts
  (defvar my-gptel-system-prompt
    "You are a LLM running inside Emacs. Your responses are inserted literally into the buffer where the prompt is sent - usually code in the language being discussed. Do not use markdown or org to structure your comments. Instead, structure in alignment with the surrounding text. Put your commentary in comments (e.g., `;;` for elisp, `//` for go, ...).")

  (defvar my-gptel-elisp-prompt
    "You are a LLM running inside Emacs. Help me work on my elisp config. I am using emacs 30.1.90 and (use-package) with straight. Respond in org-mode. I can share any buffers or config files you request in the context.")

  (defvar my-gptel-directives
    (list (cons 'Inline my-gptel-system-prompt)
          (cons 'Elisp my-gptel-elisp-prompt)))

  :config
  ;; Basic setup
  (when (getenv "OPENAI_API_KEY")
    (setq gptel-api-key (getenv "OPENAI_API_KEY")))
  (setq gptel-default-mode 'org-mode)
  (setq gptel-directives (append my-gptel-directives gptel-directives))

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
    "Read contents of file at PATH with safety checks."
    (let ((expanded-path (expand-file-name path)))
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
          (buffer-string))))))

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

  ;; Register tools with gptel
  (defun my-gptel-setup-tools ()
    "Setup working gptel tools."
      (setq gptel-tools
            (list
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
              :confirm t)))
      (message "Clean gptel tools configured"))

  ;; Setup tools after gptel loads
  (with-eval-after-load 'gptel
    (when (fboundp 'gptel-make-tool)
    (my-gptel-setup-tools)))

  ;; Load fix for gptel-menu transient crashes (keymapp 2 error)
  ;; Load immediately - no need to wait for gptel-transient
  (let ((fix-file (expand-file-name "config/gptel-menu-fix.el" user-emacs-directory)))
    (when (file-exists-p fix-file)
      (load-file fix-file)))

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

  ;; DISABLED: Auto-setup tools - causes transient crashes
  ;; (my-gptel-enable-tools)
  )

(provide 'my-gpt)
