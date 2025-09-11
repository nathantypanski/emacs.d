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
  (claude-code-ide-show-claude-window-in-ediff t)
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

  ;; fix red font hangover from ediff sessions
  (defun my-claude-reset-faces-after-ediff ()
    "Reset faces to prevent color hangover after ediff sessions."
    ;; Reset default face properties that might be stuck
    (when (current-local-map)
      (face-remap-reset-base 'default))
    ;; Clear any lingering face remappings
    (setq face-remapping-alist nil)
    ;; Force font-lock refresh
    (when font-lock-mode
      (font-lock-refresh-defaults)
      (font-lock-fontify-buffer)))

  (add-hook 'ediff-cleanup-hook 'my-claude-reset-faces-after-ediff)

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
  :commands (gptel gptel-menu gptel-send gptel-request my-gptel-review my-gptel-explain my-gptel-claude my-gptel-openai my-gptel-check-tokens)
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
  (gptel-expert-commands t)
  (gptel-enable-enhanced-state-tracking t)
  (gptel-auto-repair-invalid-state t)

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

  ;; Model-specific token configuration
  (defun my-gptel-configure-tokens ()
    "Configure response length and max tokens based on current model."
    (pcase gptel-model
      ('claude-opus-4-20250514
       (setq gptel-response-length 16384 gptel-max-tokens 4096))
      ('claude-sonnet-4-20250514
       (setq gptel-response-length 16384 gptel-max-tokens 4096))
      ('claude-3-7-sonnet-20250219
       (setq gptel-response-length 16384 gptel-max-tokens 4096))
      ('claude-3-opus-20240229
       (setq gptel-response-length 8192 gptel-max-tokens 8192))
      ('claude-3-5-sonnet-20241022
       (setq gptel-response-length 8192 gptel-max-tokens 8192))
      ('claude-3-5-haiku-20241022
       (setq gptel-response-length 4096 gptel-max-tokens 8192))
      ('gpt-4o
       (setq gptel-response-length 4096 gptel-max-tokens 4096))
      ('gpt-4o-mini
       (setq gptel-response-length 16384 gptel-max-tokens 16384))
      ('o1-preview
       (setq gptel-response-length 32768 gptel-max-tokens 32768))
      (_
       (setq gptel-response-length 4096 gptel-max-tokens 4096))))

  ;; Token usage estimator
  (defun my-gptel-check-tokens ()
    "Check estimated token usage before sending."
    (interactive)
    (let* ((content (if (use-region-p)
                        (buffer-substring-no-properties (region-beginning) (region-end))
                      (buffer-string)))
           (estimated-tokens (/ (length content) 4))
           (total (+ estimated-tokens gptel-response-length)))
      (message "Estimated: %d input + %d output = %d total (query limit: %dK)"
               estimated-tokens gptel-response-length total
               (round (/ gptel-max-tokens 1000.0)))))

  ;; Auto-configure tokens when model changes
  (advice-add 'my-gptel-claude :after (lambda (&rest _) (my-gptel-configure-tokens)))
  (advice-add 'my-gptel-openai :after (lambda (&rest _) (my-gptel-configure-tokens)))

  ;; Configure initial tokens
  (my-gptel-configure-tokens)

  ;; Buffer behavior
  (defun my-gptel-setup-behavior ()
    "Setup gptel buffer behavior."
    (visual-line-mode 1)
    (setq-local auto-save-timeout 60)
    (setq-local auto-save-visited-mode nil))

  ;; Provider switching
  (defun my-gptel-claude (model)
    "Switch to Claude backend with MODEL selection."
    (interactive
     (list (completing-read "Claude Model: "
                            '("claude-opus-4-20250514" "claude-sonnet-4-20250514" "claude-3-7-sonnet-20250219" "claude-3-opus-20240229" "claude-3-5-sonnet-20241022" "claude-3-5-haiku-20241022")
                            nil t nil nil "claude-opus-4-20250514")))
    (require 'gptel-anthropic)
    (let ((key (or (getenv "ANTHROPIC_API_KEY")
                   (read-passwd "Anthropic API Key: "))))
      (setq gptel-model (intern model)
            gptel-backend (gptel-make-anthropic "Claude" :key key :stream t)
            gptel-api-key key)
      (message "Switched to Claude %s" model)))

  (defun my-gptel-openai (model)
    "Switch to OpenAI backend with MODEL selection."
    (interactive
     (list (completing-read "OpenAI Model: "
                            '("gpt-4o" "gpt-4o-mini" "gpt-4-turbo" "o1-preview" "o1-mini")
                            nil t nil nil "gpt-4o-mini")))
    (let* ((key (or (getenv "OPENAI_API_KEY")
                    (read-passwd "OpenAI API Key: ")))
           (backend (cdr (assoc "ChatGPT" gptel--known-backends)))
           (model-obj (cl-find model (gptel-backend-models backend)
                               :test (lambda (name model)
                                       (string= name (gptel--model-name model))))))
      (setq gptel-model model-obj
            gptel-backend backend
            gptel-api-key key)
      (message "Switched to OpenAI %s" model)))

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

  ;; Simplified tool integration
  (defvar my-gptel-tools-enabled nil)

  (defun my-gptel-enable-tools ()
    "Enable claude-agent tools for gptel."
    (interactive)
    (unless my-gptel-tools-enabled
      (add-to-list 'load-path (expand-file-name "pkg/claude-agent" user-emacs-directory))
      (when (locate-library "claude-agent")
        (unless (locate-library "request")
          (when (fboundp 'straight-use-package)
            (straight-use-package 'request)))
        (require 'claude-agent)

        ;; Register tools directly with gptel
        (setq gptel-tools
              (list
               (gptel-make-tool
                :function (lambda (&rest args)
                           (let ((path (plist-get args :path)))
                             (claude-agent--tool-read-file `((path . ,path)))))
                :name "read_file"
                :description "Read contents of a file"
                :args (list (list :name "path" :type "string" :description "File path"))
                :category "filesystem")

               (gptel-make-tool
                :function (lambda (&rest args)
                           (let ((path (plist-get args :path)))
                             (claude-agent--tool-list-files `((path . ,path)))))
                :name "list_files"
                :description "List files in directory"
                :args (list (list :name "path" :type "string" :description "Directory path"))
                :category "filesystem")

               (gptel-make-tool
                :function (lambda (&rest args)
                           (let ((command (plist-get args :command)))
                             (claude-agent--tool-bash `((command . ,command)))))
                :name "bash"
                :description "Execute shell command"
                :args (list (list :name "command" :type "string" :description "Shell command"))
                :category "system"
                :confirm t)))

        (setq my-gptel-tools-enabled t)
        (message "Enhanced gptel tools enabled"))))
  ;; Load fix for gptel-menu transient crashes (keymapp 2 error)
  (with-eval-after-load 'gptel-transient
    (let ((fix-file (expand-file-name "config/gptel-menu-fix.el" user-emacs-directory)))
      (when (file-exists-p fix-file)
        (load-file fix-file))))

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

  (defun my-corfu-disable-ispell-completion ()
    "Remove ispell-completion-at-point in modes where it causes issues."
    (setq-local completion-at-point-functions
                (remove 'ispell-completion-at-point
                        completion-at-point-functions)))

  (add-hook 'find-file-hook 'my-auto-enable-gptel-mode)
  (add-hook 'gptel-mode-hook 'my-corfu-disable-ispell-completion)

  ;; Auto-setup tools when gptel loads
  (my-gptel-enable-tools))

(provide 'my-gpt)
