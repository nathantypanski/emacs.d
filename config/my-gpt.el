;; my-gpt.el -*- lexical-binding: t; -*-
;;
;; configure llm interactions

(use-package gptel
  :straight (gptel
             :type git
             :host github
             :repo "karthink/gptel"
             :branch "master")
  :defer t
  :commands
  (gptel gptel-menu gptel-send gptel-request
         my-gptel-review my-gptel-explain my-gptel-review
         my-gptel-openai my-gptel-claude)

  :hook ((gptel-mode . my-gptel-setup-keybindings)
         (gptel-mode . my-gptel-setup-behavior))
  :custom
  (gptel-track-response nil)  ; Don't track for privacy
  (gptel-log-level 'info)
  (gptel-use-curl nil)
  (gptel-stream t)
  :init
  (defvar my-gptel-system-prompt
    "You are a LLM running inside Emacs. Your responses are inserted literally into the buffer where the prompt is sent - usually code in the language being discussed. Do not use markdown or org to structure your comments. Instead, structure in alignment with the surrounding text. Put your commentary in comments (e.g., `;;` for elisp, `//` for go, ...)."
    "my preferred system prompt for gptel")

  (defvar my-gptel-elisp-prompt
    "You are a LLM running inside Emacs. Help me work on my elisp
config. I am using emacs 30.1.90 and (use-package) with =straight=.
Respond in org-mode. I can share any buffers or config files you
request in the context."
    "my elisp authorship system prompt for gptel")

  (defvar my-gptel-nix-prompt
    "you are a nix and nixos expert. help with concise, idiomatic nix expressions and troubleshooting. explain key concepts clearly when asked."
    "prompt for nix/nixos configuration and code.")

  (defvar my-gptel-explain-prompt
    "you are a friendly technical explainer. given any code or configuration, break it down into clear, step-by-step explanations that could help a developer new to that language or tool."
    "prompt to ask for code explanations.")

  (defvar my-gptel-debug-prompt
    "you are a debugging assistant. help the user systematically diagnose and solve bugs in code or config, suggesting possible causes and useful troubleshooting steps, highlighting common mistakes relevant to the language in question."
    "prompt for troubleshooting and debugging.")

  (defvar my-gptel-edit-prompt
    "you are a code review and refactoring assistant. suggest edits for clarity, idiomatic style, or robustness, while keeping the original structure where possible. explain your suggestions."
    "prompt for code review or edit requests.")

  ;; Session management
  (defun my-gptel-setup-behavior ()
    "Setup gptel buffer behavior."
    (interactive)
    ;; Enable word wrap for better AI text display
    (visual-line-mode 1)
    ;; Auto-save gptel buffers less frequently to avoid interrupting streams
    (setq-local auto-save-timeout 60)
    ;; Disable aggressive auto-save during streaming
    (setq-local auto-save-visited-mode nil))

  (defvar my-gptel-directives
    (list
     (cons 'Nix     my-gptel-nix-prompt)
     (cons 'Explain my-gptel-explain-prompt)
     (cons 'Debug   my-gptel-debug-prompt)
     (cons 'Edit    my-gptel-edit-prompt)
     (cons 'Inline my-gptel-system-prompt)
     (cons 'Elisp  my-gptel-elisp-prompt))
    "My preferred gptel directives for multi-language workflows.")

  :config
  ;; Initialize the hash table in :config section
  (setq my-gptel-api-keys (make-hash-table :test 'equal))

  (if (getenv "OPENAI_API_KEY")
      (setq gptel-api-key (getenv "OPENAI_API_KEY")))

  ;; Set default mode for gptel conversation
  (setq gptel-default-mode 'org-mode)

  ;; Fix model name - should be string
  (setq gptel-model "gpt-4.1")

  (setq gptel-directives
        (let* ((my-keywords (mapcar #'car my-gptel-directives))
               (filtered (seq-remove (lambda (item)
                                       (memq (car item) my-keywords))
                                     gptel-directives)))
          (append my-gptel-directives filtered)))

  ;; Only require if it exists
  (when (featurep 'gptel-integrations)
    (require 'gptel-integrations))

  (defun my-gptel-get-key (provider)
    "Get API key for PROVIDER, cached."
    ;; Ensure hash table exists
    (unless (hash-table-p my-gptel-api-keys)
      (setq my-gptel-api-keys (make-hash-table :test 'equal)))
    (or (gethash provider my-gptel-api-keys)
        (let ((key (pcase provider
                     ("openai" (or (getenv "OPENAI_API_KEY")
                                   (auth-source-pick-first-password
                                    :host "api.openai.com" :user "apikey")))
                     ("anthropic" (or (getenv "ANTHROPIC_API_KEY")
                                      (auth-source-pick-first-password
                                       :host "api.anthropic.com" :user "apikey"))))))
          (when key 
            (puthash provider key my-gptel-api-keys))
          key)))

  ;; Provider switching
  (defun my-gptel-openai ()
    "Switch to OpenAI."
    (interactive)
    (if-let ((key (my-gptel-get-key "openai")))
        (setq gptel-model "gpt-4.1"
              gptel-backend (gptel-make-openai "OpenAI" :key key))
      (user-error "No OpenAI key found")))

  (defun my-gptel-claude ()
    "Switch to Claude."
    (interactive)
    (if-let ((key (my-gptel-get-key "anthropic")))
        (setq gptel-model "claude-sonnet-4-20250514"
              gptel-backend (gptel-make-anthropic "Claude" :key key))
      (user-error "No Anthropic key found")))

  ;; Two simple functions to start
  (defun my-gptel-explain ()
    "Explain current region or function."
    (interactive)
    (let ((text (if (use-region-p)
                    (buffer-substring-no-properties (region-beginning) (region-end))
                  (thing-at-point 'defun t))))
      (if text
          (gptel-request (format "Explain this code:\n\n=\n%s\n=" text))
        (message "No code to explain"))))

  (defun my-gptel-review ()
    "Review current region or buffer."
    (interactive)
    (let ((text (if (use-region-p)
                    (buffer-substring-no-properties (region-beginning) (region-end))
                  (buffer-string))))
      (gptel-request (format "Review this code for bugs and improvements:\n\n=\n%s\n=" text))))

  ;; Keybinding setup function following your org pattern
  (defun my-gptel-setup-keybindings ()
    "Setup my keybindings for gptel-mode."
    (interactive)

    ;; Ensure general is available
    (when (fboundp 'general-define-key)
      ;; First define the prefix key
      (general-define-key
       :states '(normal visual)
       "SPC a" '(:ignore t :which-key "AI"))

      ;; Local gptel-mode bindings (only active in gptel buffers)
      (general-define-key
       :states '(normal)
       :keymaps 'local
       "SPC g" '(:ignore t :which-key "gptel")
       "SPC g g" #'gptel-send
       "SPC g e" #'my-gptel-explain
       "SPC g r" #'my-gptel-review
       "SPC g 4" #'my-gptel-openai
       "SPC g c" #'my-gptel-claude
       "SPC g q" #'quit-window)

      ;; Global AI keybindings
      (general-define-key
       :states '(normal visual)
       "SPC a g" #'gptel
       "SPC a m" #'gptel-menu
       "SPC a e" #'my-gptel-explain
       "SPC a r" #'my-gptel-review
       "SPC a 4" #'my-gptel-openai
       "SPC a c" #'my-gptel-claude))

    ;; Only call if the function exists
    (when (fboundp 'evil-collection-gptel-setup)
      (evil-collection-gptel-setup))))

(use-package mcp
  :straight (:type git :host github :repo "lizqwerscott/mcp.el" :files ("*.el"))
  ;; Auto-start all MCP servers after initialization
  ;; :hook (after-init . mcp-hub-start-all-server))
  :after gptel
  :config
  ;; Configure MCP servers
  )

(provide 'my-gpt)
