;; my-gpt.el -*- lexical-binding: t; -*-
;;
;; configure llm interactions

(defvar my-gptel-system-prompt
"You are a LLM running inside Emacs. Your responses are inserted literally into the buffer where the prompt is sent - usually code in the language being discussed. Do not use markdown or org to structure your comments. Instead, structure in alignment with the surrounding text. Put your commentary in comments (e.g., `;;` for elisp, `//` for go, ...)."
  "my preferred system prompt for gptel")

(defvar my-gptel-elisp-prompt
  "You are a LLM running inside Emacs. Help me work on my elisp
config. I am using emacs 30.1.90 and (use-package) with =straight=.
Respond in org-mode. I can share any buffers or config files you
request in the context."
  "my elisp authorship system prompt for gptel")

(defvar my-gptel-go-prompt
  "you are a programmer’s assistant. respond with concise, idiomatic go code or explanations. if code is requested, use clear formatting and include comments for non-obvious lines. focus on correctness and best practices."
  "prompt for go code and advice.")

(defvar my-gptel-bash-prompt
  "you are a shell scripting assistant. replies should be concise and portable (posix/bash). always include comments for complex lines and care about quoting/escaping."
  "prompt for bash and posix shell tasks.")

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

(defvar my-gptel-directives
  (list
   (cons 'Go      my-gptel-go-prompt)
   (cons 'Bash    my-gptel-bash-prompt)
   (cons 'Nix     my-gptel-nix-prompt)
   (cons 'Explain my-gptel-explain-prompt)
   (cons 'Debug   my-gptel-debug-prompt)
   (cons 'Edit    my-gptel-edit-prompt)
   (cons 'Inline my-gptel-system-prompt)
   (cons 'Elisp  my-gptel-elisp-prompt)
   ;; You may also add your 'Inline' or 'Elisp' prompts here if you like!
   )
  "My preferred gptel directives for multi-language workflows.")


(use-package gptel
  :straight (gptel :type git :host github :repo "karthink/gptel")
  :defer t
  :commands (gptel gptel-menu gptel-send gptel-request)
  :hook (gptel-mode . visual-line-mode)
  :config
  (if (getenv "OPENAI_API_KEY")
      (setq gptel-api-key (getenv "OPENAI_API_KEY")))
  ;; Set default mode for gptel conversation
  (setq gptel-default-mode 'org-mode)
  ;; is this correct? I want to make sure my directive is default.

  (setq gptel-directives
        (let* ((my-keywords (mapcar #'car my-gptel-directives))
               (filtered (seq-remove (lambda (item)
                                       (memq (car item) my-keywords))
                                     gptel-directives)))
          (append my-gptel-directives filtered)))

  (evil-collection-gptel-setup))

(use-package mcp
  :straight
 (:type git
       :host github
       :repo "lizqwerscott/mcp.el"
       :files ("*.el")))
(use-package mcp
  :straight (:type git :host github :repo "lizqwerscott/mcp.el" :files ("*.el"))
  ;; Auto-start all MCP servers after initialization
  ;; :hook (after-init . mcp-hub-start-all-server))
  :after gptel
  :config
  ;; Configure MCP servers
  )

(provide 'my-gpt)
