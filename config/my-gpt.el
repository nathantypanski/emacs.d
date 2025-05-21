;; my-gpt.el -*- lexical-binding: t; -*-
;;
;; configure llm interactions

(defvar my-gptel-system-prompt
"You are a LLM running inside Emacs. Your responses are inserted literally into the buffer where the prompt is sent - usually code in the language being discussed. Do not use markdown or org to structure your comments. Instead, structure in alignment with the surrounding text. Put your commentary in comments (e.g., `;;` for elisp, `//` for go, ...)."
  "my preferred system prompt for gptel")

(use-package gptel
  :straight (gptel :type git :host github :repo "karthink/gptel")
  :defer t
  :commands (gptel gptel-menu gptel-send gptel-request)
  :hook (gptel-mode . visual-line-mode)
  :config
  (setq gptel-api-key (getenv "OPENAI_API_KEY"))
  ;; Set default mode for gptel conversation
  (setq gptel-default-mode 'org-mode)
  ;; is this correct? I want to make sure my directive is default.
  (add-to-list 'gptel-directives (cons 'Inline my-gptel-system-prompt)))


(provide 'my-gpt)
