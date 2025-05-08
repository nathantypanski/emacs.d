
(use-package gptel
  :straight (gptel :type git :host github :repo "karthink/gptel")
  :defer t
  :commands (gptel gptel-menu gptel-send gptel-request)
  :init
  (let ((api-key (getenv "OPENAI_API_KEY")))
    (if api-key
        (setq gptel-api-key api-key)
      (warn "OPENAI_API_KEY not set; gptel will not work without it.")))
  :config
  ;; Set default mode for gptel conversation
  (setq gptel-default-mode 'org-mode)

  ;; Optionally, set any other configurations for better integration
  ;; For example, you might want to customize gptel's output or other settings here.
  ;; (setq gptel-output-buffer "*GPTel Output*")
)

(provide 'my-gpt)

