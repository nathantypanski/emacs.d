;; -*- lexical-binding: t; -*-
(setq c-default-style '((java-mode . "java")
                        (awk-mode . "awk")
                        (other . "linux")))

(defun my-set-evil-shift-width ()
  "Set Evil's shift width for C editing."
  (after 'evil
    (setq evil-shift-width 8)))

(add-hook 'c-initialization-hook 'my-set-evil-shift-width)

(setq c-hungry-delete-key t)

(defun my-c-mode-common-setup ()
  "Setup C/C++-mode common configurations."
  ;; (setq flycheck-clang-language-standard "c17")
  (c-set-offset 'case-label '+))

(defun my-c++-mode-setup ()
  "Setup C++-mode configurations."
  (interactive)
  (google-set-c-style)
  (after 'flycheck
    ;; (setq flycheck-clang-language-standard "c++11")
    (after 'projectile
      (if (projectile-project-root)
          (add-to-list 'flycheck-clang-include-path (concat (projectile-project-root) "src")))
      )))


(add-hook 'c-mode-common-hook 'my-c-mode-common-setup)
(add-hook 'c++-mode-hook 'my-c++-mode-setup)

(after 'evil
    (evil-define-key 'normal c-mode-map (kbd "K")   'my-woman-entry)
    (evil-define-key 'insert c-mode-map (kbd "<backspace>") 'backward-delete-char-untabify)
    (evil-define-key 'normal c++-mode-map (kbd "K")   'my-woman-entry)
    (evil-define-key 'insert c++-mode-map (kbd "<backspace>") 'backward-delete-char-untabify))

(provide 'my-c)
