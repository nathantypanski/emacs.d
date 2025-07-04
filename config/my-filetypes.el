;; my-filetypes.el -*- lexical-binding: t; -*-
;;
;; This is for filetype-specific mode settings that don't belong anywhere
;; else. In the case of language-specific settings that already have sufficient
;; outside customization, place those files in their appropriate
;; config/my-languages/ language file.

(add-to-list 'auto-mode-alist '("/mutt" . mail-mode))

(use-package sudo-edit
  :commands (sudo-edit)
  :straight t
  :defer t)

(provide 'my-filetypes)
