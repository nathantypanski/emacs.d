;; my-tags.el -*- lexical-binding: t; -*-
;;
;; A perhaps-better-than-ctags tagger for Emacs.
;;
;; Uses GNU Global <http://www.gnu.org/software/global/>.


(use-package ggtags
  :ensure ggtags
  :init
  (progn
    (defun my-gtags-root-dir ()
      "Returns GTAGS root directory or nil if doesn't exist."
      (with-temp-buffer
        (if (zerop (call-process "global" nil t nil "-pr"))
            (buffer-substring (point-min) (1- (point-max)))
          nil)))

    (defun my-gtags-update ()
      "Make GTAGS incremental update"
      (call-process "global" nil nil nil "-u"))

    (defun my-gtags-update-single (filename)
      "Update Gtags database for changes in a single file"
      (interactive)
      (start-process
       "update-gtags" "update-gtags" "bash" "-c"
       (concat "cd " (my-gtags-root-dir) " && gtags --single-update " filename)))

    (defun my-gtags-update-current-file ()
      (interactive)
      (let ((filename (replace-regexp-in-string (my-gtags-root-dir)
                                                "." (buffer-file-name (current-buffer)))))
        (my-gtags-update-single filename)
        (message "Gtags updated for %s" filename)))

    (defun my-gtags-update-hook ()
      "Update GTAGS file incrementally upon saving a file"
      (when ggtags-mode
        (when (my-gtags-root-dir)
          (my-gtags-update-current-file))))

    (add-hook 'after-save-hook #'my-gtags-update-hook)

    (defun my-setup-ggtags ()
      "Configure the modes I want to use ggtags."
      (when (derived-mode-p 'c-mode 'c++-mode)
        (ggtags-mode 1)))

    (add-hook 'c-mode-common-hook 'my-setup-ggtags)
    (after 'evil
      (evil-define-key 'normal c++-mode-map (kbd "SPC d") 'ggtags-show-definition)
      (evil-define-key 'normal c++-mode-map (kbd "SPC s") 'ggtags-find-reference))))

(provide 'my-tags)
