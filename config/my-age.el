(use-package pinentry
  :ensure t
  :demand t
  :config
  (setenv "PINENTRY_PROGRAM" "pinentry-emacs")
  (pinentry-start))

(use-package age
  :straight (age :type git :host github :repo "anticomputer/age.el")
  :after pinentry
  :ensure t
  :demand t
  :custom
  ;; you should customize these and not just setq them
  ;; while it won't break anything, age.el checks for
  ;; variable customizations to supersede auto configs
  ;; this only becomes an issue if you e.g. have both
  ;; rage and age installed on a system and want to
  ;; ensure that age-program is actually what is used
  ;; as opposed to the first found compatible version
  ;; of a supported Age client
  (age-debug t)
  (age-pinentry-mode "pinentry-emacs")
  (age-program "rage")
  (age-default-identity
   (my-home-path
    (string-join
      (list ".age/" "identities"))))
  (age-default-recipient
   (my-home-path
      (string-join
       (list ".age/" "recipients"))))
  (age-file-select-keys t)
  :config
  (age-file-enable))

(use-package passage
  :after age
  :straight (passage :type git :host github :repo "anticomputer/passage.el")
  :ensure t
  :demand t
  ;;:custom
)

;;;; fails to load
;; (use-package passage
;;   :straight t
;;   :after age)

;; (setq epa-pinentry-mode 'loopback)
(provide 'my-age)
