(use-package age
  :after (org)
  :ensure t
  :demand t
  :commands age-encryption-mode age-start-decrypt age-encrypt-file
            age-find-file-hook age-file-enable age-encrypt-region
            age-decrypt-region
  :init

  :custom
  ;; you should customize these and not just setq them
  ;; while it won't break anything, age.el checks for
  ;; variable customizations to supersede auto configs
  ;; this only becomes an issue if you e.g. have both
  ;; rage and age installed on a system and want to
  ;; ensure that age-program is actually what is used
  ;; as opposed to the first found compatible version
  ;; of a supported Age client
  (age-program "rage")
  (age-default-identity
   (my-home-path
    (string-join
      (list ".age/" "identities"))))
  (age-default-recipient
   (my-home-path
      (string-join
       (list ".age/" "recipients"))))
  :config
  (age-file-enable))

(provide 'my-age)
