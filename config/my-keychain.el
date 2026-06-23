;; my-keychain.el -*- lexical-binding: t; -*-
;;
;; SSH/GPG agent environment.
;;
;; We deliberately do NOT call `keychain-refresh-environment' at startup.
;; That function shells out to `keychain --eval' and parses SSH_AUTH_SOCK with
;; a regex that predates modern keychain quoting the value: keychain 2.9.8
;; prints `SSH_AUTH_SOCK="/path"' and the regex captures the surrounding double
;; quotes, so Emacs ends up with the literal path `"/path"' (quotes included),
;; which does not exist -- and it overwrites the correct value Emacs already
;; inherited from the launching shell.
;;
;; Since Emacs is launched from a terminal whose shell has already run keychain
;; and exported SSH_AUTH_SOCK, inheriting the environment is correct and the
;; refresh is both unnecessary and harmful. The package is still available, so
;; `M-x keychain-refresh-environment' can be run manually if ever launched
;; outside a shell (note: it will still quote-mangle the value as above).

(use-package keychain-environment
  :ensure keychain-environment
  :defer t)


(provide 'my-keychain)
