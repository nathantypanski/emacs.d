;; my-env.el
;;
;; Environment variables settings.


(setenv "XDG_CONFIG_HOME" (concat (getenv "HOME") "/.config"))
(setenv "XDG_DATA_HOME" (concat (getenv "HOME") "/.local/share"))
(setenv "GTAGSLIBPATH" (concat (getenv "XDG_DATA_HOME") "gtags"))

(provide 'my-env)
