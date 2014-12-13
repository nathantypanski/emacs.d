;; my-env.el
;;
;; Environment variables settings.

(defun my-set-environment-or-default (varname &optional default)
  "Attempt to get VARNAME from the global environment and set it in Emacs.

If this fails and DEFAULT is provided, set VARNAME to the default value
DEFAULT in the Emacs environment instead."
  (if (getenv varname)
      (setenv varname (getenv varname))
      (if default (setenv varname default))))

(my-set-environment-or-default "XDG_CONFIG_HOME"
                               (concat (getenv "HOME") "/.config"))

(my-set-environment-or-default "XDG_DATA_HOME"
                               (concat (getenv "HOME") "/.local/share"))

(my-set-environment-or-default "GTAGSLIBPATH"
                               (concat (getenv "XDG_DATA_HOME") "gtags"))

(provide 'my-env)
