(use-package dockerfile-mode
  :commands dockerfile-mode
  :ensure dockerfile-mode
  :mode "\\Docker\\'")

(use-package docker
  :commands docker
  :ensure docker)

(provide 'my-docker)
