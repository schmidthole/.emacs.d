(use-package smartparens
  :defer 0.8
  :config
  (require 'smartparens-config)
  (add-hook 'prog-mode-hook #'smartparens-strict-mode))

(provide 'tay-smartparens)
