(use-package typescript-mode
  :defer t
  :config
  (setq typescript-indent-level 2))

(use-package prettier-js
  :defer t
  :diminish
  :config
  (add-hook 'typescript-mode-hook 'prettier-js-mode))

(provide 'tay-typescript)
