(if (not tay/evil)
    (use-package expand-region
      :defer 0.5
      :diminish)

  (use-package smex
    :defer 1.0
    :diminish)

  (use-package mwim
    :defer 0.5
    :diminish)

  (use-package rainbow-delimiters
    :defer 0.2
    :diminish
    :config
    (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)))

(use-package hungry-delete
  :defer 0.2
  :diminish
  :config
  (setq hungry-delete-join-reluctantly t)
  (global-hungry-delete-mode))

(use-package which-key
  :defer 1.5
  :diminish
  :config
  (which-key-setup-side-window-bottom)
  (which-key-mode))

(provide 'tay-editor)
