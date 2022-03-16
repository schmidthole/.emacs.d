(tay/do-if-ext-pkg
 (use-package expand-region
   :defer 1.2
   :diminish)

 (use-package smex
   :defer 0.85
   :diminish)

 (use-package mwim
   :defer 0.8
   :diminish)

 (use-package rainbow-delimiters
   :defer 0.75
   :diminish
   :config
   (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

 (use-package which-key
   :defer 1.4
   :diminish
   :config
   (which-key-setup-side-window-bottom)
   (which-key-mode))

 (use-package hungry-delete
   :defer 0.70
   :config
   (setq hungry-delete-join-reluctantly t)
   (global-hungry-delete-mode)))
