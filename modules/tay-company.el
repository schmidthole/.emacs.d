(tay/do-if-ext-pkg
 (use-package company :ensure t
   :defer 0.9
   :diminish
   :config
   (setq company-idle-delay 0.1)
   (setq company-minimum-prefix-length 2)
   (add-hook 'prog-mode-hook 'company-mode)))
