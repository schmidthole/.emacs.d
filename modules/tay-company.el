(tay/do-if-ext-pkg
 (use-package company :ensure t
   :config
   (setq company-idle-delay 0.5)
   (setq company-minimum-prefix-length 2)
   (add-hook 'prog-mode-hook 'company-mode)))
