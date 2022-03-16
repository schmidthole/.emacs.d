(tay/do-if-ext-pkg
 (use-package yaml-mode
   :defer t
   :config
   (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))))
