(tay/do-if-ext-pkg
 (use-package markdown-mode
   :defer t
   :config
   (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))))
