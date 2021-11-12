(tay/do-if-ext-pkg
 (use-package adoc-mode
   :config
   (add-to-list 'auto-mode-alist (cons "\\.txt\\'" 'adoc-mode))
   (add-to-list 'auto-mode-alist (cons "\\.adoc\\'" 'adoc-mode))))
