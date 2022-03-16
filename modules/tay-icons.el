(tay/do-if-ext-pkg
 (use-package all-the-icons)

 (use-package all-the-icons-dired
   :diminish
   :init
   (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

 (use-package all-the-icons-ivy
   :diminish
   :init
   (add-hook 'after-init-hook 'all-the-icons-ivy-setup)))
