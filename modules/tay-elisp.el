(tay/do-if-ext-pkg
 (use-package paredit
   :defer
   :config
   (add-hook 'emacs-lisp-mode-hook #'paredit-mode)))
