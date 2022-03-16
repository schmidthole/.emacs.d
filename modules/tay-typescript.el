(tay/do-if-ext-pkg
 (use-package typescript-mode
   :defer t
   :config
   (setq typescript-indent-level 2))

 (use-package lsp-mode
   :hook (typescript-mode . lsp-deferred)
   :hook (js-mode . lsp-deferred))

 (use-package prettier-js
   :defer t
   :diminish
   :config
   (add-hook 'typescript-mode-hook 'prettier-js-mode)))
