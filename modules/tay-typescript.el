(tay/do-if-ext-pkg
 (use-package typescript-mode
   :config
   (setq typescript-indent-level 2))

 (use-package lsp-mode
   :hook (typescript-mode . lsp-deferred)
   :hook (js-mode . lsp-deferred))

 (use-package prettier-js
   :config
   (add-hook 'typescript-mode-hook 'prettier-js-mode)))
