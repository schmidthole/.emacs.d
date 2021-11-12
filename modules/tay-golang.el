(tay/do-if-ext-pkg
 (use-package go-mode)

 (use-package lsp-mode
   :hook
   (go-mode . lsp-deferred)))
