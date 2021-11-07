(tay/do-if-ext-pkg
 (use-package typescript-mode
   :config
   (setq typescript-indent-level 2))

 (use-package lsp-mode
   :config
   (lsp-register-client
    (make-lsp-client :new-connection (lsp-tramp-connection '("typescript-language-server" "--stdio"))
                     :major-modes '(typescript-mode js-mode)
                     :remote? t
                     :server-id 'tsserver-remote))
   :hook (typescript-mode . lsp-deferred)
   :hook (js-mode . lsp-deferred))

 (use-package prettier-js
   :config
   (add-hook 'typescript-mode-hook 'prettier-js-mode)))
