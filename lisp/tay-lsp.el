(use-package lsp-mode
  :hook
  ((c-mode . lsp-deferred)
   (c++-mode . lsp-deferred)
   (python-mode . lsp-deferred)
   (typescript-mode . lsp-deferred)
   (js-mode . lsp-deferred)
   (web-mode . lsp-deferred))
  :commands
  (lsp lsp-deferred)
  :config
  ;; cc
  (setq lsp-clients-clangd-executable "/opt/homebrew/opt/llvm/bin/clangd")
  ;; python
  (setq lsp-pyls-plugins-flake8-enabled t)
  (setq lsp-pylsp-plugins-flake8-enabled t)
  (setq lsp-pyls-plugins-pycodestyle-enabled nil)
  (setq lsp-pylsp-plugins-pycodestyle-enabled nil)
  (setq lsp-pyls-plugins-pydocstyle-ignore t)
  (setq lsp-pylsp-plugins-pydocstyle-enabled nil)
  (setq lsp-pyls-plugins-pydocstyle-enabled nil)
  (setq lsp-pyls-plugins-pydocstyle-add-ignore
        '(D100 D103 D205 D400 D401))
  (setq lsp-pyls-plugins-pyflakes-enabled nil)
  (setq lsp-pylsp-plugins-pyflakes-enabled nil)
  (setq lsp-pyls-plugins-pylint-enabled t)
  (setq lsp-pylsp-plugins-pylint-enabled t))

(provide 'tay-lsp)
