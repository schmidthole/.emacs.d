(setq python-indent-offset 4
      python-shell-completion-native-enable nil
      python-shell-interpreter "/Library/Frameworks/Python.framework/Versions/3.7/bin/python3")

(tay/do-if-ext-pkg
 (use-package blacken
   :defer
   :init
   (setq blacken-line-length 100)
   (add-hook 'python-mode-hook 'blacken-mode))

 (use-package lsp-mode
   :hook
   (python-mode . lsp-deferred)
   :commands
   (lsp lsp-deferred)
   :config
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
   (setq lsp-pylsp-plugins-pylint-enabled t)))
