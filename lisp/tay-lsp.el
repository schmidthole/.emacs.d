(use-package lsp-mode
  :hook
  ((clojure-mode . lsp-deferred)
   (c-mode . lsp-deferred)
   (python-mode . lsp-deferred)
   (typescript-mode . lsp-deferred)
   (js-mode . lsp-deferred)
   (web-mode . lsp-deferred))
  :commands
  (lsp lsp-deferred)
  :config
  ;; cc
  (setq lsp-clients-clangd-executable "/usr/local/opt/llvm/bin/clangd")
  ;; clojure
  (dolist (m '(clojure-mode
               clojurescript-mode))
    (add-to-list 'lsp-language-id-configuration `(,m . "clojure")))
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
  (setq lsp-pylsp-plugins-pylint-enabled t)
  :custom
  ((lsp-clojure-server-command '("java" "-jar" "/Users/taylor/.emacs.d/bin/clj-kondo-lsp-server-2022.01.15-standalone.jar"))))

(provide 'tay-lsp)
