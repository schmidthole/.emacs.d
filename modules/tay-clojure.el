(tay/do-if-ext-pkg
 (use-package cider
   :defer t
   :config
   (setq cider-repl-display-help-banner nil)
   (add-hook 'cider-repl-mode-hook #'company-mode)
   (add-hook 'cider-mode-hook #'company-mode)
   (add-hook 'clojure-mode-hook #'company-mode)

   (add-hook 'cider-mode-hook #'eldoc-mode)
   (add-hook 'cider-repl-mode-hook #'eldoc-mode))

 (use-package paredit
   :defer t
   :config
   (add-hook 'cider-repl-mode-hook #'paredit-mode)
   (add-hook 'cider-mode-hook #'paredit-mode)
   (add-hook 'clojure-mode-hook #'paredit-mode))

 (use-package aggressive-indent
   :defer t
   :config
   (add-hook 'cider-mode-hook #'aggressive-indent-mode)
   (add-hook 'clojure-mode-hook #'aggressive-indent-mode))

 (use-package lsp-mode
   :hook ((clojure-mode . lsp-deferred))
   :commands lsp
   :custom
   ((lsp-clojure-server-command '("java" "-jar" "/Users/taylor/.emacs.d/clj-kondo-lsp-server-2022.01.15-standalone.jar")))
   :config
   (dolist (m '(clojure-mode
                clojurescript-mode))
     (add-to-list 'lsp-language-id-configuration `(,m . "clojure")))))
