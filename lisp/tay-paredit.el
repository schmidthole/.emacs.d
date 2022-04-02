 (use-package paredit
   :config
   (add-hook 'emacs-lisp-mode-hook #'paredit-mode)
   (add-hook 'cider-repl-mode-hook #'paredit-mode)
   (add-hook 'cider-mode-hook #'paredit-mode)
   (add-hook 'clojure-mode-hook #'paredit-mode))

(provide 'tay-paredit)
