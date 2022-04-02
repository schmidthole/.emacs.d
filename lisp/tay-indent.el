(use-package aggressive-indent
  :defer t
  :config
  (add-hook 'cider-mode-hook #'aggressive-indent-mode)
  (add-hook 'clojure-mode-hook #'aggressive-indent-mode)
  (add-hook 'emacs-lisp-mode-hook #'aggressiv-indent-mode))

(provide 'tay-indent)
