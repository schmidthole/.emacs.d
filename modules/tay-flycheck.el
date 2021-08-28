(tay/do-if-ext-pkg
 (use-package flycheck
   :defer
   :init
   (setq flycheck-check-syntax-automatically '(mode-enabled save))
   (setq-default flycheck-disabled-checkers '(python-pylint
                                              javascript-jshint
                                              javascript-standard))
   (add-hook 'python-mode-hook 'flycheck-mode)
   (add-hook 'js2-mode-hook 'flycheck-mode)
   (add-hook 'sh-mode-hook 'flycheck-mode)))
