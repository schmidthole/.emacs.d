(add-hook 'c-mode-hook 'eglot-ensure)
(add-hook 'c++-mode-hook 'eglot-ensure)
(add-hook 'python-mode-hook 'eglot-ensure)
(add-hook 'typescript-mode-hook 'eglot-ensure)
(add-hook 'js-mode-hook 'eglot-ensure)
(add-hook 'web-mode-hook 'eglot-ensure)
(add-hook 'go-mode-hook 'eglot-ensure)

(setq eglot-autoshutdown t)
(setq eldoc-echo-area-use-multiline-p nil)

(provide 'tay-eglot)
