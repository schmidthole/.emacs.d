(add-hook 'markdown-mode-hook (lambda () (flyspell-mode 1)))
(add-hook 'org-mode-hook (lambda () (flyspell-mode 1)))

(provide 'tay-flyspell)
