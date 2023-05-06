(setq python-indent-offset 4
      python-shell-completion-native-enable nil)

(use-package blacken
  :diminish
  :defer
  :init
  (setq blacken-line-length 100)
  (add-hook 'python-mode-hook 'blacken-mode))

(use-package python-pytest)

(provide 'tay-python)
