(setq python-indent-offset 4
      python-shell-completion-native-enable nil
      python-shell-interpreter "/Library/Frameworks/Python.framework/Versions/3.7/bin/python3")

(use-package blacken
  :diminish
  :defer
  :init
  (setq blacken-line-length 100)
  (add-hook 'python-mode-hook 'blacken-mode))

(provide 'tay-python)
