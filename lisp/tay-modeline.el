(use-package doom-modeline
  :config
  (setq doom-modeline-icon nil)
    (doom-modeline-def-modeline 'tay/doom-modeline
    '(bar
      matches
      follow
      buffer-info
      buffer-position
      remote-host
      selection-info)
    '(compilation
      major-mode
      process))
  (add-hook 'doom-modeline-mode-hook
            (lambda () (doom-modeline-set-modeline 'tay/doom-modeline 'default)))
  (doom-modeline-mode 1))

(provide 'tay-modeline)
