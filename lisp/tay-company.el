(use-package company
  :defer 1.0
  :diminish
  :config
  (setq company-idle-delay 0.1)
  (setq company-minimum-prefix-length 2)
  (with-eval-after-load 'company
    (tay/bind-key-map company-active-map "ESC" 'company-abort))
  (add-hook 'prog-mode-hook 'company-mode))

(provide 'tay-company)
