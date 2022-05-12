(use-package modus-themes
  :config
  (setq modus-themes-syntax '(faint))
  (load-theme 'modus-operandi t))

(use-package telephone-line
  :defer 0.1
  :config
  (setq telephone-line-primary-left-separator 'telephone-line-flat)
  (setq telephone-line-secondary-left-separator 'telephone-line-flat)
  (setq telephone-line-primary-right-separator 'telephone-line-flat)
  (setq telephone-line-secondary-right-separator 'telephone-line-flat)
  (telephone-line-mode 1))

(use-package ns-auto-titlebar
  :config
  (when (eq system-type 'darwin) (ns-auto-titlebar-mode)))

(use-package diminish)

(provide 'tay-theme)
