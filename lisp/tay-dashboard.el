(use-package dashboard
  :config
  (setq dashboard-center-content t)
  (setq dashboard-items '((recents  . 5)
                          (agenda . 15)))
  (dashboard-setup-startup-hook))

(provide 'tay-dashboard)
