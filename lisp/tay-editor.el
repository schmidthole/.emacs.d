(if (not tay/evil)
    (use-package expand-region
      :defer 0.5
      :diminish))

(use-package hungry-delete
  :defer 0.2
  :diminish
  :config
  (setq hungry-delete-join-reluctantly t)
  (global-hungry-delete-mode))

(provide 'tay-editor)
