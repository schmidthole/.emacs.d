(if (not tay/evil)
    (progn
      (use-package expand-region
        :defer 0.5
        :diminish)
      (use-package change-inner
        :defer 0.6
        :diminish)))

(use-package hungry-delete
  :defer 0.2
  :diminish
  :config
  (setq hungry-delete-join-reluctantly t)
  (global-hungry-delete-mode))

(use-package rainbow-mode
  :config
  (rainbow-mode))

;; (use-package ivy-posframe
;;   :config
;;   (setq ivy-posframe-display-functions-alist
;;         '((counsel-ag . ivy-display-function-fallback)
;;           (t . ivy-posframe-display)))
;;   (ivy-posframe-mode 1))

(provide 'tay-editor)
