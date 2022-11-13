
(use-package ivy
  :defer 0.2
  :diminish
  :config
  (setq ivy-sort-max-size 7500)
  (setq ivy-height 17
        ivy-wrap t
        ivy-fixed-height-minibuffer t
        ivy-read-action-format-function #'ivy-read-action-format-columns
        ivy-on-del-error-function #'ignore
        ivy-use-selectable-prompt t
        ivy-count-format "(%d/%d) "
        ivy-display-style 'fancy
        ivy-initial-inputs-alist nil
        ivy-use-virtual-buffers t)
  (ivy-mode 1))

(use-package counsel
  :diminish
  :after ivy
  :config
  (counsel-mode))

;; (use-package swiper
;;   :diminish
;;   :after ivy
;;   :config
;;   (setq swiper-goto-start-of-match t))

(provide 'tay-ivy)
