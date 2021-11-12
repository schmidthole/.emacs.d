(tay/do-if-ext-pkg
 (use-package ivy
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
        ivy-initial-inputs-alist nil)
  (ivy-mode 1))

(use-package counsel
  :after ivy
  :config
  (counsel-mode))

(use-package swiper
  :config
  (setq swiper-goto-start-of-match t))

(use-package avy
  :defer 0.4
  :config
  (defun tay/avy-select-to-line ()
    "use avy to select from the current point to a certain line. operates similar
to vi's visual line mode"
    (interactive)
    (set-mark (point))
    (avy-goto-line)
    (exchange-point-and-mark)
    (if (< (point) (mark))
        (beginning-of-line)
      (end-of-line))
    (exchange-point-and-mark)
    (if (< (point) (mark))
        (beginning-of-line)
      (end-of-line)))))
