(use-package vterm
  :defer 1.8
  :init
  (setq vterm-always-compile-module t)
  :config
  (define-key vterm-mode-map (kbd "C-u") #'vterm--self-insert)
  (defun tay/vterm-new ()
    "Make a brand new vterm buffer"
    (interactive)
    ;; keep our M-i prefix command since this is rarely used term side
    (define-key vterm-mode-map  (kbd "M-i") nil)
    (vterm t)))

(provide 'tay-vterm)
