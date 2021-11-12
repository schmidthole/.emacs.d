(tay/do-if-ext-pkg
 (use-package vterm
   :defer 0.6
   :init
   (setq vterm-always-compile-module t)
   :config
   (defun tay/vterm-new ()
     "Make a brand new vterm buffer"
     (interactive)
     ;; keep our M-i prefix command since this is rarely used term side
     (define-key vterm-mode-map  (kbd "M-i") nil)
     (vterm t)))
 (use-package vterm-toggle))
