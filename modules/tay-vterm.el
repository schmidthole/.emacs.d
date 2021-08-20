;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; VTERM
;;
;; terminal stuff. vterm
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(tay/do-if-ext-pkg
 ;; vterm is a fully usable shell, which we need a lot
 ;; it's a shame but most emacs shells fall over with one thing or another
 ;; this shell doesn't
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
     (vterm t))))
