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
   :init
   (setq vterm-always-compile-module t)
   :config
   (defun tay/vterm-new ()
     "Make a brand new vterm buffer"
     (interactive)
     (vterm t))))
