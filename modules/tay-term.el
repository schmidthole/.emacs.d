;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TERM
;;
;; terminal stuff. eshell and vterm
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun tay/eshell-new ()
  "Make a brand new eshell buffer in the current location."
  (interactive)
  (eshell 'N))

;; aliases for eshell
(add-hook 'eshell-mode-hook
	      (lambda ()
	        (eshell/alias "python" "python3 $*")
	        (eshell/alias "pip" "pip3 $*")
	        (eshell/alias "clear" "clear 1")))

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

 ;; make the eshell prompt nice
 (use-package shrink-path))
