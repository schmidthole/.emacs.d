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
 (use-package vterm
   :init
   (setq vterm-always-compile-module t))

 (use-package shrink-path))
