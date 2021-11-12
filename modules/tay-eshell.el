(defun tay/eshell-new ()
  "Make a brand new eshell buffer in the current location."
  (interactive)
  (eshell 'N))

(add-hook 'eshell-mode-hook
	      (lambda ()
	        (eshell/alias "python" "python3 $*")
	        (eshell/alias "pip" "pip3 $*")
	        (eshell/alias "clear" "clear 1")))

(tay/do-if-ext-pkg
 (use-package shrink-path))
