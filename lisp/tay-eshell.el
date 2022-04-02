(defun tay/eshell-new ()
  "Make a brand new eshell buffer in the current location."
  (interactive)
  (eshell 'N))

(add-hook 'eshell-mode-hook
	      (lambda ()
            (eshell/alias "ll" "ls -la")
	        (eshell/alias "python" "python3 $*")
	        (eshell/alias "pip" "pip3 $*")
	        (eshell/alias "clear" "clear 1")))

(use-package shrink-path
  :defer)

(provide 'tay-eshell)
