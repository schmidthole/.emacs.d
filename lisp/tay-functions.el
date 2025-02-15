;; custom functions
(defun tay/eshell-new ()
  "make a brand new eshell buffer in the current location."
  (interactive)
  (eshell 'N))

(defun tay/kill-this-buffer ()
  "kill the current buffer"
  (interactive)
  (kill-buffer (current-buffer)))

(defun tay/open-line-up (n)
  (interactive "p")
  (move-beginning-of-line 1)
  (open-line n))

(defun tay/open-line-down (n)
  (interactive "p")
  (move-end-of-line 1)
  (newline n))

(defun tay/kill-line-down (n)
  (interactive "p")
  (move-beginning-of-line 1)
  (kill-line n))

(provide 'tay-functions)
