(defcustom gtk-variant 'dark
  "Initial GTK theme variant. Valid values are dark and light."
  :type '(radio (const dark)
                (const light))
  :set (lambda (_ val)
         (setq gtk-variant val)
         (gtk-variant-set-frame nil val))
  :initialize #'custom-initialize-default)

;;;###autoload
(defun gtk-variant-set-frame (&optional frame variant)
  "Set the GTK theme variant of frame FRAME to VARIANT.
With no arguments, sets the selected frame to the variable `gtk-variant'
Recommended usage:
\(add-hook 'window-setup-hook #'gtk-variant-set-frame)
\(add-hook 'after-make-frame-functions #'gtk-variant-set-frame)
\(fn &optional FRAME VARIANT)"
  (interactive
   (list nil (intern (completing-read "GTK Variant: " '(dark light) nil t))))
  (when (display-graphic-p (or frame (selected-frame)))
    (let ((variant (or variant gtk-variant)))
      (unless (memq variant '(dark light)) (error "Invalid variant: %s" variant))
      (call-process-shell-command
       (format "xprop -f _GTK_THEME_VARIANT 8u -set _GTK_THEME_VARIANT \"%s\" -id \"%s\""
               (shell-quote-argument (symbol-name variant))
               (shell-quote-argument (frame-parameter frame 'outer-window-id)))
       nil 0))))

(add-hook 'window-setup-hook #'gtk-variant-set-frame)
(add-hook 'after-make-frame-functions #'gtk-variant-set-frame)

(provide 'gtk-variant)
