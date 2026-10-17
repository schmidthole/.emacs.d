;;; tay-modeline.el --- Doom-style mode line -*- lexical-binding: t; -*-

(require 'vc-hooks)

(declare-function modus-themes-get-color-value "modus-themes")

(defface tay/modeline-accent '((t :inherit mode-line))
  "Accent bar shown on the selected window's mode line."
  :group 'mode-line)

(defconst tay/modeline-padding 4
  "Vertical padding in pixels around mode line text in graphical frames.")

(defun tay/modeline-accent ()
  "Return the left accent bar, colored only in the selected window."
  (if (mode-line-window-selected-p)
      (propertize "  " 'face 'tay/modeline-accent)
    "  "))

(defun tay/modeline-state ()
  "Return a marker for a read-only or modified buffer."
  (let ((text (cond (buffer-read-only "RO")
                    ((buffer-modified-p) "●")
                    (t " "))))
    (concat " "
            (if (mode-line-window-selected-p)
                (propertize text 'face (if buffer-read-only 'error 'warning))
              text))))

(defun tay/modeline-buffer ()
  "Return the buffer name, bold in the selected window."
  (if (mode-line-window-selected-p)
      (propertize (concat " " (buffer-name)) 'face 'mode-line-buffer-id)
    (concat " " (buffer-name))))

(defun tay/modeline-position ()
  "Return line and column, dimmed in the selected window."
  (let ((text (format-mode-line "  %l:%c")))
    (if (mode-line-window-selected-p)
        (propertize text 'face 'shadow)
      text)))

(defun tay/modeline-vc ()
  "Return the VC branch without the backend prefix."
  (when (and vc-mode (stringp vc-mode))
    (let ((branch (replace-regexp-in-string "\\`\\s-*Git[-:]" "" vc-mode)))
      (concat "  "
              (if (mode-line-window-selected-p)
                  (propertize branch 'face 'success)
                branch)))))

(defun tay/modeline-mode ()
  "Return the major mode name, bold in the selected window."
  (let ((name (format-mode-line mode-name)))
    (concat "  "
            (if (mode-line-window-selected-p)
                (propertize name 'face 'bold)
              name))))

(defconst tay/modeline-format
  '("%e"
    (:eval (tay/modeline-accent))
    (:eval (tay/modeline-state))
    (:eval (tay/modeline-buffer))
    (:eval (tay/modeline-position))
    mode-line-format-right-align
    (project-mode-line project-mode-line-format)
    (:eval (tay/modeline-vc))
    (:eval (tay/modeline-mode))
    mode-line-process
    "  ")
  "Value of `mode-line-format'.")

(defun tay/modeline-apply-faces (&rest _)
  "Pad the mode line and color the accent bar from the current theme."
  (let ((accent (or (ignore-errors
                      (modus-themes-get-color-value 'bg-blue-intense))
                    (face-foreground 'link nil t))))
    (set-face-attribute 'tay/modeline-accent nil
                        :background accent :foreground accent
                        :box (list :line-width tay/modeline-padding
                                   :color accent)))
  (dolist (face '(mode-line-active mode-line-inactive))
    (let ((background (face-background face nil t)))
      (when (stringp background)
        (set-face-attribute face nil
                            :box (list :line-width tay/modeline-padding
                                       :color background))))))

(defun tay/modeline-setup ()
  "Install the mode line; safe to call again after reloading."
  (setq-default mode-line-format tay/modeline-format)
  (add-hook 'enable-theme-functions #'tay/modeline-apply-faces)
  (tay/modeline-apply-faces))

(provide 'tay-modeline)
;;; tay-modeline.el ends here
