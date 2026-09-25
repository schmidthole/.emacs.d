;;; tay-appearance.el --- Follow macOS appearance -*- lexical-binding: t; -*-

(require 'subr-x)

(defvar tay/appearance-timer nil
  "Timer that keeps terminal and daemon Emacs in sync with macOS.")

(defun tay/system-appearance ()
  "Return macOS appearance as `light' or `dark', or nil on a read error.
macOS removes AppleInterfaceStyle when light appearance is selected."
  (when (eq system-type 'darwin)
    (condition-case nil
        (with-temp-buffer
          (let ((status (call-process "/usr/bin/defaults" nil (list t nil) nil
                                      "read" "-g" "AppleInterfaceStyle")))
            (cond
             ((and (eq status 0)
                   (equal (string-trim (buffer-string)) "Dark"))
              'dark)
             ((memq status '(0 1)) 'light))))
      (file-error nil))))

(defun tay/apply-appearance (appearance)
  "Apply the Modus theme matching APPEARANCE without stacking themes."
  (when (memq appearance '(light dark))
    (let ((theme (if (eq appearance 'dark) 'modus-vivendi 'modus-operandi)))
      (unless (equal custom-enabled-themes (list theme))
        ;; Load before disabling the previous theme, so a load failure leaves
        ;; the existing appearance intact.
        (load-theme theme t)
        (dolist (enabled custom-enabled-themes)
          (unless (eq enabled theme)
            (disable-theme enabled)))))))

(defun tay/sync-appearance (&rest _)
  "Synchronize the Emacs theme with macOS."
  (tay/apply-appearance (tay/system-appearance)))

(defun tay/appearance-start ()
  "Start automatic Modus switching; safe to call again after reloading."
  (interactive)
  (when (timerp tay/appearance-timer)
    (cancel-timer tay/appearance-timer))
  (setq tay/appearance-timer nil)
  (if (eq system-type 'darwin)
      (progn
        (tay/sync-appearance)
        ;; The NS hook alone does not cover terminal-only/daemon sessions.
        (add-hook 'ns-system-appearance-change-functions #'tay/apply-appearance)
        (setq tay/appearance-timer
              (run-at-time 2 2 #'tay/sync-appearance)))
    (tay/apply-appearance 'dark)))

(provide 'tay-appearance)
;;; tay-appearance.el ends here
