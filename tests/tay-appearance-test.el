;;; tay-appearance-test.el --- Appearance switching tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'tay-appearance)

(ert-deftest tay/appearance-reads-macos-light-and-dark ()
  (let ((system-type 'darwin))
    (dolist (case '((0 "Dark\n" dark) (1 "" light) (2 "" nil)))
      (cl-letf (((symbol-function 'call-process)
                 (lambda (&rest _) (insert (nth 1 case)) (car case))))
        (should (eq (tay/system-appearance) (nth 2 case)))))
    (cl-letf (((symbol-function 'call-process)
               (lambda (&rest _) (signal 'file-error '("unavailable")))))
      (should-not (tay/system-appearance)))))

(ert-deftest tay/appearance-switches-real-themes-without-stacking ()
  (unwind-protect
      (progn
        (tay/apply-appearance 'light)
        (should (equal custom-enabled-themes '(modus-operandi)))
        ;; Batch frames have no color display; inspect the loaded face spec.
        (should (equal (plist-get (cdaar (alist-get 'modus-operandi
                                                   (get 'default 'theme-face)))
                                  :background)
                       "#ffffff"))
        (tay/apply-appearance 'dark)
        (should (equal custom-enabled-themes '(modus-vivendi)))
        (should (equal (plist-get (cdaar (alist-get 'modus-vivendi
                                                   (get 'default 'theme-face)))
                                  :background)
                       "#000000"))
        (cl-letf (((symbol-function 'load-theme)
                   (lambda (&rest _) (ert-fail "Unchanged appearance reloaded"))))
          (tay/apply-appearance 'dark)
          (tay/apply-appearance nil))
        (tay/apply-appearance 'light)
        (should (equal custom-enabled-themes '(modus-operandi))))
    (mapc #'disable-theme custom-enabled-themes)))

(ert-deftest tay/appearance-restart-replaces-timer ()
  (let ((system-type 'darwin)
        (tay/appearance-timer nil)
        (ns-system-appearance-change-functions nil))
    (cl-letf (((symbol-function 'tay/sync-appearance) #'ignore))
      (unwind-protect
          (progn
            (tay/appearance-start)
            (let ((old tay/appearance-timer))
              (tay/appearance-start)
              (should-not (memq old timer-list))
              (should (memq tay/appearance-timer timer-list)))
            (should (equal ns-system-appearance-change-functions
                           '(tay/apply-appearance))))
        (cancel-timer tay/appearance-timer)))))
