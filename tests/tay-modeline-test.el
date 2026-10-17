;;; tay-modeline-test.el --- Mode line tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'tay-modeline)

(defmacro tay/test-modeline-selected (selected &rest body)
  "Run BODY with the window reported as SELECTED."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'mode-line-window-selected-p)
              (lambda () ,selected)))
     ,@body))

(ert-deftest tay/modeline-accent-only-when-selected ()
  (tay/test-modeline-selected t
    (should (eq (get-text-property 0 'face (tay/modeline-accent))
                'tay/modeline-accent)))
  (tay/test-modeline-selected nil
    (should-not (get-text-property 0 'face (tay/modeline-accent)))))

(ert-deftest tay/modeline-state-markers ()
  (tay/test-modeline-selected t
    (with-temp-buffer
      (should (equal (substring-no-properties (tay/modeline-state)) "  "))
      (insert "x")
      (should (equal (substring-no-properties (tay/modeline-state)) " ●"))
      (setq buffer-read-only t)
      (should (equal (substring-no-properties (tay/modeline-state)) " RO"))))
  (tay/test-modeline-selected nil
    (with-temp-buffer
      (insert "x")
      (should-not (get-text-property 0 'face (tay/modeline-state))))))

(ert-deftest tay/modeline-vc-strips-backend-prefix ()
  (tay/test-modeline-selected nil
    (with-temp-buffer
      (setq vc-mode " Git:main")
      (should (equal (tay/modeline-vc) "  main"))
      (setq vc-mode " Git-feature/x")
      (should (equal (tay/modeline-vc) "  feature/x"))
      (setq vc-mode nil)
      (should-not (tay/modeline-vc)))))

(ert-deftest tay/modeline-inactive-segments-have-no-faces ()
  (tay/test-modeline-selected nil
    (with-temp-buffer
      (setq vc-mode " Git:main")
      (dolist (segment '(tay/modeline-buffer tay/modeline-position
                         tay/modeline-vc tay/modeline-mode))
        (let ((text (funcall segment)))
          (should-not (text-property-not-all 0 (length text) 'face nil
                                             text)))))))

(ert-deftest tay/modeline-setup-is-idempotent ()
  (let ((mode-line-format nil)
        (enable-theme-functions nil))
    (cl-letf (((symbol-function 'tay/modeline-apply-faces) #'ignore))
      (tay/modeline-setup)
      (tay/modeline-setup)
      (should (equal enable-theme-functions '(tay/modeline-apply-faces)))
      (should (equal (default-value 'mode-line-format) tay/modeline-format)))))
