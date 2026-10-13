;;; mac-launcher-test.el --- application launcher tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'mac-launcher)

(ert-deftest mac-launcher-discovers-bundles-without-entering-apps-or-links ()
  (let* ((root (make-temp-file "launcher-" t))
         (mac-launcher-directories (list root (concat root "/missing"))))
    (unwind-protect
        (progn
          (make-directory (concat root "/tools/editor.app/hidden.app") t)
          (make-directory (concat root "/browser.app"))
          (make-directory (concat root "/.hidden/secret.app") t)
          (make-symbolic-link root (concat root "/cycle"))
          (make-symbolic-link (concat root "/browser.app") (concat root "/alias.app"))
          (should (equal (mapcar #'car (mac-launcher--applications))
                         '("alias" "browser" "editor"))))
      (delete-directory root t))))

(ert-deftest mac-launcher-disambiguates-duplicate-names ()
  (let* ((root (make-temp-file "launcher-" t))
         (mac-launcher-directories (list root root)))
    (unwind-protect
        (progn
          (make-directory (concat root "/one/editor.app") t)
          (make-directory (concat root "/two/editor.app") t)
          (let ((applications (mac-launcher--applications)))
            (should (= (length applications) 2))
            (should-not (equal (caar applications) (caadr applications)))
            (should (equal (mapcar #'cdr applications)
                           (list (concat root "/one/editor.app")
                                 (concat root "/two/editor.app"))))))
      (delete-directory root t))))

(defun mac-launcher-test--popup (reader opener)
  "exercise popup lifecycle with reader and opener, without creating a window."
  (let ((system-type 'darwin)
        (native-comp-enable-subr-trampolines nil)
        deleted opened parameters)
    (cl-letf (((symbol-function 'active-minibuffer-window) (lambda () nil))
              ((symbol-function 'mac-launcher--applications)
               (lambda () '(("test" . "/applications/a \"quote\".app"))))
              ((symbol-function 'make-frame)
               (lambda (params) (setq parameters params) 'popup))
              ((symbol-function 'select-frame-set-input-focus) #'ignore)
              ((symbol-function 'frame-live-p) (lambda (frame) (eq frame 'popup)))
              ((symbol-function 'delete-frame)
               (lambda (frame &optional force) (setq deleted (list frame force))))
              ((symbol-function 'completing-read) reader)
              ((symbol-function 'call-process)
               (lambda (&rest args)
                 (should deleted)
                 (setq opened args)
                 (funcall opener))))
      (let ((result (unwind-protect (mac-launcher)
                      (should (equal deleted '(popup t))))))
        (should (equal deleted '(popup t)))
        (should (eq (alist-get 'window-system parameters) 'ns))
        (should (eq (alist-get 'minibuffer parameters) 'only))
        (list result opened)))))

(ert-deftest mac-launcher-opens-exact-path-after-cleanup ()
  (should (equal (mac-launcher-test--popup (lambda (&rest _) "test") (lambda () 0))
                 '(t ("/usr/bin/open" nil nil nil "/applications/a \"quote\".app")))))

(ert-deftest mac-launcher-cancellation-cleans-up-without-launching ()
  (should (equal (mac-launcher-test--popup (lambda (&rest _) (signal 'quit nil))
                                           (lambda () (ert-fail "unexpected launch")))
                 '(nil nil))))

(ert-deftest mac-launcher-completion-error-cleans-up ()
  (should-error
   (mac-launcher-test--popup (lambda (&rest _) (error "completion failed"))
                             (lambda () 0))
   :type 'error))

(ert-deftest mac-launcher-open-failure-is-reported ()
  (should-error (mac-launcher-test--popup (lambda (&rest _) "test") (lambda () 1))
                :type 'user-error))

(ert-deftest mac-launcher-refuses-an-active-minibuffer ()
  (let ((system-type 'darwin))
    (cl-letf (((symbol-function 'active-minibuffer-window) (lambda () t))
              ((symbol-function 'make-frame) (lambda (&rest _) (ert-fail "unexpected popup"))))
      (should-error (mac-launcher) :type 'user-error))))

;;; mac-launcher-test.el ends here
