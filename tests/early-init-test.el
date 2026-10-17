;;; early-init-test.el --- frame appearance tests -*- lexical-binding: t; -*-

(require 'ert)

(defconst tay/test-early-init-file
  (expand-file-name "../early-init.el"
                    (file-name-directory (or load-file-name buffer-file-name))))

(ert-deftest tay/early-init-disables-frame-chrome ()
  (let ((default-frame-alist '((menu-bar-lines . 1)
                               (tool-bar-lines . 1)
                               (vertical-scroll-bars . right)))
        (menu-bar-mode t)
        (tool-bar-mode t)
        (scroll-bar-mode 'right))
    (load tay/test-early-init-file nil t)
    (should-not menu-bar-mode)
    (should (= (alist-get 'menu-bar-lines default-frame-alist) 0))
    (when (fboundp 'tool-bar-mode)
      (should-not tool-bar-mode)
      (should (= (alist-get 'tool-bar-lines default-frame-alist) 0)))
    (when (fboundp 'scroll-bar-mode)
      (should-not scroll-bar-mode)
      (should-not (alist-get 'vertical-scroll-bars default-frame-alist)))))

(ert-deftest tay/early-init-gui-font ()
  (let ((default-frame-alist '((width . 100))))
    (load tay/test-early-init-file nil t)
    (load tay/test-early-init-file nil t)
    (should (equal (alist-get 'font default-frame-alist) "JetBrains Mono-12"))
    (should (= (alist-get 'width default-frame-alist) 100))
    (should (= (length (seq-filter (lambda (entry) (eq (car entry) 'font))
                                   default-frame-alist))
               1))))

(ert-deftest tay/early-init-macos-titlebar ()
  (let ((system-type 'darwin)
        (default-frame-alist nil))
    (load tay/test-early-init-file nil t)
    (should (alist-get 'ns-transparent-titlebar default-frame-alist))
    (should (eq (alist-get 'ns-appearance default-frame-alist) 'dark))))

(ert-deftest tay/early-init-other-platforms ()
  (let ((system-type 'gnu/linux)
        (default-frame-alist nil))
    (load tay/test-early-init-file nil t)
    (should-not (assq 'ns-transparent-titlebar default-frame-alist))
    (should-not (assq 'ns-appearance default-frame-alist))))

;;; early-init-test.el ends here
