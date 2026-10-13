;;; ghostel-test.el --- terminal window navigation tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'package)
(require 'use-package)

(defconst tay/test-ghostel-root
  (expand-file-name "../" (file-name-directory (or load-file-name buffer-file-name))))

(let ((package-user-dir (expand-file-name "elpa" tay/test-ghostel-root)))
  (package-initialize))
(require 'ghostel)

(with-temp-buffer
  (insert-file-contents (expand-file-name "init.el" tay/test-ghostel-root))
  (goto-char (point-min))
  (let ((use-package-ensure-function #'ignore))
    (condition-case nil
        (while t
          (let ((form (read (current-buffer))))
            (when (or (equal form '(keymap-global-set "M-o" #'other-window))
                      (and (eq (car-safe form) 'use-package)
                           (eq (cadr form) 'ghostel)))
              (eval form t))))
      (end-of-file nil))))

(ert-deftest tay/ghostel-keeps-terminal-keys ()
  (should (member "M-o" ghostel-keymap-exceptions))
  (should (eq (keymap-lookup ghostel-semi-char-mode-map "M-p")
              #'ghostel--send-event))
  (should (eq (keymap-lookup ghostel-char-mode-map "M-p")
              #'ghostel--send-event)))

(ert-deftest tay/ghostel-meta-o-switches-splits ()
  (save-window-excursion
    (delete-other-windows)
    (let ((next (split-window-right)))
      (with-temp-buffer
        (switch-to-buffer (current-buffer))
        (dolist (map (list ghostel-semi-char-mode-map
                           ghostel-char-mode-map
                           ghostel-readonly-mode-map
                           ghostel-readonly-fast-exit-mode-map
                           ghostel-line-mode-map))
          (let ((first (selected-window)))
            (use-local-map map)
            (should (eq (key-binding (kbd "M-o")) #'other-window))
            (call-interactively (key-binding (kbd "M-o")))
            (should (eq (selected-window) next))
            (select-window first)))))))

;;; ghostel-test.el ends here
