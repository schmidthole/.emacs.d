;;; window-resize-test.el --- window resize key tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'repeat)

(defconst tay/test-window-resize-init-file
  (expand-file-name "../init.el"
                    (file-name-directory (or load-file-name buffer-file-name))))

;; evaluate only the global key forms and window resize definitions from init.el.
(with-temp-buffer
  (insert-file-contents tay/test-window-resize-init-file)
  (goto-char (point-min))
  (condition-case nil
      (while t
        (let ((form (read (current-buffer))))
          (when (or (memq (car-safe form)
                          '(keymap-global-set defvar-keymap repeat-mode))
                    (and (eq (car-safe form) 'dolist)
                         (eq (car-safe (nth 2 form)) 'keymap-global-unset))
                    (and (eq (car-safe form) 'defun)
                         (string-prefix-p "tay/window-"
                                          (symbol-name (cadr form)))))
            (eval form t))))
    (end-of-file nil)))

(ert-deftest tay/window-resize-keys-bound ()
  (should (eq (keymap-global-lookup "M-i H") #'tay/window-narrower))
  (should (eq (keymap-global-lookup "M-i J") #'tay/window-shorter))
  (should (eq (keymap-global-lookup "M-i K") #'tay/window-taller))
  (should (eq (keymap-global-lookup "M-i L") #'tay/window-wider))
  (should (eq (keymap-global-lookup "M-i v") #'split-window-right))
  (should (eq (keymap-global-lookup "M-i s") #'split-window-below))
  (should (eq (keymap-global-lookup "M-o") #'other-window)))

(ert-deftest tay/window-resize-repeat-map ()
  (should repeat-mode)
  (dolist (command '(tay/window-narrower
                     tay/window-shorter
                     tay/window-taller
                     tay/window-wider))
    (should (eq (get command 'repeat-map) 'tay/window-resize-repeat-map)))
  (should (eq (keymap-lookup tay/window-resize-repeat-map "H")
              #'tay/window-narrower))
  (should (eq (keymap-lookup tay/window-resize-repeat-map "J")
              #'tay/window-shorter))
  (should (eq (keymap-lookup tay/window-resize-repeat-map "K")
              #'tay/window-taller))
  (should (eq (keymap-lookup tay/window-resize-repeat-map "L")
              #'tay/window-wider))
  (should-not (keymap-lookup tay/window-resize-repeat-map "h"))
  (should (get 'enlarge-window-horizontally 'repeat-map)))

(ert-deftest tay/window-resize-horizontal-steps ()
  (save-window-excursion
    (delete-other-windows)
    (split-window-right)
    (let ((width (window-total-width)))
      (tay/window-wider)
      (should (= (window-total-width) (+ width 5)))
      (tay/window-narrower)
      (tay/window-narrower)
      (should (= (window-total-width) (- width 5))))))

(ert-deftest tay/window-resize-vertical-steps ()
  (save-window-excursion
    (delete-other-windows)
    (split-window-below)
    (let ((height (window-total-height)))
      (tay/window-shorter)
      (should (= (window-total-height) (- height 5)))
      (tay/window-taller)
      (tay/window-taller)
      (should (= (window-total-height) (+ height 5))))))

(ert-deftest tay/window-resize-single-window-errors-once ()
  (save-window-excursion
    (delete-other-windows)
    (let ((width (window-total-width)))
      (should-error (tay/window-wider))
      (should (= (window-total-width) width)))))

;;; window-resize-test.el ends here
