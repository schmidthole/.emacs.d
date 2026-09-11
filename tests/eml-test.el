;;; eml-test.el --- launcher tests -*- lexical-binding: t; -*-

;;; commentary:
;; verify editing positions, reader refresh, and repository dispatch.

;;; code:

(require 'ert)
(require 'cl-lib)
(let ((load-prefer-newer t))
  (require 'eml))

(ert-deftest eml-open-visits-file-at-position ()
  (let ((file (make-temp-file "eml-test-" nil ".txt" "first\nsecond\nthird\n")))
    (unwind-protect
        (save-window-excursion
          (eml-open "open" file 2 3)
          (should (equal buffer-file-name file))
          (should (= (line-number-at-pos) 2))
          (should (= (current-column) 2))
          (should (= (length (window-list)) 1)))
      (when-let* ((buffer (get-file-buffer file))) (kill-buffer buffer))
      (delete-file file))))

(ert-deftest eml-folio-opens-and-refreshes-without-losing-unsaved-edits ()
  (let ((file (make-temp-file "eml-test-" nil ".md" "# first\n\noriginal\n"))
        reader source)
    (unwind-protect
        (save-window-excursion
          (eml-open "folio" file 3)
          (setq reader (current-buffer)
                source folio-md--source)
          (should (derived-mode-p 'folio-md-mode))
          (should buffer-read-only)
          (should (string-match-p "original" (buffer-string)))
          (with-temp-file file (insert "# first\n\nupdated\n"))
          (folio-md-refresh)
          (should (string-match-p "updated" (buffer-string)))
          (with-current-buffer source
            (goto-char (point-max))
            (insert "\nunsaved feedback\n"))
          (with-temp-file file (insert "# changed again\n"))
          (folio-md-refresh)
          (should (string-match-p "unsaved feedback" (buffer-string))))
      (when (buffer-live-p reader) (kill-buffer reader))
      (when (buffer-live-p source)
        (with-current-buffer source (set-buffer-modified-p nil))
        (kill-buffer source))
      (delete-file file))))

(ert-deftest eml-magit-dispatches-exact-directory ()
  (let (seen)
    (cl-letf (((symbol-function 'require) (lambda (&rest _) t))
              ((symbol-function 'magit-status) (lambda (directory) (setq seen directory))))
      (save-window-excursion (eml-open "magit" "/tmp/specific-worktree/")))
    (should (equal seen "/tmp/specific-worktree/"))))

(ert-deftest eml-folio-uses-launcher-pandoc-with-minimal-server-path ()
  (require 'folio-md)
  (let ((pandoc (executable-find "pandoc"))
        (folio-md-pandoc-program "pandoc")
        (file (make-temp-file "eml-test-" nil ".md" "# proposal\n"))
        reader source)
    (should pandoc)
    (unwind-protect
        (save-window-excursion
          (let ((exec-path '("/usr/bin" "/bin"))
                (process-environment '("PATH=/usr/bin:/bin")))
            (eml-open "folio" file 1 1 pandoc)
            (setq reader (current-buffer)
                  source folio-md--source)
            (should (derived-mode-p 'folio-md-mode))
            (should (equal folio-md-pandoc-program pandoc))
            (with-temp-file file (insert "# updated proposal\n"))
            (folio-md-refresh)
            (should (string-match-p "updated proposal" (buffer-string)))))
      (when (buffer-live-p reader) (kill-buffer reader))
      (when (buffer-live-p source) (kill-buffer source))
      (delete-file file))))

(ert-deftest eml-folio-preserves-custom-pandoc ()
  (require 'folio-md)
  (let ((folio-md-pandoc-program "/custom/pandoc")
        (exec-path nil))
    (cl-letf (((symbol-function 'find-file-noselect) (lambda (_) (current-buffer)))
              ((symbol-function 'folio-md-open) (lambda (_) nil))
              ((symbol-function 'folio-md--goto-source-line) (lambda (_) nil)))
      (save-window-excursion
        (with-temp-buffer (eml-open "folio" "/tmp/file.md" 1 1 "/launcher/pandoc"))))
    (should (equal folio-md-pandoc-program "/custom/pandoc"))))

(ert-deftest eml-rejects-unknown-action ()
  (should-error (eml-open "unknown" "/tmp/file") :type 'user-error))

(provide 'eml-test)
;;; eml-test.el ends here
