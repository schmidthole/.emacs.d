;;; folio-md-test.el --- reader folding tests -*- lexical-binding: t; -*-

;;; commentary:
;; keep folded sections separated from the following heading.

;;; code:

(require 'ert)
(let ((load-prefer-newer t))
  (require 'folio-md))

(ert-deftest folio-md-folds-preserve-heading-line-breaks ()
  (dolist (markdown '("# first\n\nbody\n\n# second\n\nbody\n\n# third\n"
                      "## first\n\nbody\n\n## second\n\nbody\n\n## third\n"
                      "## first\n\n## second\n\n## third\n"))
    (with-temp-buffer
      (folio-md-mode)
      (setq folio-md--ast (folio-md--parse markdown))
      (folio-md--render)
      (let ((headings (mapcar #'cdr (folio-md--headings))))
        (dolist (heading headings)
          (goto-char heading)
          (folio-md-toggle-section)
          (dolist (next headings)
            (should-not (invisible-p next))
            (when (> next (point-min))
              (should (eq (char-before next) ?\n))
              (should-not (invisible-p (1- next))))))
        (dolist (heading headings)
          (goto-char heading)
          (folio-md-toggle-section))
        (should-not (overlays-in (point-min) (point-max)))))))

(ert-deftest folio-md-nested-fold-keeps-its-line-break-after-parent-unfolds ()
  (with-temp-buffer
    (folio-md-mode)
    (setq folio-md--ast
          (folio-md--parse "## parent\n\n### child\n\nbody\n\n### sibling\n\nbody\n\n## next\n"))
    (folio-md--render)
    (let* ((headings (mapcar #'cdr (folio-md--headings)))
           (parent (nth 0 headings))
           (child (nth 1 headings))
           (sibling (nth 2 headings))
           (next (nth 3 headings)))
      (goto-char child)
      (folio-md-toggle-section)
      (goto-char parent)
      (folio-md-toggle-section)
      (should (invisible-p child))
      (should-not (invisible-p (1- next)))
      (folio-md-toggle-section)
      (should-not (invisible-p child))
      (should-not (invisible-p sibling))
      (should-not (invisible-p (1- sibling)))
      (should (= (length (overlays-in (point-min) (point-max))) 1)))))

(provide 'folio-md-test)
;;; folio-md-test.el ends here
