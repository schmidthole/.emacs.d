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

(ert-deftest folio-md-paragraphs-flow-without-fill ()
  (let ((words (mapconcat #'identity (make-list 60 "word") " ")))
    (with-temp-buffer
      (folio-md-mode)
      (should visual-line-mode)
      (setq folio-md--ast (folio-md--parse (concat words "\n\n- " words "\n")))
      (folio-md--render)
      (goto-char (point-min))
      (should (equal (buffer-substring (point) (line-end-position)) words))
      (search-forward "• ")
      (should (equal (buffer-substring (point) (line-end-position)) words))
      (should (equal (get-text-property (point) 'wrap-prefix) "  ")))))

;; carets and tildes carry meaning in exponents and must stay visible.
(defun folio-md-test--render (markdown)
  "return the rendered text of MARKDOWN."
  (with-temp-buffer
    (folio-md-mode)
    (setq folio-md--ast (folio-md--parse markdown))
    (folio-md--render)
    (buffer-substring-no-properties (point-min) (point-max))))

(ert-deftest folio-md-keeps-exponent-carets ()
  (let ((text (folio-md-test--render
               (concat "fewer than 2^32 and **2^30** and `2^-37` and 2^-33.\n\n"
                       "n² / 2^97 and n^2 and x^2^ and H~2~O and ~~old~~ and \\^\n\n"
                       "| bound | value |\n| --- | --- |\n| **2^32** | 2^-33 |\n"))))
    (dolist (expected '("fewer than 2^32 and 2^30 and 2^-37 and 2^-33."
                        "n² / 2^97 and n^2 and x^2^ and H~2~O and old and ^"
                        "│ 2^32 " "│ 2^-33 "))
      (should (string-search expected text)))
    (should-not (string-search "232" text))))

(ert-deftest folio-md-marked-inlines-stay-visible ()
  (let ((folio-md--line 1)
        (str (lambda (text) `((t . "Str") (c . ,text)))))
    (should (equal (folio-md--inlines
                    `(,(funcall str "2")
                      ((t . "Superscript") (c ,(funcall str "32")))))
                   "2^32^"))
    (should (equal (folio-md--inlines
                    `(,(funcall str "H")
                      ((t . "Subscript") (c ,(funcall str "2")))
                      ,(funcall str "O")))
                   "H~2~O"))
    (should (equal (folio-md--inlines
                    `(((t . "SmallCaps") (c ,(funcall str "nist")))))
                   "NIST"))))

;; faces and navigation keys follow info.
(ert-deftest folio-md-faces-inherit-info ()
  (dolist (pair '((folio-md-title . info-title-1) (folio-md-heading . info-title-2)
                  (folio-md-heading-3 . info-title-3) (folio-md-heading-4 . info-title-4)
                  (folio-md-link . info-xref) (folio-md-code . Info-quoted)))
    (should (eq (face-attribute (car pair) :inherit) (cdr pair))))
  (with-temp-buffer
    (folio-md-mode)
    (setq folio-md--ast (folio-md--parse "# t\n\n## a\n\n### b\n\n#### c\n\n[x](#a)\n"))
    (folio-md--render)
    (cl-loop for pos in (mapcar #'cdr (folio-md--headings))
             for face in '(folio-md-title folio-md-heading folio-md-heading-3 folio-md-heading-4)
             do (should (memq face (ensure-list (get-text-property pos 'face)))))
    (goto-char (point-max))
    (search-backward "x")
    (should (memq 'folio-md-link (ensure-list (get-text-property (point) 'face))))))

(ert-deftest folio-md-info-style-heading-motion ()
  (with-temp-buffer
    (folio-md-mode)
    (setq folio-md--ast
          (folio-md--parse "## one\n\nbody\n\n### child\n\nbody\n\n## two\n\nbody\n"))
    (folio-md--render)
    (let ((headings (mapcar #'cdr (folio-md--headings))))
      (goto-char (point-min))
      (should (eq (lookup-key folio-md-mode-map "n") #'folio-md-next-heading))
      (should (eq (lookup-key folio-md-mode-map "u") #'folio-md-up-heading))
      (should (eq (lookup-key folio-md-mode-map "l") #'folio-md-back))
      (folio-md-next-heading)
      (should (= (point) (nth 1 headings)))
      (forward-line 2)
      (folio-md-up-heading)
      (should (= (point) (nth 0 headings)))
      (should-error (folio-md-up-heading) :type 'user-error)
      (should-error (folio-md-previous-heading) :type 'user-error)
      (folio-md-next-heading 2)
      (should (= (point) (nth 2 headings)))
      (should-error (folio-md-next-heading) :type 'user-error)
      ;; hidden headings inside a fold are skipped.
      (goto-char (nth 0 headings))
      (folio-md-toggle-section)
      (folio-md-next-heading)
      (should (= (point) (nth 2 headings))))))

(ert-deftest folio-md-back-returns-from-anchor-links ()
  (with-temp-buffer
    (folio-md-mode)
    (setq folio-md--ast (folio-md--parse "see [later](#later)\n\n## later\n\nbody\n"))
    (folio-md--render)
    (should-error (folio-md-back) :type 'user-error)
    (goto-char (point-min))
    (search-forward "lat")
    (let ((origin (point)))
      (folio-md-follow-link)
      (should (get-text-property (point) 'folio-md-heading))
      (folio-md-back)
      (should (= (line-beginning-position)
                 (save-excursion (goto-char origin) (line-beginning-position))))
      (should-error (folio-md-back) :type 'user-error))))

;; folio replaces the selected window instead of splitting.
(ert-deftest folio-md-commands-reuse-the-selected-window ()
  (let* ((dir (make-temp-file "folio-md-test" t))
         (file (expand-file-name "doc.md" dir))
         (other (expand-file-name "other.md" dir)))
    (unwind-protect
        (save-window-excursion
          (with-temp-file file (insert "# doc\n\nsee [other](other.md)\n"))
          (with-temp-file other (insert "# other\n"))
          (delete-other-windows)
          (folio-md-open file)
          (should (one-window-p))
          (should (derived-mode-p 'folio-md-mode))
          (search-forward "oth")
          (folio-md-follow-link)
          (should (one-window-p))
          (should (equal (buffer-name) "*Folio: other.md*"))
          (folio-md-back)
          (should (one-window-p))
          (should (equal (buffer-name) "*Folio: doc.md*"))
          (folio-md-source)
          (should (one-window-p))
          (should (equal buffer-file-name file)))
      (dolist (b (buffer-list))
        (when (string-prefix-p (file-name-as-directory dir)
                               (or (buffer-local-value 'default-directory b) ""))
          (kill-buffer b)))
      (delete-directory dir t))))

(provide 'folio-md-test)
;;; folio-md-test.el ends here
