;;; folio-md.el --- A Markdown reading surface for terminal Emacs -*- lexical-binding: t; -*-

;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: text, convenience
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; M-x folio-md opens the current Markdown buffer in a separate, read-only
;; reading buffer.  Requires Pandoc 3.x on PATH.  No browser or markdown-mode
;; dependency.  Uses commonmark_x+sourcepos, with no filters or code execution.
;; e jumps to source, o follows a link, TAB folds a section, i selects a heading,
;; g refreshes and q returns.  M-x folio-md-open opens a file directly.

;;; Code:
(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'browse-url)
(require 'url-util)

(defgroup folio-md nil "Read Markdown in a terminal." :group 'text)
(defcustom folio-md-pandoc-program "pandoc" "Pandoc executable."
  :type 'string :group 'folio-md)
(defcustom folio-md-width 88 "Maximum reading width in terminal columns."
  :type 'integer :group 'folio-md)
(defcustom folio-md-refresh-interval 2
  "Seconds between checks for source changes.  Nil disables polling."
  :type '(choice (const nil) number) :group 'folio-md)
(defface folio-md-title '((t (:inherit font-lock-function-name-face :weight bold)))
  "Document title." :group 'folio-md)
(defface folio-md-heading '((t (:inherit font-lock-keyword-face :weight bold)))
  "Section heading." :group 'folio-md)
(defface folio-md-muted '((t (:inherit shadow))) "Secondary text." :group 'folio-md)
(defface folio-md-code '((t (:inherit fixed-pitch))) "Inline code." :group 'folio-md)
(defface folio-md-code-block
  '((((class color) (background dark)) (:background "#202530"))
    (((class color) (background light)) (:background "#eeeeee"))
    (t (:inherit fixed-pitch))) "Code block background." :group 'folio-md)

(defvar-local folio-md--source nil)
(defvar-local folio-md--ast nil)
(defvar-local folio-md--tick nil)
(defvar-local folio-md--timer nil)
(defvar-local folio-md--width nil)
(defvar folio-md--line 1)
(defvar folio-md--render-width 88)

(defun folio-md--tag (node) (alist-get 't node))
(defun folio-md--content (node) (alist-get 'c node))
(defun folio-md--attr-line (attr)
  "Return earliest source line from Pandoc ATTR, if available."
  (let ((pos (cadr (assoc "data-pos" (nth 2 attr)))) (start 0) lines)
    (when pos
      (while (string-match "\\(?:\\`\\|;\\)\\([0-9]+\\):" pos start)
        (push (string-to-number (match-string 1 pos)) lines)
        (setq start (match-end 0))))
    (if lines (apply #'min lines) folio-md--line)))

(defun folio-md--styled (text face)
  (let ((s (copy-sequence text)))
    (add-face-text-property 0 (length s) face t s) s))

(defun folio-md--inlines (nodes)
  "Render inline NODES to a propertized string."
  (mapconcat
   (lambda (node)
     (let* ((tag (folio-md--tag node)) (c (folio-md--content node))
            (line folio-md--line)
            (s
             (pcase tag
               ("Str" c) ("Space" " ") ("SoftBreak" " ") ("LineBreak" "\n")
               ("Strong" (folio-md--styled (folio-md--inlines c) 'bold))
               ("Emph" (folio-md--styled (folio-md--inlines c) 'italic))
               ("Strikeout" (folio-md--styled (folio-md--inlines c) '(:strike-through t)))
               ("Code" (setq line (folio-md--attr-line (car c)))
                (folio-md--styled (cadr c) 'folio-md-code))
               ("Span" (let ((folio-md--line (folio-md--attr-line (car c))))
                         (setq line folio-md--line)
                         (folio-md--inlines (cadr c))))
               ((or "Link" "Image")
                (let* ((label (folio-md--inlines (cadr c)))
                       (url (car (nth 2 c))))
                  (propertize (if (equal tag "Image")
                                  (concat "[Image: " label "]") label)
                              'face 'link 'folio-md-url url
                              'mouse-face 'highlight 'help-echo url)))
               ("Math" (folio-md--styled (cadr c) 'folio-md-code))
               ("Quoted" (concat "“" (folio-md--inlines (cadr c)) "”"))
               ((or "SmallCaps" "Superscript" "Subscript") (folio-md--inlines c))
               ("RawInline" (folio-md--styled (cadr c) 'folio-md-muted))
               ("Note" (concat " [Note: " (folio-md--plain-blocks c) "]"))
               (_ (format "[%s]" tag)))))
       ;; Keep more precise nested source locations.
       (let ((p 0))
         (while (< p (length s))
           (unless (get-text-property p 'folio-md-source-line s)
             (put-text-property p (1+ p) 'folio-md-source-line line s))
           (setq p (1+ p))))
       s)) nodes ""))

(defun folio-md--plain-blocks (blocks)
  (mapconcat
   (lambda (node)
     (let ((c (folio-md--content node)))
       (pcase (folio-md--tag node)
         ((or "Plain" "Para") (folio-md--inlines c))
         ("Div" (folio-md--plain-blocks (cadr c)))
         ("CodeBlock" (cadr c))
         (_ (format "[%s]" (folio-md--tag node)))))) blocks " "))

(defun folio-md--wrap (text width)
  "Wrap TEXT to WIDTH columns, retaining faces, links and source properties."
  (with-temp-buffer
    (insert text)
    (let ((fill-column (max 8 width)) (sentence-end-double-space nil)
          (use-hard-newlines t))
      ;; Preserve explicit Markdown hard breaks.
      (goto-char (point-min))
      (while (search-forward "\n" nil t)
        (put-text-property (1- (point)) (point) 'hard t))
      (fill-region (point-min) (point-max)))
    (split-string (buffer-string) "\n" nil)))

(defun folio-md--insert-line (text &optional prefix)
  (insert (propertize (or prefix "") 'folio-md-source-line folio-md--line)
          text (propertize "\n" 'folio-md-source-line folio-md--line)))

(defun folio-md--paragraph (text prefix &optional compact)
  (dolist (line (folio-md--wrap text (- folio-md--render-width (string-width prefix))))
    (folio-md--insert-line line prefix))
  (unless compact (insert "\n")))

(defun folio-md--highlight-code (code language)
  "Fontify CODE in a known built-in mode; never execute it."
  (let ((mode (cdr (assoc language
                          '(("python" . python-mode) ("py" . python-mode)
                            ("elisp" . emacs-lisp-mode) ("emacs-lisp" . emacs-lisp-mode)
                            ("javascript" . js-mode) ("js" . js-mode)
                            ("json" . js-mode) ("c" . c-mode) ("cpp" . c++-mode)
                            ("css" . css-mode) ("html" . html-mode)
                            ("sh" . sh-mode) ("bash" . sh-mode))))))
    (with-temp-buffer
      (insert code)
      (when (and mode (fboundp mode))
        (delay-mode-hooks (funcall mode))
        (font-lock-ensure))
      (let ((s (buffer-string)))
        (add-face-text-property 0 (length s) 'folio-md-code-block t s)
        s))))

(defun folio-md--code-block (c prefix)
  (let* ((attr (car c)) (language (or (car (cadr attr)) "text"))
         (folio-md--line (folio-md--attr-line attr))
         (code (cadr c)) (start (point)))
    (folio-md--insert-line (folio-md--styled (concat "╭─ " language) 'folio-md-muted) prefix)
    (dolist (line (split-string (folio-md--highlight-code code language) "\n" nil))
      (folio-md--insert-line
       (concat (folio-md--styled "│ " 'folio-md-muted) line) prefix))
    (folio-md--insert-line (folio-md--styled "╰─" 'folio-md-muted) prefix)
    (add-text-properties start (point)
                         `(folio-md-code ,code folio-md-source-line ,folio-md--line))
    (insert "\n")))

(defun folio-md--table (c prefix)
  "Render a Pandoc 1.23 table with wrapped cells and terminal-width padding."
  (let* ((head (cadr (nth 3 c)))
         (rows (append head
                       (cl-mapcan (lambda (b) (append (nth 2 b) (nth 3 b))) (nth 4 c))
                       (cadr (nth 5 c))))
         (specs (nth 2 c)) (n (length specs))
         (available (- folio-md--render-width (string-width prefix) (* 3 n) 1))
         (widths (make-vector n 8))
         (cells (mapcar (lambda (r)
                          (mapcar (lambda (cell) (folio-md--plain-blocks (nth 4 cell)))
                                  (cadr r))) rows)))
    (when (> n 0)
      ;; Grow columns to content, then shrink the largest until the table fits.
      (dotimes (i n)
        (aset widths i (max 8 (min 40 (apply #'max 0
                        (mapcar (lambda (r) (string-width (or (nth i r) ""))) cells))))))
      (while (and (> (apply #'+ (append widths nil)) available)
                  (> (apply #'max (append widths nil)) 8))
        (let* ((largest (apply #'max (append widths nil)))
               (i (cl-position largest widths)))
          (aset widths i (1- largest))))
      (cl-labels
          ((rule (left mid right)
             (folio-md--insert-line
              (folio-md--styled
               (concat left (mapconcat (lambda (w) (make-string (+ w 2) ?─))
                                       (append widths nil) mid) right) 'folio-md-muted) prefix)))
        (rule "┌" "┬" "┐")
        (cl-loop for row in cells for rownum from 0 do
                 (let* ((wrapped (cl-loop for i below n collect
                                         (folio-md--wrap (or (nth i row) "") (aref widths i))))
                        (height (apply #'max (mapcar #'length wrapped))))
                   (dotimes (j height)
                     (let ((parts
                            (cl-loop for i below n collect
                                     (let* ((s (or (nth j (nth i wrapped)) ""))
                                            (pad (max 0 (- (aref widths i) (string-width s))))
                                            (align (folio-md--tag (car (nth i specs)))))
                                       (when (< rownum (length head))
                                         (setq s (folio-md--styled s 'bold)))
                                       (concat " "
                                               (pcase align
                                                 ("AlignRight" (concat (make-string pad ?\s) s))
                                                 ("AlignCenter" (concat (make-string (/ pad 2) ?\s) s
                                                                        (make-string (- pad (/ pad 2)) ?\s)))
                                                 (_ (concat s (make-string pad ?\s)))) " ")))))
                       (folio-md--insert-line (concat "│" (mapconcat #'identity parts "│") "│") prefix)))
                   (when (and head (= rownum (1- (length head)))) (rule "├" "┼" "┤"))))
        (rule "└" "┴" "┘"))
      (insert "\n"))))

(defun folio-md--blocks (blocks &optional prefix)
  (setq prefix (or prefix ""))
  (dolist (node blocks)
    (let ((tag (folio-md--tag node)) (c (folio-md--content node))
          (start (point)))
      (pcase tag
        ("Div" (let ((folio-md--line (folio-md--attr-line (car c))))
                 (folio-md--blocks (cadr c) prefix)))
        ((or "Plain" "Para") (folio-md--paragraph (folio-md--inlines c) prefix (equal tag "Plain")))
        ("Header"
         (let* ((level (car c)) (attr (cadr c))
                (folio-md--line (folio-md--attr-line attr)) (start (point))
                (label (folio-md--inlines (nth 2 c))))
           (folio-md--insert-line
            (folio-md--styled (concat (if (= level 1) "" (make-string (1- level) ?›))
                                     (if (= level 1) "" " ") label)
                              (if (= level 1) 'folio-md-title 'folio-md-heading)) prefix)
           (when (= level 1)
             (folio-md--insert-line (folio-md--styled
                                     (make-string (min folio-md--render-width (string-width label)) ?━)
                                     'folio-md-muted) prefix))
           (add-text-properties start (point)
                                `(folio-md-heading ,level folio-md-anchor ,(car attr)
                                  folio-md-label ,(substring-no-properties label)
                                  folio-md-source-line ,folio-md--line))
           (insert "\n")))
        ("CodeBlock" (folio-md--code-block c prefix))
        ("BlockQuote" (folio-md--blocks c (concat prefix (folio-md--styled "│ " 'folio-md-muted))))
        ((or "BulletList" "OrderedList")
         (let ((items (if (equal tag "BulletList") c (cadr c)))
               (number (if (equal tag "OrderedList") (caar c) 1)))
           (dolist (item items)
             (let* ((bullet (if (equal tag "BulletList") "• " (format "%d. " number)))
                    (start (point))
                    (indent (concat prefix (make-string (string-width bullet) ?\s))))
               (folio-md--blocks item indent)
               (save-excursion
                 (goto-char (+ start (length prefix)))
                 (delete-char (length bullet)) (insert bullet)
                 (when (looking-at "\\[\\([xX ]\\)\\] ")
                   (let* ((checked (not (equal (match-string 1) " ")))
                          (line (get-text-property (point) 'folio-md-source-line)))
                     (replace-match (propertize (if checked "✓ " "○ ")
                                                'folio-md-source-line line) t t)))))
             (setq number (1+ number)))
           (insert "\n")))
        ("Table" (let ((folio-md--line (folio-md--attr-line (car c))))
                   (folio-md--table c prefix)))
        ("HorizontalRule" (folio-md--insert-line
                            (folio-md--styled (make-string (max 1 (- folio-md--render-width
                                                         (string-width prefix))) ?─) 'folio-md-muted) prefix)
         (insert "\n"))
        ("DefinitionList"
         (dolist (entry c)
           (folio-md--paragraph (folio-md--styled (folio-md--inlines (car entry)) 'bold) prefix)
           (dolist (definition (cadr entry)) (folio-md--blocks definition (concat prefix "  ")))))
        ("RawBlock" (folio-md--paragraph (folio-md--styled (cadr c) 'folio-md-muted) prefix))
        ("Null" nil)
        (_ (folio-md--paragraph (format "[Unsupported Markdown block: %s — press e for source]" tag) prefix)))
      (let ((p start))
        (while (< p (point))
          (let ((end (next-single-property-change p 'folio-md-source-line nil (point))))
            (unless (get-text-property p 'folio-md-source-line)
              (put-text-property p end 'folio-md-source-line folio-md--line))
            (setq p end)))))))

(defun folio-md--parse (source)
  (unless (executable-find folio-md-pandoc-program)
    (user-error "Install Pandoc 3.x or set folio-md-pandoc-program"))
  (with-temp-buffer
    (insert source)
    (let ((status (call-process-region (point-min) (point-max)
                                      folio-md-pandoc-program t t nil
                                      "--from=commonmark_x+sourcepos" "--to=json")))
      (unless (eq status 0) (error "Pandoc failed: %s" (buffer-string)))
      (goto-char (point-min))
      (alist-get 'blocks (json-parse-buffer :object-type 'alist :array-type 'list
                                            :null-object nil :false-object nil)))))

(defun folio-md--target-width ()
  (let ((win (get-buffer-window (current-buffer) t)))
    (max 20 (min folio-md-width (if win (- (window-body-width win) 4) folio-md-width)))))

(defun folio-md--goto-source-line (line)
  (goto-char (point-min))
  (let ((p (point-min)) (best (point-min)) (distance most-positive-fixnum))
    (while (< p (point-max))
      (let ((n (get-text-property p 'folio-md-source-line)))
        (when (and n (< (abs (- n line)) distance))
          (setq best p distance (abs (- n line)))))
      (setq p (next-single-property-change p 'folio-md-source-line nil (point-max))))
    (goto-char best)))

(defun folio-md--render ()
  (let* ((line (or (get-text-property (point) 'folio-md-source-line) 1))
         (ast folio-md--ast)
         (folio-md--render-width (folio-md--target-width))
         (folio-md--line 1)
         ;; Construct before replacing so a rendering error retains the old view.
         (rendered (with-temp-buffer
                     (folio-md--blocks ast) (buffer-string))))
    (let ((inhibit-read-only t))
      (remove-overlays) (erase-buffer) (insert rendered)
      (setq folio-md--width folio-md--render-width)
      (folio-md--goto-source-line line)
      (set-buffer-modified-p nil))))

(defun folio-md-refresh ()
  "Read source and render again.  Unsaved edits take precedence over disk."
  (interactive)
  (unless (buffer-live-p folio-md--source) (user-error "Source buffer was closed"))
  (let* ((source folio-md--source)
         (text (with-current-buffer source
                 (save-restriction
                   (widen)
                   (when (and buffer-file-name (not (buffer-modified-p))
                              (not (verify-visited-file-modtime source)))
                     (revert-buffer t t t))
                   (buffer-substring-no-properties (point-min) (point-max)))))
         (ast (folio-md--parse text)))
    (setq folio-md--ast ast)
    (folio-md--render)
    (setq folio-md--tick (with-current-buffer source (buffer-chars-modified-tick)))))

(defun folio-md--poll (buffer)
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (and (get-buffer-window buffer t) (buffer-live-p folio-md--source))
        (condition-case err
            (cond
             ((with-current-buffer folio-md--source
                (or (not (equal (buffer-chars-modified-tick)
                               (buffer-local-value 'folio-md--tick buffer)))
                    (and buffer-file-name (not (buffer-modified-p))
                         (not (verify-visited-file-modtime (current-buffer))))))
              (folio-md-refresh))
             ((not (equal folio-md--width (folio-md--target-width))) (folio-md--render)))
          (error (message "Folio refresh: %s" (error-message-string err))))))))

(defun folio-md-source ()
  "Visit the source line represented at point."
  (interactive)
  (let ((line (or (get-text-property (point) 'folio-md-source-line) 1))
        (source folio-md--source))
    (unless (buffer-live-p source) (user-error "Source buffer was closed"))
    (pop-to-buffer source)
    (widen) (goto-char (point-min)) (forward-line (1- line))))

(defun folio-md--headings ()
  (let ((p (point-min)) result)
    (while (< p (point-max))
      (when (get-text-property p 'folio-md-heading)
        (push (cons (format "%s  · line %s" (get-text-property p 'folio-md-label)
                            (get-text-property p 'folio-md-source-line)) p) result))
      (setq p (next-single-property-change p 'folio-md-heading nil (point-max))))
    (nreverse result)))

(defun folio-md-heading ()
  "Select a document heading with completion."
  (interactive)
  (let* ((items (folio-md--headings))
         (choice (completing-read "Heading: " items nil t)))
    (when-let ((pos (cdr (assoc choice items))))
      (remove-overlays (point-min) (point-max) 'folio-md-fold t)
      (goto-char pos))))

(defun folio-md-toggle-section ()
  "Fold or unfold the section containing point."
  (interactive)
  (let* ((positions (mapcar #'cdr (folio-md--headings)))
         (start (car (last (cl-remove-if (lambda (p) (> p (point))) positions)))))
    (unless start (user-error "No heading before point"))
    (let* ((level (get-text-property start 'folio-md-heading))
           (body (save-excursion (goto-char start) (forward-line 1) (point)))
           (end (or (cl-find-if (lambda (p) (and (> p start)
                                  (<= (get-text-property p 'folio-md-heading) level))) positions)
                    (point-max)))
           (existing (cl-find-if (lambda (o) (and (overlay-get o 'folio-md-fold)
                                                   (= (overlay-start o) body)))
                                 (overlays-in body end))))
      (if existing (delete-overlay existing)
        (let ((o (make-overlay body (max body (1- end)))))
          (overlay-put o 'folio-md-fold t)
          (overlay-put o 'invisible 'folio-md)
          (overlay-put o 'isearch-open-invisible #'delete-overlay)))
      (goto-char start))))

(defun folio-md-follow-link ()
  "Follow the link at point; local Markdown opens in Folio."
  (interactive)
  (let ((url (get-text-property (point) 'folio-md-url)))
    (unless url (user-error "No link at point"))
    (cond
     ((string-prefix-p "#" url)
      (let ((p (cl-find-if (lambda (pos) (equal (get-text-property pos 'folio-md-anchor)
                                                (substring url 1)))
                           (mapcar #'cdr (folio-md--headings)))))
        (if p (progn (remove-overlays (point-min) (point-max) 'folio-md-fold t) (goto-char p))
          (user-error "Anchor not found: %s" url))))
     ((string-match-p "\\`https?://" url) (browse-url url))
     ((string-match-p "\\`[[:alpha:]][[:alnum:]+.-]*:" url)
      (user-error "Unsupported link scheme: %s" url))
     (t (let* ((parts (split-string url "#"))
               (file (expand-file-name (url-unhex-string (car parts)) default-directory)))
          (if (string-match-p "\\.\\(?:md\\|markdown\\)\\'" file)
              (progn (folio-md-open file)
                     (when (cadr parts)
                       (let ((p (cl-find-if (lambda (pos)
                                            (equal (get-text-property pos 'folio-md-anchor) (cadr parts)))
                                          (mapcar #'cdr (folio-md--headings)))))
                         (when p (goto-char p)))))
            (find-file file)))))))

(defun folio-md-copy-code ()
  "Copy the original fenced code block at point without visual borders."
  (interactive)
  (let ((code (get-text-property (point) 'folio-md-code)))
    (unless code (user-error "No code block at point"))
    (kill-new code) (message "Code copied")))

(defun folio-md--cleanup ()
  (when (timerp folio-md--timer) (cancel-timer folio-md--timer)))

(defvar folio-md-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "e") #'folio-md-source)
    (define-key map (kbd "o") #'folio-md-follow-link)
    (define-key map (kbd "RET") #'folio-md-follow-link)
    (define-key map (kbd "TAB") #'folio-md-toggle-section)
    (define-key map (kbd "i") #'folio-md-heading)
    (define-key map (kbd "g") #'folio-md-refresh)
    (define-key map (kbd "c") #'folio-md-copy-code)
    map))

(define-derived-mode folio-md-mode special-mode "Folio"
  "Read Markdown with source navigation and terminal-friendly layout."
  (setq-local truncate-lines t)
  (setq-local header-line-format " Folio   e source · i headings · TAB fold · o link · c copy code · g refresh · q back")
  (setq-local buffer-invisibility-spec '((folio-md . t)))
  (setq-local show-trailing-whitespace nil)
  (add-hook 'kill-buffer-hook #'folio-md--cleanup nil t))

;;;###autoload
(defun folio-md ()
  "Read the current buffer as Markdown."
  (interactive)
  (let* ((source (if (derived-mode-p 'folio-md-mode) folio-md--source (current-buffer)))
         (buffer (get-buffer-create (format "*Folio: %s*" (buffer-name source)))))
    (pop-to-buffer buffer)
    (unless (derived-mode-p 'folio-md-mode) (folio-md-mode))
    (setq folio-md--source source
          default-directory (buffer-local-value 'default-directory source))
    (folio-md-refresh)
    (folio-md--cleanup)
    (when folio-md-refresh-interval
      (setq folio-md--timer (run-with-idle-timer folio-md-refresh-interval t
                                                #'folio-md--poll buffer)))))

;;;###autoload
(defun folio-md-open (file)
  "Open Markdown FILE in a Folio reading buffer."
  (interactive "fMarkdown file: ")
  (pop-to-buffer (find-file-noselect file))
  (folio-md))

(provide 'folio-md)
;;; folio-md.el ends here
