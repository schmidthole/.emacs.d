;;; jumpa.el --- simple line jumping functionality -*- lexical-binding: t; -*-

(defface jumpa-overlay-face
  '((t (:background "yellow" :foreground "black" :weight bold)))
  "face used for jumpa overlays in the fringe.")

(defvar jumpa--overlays nil
  "list of active overlays.")

(defvar jumpa--homerow-chars '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
  "homerow characters for combinations.")

(defvar jumpa--line-map nil
  "map from two-char strings to line numbers.")

(defvar jumpa--first-char nil
  "first character entered by user.")

(defun jumpa--generate-combinations ()
  "generate homerow letter combinations."
  (let ((combinations '()))
    (dolist (first jumpa--homerow-chars)
      (dolist (second jumpa--homerow-chars)
        (push (format "%c%c" first second) combinations)))
    (reverse combinations)))

(defun jumpa--clear-overlays ()
  "remove all jumpa overlays."
  (mapc #'delete-overlay jumpa--overlays)
  (setq jumpa--overlays nil)
  (redisplay t))

(defun jumpa--reset-state ()
  "reset all jumpa state."
  (jumpa--clear-overlays)
  (setq jumpa--line-map nil
        jumpa--first-char nil))

(defun jumpa--create-overlay (line-number combo)
  "create overlay for line-number with combo text."
  (save-excursion
    (goto-char (point-min))
    (forward-line (1- line-number))
    (let* ((overlay (make-overlay (line-beginning-position) (line-beginning-position)))
           (display-string (propertize combo 'face 'jumpa-overlay-face)))
      (overlay-put overlay 'before-string display-string)
      (push overlay jumpa--overlays))))

(defun jumpa--show-lines ()
  "show overlays for visible lines."
  (let ((combinations (jumpa--generate-combinations))
        (line-start (line-number-at-pos (window-start)))
        (line-end (line-number-at-pos (window-end)))
        (combo-index 0))
    (setq jumpa--line-map (make-hash-table :test 'equal))
    (save-excursion
      (goto-char (window-start))
      (while (and (<= (line-number-at-pos) line-end) 
                  (< combo-index (length combinations))
                  (not (eobp)))
        (unless (string-match-p "^[[:space:]]*$" 
                               (buffer-substring-no-properties 
                                (line-beginning-position) 
                                (line-end-position)))
          (let ((combo (nth combo-index combinations))
                (line-num (line-number-at-pos)))
            (puthash combo line-num jumpa--line-map)
            (jumpa--create-overlay line-num combo)
            (setq combo-index (1+ combo-index))))
        (when (= (forward-line 1) 1)
          (goto-char (point-max)))))))

(defun jumpa--filter-by-first-char (char)
  "filter overlays to show only those starting with char."
  (let ((old-map jumpa--line-map)
        (filtered-map (make-hash-table :test 'equal)))
    (jumpa--clear-overlays)
    (maphash (lambda (combo line)
               (when (= (aref combo 0) char)
                 (let ((second-char (substring combo 1)))
                   (puthash second-char line filtered-map))))
             old-map)
    (setq jumpa--line-map filtered-map)
    (maphash (lambda (combo line)
               (jumpa--create-overlay line combo))
             jumpa--line-map)))

(defun jumpa--read-char-loop ()
  "read characters until jumpa is complete or cancelled."
  (let ((continue t))
    (while (and continue jumpa--overlays)
      (condition-case nil
          (let ((char (read-char "jumpa: ")))
            (cond
             ((not jumpa--first-char)
              (if (memq char jumpa--homerow-chars)
                  (progn
                    (setq jumpa--first-char char)
                    (jumpa--filter-by-first-char char))
                (jumpa--reset-state)
                (setq continue nil)
                (message "invalid character")))
             
             (t
              (let* ((combo (char-to-string char))
                     (line (gethash combo jumpa--line-map)))
                (jumpa--reset-state)
                (setq continue nil)
                (if line
                    (progn
                      (goto-char (point-min))
                      (forward-line (1- line)))
                  (message "invalid combination"))))))
        (quit
         (jumpa--reset-state)
         (setq continue nil)
         (message "jumpa cancelled"))))))

;;;###autoload
(defun jumpa ()
  "jump to any visible line using homerow combinations."
  (interactive)
  (jumpa--reset-state)
  (jumpa--show-lines)
  (jumpa--read-char-loop))

(provide 'jumpa)

;;; jumpa.el ends here