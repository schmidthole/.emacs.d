;;; eml.el --- terminal launcher views -*- lexical-binding: t; -*-

;;; commentary:
;; dispatch launcher requests in the requesting client frame.

;;; code:

(declare-function magit-status "magit-status" (&optional directory cache))
(declare-function folio-md-open "folio-md" (file))
(declare-function folio-md--goto-source-line "folio-md" (line))

(defvar folio-md-pandoc-program)

(defconst eml--directory
  (file-name-directory (or load-file-name buffer-file-name)))

(defun eml-open (action file &optional line column pandoc)
  "open file using action at one-based line and column.
use the launcher's pandoc when the server cannot find the default executable."
  (pcase action
    ("magit"
     (require 'magit)
     (magit-status file))
    ((or "open" "folio")
     (switch-to-buffer (find-file-noselect file))
     (widen)
     (goto-char (point-min))
     (forward-line (1- (or line 1)))
     (move-to-column (1- (or column 1)))
     (when (equal action "folio")
       (require 'folio-md (expand-file-name "folio-md.el" eml--directory))
       (when (and pandoc
                  (equal folio-md-pandoc-program "pandoc")
                  (not (executable-find folio-md-pandoc-program)))
         (setq folio-md-pandoc-program pandoc))
       (folio-md-open file)
       (folio-md--goto-source-line (or line 1))))
    (_ (user-error "unknown launcher action")))
  (delete-other-windows)
  nil)

(provide 'eml)
;;; eml.el ends here
