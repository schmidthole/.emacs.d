(setq org-startup-indented t)

(setq org-export-with-author nil)
(setq org-export-with-toc nil)
(setq org-export-with-footnotes nil)
(setq org-html-validation-link nil)
(setq org-export-html-postamble nil)
(setq org-export-time-stamp-file nil)

(setq org-M-RET-may-split-line nil)
(add-hook 'org-mode-hook (lambda () (electric-indent-local-mode -1)))

(setq org-todo-keywords
      '((sequence "TODO" "IN PROGRESS" "|" "DONE" "REVIEW")))
(setq org-todo-keyword-faces
      '(("IN PROGRESS" . org-warning) ("REVIEW" . org-warning)))

(setq org-agenda-files '("~/notes/agenda.org"))

(provide 'tay-org)
