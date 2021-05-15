;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ORG
;;
;; org mode settings
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'org)

(setq org-startup-indented t
      org-agenda-files '("~/orgs")
      org-todo-keywords '((sequence "TODO" "|" "DONE" "ONHOLD"))
      org-hide-emphasis-markers t
      org-hide-macro-markers t
      org-hide-leading-stars t)

(add-to-list 'org-modules 'org-habit t)

(setq org-capture-templates
      '(("t" "Basic TODO" entry (file+headline "~/orgs/planner.org" "INBOX")
         "* TODO %? %i\n%u\n")
        ("d" "Deadline TODO" entry (file+headline "~/orgs/planner.org" "INBOX")
         "* TODO %? %i\nDEADLINE:%^t\n")
        ("p" "Personal TODO" entry (file+headline "~/orgs/personal.org" "INBOX")
         "* TODO %? %i\nDEADLINE:%^t\n")))
