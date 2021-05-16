;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ORG
;;
;; org mode settings
;;
;; these settings are pretty specific to my paths and uses, they should be
;; refactored
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'org)

;; opinionated org settings
;;
;; indented, hidden stars and markdown
(setq org-startup-indented t
      org-agenda-files '("~/orgs")
      org-todo-keywords '((sequence "TODO" "|" "DONE" "ONHOLD"))
      org-hide-emphasis-markers t
      org-hide-macro-markers t
      org-hide-leading-stars t)

(add-to-list 'org-modules 'org-habit t)

;; basic org capture templates
;;
;; - BASIC: adds a simple todo entry
;; - DEADLINE: todo with a deadline
;; - PERSONAL: adds a deadline todo to the personal list
(setq org-capture-templates
      '(("t" "Basic TODO" entry (file+headline "~/orgs/planner.org" "INBOX")
         "* TODO %? %i\n%u\n")
        ("d" "Deadline TODO" entry (file+headline "~/orgs/planner.org" "INBOX")
         "* TODO %? %i\nDEADLINE:%^t\n")
        ("p" "Personal TODO" entry (file+headline "~/orgs/personal.org" "INBOX")
         "* TODO %? %i\nDEADLINE:%^t\n")))
