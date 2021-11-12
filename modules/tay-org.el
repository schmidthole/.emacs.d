(require 'org)

(setq org-startup-indented t
      org-agenda-files '("~/orgs")
      org-todo-keywords '((sequence "TODO" "|" "DONE" "ONHOLD"))
      org-hide-leading-stars t
      org-fontify-done-headline t
      org-fontify-quote-and-verse-blocks t
      org-fontify-whole-heading-line t
      org-indirect-buffer-display 'current-window
      org-archive-subtree-save-file-p t)

(define-key org-mode-map (kbd "C-'") nil)

(add-to-list 'org-modules 'org-clock t)
(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)

(setq-default
   org-agenda-deadline-faces
   '((1.001 . error)
     (1.0 . org-warning)
     (0.5 . org-upcoming-deadline)
     (0.0 . org-upcoming-distant-deadline))
   org-agenda-window-setup 'current-window
   org-agenda-skip-unavailable-files t
   org-agenda-span 10
   org-agenda-start-on-weekday nil
   org-agenda-start-day "-3d"
   org-agenda-inhibit-startup t)

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

(setq org-html-htmlize-output-type 'css)
