;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EMAIL
;;
;; mu4e settings and smtp sending
;;
;; this module is extremely opinionated and specific to me. it assumes we are
;; using mu4e, mbsync, and smpt sendmail for email. the formatting is also
;; setup to play nice with gmail.
;;
;; most of the specific settings should reside in `personal.el'
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu/mu4e/")

;; mbsync settings
(setq mu4e-get-mail-command "mbsync -a"
      mu4e-change-filenames-when-moving t)

;; general mu4e settings
(setq mu4e-update-interval nil
      mu4e-compose-format-flowed t
      mu4e-view-show-addresses t
      mu4e-sent-messages-behavior 'sent
      mu4e-hide-index-messages t
      mu4e-view-show-images t
      mu4e-view-image-max-width 800
      message-send-mail-function #'smtpmail-send-it
      smtpmail-stream-type 'starttls
      message-kill-buffer-on-exit t
      mu4e-completing-read-function #'ivy-completing-read
      user-mail-agent 'mu4e-user-agent)

;; truncate lines in emails
(add-hook 'mu4e-view-mode-hook (lambda () (setq truncate-lines nil)))

;; gmail settings
(setq mu4e-sent-messages-behavior 'delete
      mu4e-index-cleanup nil
      mu4e-index-lazy-check t)

(setq message-cite-function  'message-cite-original
      message-citation-line-function  'message-insert-formatted-citation-line
      message-cite-reply-position 'above
      message-yank-prefix  "    "
      message-yank-cited-prefix  "    "
      message-yank-empty-prefix  "    "
      message-citation-line-format "On %e %B %Y %R, %f wrote:\n"))

(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-stream-type 'starttls
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587)
