(add-to-list 'load-path "/opt/homebrew/share/emacs/site-lisp/mu/mu4e/")
(require 'mu4e)

(setq mu4e-mu-binary "/opt/homebrew/bin/mu")

(setq mu4e-change-filenames-when-moving t)
(setq mu4e-update-interval (* 10 60))
(setq mu4e-get-mail-command "mbsync -a")
(setq mu4e-maildir "~/mail")
(setq mu4e-compose-format-flowed t)

(setq mu4e-maildir-shortcuts
      '((:maildir "/icloud/Inbox"    :key ?i)))

(add-hook 'mu4e-view-mode-hook (lambda () (setq truncate-lines nil)))

(setq message-send-mail-function 'smtpmail-send-it)
(setq message-charset-mail-charset 'utf-8)

(setq message-cite-function  'message-cite-original
      message-citation-line-function  'message-insert-formatted-citation-line
      message-cite-reply-position 'above
      message-yank-prefix  "    "
      message-yank-cited-prefix  "    "
      message-yank-empty-prefix  "    "
      message-citation-line-format "On %e %B %Y %R, %f wrote:\n")

(use-package org-mime
  :ensure t
  :config
  (setq org-mime-export-options '(:section-numbers
				  nil
				  :with-author nil
				  :with-toc nil))
  (add-hook 'message-send-hook 'org-mime-htmlize))

(provide 'email)
