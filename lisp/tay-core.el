;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UI
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq hl-line-sticky-flag nil)
(setq global-hl-line-sticky-flag nil)
(setq blink-matching-paren nil)
(setq x-stretch-cursor nil)
(setq show-paren-delay 0.1)
(setq show-paren-highlight-openparen t)
(setq show-paren-when-point-inside-paren t)
(setq show-paren-when-point-in-periphery t)
(setq highlight-nonselected-windows nil)
(setq idle-update-delay 1.0)
(setq fast-but-imprecise-scrolling t)
(setq indicate-buffer-boundaries nil)
(setq indicate-empty-lines nil)

(setq-default cursor-in-non-selected-windows nil)

(blink-cursor-mode -1)
(show-paren-mode 1)

(setq display-line-numbers-type 'relative)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'org-mode-hook 'display-line-numbers-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EDITOR
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq create-lockfiles nil)
(setq auto-save-default nil)
(setq create-lockfiles nil)
(setq make-backup-files nil)
(setq uniquify-buffer-name-style 'forward)
(setq ring-bell-function #'ignore)
(setq visible-bell nil)
(setq find-file-visit-truename t)
(setq vc-follow-symlinks t)
(setq find-file-suppress-same-file-warnings t)
(setq kill-do-not-save-duplicates t)
(setq confirm-nonexistent-file-or-buffer nil)
(setq save-interprogram-paste-before-kill t)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default tab-always-indent nil)
(setq-default fill-column 80)
(setq-default truncate-lines t)

(delete-selection-mode 1)
(global-auto-revert-mode t)
(recentf-mode 1)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MINIBUFFER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq echo-keystrokes 0.02)

(fset #'yes-or-no-p #'y-or-n-p)

(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MODELINE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq display-time-mail-string "")
(setq display-time-default-load-average nil)

(column-number-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SCROLLING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq scroll-conservatively 101)
(setq scroll-preserve-screen-position t)

(add-hook 'eshell-mode-hook (lambda () hscroll-margin 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; STARTUP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq byte-compile-warnings '(not nresolved
                                  free-vars
                                  callargs
                                  redefine
                                  obsolete
                                  noruntime
                                  cl-functions
                                  interactive-only
                                  ))

(provide 'tay-core)
