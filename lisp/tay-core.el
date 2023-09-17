;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UI
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq hl-line-sticky-flag nil)
(setq global-hl-line-sticky-flag nil)
(setq show-paren-when-point-inside-paren t)
(setq show-paren-when-point-in-periphery t)
(setq fast-but-imprecise-scrolling t)
(setq frame-resize-pixelwise t)

(setq-default cursor-in-non-selected-windows nil)

(blink-cursor-mode -1)
(show-paren-mode 1)

(setq display-line-numbers-type 'relative)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'org-mode-hook 'display-line-numbers-mode)

(setq frame-title-format "TAYMACS")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EDITOR
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq create-lockfiles nil)
(setq auto-save-default nil)
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

(setq tab-bar-show 1)
(setq tab-bar-close-button-show nil)
(setq tab-bar-new-button-show nil)
(setq tab-bar-new-tab-choice "*scratch*")
(tab-bar-mode 1)


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
(pixel-scroll-precision-mode)

(add-hook 'eshell-mode-hook (lambda () hscroll-margin 0))
(add-hook 'shell-mode-hook (lambda () hscroll-margin 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EXTERNAL PACKAGE SETUP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'use-package)
(setq use-package-verbose t)
(setq use-package-always-ensure t)

(require 'package)
(setq package-check-signature nil)
(add-to-list 'package-archives
	         '("melpa" . "https://melpa.org/packages/")
             '("nongnu" . "https://elpa.nongnu.org/nongnu/"))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; STARTUP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq byte-compile-warnings '(not all))

(provide 'tay-core)
