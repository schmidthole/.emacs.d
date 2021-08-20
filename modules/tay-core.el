;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CORE
;;
;; Core emacs settings that do not require external dependencies
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UI
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(push '(menu-bar-lines . 0)   default-frame-alist)
(push '(tool-bar-lines . 0)   default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

(setq menu-bar-mode nil
      tool-bar-mode nil
      scroll-bar-mode nil
      hl-line-sticky-flag nil
      global-hl-line-sticky-flag nil
      blink-matching-paren nil
      x-stretch-cursor nil
      show-paren-delay 0.1
      show-paren-highlight-openparen t
      show-paren-when-point-inside-paren t
      show-paren-when-point-in-periphery t
      highlight-nonselected-windows nil
      auto-mode-case-fold nil
      bidi-inhibit-bpa t
      idle-update-delay 1.0
      inhibit-compacting-font-caches t
      x-underline-at-descent-line t
      fast-but-imprecise-scrolling t
      frame-inhibit-implied-resize t
      indicate-buffer-boundaries nil
      indicate-empty-lines nil
      frame-resize-pixelwise t
      window-resize-pixelwise nil
      window-divider-default-places t
      window-divider-default-bottom-width 1
      window-divider-default-right-width 1
      fast-but-imprecise-scrolling t
      frame-inhibit-implied-resize t
      indicate-buffer-boundaries nil
      indicate-empty-lines nil
      frame-resize-pixelwise t
      window-resize-pixelwise nil)

(setq window-divider-default-places t
      window-divider-default-bottom-width 1
      window-divider-default-right-width 1)
(setq-default cursor-in-non-selected-windows nil
              bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)

(blink-cursor-mode -1)
(global-hl-line-mode)
(show-paren-mode 1)
(window-divider-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EDITOR
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq create-lockfiles nil
      auto-save-default nil
      create-lockfiles nil
      make-backup-files nil
      uniquify-buffer-name-style 'forward
      ring-bell-function #'ignore
      visible-bell nil
      find-file-visit-truename t
      vc-follow-symlinks t
      find-file-suppress-same-file-warnings t
      require-final-newline t
      kill-do-not-save-duplicates t
      confirm-nonexistent-file-or-buffer nil
      save-interprogram-paste-before-kill t
      truncate-partial-width-windows nil)

(setq-default indent-tabs-mode nil
              tab-width 4
              tab-always-indent nil
              fill-column 80
              truncate-lines t)

(delete-selection-mode 1)
(global-auto-revert-mode t)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MINIBUFFER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq enable-recursive-minibuffers t
      echo-keystrokes 0.02
      minibuffer-prompt-properties
      '(read-only t intangible t cursor-intangible t face minibuffer-prompt))

(fset #'yes-or-no-p #'y-or-n-p)

(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MODELINE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq display-time-mail-string ""
      display-time-day-and-date t
      display-time-default-load-average nil)

(display-time-mode 1)
(column-number-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SCROLLING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq hscroll-margin 2
      hscroll-step 1
      scroll-conservatively 101
      scroll-margin 0
      scroll-preserve-screen-position t
      auto-window-vscroll nil
      mouse-wheel-scroll-amount '(2 ((shift) . hscroll))
      mouse-wheel-scroll-amount-horizontal 2)

(add-hook 'eshell-mode-hook (lambda () hscroll-margin 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; STARTUP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq inhibit-startup-message t
      inhibit-startup-echo-area-message user-login-name
      inhibit-default-init t
      initial-major-mode 'fundamental-mode
      initial-scratch-message nil)

(setq byte-compile-warnings '(not nresolved
                                  free-vars
                                  callargs
                                  redefine
                                  obsolete
                                  noruntime
                                  cl-functions
                                  interactive-only
                                  ))
