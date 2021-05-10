;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; Taylor's extra config
;; ;;
;; ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq gc-cons-threshold most-positive-fixnum gc-cons-percentage 0.6)
(defvar tdm--file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DEFAULTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; get rid of extra ui elements first
(push '(menu-bar-lines . 0)   default-frame-alist)
(push '(tool-bar-lines . 0)   default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

(setq menu-bar-mode nil
      tool-bar-mode nil
      scroll-bar-mode nil)

;; optimize settings for faster render times
(setq auto-mode-case-fold nil)

(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)

(setq bidi-inhibit-bpa t)

(setq-default cursor-in-non-selected-windows nil)

(setq highlight-nonselected-windows nil)

(setq fast-but-imprecise-scrolling t)
(setq frame-inhibit-implied-resize t)

(setq idle-update-delay 1.0)

(setq inhibit-compacting-font-caches t)

;; general frame/window settings
(setq indicate-buffer-boundaries nil
      indicate-empty-lines nil)

(setq frame-resize-pixelwise t)
(setq window-resize-pixelwise nil)

(setq window-divider-default-places t
      window-divider-default-bottom-width 1
      window-divider-default-right-width 1)
(window-divider-mode 1)

(setq split-width-threshold 160
      split-height-threshold nil)

(setq resize-mini-windows 'grow-only)

;; nice scrolling
(setq hscroll-margin 2
      hscroll-step 1
      scroll-conservatively 101
      scroll-margin 0
      scroll-preserve-screen-position t
      auto-window-vscroll nil
      mouse-wheel-scroll-amount '(2 ((shift) . hscroll))
      mouse-wheel-scroll-amount-horizontal 2)

(add-hook 'eshell-mode-hook (lambda () hscroll-margin 0))

;; setup scratch buffer with no mode or message
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message user-login-name
      inhibit-default-init t
      initial-major-mode 'fundamental-mode
      initial-scratch-message nil)

;; tab
(setq-default indent-tabs-mode nil
              tab-width 4)

(setq-default tab-always-indent nil)

(setq tabify-regexp "^\t* [ \t]+")

;; cursor
(blink-cursor-mode -1)
(setq blink-matching-paren nil)
(setq x-stretch-cursor nil)

;; line and overflow
(setq-default fill-column 80)

(setq-default truncate-lines t)
(setq truncate-partial-width-windows nil)

;; highlight line
(setq hl-line-sticky-flag nil
      global-hl-line-sticky-flag nil)
(global-hl-line-mode)

;; show parens
(setq show-paren-delay 0.1
      show-paren-highlight-openparen t
      show-paren-when-point-inside-paren t
      show-paren-when-point-in-periphery t)
(show-paren-mode 1)

;; buffer and file handling
(setq create-lockfiles nil
      make-backup-files nil)

(setq uniquify-buffer-name-style 'forward
      ring-bell-function #'ignore
      visible-bell nil)

(setq find-file-visit-truename t
      vc-follow-symlinks t)

(setq find-file-suppress-same-file-warnings t)

(setq require-final-newline t)

(setq kill-do-not-save-duplicates t)

(setq confirm-nonexistent-file-or-buffer nil)

(global-auto-revert-mode t)

(setq x-underline-at-descent-line t)

;; minibuffer
(setq enable-recursive-minibuffers t)

(setq echo-keystrokes 0.02)

(fset #'yes-or-no-p #'y-or-n-p)

(setq minibuffer-prompt-properties '(read-only t intangible t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

;; modeline
(setq display-time-mail-string "")
(setq display-time-day-and-date t)
(setq display-time-default-load-average nil)
(display-time-mode 1)
(column-number-mode 1)

;; setup separate custom settings file
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; External Packages
;;
;; everything below is contained in external melpa packages
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; bootstrap melpa
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; setup use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-verbose t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GENERAL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; load macos path
(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-copy-env "PATH"))

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UI
;;
;; - doom-themes (doom-vibrant)
;; - telephone-line
;; - ns-auto-titlebar
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t    
        doom-themes-enable-italic t) 
  (load-theme 'doom-vibrant t)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

(use-package solaire-mode
  :ensure t
  :config
  (add-hook 'change-major-mode-hook 'turn-on-solaire-mode)
  (add-hook 'after-revert-hook 'turn-on-solaire-mode)
  (add-hook 'ediff-prepare-buffer-hook 'solaire-mode)
  (add-hook 'minibuffer-setup-hook 'solaire-mode-in-minibuffer)
  (setq solaire-mode-auto-swap-bg nil)

  (solaire-global-mode +1))

(use-package telephone-line
  :ensure t
  :config
  (setq telephone-line-primary-left-separator 'telephone-line-flat)
  (setq telephone-line-primary-right-separator 'telephone-line-flat)
  (setq telephone-line-secondary-left-separator 'telephone-line-flat)
  (setq telephone-line-secondary-right-separator 'telephone-line-flat)
  (telephone-line-mode 1))

(use-package ns-auto-titlebar
  :ensure t
  :config
  (when (eq system-type 'darwin) (ns-auto-titlebar-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EDITOR
;;
;; - smartparens
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package smartparens
  :ensure t
  :config
  (let ((unless-list '(sp-point-before-word-p
                       sp-point-after-word-p
                       sp-point-before-same-p)))
    (sp-pair "'"  nil :unless unless-list)
    (sp-pair "\"" nil :unless unless-list))

  (dolist (brace '("(" "{" "["))
    (sp-pair brace nil
             :post-handlers '(("||\n[i]" "RET") ("| " "SPC"))
             :unless '(sp-point-before-word-p sp-point-before-same-p)))

  (sp-local-pair sp-lisp-modes "(" ")" :unless '(:rem sp-point-before-same-p))

  (require 'smartparens-config)
  (smartparens-global-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SEARCH
;;
;; - ivy
;; - counsel
;; - swiper
;; - ivy-rich
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package ivy
  :ensure t
  :config
  (setq ivy-sort-max-size 7500)
  (require 'counsel nil t)
  (setq ivy-height 17
        ivy-wrap t
        ivy-fixed-height-minibuffer t
        ivy-read-action-format-function #'ivy-read-action-format-columns
        ivy-on-del-error-function #'ignore
        ivy-use-selectable-prompt t
        ivy-count-format "(%d/%d) "
        ivy-display-style 'fancy
        ivy-initial-inputs-alist nil)
  (ivy-mode 1))

(use-package ivy-rich
  :ensure t
  :config
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
  (setq ivy-rich-parse-remote-buffer nil)
  (setq ivy-rich-path-style 'abbrev)
  (ivy-rich-mode 1))

(use-package counsel
  :ensure t
  :after ivy
  :config
  (counsel-mode))

(use-package swiper
  :ensure t
  :config
  (setq swiper-goto-start-of-match t)
  :bind
  (("C-s" . swiper)
   ("C-r" . swiper-backward)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DIRED
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package dired
  :init
  (setq dired-auto-revert-buffer t 
        dired-dwim-target t
        dired-hide-details-hide-symlink-targets nil
        dired-recursive-copies  'always
        dired-recursive-deletes 'top
        dired-create-destination-dirs 'ask))

(use-package diredfl
  :ensure t
  :config
  (add-hook 'dired-mode-hook 'diredfl-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ESHELL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package eshell
  :init
  (defun tay/eshell-new ()
    "Make a brand new eshell buffer in the current location."
    (interactive)
    (eshell 'N))

  ;; aliases for eshell
  (add-hook 'eshell-mode-hook
	        (lambda ()
	          (eshell/alias "python" "python3 $*")
	          (eshell/alias "pip" "pip3 $*")
	          (eshell/alias "clear" "clear 1"))))

(use-package shrink-path
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UNDO
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package undo-fu
  :ensure t
  :after evil
  :config
  (define-key evil-normal-state-map "u" 'undo-fu-only-undo)
  (define-key evil-normal-state-map "\C-r" 'undo-fu-only-redo))

(use-package volatile-highlights
  :ensure t
  :after undo-fu
  :config
  (vhl/define-extension 'undo-fu 'undo-fu-only-undo 'undo-fu-only-redo)
  (vhl/install-extension 'undo-fu)
  (volatile-highlights-mode t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LOOKUP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package dumb-jump
  :ensure t
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; WORKSPACES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package perspective
  :ensure t
  :config
  (persp-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GIT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package magit
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EVIL EVIL EVIL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package evil
  :ensure t
  :init
  (defvar evil-want-C-g-bindings t)
  (defvar evil-want-C-u-scroll t)
  (defvar evil-want-C-u-delete t)
  (defvar evil-want-C-w-scroll t)
  (defvar evil-want-C-w-delete t)
  (defvar evil-want-Y-yank-to-eol t)
  (defvar evil-want-abbrev-expand-on-insert-exit nil)
  (defvar evil-respect-visual-line-mode t)

  (setq evil-ex-search-vim-style-regexp t
        evil-ex-visual-char-range t
        evil-mode-line-format 'nil
        evil-symbol-word-search t
        evil-ex-interactive-search-highlight 'selected-window
        evil-kbd-macro-suppress-motion-error t
        evil-want-keybinding nil)
  :config
  (advice-add #'evil-visual-update-x-selection :override #'ignore)
  (advice-add #'evil-window-split  :override #'+evil-window-split-a)
  (advice-add #'evil-window-vsplit :override #'+evil-window-vsplit-a)
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(use-package evil-easymotion
  :ensure t)

(use-package evil-traces
  :ensure t
  :config
  (evil-traces-use-diff-faces) 
  (evil-traces-mode))

(use-package evil-goggles
  :ensure t
  :config
  (evil-goggles-mode)
  (evil-goggles-use-diff-faces))

(use-package general
  :ensure t
  :config
  (general-define-key
   :states '(normal visual)
   :keymaps 'override
   :prefix "SPC"

   ;; file and buffer keymaps
   "f" 'counsel-find-file
   "b" 'ivy-switch-buffer
   "k" (lambda () (interactive) (kill-buffer (current-buffer)))

   ;; searching
   "s" '(:ignore t :which-key "search")
   "s s" 'swiper
   "s a" 'counsel-ag

   ":" 'counsel-M-x

   ;; open things
   "o" '(:ignore t :which-key "open")
   "o e" 'tay/eshell-new
   "o m" 'mu4e
   "o g" 'magit
   
   ;; evil motion
   "SPC" '(:ignore t :which-key "motion")
   "SPC j" 'evilem-motion-next-visual-line
   "SPC k" 'evilem-motion-previous-visual-line

   ;; windows
   "w" '(:ignore t :which-key "window")
   "w v" 'split-window-right
   "w s" 'split-window-below
   "w h" 'evil-window-left
   "w j" 'evil-window-down
   "w k" 'evil-window-up
   "w l" 'evil-window-right
   "w d" 'evil-window-delete
   "w o" 'delete-other-windows

   "p" '(:ignore t :which-key "perspective")
   "p a" 'persp-add-buffer
   "p k" 'persp-remove-buffer
   "p b" 'persp-switch-to-buffer
   "p s" 'persp-switch
   "p n" 'persp-next
   "p p" 'persp-prev))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EMAIL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu/mu4e/")
(load (expand-file-name "personal.el" user-emacs-directory) 'noerror)

(use-package mu4e
  :ensure nil
  :config
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

(use-package smtpmail
  :ensure nil
  :custom
  (message-send-mail-function 'smtpmail-send-it)
  (smtpmail-stream-type 'starttls)
  (smtpmail-default-smtp-server "smtp.gmail.com")
  (smtpmail-smtp-server "smtp.gmail.com")
  (smtpmail-smtp-service 587))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CLEANUP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'emacs-startup-hook
    (lambda () (setq gc-cons-threshold 16777216 gc-cons-percentage 0.1)))

(add-hook 'emacs-startup-hook
    (lambda () (setq file-name-handler-alist tdm--file-name-handler-alist)))
