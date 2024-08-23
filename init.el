;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  ______ ______  __  __  __    __  ______  ______  ______
;; /\__  _/\  __ \/\ \_\ \/\ "-./  \/\  __ \/\  ___\/\  ___\
;; \/_/\ \\ \  __ \ \____ \ \ \-./\ \ \  __ \ \ \___\ \___  \
;;    \ \_\\ \_\ \_\/\_____\ \_\ \ \_\ \_\ \_\ \_____\/\_____\
;;     \/_/ \/_/\/_/\/_____/\/_/  \/_/\/_/\/_/\/_____/\/_____/
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'eglot)

(load (expand-file-name "private.el" user-emacs-directory))
(require 'tay-private)

(when (window-system)
  (set-frame-font "Jetbrains Mono"))

(setq frame-title-format "TAYMACS"
      inhibit-startup-message t
      inhibit-startup-echo-area-message user-login-name
      initial-major-mode 'fundamental-mode
      initial-scratch-message nil
      fast-but-imprecise-scrolling t
      frame-resize-pixelwise t
      column-number-mode t
      scroll-conservatively 101
      scroll-preserve-screen-position t
      kill-do-not-save-duplicates t
      save-interprogram-paste-before-kill t
      js-indent-level 2
      ring-bell-function #'ignore
      visible-bell nil
      custom-file (expand-file-name "custom.el" user-emacs-directory)
      create-lockfiles nil
      auto-save-default nil
      make-backup-files nil
      uniquify-buffer-name-style 'forward
      dired-auto-revert-buffer t
      dired-dwim-target t
      dired-hide-details-hide-symlink-targets nil
      dired-recursive-copies  'always
      dired-recursive-deletes 'always
      dired-create-destination-dirs 'ask
      eglot-autoshutdown t
      eldoc-echo-area-use-multiline-p nil
      display-line-numbers-type 'relative
      tab-always-indent 'complete)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default tab-always-indent nil)
(setq-default c-default-style "linux")
(setq-default c-basic-offset 4)

(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)

(pixel-scroll-precision-mode)
(show-paren-mode 1)
(delete-selection-mode 1)
(fset #'yes-or-no-p #'y-or-n-p)
(global-auto-revert-mode t)
(load custom-file 'noerror)
(fido-vertical-mode 1)
(electric-pair-mode 1)

(add-hook 'dired-load-hook (function (lambda ()
                                       (load "dired-x"))))
(add-hook 'js-mode-hook 'eglot-ensure)
(add-hook 'web-mode-hook 'eglot-ensure)
(add-hook 'go-mode-hook 'eglot-ensure)
(add-hook 'go-mode-hook (lambda ()
                          (add-hook 'before-save-hook 'eglot-format-buffer nil t)))

(add-hook 'eshell-mode-hook
	      (lambda ()
            (eshell/alias "clear" "clear 1")))
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(defun tay/eshell-new ()
  "Make a brand new eshell buffer in the current location."
  (interactive)
  (eshell 'N))

(defun tay/bind-key (keychord func)
  "simply setup a global keybinding"
  (global-set-key (kbd keychord) func))

(defun tay/bind-key-map (mode-map keychord func)
  "bind a key for a specific mode map"
  (define-key mode-map (kbd keychord) func))

(defun tay/kill-this-buffer ()
  "kill the current buffer"
  (interactive)
  (kill-buffer (current-buffer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; external packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)
(setq package-check-signature nil)

(require 'use-package)
(setq use-package-verbose t)
(setq use-package-always-ensure t)

(add-to-list 'package-archives
	         '("melpa" . "https://melpa.org/packages/")
             '("nongnu" . "https://elpa.nongnu.org/nongnu/"))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(use-package ns-auto-titlebar
  :config
  (when (eq system-type 'darwin) (ns-auto-titlebar-mode)))

(use-package fleetish-theme
  :config
  (load-theme 'fleetish t))

(use-package go-mode)
(use-package go-dlv)
(use-package expand-region)
(use-package iedit)

(use-package markdown-mode
  :config
  (add-hook 'markdown-mode-hook 'display-line-numbers-mode)
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode)))

(use-package yaml-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode))
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))

(use-package web-mode
  :custom
  (web-mode-enable-engine-detection t)
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-enable-css-colorization t)
  (web-mode-enable-auto-pairing t)
  (web-mode-enable-current-element-highlight t)
  (web-mode-enable-current-column-highlight t)
  (web-mode-code-indent-offset 2)
  :config
  (add-to-list 'auto-mode-alist '("\\.html'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.css'" . web-mode)))

(use-package chatgpt-shell)

(use-package magit)

(use-package doom-modeline
  :custom
  (doom-modeline-icon nil)
  :config
  (doom-modeline-def-modeline 'tay/doom-modeline
    '(bar
      matches
      follow
      buffer-info
      buffer-position
      remote-host
      selection-info)
    '(compilation
      major-mode
      process))
  (add-hook 'doom-modeline-mode-hook
            (lambda () (doom-modeline-set-modeline 'tay/doom-modeline 'default)))
  (doom-modeline-mode 1))

(use-package corfu
  :custom
  (corfu-auto t)
  (corfu-auto-delay 1)
  (corfu-auto-prefix 2)
  (corfu-quit-no-match 'separator)
  :init
  (global-corfu-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; keybindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tay/bind-key "M-i" nil)
(tay/bind-key "C-z" nil)
(tay/bind-key "C-x C-z" nil)
(tay/bind-key "C-x C-r" nil)
(tay/bind-key "M-l" nil)
(tay/bind-key "M-c" nil)

(tay/bind-key "C-=" 'er/expand-region)
(tay/bind-key "C-x C-k" 'tay/kill-this-buffer)
(tay/bind-key "C-x C-b" 'switch-to-buffer)
(tay/bind-key "M-i 0" 'toggle-frame-fullscreen)
(tay/bind-key "M-i t" 'tay/eshell-new)
(tay/bind-key-map eglot-mode-map "M-i r" 'eglot-rename)
(tay/bind-key-map eglot-mode-map "M-i i" 'eglot-code-action-organize-imports)
(tay/bind-key-map eglot-mode-map "M-i e" 'flymake-show-buffer-diagnostics)
(tay/bind-key "M-o" 'other-window)
(tay/bind-key "M-i v" 'split-window-right)
(tay/bind-key "M-i s" 'split-window-below)
(tay/bind-key "M-i d" 'delete-window)
(tay/bind-key "M-i c" 'chatgpt-shell)
(tay/bind-key-map eglot-mode-map "M-[" 'flymake-goto-prev-error)
(tay/bind-key-map eglot-mode-map "M-]" 'flymake-goto-next-error)
(tay/bind-key-map isearch-mode-map "C-o" 'isearch-occur)
(tay/bind-key "M-i g" 'magit-status)
