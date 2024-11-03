;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  ______ ______  __  __  __    __  ______  ______  ______
;; /\__  _/\  __ \/\ \_\ \/\ "-./  \/\  __ \/\  ___\/\  ___\
;; \/_/\ \\ \  __ \ \____ \ \ \-./\ \ \  __ \ \ \___\ \___  \
;;    \ \_\\ \_\ \_\/\_____\ \_\ \ \_\ \_\ \_\ \_____\/\_____\
;;     \/_/ \/_/\/_/\/_____/\/_/  \/_/\/_/\/_/\/_____/\/_____/
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)
(add-to-list 'package-archives
	         '("melpa" . "https://melpa.org/packages/"))

(use-package emacs
  :ensure nil
  :custom
  (frame-title-format "TAYMACS")
  (inhibit-startup-message t)
  (inhibit-startup-echo-area-message user-login-name)
  (initial-scratch-message "")
  (frame-resize-pixelwise t)
  (kill-do-not-save-duplicates t)
  (column-number-mode t)
  (save-interprogram-paste-before-kill t)
  (visible-bell nil)
  (create-lockfiles nil)
  (uniquify-buffer-name-style 'forward)
  (tab-always-indent 'complete)
  (make-backup-files nil)
  (auto-save-default nil)
  (warning-minimum-level :emergency)
  (treesit-font-lock-level 4)
  (delete-by-moving-to-trash t)
  (go-ts-mode-indent-offset 4)
  (js-indent-level 2)
  (use-short-answers t)
  (pixel-scroll-precision-mode t)
  (pixel-scroll-precision-use-momentum nil)
  (scroll-margin 0)
  (scroll-conservatively 101)
  (scroll-preserve-screen-position t)
  (indent-tabs-mode nil)
  (tab-width 4)
  (tab-always-indent nil)
  (display-line-numbers-type 'relative)
  (custom-file (expand-file-name "custom.el" user-emacs-directory))
  :bind
  (("M-i" . nil)
   ("C-z" . nil)
   ("C-x C-z" . nil)
   ("C-x C-r" . nil)
   ("M-l" . nil)
   ("M-c" . nil)
   ("C-o" . nil)
   ("C-j" . nil)
   ("M-k" . nil)
   ("C-t" . nil)

   ("C-x C-k" . tay/kill-this-buffer)
   ("C-x C-b" . switch-to-buffer)
   ("C-o" . tay/open-line-up)
   ("C-j" . tay/open-line-down)
   ("M-k" . tay/kill-line-down)
   ("M-i 0" . toggle-frame-fullscreen)
   ("M-o" . other-window)
   ("M-i v" . split-window-right)
   ("M-i s" . split-window-below)
   ("M-i d" . delete-window)
   ("M-p" . beginning-of-defun)
   ("M-n" . end-of-defun))
  :init
  (when (window-system)
    (set-frame-font "Jetbrains Mono"))
  
  (tool-bar-mode 0)
  (scroll-bar-mode 0)
  (show-paren-mode 1)
  (delete-selection-mode 1)
  (global-auto-revert-mode t)
  (fido-vertical-mode 1)
  (recentf-mode 1)
  (savehist-mode 1)
  (save-place-mode 1)
  :config
  (load custom-file 'noerror)
  (load (expand-file-name "private.el" user-emacs-directory))
  (setq-default truncate-lines t)
  (require 'tay-private)
  (add-hook 'prog-mode-hook 'display-line-numbers-mode))

(use-package org
  :ensure nil
  :custom
  (org-startup-truncated t)
  (org-startup-indented t)
  :hook
  ((org-mode . visual-line-mode)))

(use-package eldoc
  :ensure nil
  :custom
  (eldoc-echo-area-use-multiline-p nil)
  :init
  (global-eldoc-mode))

(use-package eglot
  :ensure nil
  :demand t
  :custom
  (eglot-autoshutdown t)
  :bind
  (:map eglot-mode-map
        ("M-i r" . eglot-rename)
        ("M-i i" . eglot-code-action-organize-imports)
        ("M-i e" . flymake-show-buffer-diagnostics)
        ("M-i r" . eglot-rename)
        ("M-i i" . eglot-code-action-organize-imports)
        ("M-[" . flymake-goto-prev-error)
        ("M-]" . flymake-goto-next-error))
  :hook
  ((go-ts-mode . eglot-ensure)
   (typescript-ts-mode . eglot-ensure)
   (tsx-ts-mode . eglot-ensure)))

(use-package dired
  :ensure nil
  :custom
  (dired-auto-revert-buffer t)
  (dired-dwim-target t)
  (dired-hide-details-hide-symlink-targets nil)
  (dired-recursive-copies  'always)
  (dired-recursive-deletes 'always)
  (dired-create-destination-dirs 'ask)
  (dired-kill-when-opening-new-dired-buffer t)
  :init
  (add-hook 'dired-load-hook (function (lambda ()
                                         (load "dired-x"))))
  (add-hook 'dired-mode-hook (function (lambda ()
                                         (dired-hide-details-mode 1)))))

(use-package eshell
  :ensure nil
  :bind
  ("M-i t" . (lambda () (interactive) (eshell 'N)))
  :hook
  ((eshell-mode . visual-line-mode)))

(use-package which-key
  :ensure nil
  :config
  (add-hook 'after-init-hook 'which-key-mode))

(use-package winner
  :bind
  (("M-i h" . winner-undo)
   ("M-i l" . winner-redo))
  :config
  (winner-mode))

(defun tay/eshell-new ()
  "make a brand new eshell buffer in the current location."
  (interactive)
  (eshell 'N))

(defun tay/kill-this-buffer ()
  "kill the current buffer"
  (interactive)
  (kill-buffer (current-buffer)))

(defun tay/open-line-up (n)
  (interactive "p")
  (move-beginning-of-line 1)
  (open-line n))

(defun tay/open-line-down (n)
  (interactive "p")
  (move-end-of-line 1)
  (newline n))

(defun tay/kill-line-down (n)
  (interactive "p")
  (move-beginning-of-line 1)
  (kill-line n))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; external packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package fleetish-theme
  :ensure t
  :config
  (load-theme 'fleetish t))

(use-package ns-auto-titlebar
  :ensure t
  :config
  (when (eq system-type 'darwin) (ns-auto-titlebar-mode)))

(use-package go-dlv
  :ensure t
  :defer t)

(use-package magit
  :ensure t
  :defer t
  :bind
  (("M-i g" . magit-status)))

(use-package markdown-mode
  :ensure t
  :defer t
  :config
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode)))

(use-package web-mode
  :ensure t
  :defer t
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

(use-package doom-modeline
  :ensure t
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
  :ensure t
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.3)
  (corfu-auto-prefix 2)
  (corfu-quit-no-match 'separator)
  :init
  (global-corfu-mode)
  :config
  (add-hook 'eshell-mode-hook (lambda ()
                                (setq-local corfu-auto nil)
                                (corfu-mode)))
  (add-hook 'markdown-mode-hook (lambda ()
                                  (setq-local corfu-auto nil)))
  (add-hook 'org-mode-hook (lambda ()
                             (setq-local corfu-auto nil)))
  (add-hook 'git-commit-mode-hook (lambda ()
                                    (setq-local corfu-auto nil))))

(use-package marginalia
  :ensure t
  :init
  (marginalia-mode))

(use-package gptel
  :ensure t
  :defer t
  :bind
  (("C-c RET" . gptel-send))
  :init
  (setq gptel-model "gpt-4o")
  (setq gptel-prompt-prefix-alist '((markdown-mode . "# ")
                                    (org-mode . "* ")
                                    (text-mode . "# ")))
  (setq gptel-default-mode 'org-mode)
  :config
  (add-hook 'gptel-post-stream-hook 'gptel-auto-scroll)
  (add-hook 'gptel-post-response-functions 'gptel-end-of-response))

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(use-package apheleia
  :ensure t
  :config
  (apheleia-global-mode +1))

(use-package treesit-auto
  :ensure t
  :after eglot
  :demand t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode t))

(use-package expand-region
  :ensure t
  :bind  (("C-=" . er/expand-region)))

;; (use-package evil
;;   :ensure t
;;   :init
;;   (setq evil-want-C-u-scroll t)
;;   (setq evil-want-keybinding nil)
;;   :config
;;   (evil-mode 1))

;; (use-package evil-collection
;;   :after evil
;;   :ensure t
;;   :config
;;   (evil-collection-init))
