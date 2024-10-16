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

(load-file (expand-file-name "letters.el" user-emacs-directory))

(unless (file-exists-p "~/.emacs.d/backups")
  (make-directory "~/.emacs.d/backups" t))

(when (window-system)
  (set-frame-font "Jetbrains Mono"))

(setq frame-title-format "TAYMACS"
      inhibit-startup-message t
      inhibit-startup-echo-area-message user-login-name
      frame-resize-pixelwise t
      kill-do-not-save-duplicates t
      column-number-mode t
      save-interprogram-paste-before-kill t
      js-indent-level 2
      visible-bell nil
      custom-file (expand-file-name "custom.el" user-emacs-directory)
      create-lockfiles nil
      uniquify-buffer-name-style 'forward
      dired-auto-revert-buffer t
      dired-dwim-target t
      dired-hide-details-hide-symlink-targets nil
      dired-recursive-copies  'always
      dired-recursive-deletes 'always
      dired-create-destination-dirs 'ask
      eglot-autoshutdown t
      eldoc-echo-area-use-multiline-p nil
      tab-always-indent 'complete
      org-agenda-files '("~/org/agenda.org")
      backup-directory-alist `(("." . "~/.emacs.d/backups"))
      auto-save-file-name-transforms `((".*" "~/.emacs.d/backups/" t))
      auto-save-interval 50)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default tab-always-indent nil)

(tool-bar-mode 0)
(scroll-bar-mode 0)

(setq modus-themes-common-palette-overrides
      '((border-mode-line-active bg-mode-line-active)
        (border-mode-line-inactive bg-mode-line-inactive)
        (fringe unspecified)
        (fg-prompt cyan)
        (bg-prompt bg-cyan-nuanced)))
;; (load-theme 'modus-operandi t)

(show-paren-mode 1)
(delete-selection-mode 1)
(fset #'yes-or-no-p #'y-or-n-p)
(global-auto-revert-mode t)
(load custom-file 'noerror)
(fido-vertical-mode 1)

(add-hook 'dired-load-hook (function (lambda ()
                                       (load "dired-x"))))
(add-hook 'dired-mode-hook (function (lambda ()
                                       (dired-hide-details-mode 1))))
(add-hook 'js-mode-hook 'eglot-ensure)
(add-hook 'web-mode-hook 'eglot-ensure)

(add-hook 'eshell-mode-hook
	      (lambda ()
            (eshell/alias "clear" "clear 1")))

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

(global-set-key (kbd "M-i") nil)
(global-set-key (kbd "C-z") nil)
(global-set-key (kbd "C-x C-z") nil)
(global-set-key (kbd "C-x C-r") nil)
(global-set-key (kbd "M-l") nil)
(global-set-key (kbd "M-c") nil)
(global-set-key (kbd "C-o") nil)
(global-set-key (kbd "C-j") nil)
(global-set-key (kbd "M-k") nil)
(global-set-key (kbd "C-i") nil)

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

(use-package go-dlv)
(use-package expand-region)
(use-package iedit)
(use-package magit)
(use-package hcl-mode)

(use-package go-mode
  :config
  (add-hook 'go-mode-hook 'eglot-ensure)
  (add-hook 'go-mode-hook (lambda ()
                            (add-hook 'before-save-hook 'eglot-format-buffer nil t))))

(use-package markdown-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode)))

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
  (corfu-auto-delay 0.3)
  (corfu-auto-prefix 2)
  (corfu-quit-no-match 'separator)
  :init
  (global-corfu-mode))

(use-package avy
  :bind
  ("C-'" . avy-goto-line)
  ("C-i" . avy-goto-word-1))

(use-package gptel
  :init
  (setq gptel-model "gpt-4o")
  :config
  (add-hook 'gptel-post-stream-hook 'gptel-auto-scroll)
  (add-hook 'gptel-post-response-functions 'gptel-end-of-response))

(use-package fleetish-theme
  :config
  (load-theme 'fleetish t))

(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

(use-package dockerfile-mode)

(use-package volatile-highlights
  :config
  (volatile-highlights-mode t))

(use-package pulsar
  :config
  (pulsar-global-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; keybindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C-x C-k") 'tay/kill-this-buffer)
(global-set-key (kbd "C-x C-b") 'switch-to-buffer)
(global-set-key (kbd "M-i 0") 'toggle-frame-fullscreen)
(global-set-key (kbd "M-i t") 'tay/eshell-new)
(define-key eglot-mode-map (kbd "M-i r") 'eglot-rename)
(define-key eglot-mode-map (kbd "M-i i") 'eglot-code-action-organize-imports)
(define-key eglot-mode-map (kbd "M-i e") 'flymake-show-buffer-diagnostics)
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "M-i v") 'split-window-right)
(global-set-key (kbd "M-i s") 'split-window-below)
(global-set-key (kbd "M-i d") 'delete-window)
(define-key eglot-mode-map (kbd "M-[") 'flymake-goto-prev-error)
(define-key eglot-mode-map (kbd "M-]") 'flymake-goto-next-error)
(define-key isearch-mode-map (kbd "C-o") 'isearch-occur)
(global-set-key (kbd "M-i g") 'magit-status)
(global-set-key (kbd "C-c RET") 'gptel-send)

;; editing mods
(global-set-key (kbd "C-o") 'tay/open-line-up)
(global-set-key (kbd "C-j") 'tay/open-line-down)
(global-set-key (kbd "M-k") 'tay/kill-line-down)
