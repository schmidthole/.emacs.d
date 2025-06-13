;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  ______ ______  __  __  __    __  ______  ______  ______
;; /\__  _/\  __ \/\ \_\ \/\ "-./  \/\  __ \/\  ___\/\  ___\
;; \/_/\ \\ \  __ \ \____ \ \ \-./\ \ \  __ \ \ \___\ \___  \
;;    \ \_\\ \_\ \_\/\_____\ \_\ \ \_\ \_\ \_\ \_____\/\_____\
;;     \/_/ \/_/\/_/\/_____/\/_/  \/_/\/_/\/_/\/_____/\/_____/
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; setup the custom settings file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

;; load private settings
(load (expand-file-name "private.el" user-emacs-directory))
(require 'private)

;; load the directory that contains all custom modules
(setq tay/lisp-modules (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path tay/lisp-modules)

(setq frame-title-format "taymacs")

(tool-bar-mode 0)
(scroll-bar-mode 0)

(global-set-key (kbd "M-i") nil)
(global-set-key (kbd "C-z") nil)
(global-set-key (kbd "C-x C-z") nil)
(global-set-key (kbd "C-x C-r") nil)
(global-set-key (kbd "M-l") nil)
(global-set-key (kbd "M-c") nil)
(global-set-key (kbd "C-o") nil)
(global-set-key (kbd "C-j") nil)
(global-set-key (kbd "M-k") nil)
(global-set-key (kbd "C-t") nil)

(require 'tay-functions)

(setq modus-themes-bold-constructs t)
(setq modus-themes-italic-constructs t)
(setq modus-themes-common-palette-overrides
      '((border-mode-line-active bg-mode-line-active)
        (border-mode-line-inactive bg-mode-line-inactive)
	(fringe unspecified)
	(prose-done green-intense)
        (prose-todo red-intense)
	(fg-prompt fg-main)
        (bg-prompt bg-cyan-intense)))
(load-theme 'modus-operandi t)

;; base emacs settings
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message user-login-name)
(setq initial-scratch-message "")
(setq frame-resize-pixelwise t)
(setq kill-do-not-save-duplicates t)
(setq column-number-mode t)
(setq save-interprogram-paste-before-kill t)
(setq visible-bell nil)
(setq create-lockfiles nil)
(setq uniquify-buffer-name-style 'forward)
(setq tab-always-indent 'complete)
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq warning-minimum-level :emergency)
(setq treesit-font-lock-level 4)
(setq delete-by-moving-to-trash t)
(setq use-short-answers t)
(setq pixel-scroll-precision-mode t)
(setq pixel-scroll-precision-use-momentum nil)
(setq scroll-margin 0)
(setq scroll-conservatively 101)
(setq scroll-preserve-screen-position t)
(setq indent-tabs-mode nil)
(setq tab-width 4)
(setq tab-always-indent nil)
(setq global-auto-revert-non-file-buffers t)  
(setq-default comint-process-echoes t)

(setq-default truncate-lines t)

(when (window-system)
  (set-frame-font "Jetbrains Mono"))

(show-paren-mode 1)
(delete-selection-mode 1)
(global-auto-revert-mode t)
(recentf-mode 1)
(savehist-mode 1)
(save-place-mode 1)
(fido-vertical-mode 1)

;; completions
(add-hook 'prog-mode-hook #'completion-preview-mode)

(with-eval-after-load 'completion-preview
  (setq completion-preview-minimum-symbol-length 2)
  (push 'org-self-insert-command completion-preview-commands)

  (keymap-set completion-preview-active-mode-map "M-n" #'completion-preview-next-candidate)
  (keymap-set completion-preview-active-mode-map "M-p" #'completion-preview-prev-candidate)
  (keymap-set completion-preview-active-mode-map "M-i" #'completion-preview-insert))

;; dired
(setq dired-auto-revert-buffer t)
(setq dired-dwim-target t)
(setq dired-hide-details-hide-symlink-targets nil)
(setq dired-recursive-copies  'always)
(setq dired-recursive-deletes 'always)
(setq dired-create-destination-dirs 'ask)
(setq dired-kill-when-opening-new-dired-buffer t)

(add-hook 'dired-load-hook (function (lambda ()
                                       (load "dired-x"))))
(add-hook 'dired-mode-hook (function (lambda ()
                                       (dired-hide-details-mode 1))))

;; eglot
(setq eglot-autoshutdown t)
(require 'eglot)

(add-hook 'go-ts-mode-hook 'eglot-ensure)
(add-hook 'typescript-ts-mode-hook 'eglot-ensure)
(add-hook 'tsx-ts-mode-hook 'eglot-ensure)
(add-hook 'python-ts-mode-hook 'eglot-ensure)

;; eldoc
(setq eldoc-echo-area-use-multiline-p nil)
(global-eldoc-mode)

;; eshell
(setq eshell-visual-commands nil)
(add-hook 'eshell-mode-hook 'visual-line-mode)
(add-hook 'eshell-mode-hook
	  (lambda ()
            (eshell/alias "ll" "ls -la")
	    (eshell/alias "python" "python3 $*")
	    (eshell/alias "pip" "pip3 $*")
	    (eshell/alias "clear" "clear 1")
            (setq-local global-hl-line-mode nil)))

;; isearch
(setq isearch-lazy-count t)
(setq search-whitespace-regexp ".*?")

;; javascript
(setq-default js-indent-level 2)

;; org-mode
(setq org-agenda-files '("~/org/agenda.org"))
(setq org-todo-keywords '((sequence "TODO" "IN PROGRESS" "|" "DONE")))
(setq org-startup-truncated t)
(setq org-startup-indented t)

(add-hook 'org-mode-hook 'visual-line-mode)

;; which-key
(add-hook 'after-init-hook 'which-key-mode)

;; external packages
;; (use-package fleetish-theme
;;   :ensure t
;;   :config
;;   (load-theme 'fleetish t))

(use-package apheleia
  :ensure t
  :config
  (apheleia-global-mode +1))

(use-package avy
  :ensure t
  :bind
  (("C-;" . avy-goto-word-1)
   ("C-'" . avy-goto-line)))

(use-package coterm
  :ensure t
  :config
  (coterm-mode))

(use-package csv-mode
  :ensure t
  :config
  (require 'rainbow-csv)
  (add-hook 'csv-mode-hook 'rainbow-csv-mode))

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(use-package expand-region
  :ensure t
  :bind
  (("C-=" . er/expand-region)))

(use-package gptel
  :ensure t
  :defer t
  :bind
  (("C-c RET" . gptel-send))
  :init
  (setq gptel-model "claude-3-7-sonnet-20250219")
  (setq gptel-backend (gptel-make-anthropic "Claude"
                        :stream t
                        :key claude-api-key
			:models '(claude-3-7-sonnet-20250219)))
  (setq gptel-prompt-prefix-alist '((org-mode . "* PROMPT\n\n")))
  (setq gptel-response-prefix-alist '((org-mode . "* RESPONSE\n\n")))
  (setq gptel-default-mode 'org-mode)
  :config
  (add-hook 'gptel-post-stream-hook 'gptel-auto-scroll)
  (add-hook 'gptel-post-response-functions 'gptel-end-of-response))

(use-package markdown-mode
  :ensure t
  :defer t
  :config
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode)))

(use-package treesit-auto
  :after eglot
  :demand t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode t))

(use-package magit
  :ensure t)

(use-package vterm
  :ensure t
  :init
  (setq vterm-always-compile-module t))

;; keybindings
(global-set-key (kbd "C-x C-k") 'tay/kill-this-buffer)
(global-set-key (kbd "C-x C-b") 'switch-to-buffer)
(global-set-key (kbd "C-o") 'tay/open-line-up)
(global-set-key (kbd "C-j") 'tay/open-line-down)
(global-set-key (kbd "M-k") 'tay/kill-line-down)

(global-set-key (kbd "M-i g") 'magit-status)

(define-key eglot-mode-map (kbd "M-i i") 'eglot-code-action-organize-imports)
(define-key eglot-mode-map (kbd "M-i e") 'flymake-show-buffer-diagnostics)
(define-key eglot-mode-map (kbd "M-i r") 'eglot-rename)
(define-key eglot-mode-map (kbd "M-i i") 'eglot-code-action-organize-imports)
(define-key eglot-mode-map (kbd "M-[") 'flymake-goto-prev-error)
(define-key eglot-mode-map (kbd "M-]") 'flymake-goto-next-error)

(require 'weather-mode)
(require 'email)
(put 'downcase-region 'disabled nil)
