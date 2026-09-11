;;; init.el --- terminal emacs configuration -*- lexical-binding: t; -*-

(when (< emacs-major-version 31)
  (error "taymacs requires emacs 31 or newer"))

;; custom settings

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

;; homebrew executables

(when (eq system-type 'darwin)
  (dolist (directory '("/usr/local/bin" "/opt/homebrew/bin"))
    (when (file-directory-p directory)
      (add-to-list 'exec-path directory)
      (unless (member directory (split-string (or (getenv "PATH") "") path-separator))
        (setenv "PATH" (concat directory path-separator (getenv "PATH")))))))

;; appearance

(setq modus-themes-common-palette-overrides
      '((bg-mode-line-active bg-dim)
        (fg-mode-line-active fg-main)
        (border-mode-line-active unspecified)
        (bg-mode-line-inactive bg-main)
        (fg-mode-line-inactive fg-dim)
        (border-mode-line-inactive unspecified)))

(load-theme 'modus-vivendi t)
(menu-bar-mode -1)

;; editing commands

(defun tay/open-line-up (n)
  "open n lines above the current line."
  (interactive "p")
  (move-beginning-of-line 1)
  (open-line n))

(defun tay/open-line-down (n)
  "open n lines below the current line."
  (interactive "p")
  (move-end-of-line 1)
  (newline n))

(defun tay/kill-line-down (n)
  "kill n lines starting with the current line."
  (interactive "p")
  (move-beginning-of-line 1)
  (kill-line n))

;; keys

(dolist (key '("M-i" "C-z" "C-x C-z" "C-x C-r"))
  (keymap-global-unset key t))

(keymap-global-set "C-x C-k" #'kill-current-buffer)
(keymap-global-set "C-x C-b" #'switch-to-buffer)
(keymap-global-set "C-o" #'tay/open-line-up)
(keymap-global-set "C-j" #'tay/open-line-down)
(keymap-global-set "M-k" #'tay/kill-line-down)
(keymap-global-set "M-o" #'other-window)
(keymap-global-set "M-i v" #'split-window-right)
(keymap-global-set "M-i s" #'split-window-below)
(keymap-global-set "C-;" #'jumpa)

;; core

(setq inhibit-startup-message t
      inhibit-startup-echo-area-message user-login-name
      initial-scratch-message ""
      ring-bell-function #'ignore
      uniquify-buffer-name-style 'forward
      tab-always-indent nil
      delete-by-moving-to-trash t
      use-short-answers t
      scroll-margin 2
      global-auto-revert-non-file-buffers t
      vc-handled-backends '(Git))

(setq-default indent-tabs-mode nil
              tab-width 4
              truncate-lines t)

(delete-selection-mode 1)
(global-auto-revert-mode 1)
(fido-vertical-mode 1)
(electric-pair-mode 1)
(which-key-mode 1)

;; mode line

(setq-default mode-line-format
              '("%e"
                mode-line-modified
                mode-line-buffer-identification
                "  %l:%c"
                mode-line-format-right-align
                (project-mode-line project-mode-line-format)
                "  "
                (vc-mode vc-mode)
                "  "
                mode-name
                mode-line-process
                " ")
              mode-line-buffer-identification '(" %b"))

(setq project-mode-line t)

;; completion

(add-hook 'prog-mode-hook #'completion-preview-mode)

(with-eval-after-load 'completion-preview
  (keymap-set completion-preview-active-mode-map "M-n"
              #'completion-preview-next-candidate)
  (keymap-set completion-preview-active-mode-map "M-p"
              #'completion-preview-prev-candidate)
  (keymap-set completion-preview-active-mode-map "M-i"
              #'completion-preview-insert))

;; dired

(setq dired-dwim-target t
      dired-hide-details-hide-symlink-targets nil
      dired-recursive-copies 'always
      dired-recursive-deletes 'always
      dired-kill-when-opening-new-dired-buffer t)

(with-eval-after-load 'dired-aux
  (setq dired-create-destination-dirs 'ask))

(add-hook 'dired-mode-hook #'dired-hide-details-mode)

;; search

(setq isearch-lazy-count t
      search-whitespace-regexp ".*?")

;; tree-sitter

(setopt treesit-enabled-modes t
        treesit-auto-install-grammar 'always
        treesit-font-lock-level 4)

(autoload 'markdown-ts-mode "markdown-ts-mode" nil t)
(add-to-list 'auto-mode-alist
             '("\\.\\(?:md\\|markdown\\)\\'" . markdown-ts-mode))
(add-hook 'markdown-ts-mode-hook #'visual-line-mode)

;; language servers and diagnostics

(dolist (hook '(go-ts-mode-hook
                typescript-ts-mode-hook
                tsx-ts-mode-hook
                python-ts-mode-hook))
  (add-hook hook #'eglot-ensure))

(with-eval-after-load 'eglot
  (setq eglot-autoshutdown t
        eglot-events-buffer-config '(:size 0 :format full))
  (keymap-set eglot-mode-map "M-i i" #'eglot-code-action-organize-imports)
  (keymap-set eglot-mode-map "M-i e" #'flymake-show-buffer-diagnostics)
  (keymap-set eglot-mode-map "M-i r" #'eglot-rename)
  (keymap-set eglot-mode-map "M-[" #'flymake-goto-prev-error)
  (keymap-set eglot-mode-map "M-]" #'flymake-goto-next-error))

(with-eval-after-load 'flymake
  (setq flymake-indicator-type 'margins
        flymake-margin-indicators-string
        '((error "!" compilation-error)
          (warning "?" compilation-warning)
          (note "i" compilation-info))))

;; indentation

(set-default 'go-ts-indent-offset 4)
(setq-default js-indent-level 2)

;; packages

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))

(use-package protobuf-mode
  :ensure t
  :mode "\\.proto\\'")

;;; init.el ends here
