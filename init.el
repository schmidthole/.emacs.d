;;; init.el --- emacs configuration -*- lexical-binding: t; -*-

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

;; system clipboard

(defun tay/clipboard-copy (text)
  "copy text to the macos clipboard."
  (let ((process-connection-type nil)
        (process-environment (cons "LC_CTYPE=UTF-8" process-environment))
        (coding-system-for-write 'utf-8))
    (let ((process (start-process "pbcopy" nil "pbcopy")))
      (process-send-string process text)
      (process-send-eof process))))

(defun tay/clipboard-paste ()
  "return the macos clipboard when it differs from the last kill."
  (let* ((process-environment (cons "LC_CTYPE=UTF-8" process-environment))
         (coding-system-for-read 'utf-8)
         (text (shell-command-to-string "pbpaste")))
    (unless (or (string-empty-p text) (equal text (car kill-ring)))
      text)))

(when (and (eq system-type 'darwin) (executable-find "pbcopy"))
  (setq interprogram-cut-function #'tay/clipboard-copy
        interprogram-paste-function #'tay/clipboard-paste))

;; appearance

(setq modus-themes-common-palette-overrides
      '((bg-mode-line-active bg-active)
        (fg-mode-line-active fg-main)
        (border-mode-line-active unspecified)
        (bg-mode-line-inactive bg-dim)
        (fg-mode-line-inactive fg-dim)
        (border-mode-line-inactive unspecified)))

;; linear palettes. dark is read from linear's computed syntax colors; light is
;; sampled from a screenshot, so its strings and variables are not exact yet.

(setq modus-vivendi-palette-overrides
      '((bg-main "#1a1a1b")
        (fg-main "#e3e4e6")
        (bg-dim "#232325")
        (fg-dim "#959597")
        (bg-active "#2c2c2f")
        (bg-hl-line "#222224")
        (bg-region "#33374f")
        (fg-region unspecified)
        (bg-paren-match "#2c2c2f")
        (cursor "#e3e4e6")
        (fg-line-number-active "#e3e4e6")
        (fg-line-number-inactive "#565759")
        (bg-line-number-active unspecified)
        (bg-line-number-inactive unspecified)
        (keyword "#f79ce0")
        (builtin "#76cff6")
        (fnname "#83dcdc")
        (fnname-call "#83dcdc")
        (constant "#8fa6ff")
        (number "#83dcdc")
        (type "#76cff6")
        (variable "#f7bf8b")
        (variable-use fg-main)
        (property "#f7bf8b")
        (string "#ffdf9f")
        (docstring "#959597")
        (comment "#959597")
        (operator fg-main)
        (punctuation fg-main)
        (bracket fg-main)
        (delimiter fg-main)
        (preprocessor "#f79ce0")
        (rx-construct "#d85927")
        (rx-backslash "#d85927")))

(setq modus-operandi-palette-overrides
      '((bg-main "#ffffff")
        (fg-main "#2f2f31")
        (bg-dim "#f9f9fa")
        (fg-dim "#5c5c5e")
        (bg-active "#e8e8ea")
        (bg-hl-line "#f4f4f5")
        (bg-region "#dde0f5")
        (fg-region unspecified)
        (bg-paren-match "#e8e8ea")
        (cursor "#2f2f31")
        (fg-line-number-active "#2f2f31")
        (fg-line-number-inactive "#9c9d9f")
        (bg-line-number-active unspecified)
        (bg-line-number-inactive unspecified)
        (keyword "#c45fae")
        (builtin "#4396c5")
        (fnname "#3e898a")
        (fnname-call "#3e898a")
        (constant fg-main)
        (number "#3e898a")
        (type "#3e898a")
        (variable fg-main)
        (variable-use fg-main)
        (property fg-main)
        (string "#50a810")
        (docstring "#5c5c5e")
        (comment "#5c5c5e")
        (operator fg-main)
        (punctuation fg-main)
        (bracket fg-main)
        (delimiter fg-main)
        (preprocessor "#c45fae")
        (rx-construct "#4396c5")
        (rx-backslash "#c45fae")))

(load (expand-file-name "user-lisp/tay-appearance.el" user-emacs-directory)
      nil t)
(tay/appearance-start)

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

(defun tay/window-narrower ()
  "make the selected window 5 columns narrower."
  (interactive)
  (shrink-window-horizontally 5))

(defun tay/window-wider ()
  "make the selected window 5 columns wider."
  (interactive)
  (enlarge-window-horizontally 5))

(defun tay/window-taller ()
  "make the selected window 5 lines taller."
  (interactive)
  (enlarge-window 5))

(defun tay/window-shorter ()
  "make the selected window 5 lines shorter."
  (interactive)
  (shrink-window 5))

(defvar-keymap tay/window-resize-repeat-map
  :doc "repeat window resizing with shifted hjkl."
  :repeat t
  "H" #'tay/window-narrower
  "J" #'tay/window-shorter
  "K" #'tay/window-taller
  "L" #'tay/window-wider)

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
(keymap-global-set "M-i H" #'tay/window-narrower)
(keymap-global-set "M-i J" #'tay/window-shorter)
(keymap-global-set "M-i K" #'tay/window-taller)
(keymap-global-set "M-i L" #'tay/window-wider)
(repeat-mode 1)
(keymap-global-set "C-;" #'jumpa)
;; Ghostty sends F6 for C-; so it also survives legacy terminal forwarding.
(keymap-global-set "<f6>" #'jumpa)

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

;; keep backups, auto-saves, and lock files out of project directories

(let ((backups (expand-file-name "var/backups/" user-emacs-directory))
      (auto-saves (expand-file-name "var/auto-saves/" user-emacs-directory))
      (locks (expand-file-name "var/locks/" user-emacs-directory)))
  (dolist (directory (list backups auto-saves locks))
    (make-directory directory t))
  (setq backup-directory-alist `(("." . ,backups))
        auto-save-file-name-transforms `((".*" ,auto-saves t))
        lock-file-name-transforms `((".*" ,locks t))))

(setq-default indent-tabs-mode nil
              tab-width 4
              truncate-lines t)

(delete-selection-mode 1)
(global-auto-revert-mode 1)
(fido-vertical-mode 1)
(electric-pair-mode 1)
(which-key-mode 1)

;; mode line

(load (expand-file-name "user-lisp/tay-modeline.el" user-emacs-directory)
      nil t)
(tay/modeline-setup)

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

(use-package ghostel
  :ensure t
  :commands (ghostel ghostel-project)
  :config
  (setopt ghostel-keymap-exceptions
          (cons "M-o" (remove "M-o" ghostel-keymap-exceptions)))
  (keymap-set ghostel-char-mode-map "M-o" #'other-window))

(use-package agent
  :load-path "user-lisp"
  :bind (("M-i a n" . agent-new)
         ("M-i a a" . agent-switch)
         ("M-i a r" . agent-rename)))

;;; init.el ends here
