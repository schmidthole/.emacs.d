;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  ______ ______  __  __  __    __  ______  ______  ______
;; /\__  _/\  __ \/\ \_\ \/\ "-./  \/\  __ \/\  ___\/\  ___\
;; \/_/\ \\ \  __ \ \____ \ \ \-./\ \ \  __ \ \ \___\ \___  \
;;    \ \_\\ \_\ \_\/\_____\ \_\ \ \_\ \_\ \_\ \_____\/\_____\
;;     \/_/ \/_/\/_/\/_____/\/_/  \/_/\/_/\/_/\/_____/\/_____/
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ____________________________________________________________________________
;;|
;;| load in custom files and directories which may contain lisp modules

;; setup the custom settings file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

;; load private settings
(load (expand-file-name "private.el" user-emacs-directory) 'noerror)

;; load the directory that contains all custom modules
(setq tay/lisp-modules (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path tay/lisp-modules)

;; ____________________________________________________________________________
;;|
;;| custom modules
(require 'weather-mode)
(require 'claudia)

;; ____________________________________________________________________________
;;|
;;| custom functions

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

;; ____________________________________________________________________________
;;|
;;| core emacs settings

;; unset keybindings we dont like or will be rebound later
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

;; set some basic keybindings that use custom functions
(global-set-key (kbd "C-x C-k") 'tay/kill-this-buffer)
(global-set-key (kbd "C-x C-b") 'switch-to-buffer)
(global-set-key (kbd "C-o") 'tay/open-line-up)
(global-set-key (kbd "C-j") 'tay/open-line-down)
(global-set-key (kbd "M-k") 'tay/kill-line-down)
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "M-i v") 'split-window-right)
(global-set-key (kbd "M-i s") 'split-window-below)

;; startup things
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message user-login-name
      initial-scratch-message ""
      initial-major-mode 'outline-mode)

(setq frame-resize-pixelwise t
      kill-do-not-save-duplicates t
      column-number-mode t
      save-interprogram-paste-before-kill t
      visible-bell nil
      create-lockfiles nil
      uniquify-buffer-name-style 'forward
      tab-always-indent nil
      make-backup-files nil
      auto-save-default nil
      warning-minimum-level :emergency
      treesit-font-lock-level 4
      delete-by-moving-to-trash t
      use-short-answers t
      pixel-scroll-precision-mode t
      pixel-scroll-precision-use-momentum nil
      scroll-margin 2
      scroll-conservatively 101
      scroll-preserve-screen-position t
      global-auto-revert-non-file-buffers t
      kill-buffer-query-functions (remq 'process-kill-buffer-query-function kill-buffer-query-functions))

(setq-default indent-tabs-mode nil
              tab-width 4
              comint-process-echoes t
              truncate-lines t)

(when (window-system)
  (set-frame-font "Jetbrains Mono"))

(show-paren-mode 1)
(delete-selection-mode 1)
(global-auto-revert-mode t)
(recentf-mode 1)
(savehist-mode 1)
(save-place-mode 1)
(fido-vertical-mode 1)

;; ____________________________________________________________________________
;;|
;;| modeline

(defun tay/shorten-vc-mode (vc)
  (let* ((vc (replace-regexp-in-string "^ Git[:-]"
                                       (if (char-displayable-p ?) "  " "Git: ")
                                       vc)))
    (if (> (length vc) 20)
        (concat (substring vc 0 20)
                (if (char-displayable-p ?…) "…" "..."))
      vc)))

(defun tay/mode-line-position ()
  "mode-line lighter function for only display row/column without the buffer percentage"
  `((line-number-mode
     (column-number-mode
      (column-number-indicator-zero-based
       (10
        (:propertize
         mode-line-position-column-line-format
         display (min-width (10.0))
         ,@mode-line-position--column-line-properties))
       (10
        (:propertize
         (:eval (string-replace
                 "%c" "%C" (car mode-line-position-column-line-format)))
         display (min-width (10.0))
         ,@mode-line-position--column-line-properties)))
      (6
       (:propertize
	mode-line-position-line-format
        display (min-width (6.0))
        ,@mode-line-position--column-line-properties)))
     (column-number-mode
      (column-number-indicator-zero-based
       (6
        (:propertize
         mode-line-position-column-format
         display (min-width (6.0))
         ,@mode-line-position--column-line-properties))
       (6
        (:propertize
         (:eval (string-replace
                 "%c" "%C" (car mode-line-position-column-format)))
         display (min-width (6.0))
         ,@mode-line-position--column-line-properties)))))))

(defun tay/mode-line-major-modes ()
  "mode-line lighter function for only displaying the major mode"
  (let ((recursive-edit-help-echo
         "Recursive edit, type C-M-c to get out"))
    (list (propertize "%[" 'help-echo recursive-edit-help-echo)
	  "("
	  `(:propertize ("" mode-name)
			help-echo "Major mode\n\
mouse-1: Display major mode menu\n\
mouse-2: Show help for major mode\n\
mouse-3: Toggle minor modes"
			mouse-face mode-line-highlight
			local-map ,mode-line-major-mode-keymap)
	  '("" mode-line-process)
	  (propertize "%n" 'help-echo "mouse-2: Remove narrowing from buffer"
		      'mouse-face 'mode-line-highlight
		      'local-map (make-mode-line-mouse-map
				  'mouse-2 #'mode-line-widen))
	  ")"
	  (propertize "%]" 'help-echo recursive-edit-help-echo)
	  " ")))

(setq-default mode-line-format
              '("%e" "  "
                (:propertize " " display (raise +0.1)) ;; Top padding
                (:propertize " " display (raise -0.1)) ;; Bottom padding

                (:propertize
                 ("" mode-line-modified))

                mode-line-frame-identification
                mode-line-buffer-identification
                "   "
                (:eval (tay/mode-line-position))
                mode-line-format-right-align
                "  "
                (project-mode-line project-mode-line-format)
                "  "
                (vc-mode (:eval (tay/shorten-vc-mode vc-mode)))
                "  "
                (:eval (tay/mode-line-major-modes))
                "  ")
              project-mode-line t
              mode-line-buffer-identification '(" %b")
              mode-line-position-column-line-format '(" %l:%c"))

(setq mode-line-modes-delimiters '("" . ""))

;; ____________________________________________________________________________
;;|
;;| completion

(add-hook 'prog-mode-hook #'completion-preview-mode)

(with-eval-after-load 'completion-preview
  (setq completion-preview-minimum-symbol-length 2)
  (push 'org-self-insert-command completion-preview-commands)

  (keymap-set completion-preview-active-mode-map "M-n" #'completion-preview-next-candidate)
  (keymap-set completion-preview-active-mode-map "M-p" #'completion-preview-prev-candidate)
  (keymap-set completion-preview-active-mode-map "M-i" #'completion-preview-insert))

;; ____________________________________________________________________________
;;|
;;| dired

(setq dired-auto-revert-buffer t
      dired-dwim-target t
      dired-hide-details-hide-symlink-targets nil
      dired-recursive-copies 'always
      dired-recursive-deletes 'always
      dired-create-destination-dirs 'ask
      dired-kill-when-opening-new-dired-buffer t)

(add-hook 'dired-load-hook (function (lambda ()
                                       (load "dired-x"))))
(add-hook 'dired-mode-hook (function (lambda ()
                                       (dired-hide-details-mode 1))))

(defun tay/window-dired-vc-root-left (&optional directory-path)
  "Creates *Dired-Side* like an IDE side explorer"
  (interactive)
  (add-hook 'dired-mode-hook 'dired-hide-details-mode)

  (let ((dir (if directory-path
                 (dired-noselect directory-path)
               (if (eq (vc-root-dir) nil)
                   (dired-noselect default-directory)
                 (dired-noselect (vc-root-dir))))))

    (display-buffer-in-side-window
     dir `((side . left)
           (slot . 0)
           (window-width . 30)
           (window-parameters . ((no-other-window . t)
                                 (no-delete-other-windows . t)
                                 (mode-line-format . (" "
                                                      "%b"))))))
    (with-current-buffer dir
      (let ((window (get-buffer-window dir)))
        (when window
          (select-window window)
          (rename-buffer "*Dired-Side*")
          )))))

(global-set-key (kbd "s-b") 'tay/window-dired-vc-root-left)

;; ____________________________________________________________________________
;;|
;;| eshell

(setq eshell-visual-commands nil)
(add-hook 'eshell-mode-hook 'visual-line-mode)
(add-hook 'eshell-mode-hook
	      (lambda ()
            (eshell/alias "ll" "ls -la")
	        (eshell/alias "python" "python3 $*")
	        (eshell/alias "pip" "pip3 $*")
	        (eshell/alias "clear" "clear 1")
            (setenv "TERM" "xterm-256color")
            (setq-local global-hl-line-mode nil)))

;; ____________________________________________________________________________
;;|
;;| isearch

(setq isearch-lazy-count t
      search-whitespace-regexp ".*?")

;; ____________________________________________________________________________
;;|
;;| electric pair

(electric-pair-mode)

;; ____________________________________________________________________________
;;|
;;| eldoc

(setq eldoc-echo-area-use-multiline-p nil)
(global-eldoc-mode)

;; ____________________________________________________________________________
;;|
;;| which-key
(which-key-mode 1)

;; ____________________________________________________________________________
;;|
;;| vc

(setq vc-auto-revert-mode t
      vc-dir-hide-up-to-date-on-revert t)

;; ____________________________________________________________________________
;;|
;;| diff

(setq diff-default-read-only t
      diff-advance-after-apply-hunk t
      diff-update-on-the-fly t
      diff-font-lock-syntax 'hunk-also
      diff-font-lock-prettify nil)

;; ____________________________________________________________________________
;;|
;;| ediff

(setq ediff-split-window-function 'split-window-horizontally
      ediff-window-setup-function 'ediff-setup-windows-plain
      ediff-keep-variants nil
      ediff-make-buffers-readonly-at-startup nil
      ediff-show-clashes-only t)

;; ____________________________________________________________________________
;;|
;;| eglot

(setq eglot-autoshutdown t)
(require 'eglot)
(setq eglot-autoshutdown t
      eglot-events-buffer-size 0
      eglot-events-buffer-config '(:size 0 :format full)
      eglot-prefer-plaintext nil
      jsonrpc-event-hook nil
      eglot-code-action-indications nil)

(add-hook 'go-ts-mode-hook 'eglot-ensure)
(add-hook 'typescript-ts-mode-hook 'eglot-ensure)
(add-hook 'tsx-ts-mode-hook 'eglot-ensure)
(add-hook 'python-ts-mode-hook 'eglot-ensure)

(define-key eglot-mode-map (kbd "M-i i") 'eglot-code-action-organize-imports)
(define-key eglot-mode-map (kbd "M-i e") 'flymake-show-buffer-diagnostics)
(define-key eglot-mode-map (kbd "M-i r") 'eglot-rename)
(define-key eglot-mode-map (kbd "M-[") 'flymake-goto-prev-error)
(define-key eglot-mode-map (kbd "M-]") 'flymake-goto-next-error)

;; ____________________________________________________________________________
;;|
;;| flymake

(setq flymake-indicator-type 'margins
      flymake-margin-indicators-string
      `((error "!" compilation-error)      
        (warning "?" compilation-warning)
        (note "i" compilation-info)))

(add-hook 'go-ts-mode-hook 'flymake-mode)
(add-hook 'typescript-ts-mode-hook 'flymake-mode)
(add-hook 'tsx-ts-mode-hook 'flymake-mode)
(add-hook 'python-ts-mode-hook 'flymake-mode)

;; ____________________________________________________________________________
;;|
;;| org

(setq org-agenda-files '("~/org/agenda.org" "~/org/calsync.org")
      org-todo-keywords '((sequence "TODO" "IN PROGRESS" "|" "DONE"))
      org-startup-truncated t
      org-startup-indented t
      org-startup-folded t
      org-insert-heading-respect-content t
      org-export-with-toc nil
      org-export-with-author nil
      org-export-time-stamp-file nil
      org-export-with-section-numbers nil
      org-html-validation-link nil
      org-html-head-extra (concat "<style>\n"
                                  (with-temp-buffer
                                    (insert-file-contents (expand-file-name "css/org-export.css" user-emacs-directory))
                                    (buffer-string))
                                  "\n</style>"))

(add-hook 'org-mode-hook 'visual-line-mode)

;; ____________________________________________________________________________
;;|
;;| golang

(add-hook 'go-ts-mode-hook
          (lambda ()
            (setq tab-width 4)
            (setq go-ts-mode-indent-offset 4)))

;; ____________________________________________________________________________
;;|
;;| javascript

(setq-default js-indent-level 2)
(setq-default typescript-indent-level 2)

;; ____________________________________________________________________________
;;|
;;| sql

(add-hook 'sql-mode-hook
          (lambda ()
            (setq tab-width 2)))
;; ____________________________________________________________________________
;;|
;;| outline

(add-to-list 'auto-mode-alist '("\\.txt\\'" . outline-mode))

;; ____________________________________________________________________________
;;|
;;| external packages

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(use-package fleetish-theme
  :ensure t
  :straight t
  :config
  (load-theme 'fleetish t))

(use-package exec-path-from-shell
  :ensure t
  :straight t
  :config
  (exec-path-from-shell-initialize))

(use-package avy
  :ensure t
  :straight t
  :bind
  (("C-;" . avy-goto-word-1)
   ("C-'" . avy-goto-line)))

(use-package expand-region
  :ensure t
  :straight t
  :bind
  (("C-=" . er/expand-region)))

(use-package gptel
  :ensure t
  :straight t
  :bind
  (("C-c RET" . gptel-send))
  :init
  (setq gptel-model "claude-sonnet-4-20250514")
  (setq gptel-backend (gptel-make-anthropic "Claude"
                        :stream t
                        :key claude-api-key
			            :models '(claude-sonnet-4-20250514)))
  (setq gptel-prompt-prefix-alist '((org-mode . "* PROMPT\n\n")))
  (setq gptel-response-prefix-alist '((org-mode . "* RESPONSE\n\n")))
  (setq gptel-default-mode 'org-mode)
  :config
  (add-hook 'gptel-post-stream-hook 'gptel-auto-scroll)
  (add-hook 'gptel-post-response-functions 'gptel-end-of-response))

(use-package treesit-auto
  :after eglot
  :straight t
  :demand t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode t))

(use-package vterm
  :ensure t
  :straight t)

(use-package claude-code-ide
  :straight (:type git :host github :repo "manzaltu/claude-code-ide.el")
  :bind ("C-c C-'" . claude-code-ide-menu) ; Set your favorite keybinding
  :init
  (setq claude-code-ide-terminal-backend 'vterm)
  :config
  (claude-code-ide-emacs-tools-setup))
