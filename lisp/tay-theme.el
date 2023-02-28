;; (use-package modus-themes
;;   :config
;;   ;; (setq modus-themes-syntax '(faint))
;;   ;; (setq modus-themes-italic-constructs t)
;;   (setq modus-themes-completions '((selection . (intense accented background))))
;;   (setq modus-themes-headings '((t . (background bold rainbow))))
;;   (setq modus-themes-mode-line '(borderless moody))
;;   (setq modus-themes-prompts '(background intense))
;;   (setq modus-themes-lang-checkers '(straight-underline background))
;;   (setq modus-themes-subtle-line-numbers t)
;;   (load-theme 'modus-vivendi t))

(use-package doom-themes
  :config
  (load-theme 'doom-dracula t))

(use-package solaire-mode
  :config
  (solaire-global-mode +1))

(use-package doom-modeline
  :config
  (setq doom-modeline-height 20)
  (setq doom-modeline-buffer-encoding nil)
  (setq doom-modeline-bar-width 2)
  (setq doom-modeline-percent-position nil)
  (setq doom-modeline-lsp nil)
  (setq doom-modeline-buffer-modification-icon nil)
  (setq doom-modeline-repl nil)
  (setq doom-modeline-env-version nil)
  (doom-modeline-def-modeline 'tay/doom-modeline
    '(bar matches follow buffer-info buffer-position remote-host selection-info)
    '(compilation major-mode process))
  (add-hook 'doom-modeline-mode-hook
            (lambda () (doom-modeline-set-modeline 'tay/doom-modeline 'default)))
  (doom-modeline-mode 1))

;; (use-package moody
;;   :config
;;   (setq x-underline-at-descent-line t)
;;   (moody-replace-mode-line-buffer-identification)
;;   (moody-replace-vc-mode)
;;   (moody-replace-eldoc-minibuffer-message-function))

(use-package ns-auto-titlebar
  :config
  (when (eq system-type 'darwin) (ns-auto-titlebar-mode)))

(use-package diminish)

(provide 'tay-theme)
