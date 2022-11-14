(use-package modus-themes
  :config
  (setq modus-themes-syntax '(faint))
  (setq modus-themes-italic-constructs t)
  (setq modus-themes-completions '((selection . (intense accented background))))
  (setq modus-themes-headings '((t . (background bold))))
  (setq modus-themes-mode-line '(borderless moody))
  (setq modus-themes-prompts '(background intense))
  (setq modus-themes-lang-checkers '(straight-underline background))
  (setq modus-themes-subtle-line-numbers t)
  (load-theme 'modus-vivendi t))

(use-package doom-modeline
  :config
  (setq doom-modeline-height 20)
  (setq doom-modeline-buffer-encoding nil)
  (setq doom-modeline-bar-width 2)
  (setq doom-modeline-percent-position nil)
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
