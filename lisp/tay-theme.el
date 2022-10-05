(use-package modus-themes
  :config
  (setq modus-themes-syntax '(faint alt-syntax))
  (setq modus-themes-fringes '(faint))
  (setq modus-themes-completions '((selection . (intense accented background))))
  (setq modus-themes-headings '((t . (background bold))))
  (setq modus-themes-mode-line '(borderless moody))
  (setq modus-themes-prompts '(background intense))
  (load-theme 'modus-operandi t))

;; (use-package ef-themes
;;   :config
;;   (load-theme 'ef-light :no-confirm))

;; (use-package telephone-line
;;   :defer 0.1
;;   :config
;;   (setq telephone-line-primary-left-separator 'telephone-line-flat)
;;   (setq telephone-line-secondary-left-separator 'telephone-line-flat)
;;   (setq telephone-line-primary-right-separator 'telephone-line-flat)
;;   (setq telephone-line-secondary-right-separator 'telephone-line-flat)
;;   (telephone-line-mode 1))

(use-package moody
  :config
  (setq x-underline-at-descent-line t)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode)
  (moody-replace-eldoc-minibuffer-message-function))

(use-package ns-auto-titlebar
  :config
  (when (eq system-type 'darwin) (ns-auto-titlebar-mode)))

(use-package diminish)

(provide 'tay-theme)
