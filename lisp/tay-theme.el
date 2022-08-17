(use-package modus-themes
  :config
  (setq modus-themes-mode-line '(moody borderless))
  (setq modus-themes-syntax '(faint alt-syntax))
  (load-theme 'modus-vivendi t))

;; (require 'nord-theme)
;; (load-theme 'nord t)

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
