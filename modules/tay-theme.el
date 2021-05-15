;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; THEME
;;
;; color scheme, modeline, titlebar
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(tay/do-if-ext-pkg
 (use-package doom-themes
   :config
   (setq doom-themes-enable-bold t
         doom-themes-enable-italic t)
   (load-theme 'doom-vibrant t)
   (doom-themes-visual-bell-config)
   (doom-themes-org-config))

 (use-package solaire-mode
   :config
   (add-hook 'change-major-mode-hook 'turn-on-solaire-mode)
   (add-hook 'after-revert-hook 'turn-on-solaire-mode)
   (add-hook 'ediff-prepare-buffer-hook 'solaire-mode)
   (add-hook 'minibuffer-setup-hook 'solaire-mode-in-minibuffer)
   (setq solaire-mode-auto-swap-bg nil)

   (solaire-global-mode +1))

 (use-package telephone-line
   :config
   (setq telephone-line-primary-left-separator 'telephone-line-flat)
   (setq telephone-line-primary-right-separator 'telephone-line-flat)
   (setq telephone-line-secondary-left-separator 'telephone-line-flat)
   (setq telephone-line-secondary-right-separator 'telephone-line-flat)
   (telephone-line-mode 1))

 (use-package ns-auto-titlebar
   :config
   (when (eq system-type 'darwin) (ns-auto-titlebar-mode))))
