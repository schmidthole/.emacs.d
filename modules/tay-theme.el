;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; THEME
;;
;; color scheme, modeline, titlebar
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(tay/do-if-ext-pkg
 ;; i really like the doom themes, they are well put together and dont really
 ;; fall short for any major modes
 (use-package doom-themes
   :config
   (setq doom-themes-enable-bold t
         doom-themes-enable-italic t)
   (load-theme 'doom-vibrant t)

   (doom-themes-visual-bell-config)
   (doom-themes-org-config))

 ;; another companion to doom themes that colors buffers different based
 ;; on mode
 (use-package solaire-mode
   :config
   (add-hook 'change-major-mode-hook 'turn-on-solaire-mode)
   (add-hook 'after-revert-hook 'turn-on-solaire-mode)
   (add-hook 'ediff-prepare-buffer-hook 'solaire-mode)
   (add-hook 'minibuffer-setup-hook 'solaire-mode-in-minibuffer)
   (setq solaire-mode-auto-swap-bg nil)

   (solaire-global-mode +1))

 ;; this modeline is minimal and easy to read. it also does the declutter
 ;; tasks for you
 (use-package telephone-line
   :config
   (setq telephone-line-primary-left-separator 'telephone-line-flat)
   (setq telephone-line-primary-right-separator 'telephone-line-flat)
   (setq telephone-line-secondary-left-separator 'telephone-line-flat)
   (setq telephone-line-secondary-right-separator 'telephone-line-flat)
   (telephone-line-mode 1))

 ;; actual real macos title bar that looks good
 (use-package ns-auto-titlebar
   :config
   (when (eq system-type 'darwin) (ns-auto-titlebar-mode))))
