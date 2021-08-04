;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; THEME
;;
;; color scheme, modeline, titlebar
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(tay/do-if-ext-pkg
 ;; (use-package doom-themes
 ;;   :config
 ;;   (setq doom-challenger-deep-brighter-modeline t)
 ;;   (load-theme 'doom-challenger-deep t))

 (use-package modus-themes
   :config
   (load-theme 'modus-operandi t))

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
