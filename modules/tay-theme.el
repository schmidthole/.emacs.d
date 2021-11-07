;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; THEME
;;
;; color scheme, modeline, titlebar
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(tay/do-if-ext-pkg
 ;; (use-package doom-themes
 ;;   :config
 ;;   (load-theme 'doom-solarized-dark t))

 (use-package solaire-mode
   :config
   (solaire-global-mode +1))

 (use-package modus-themes
   :config
   (setq modus-themes-bold-constructs t)
   (setq modus-themes-slanted-constructs t)
   (setq modus-themes-prompts '(background intense bold))
   ;; (setq modus-themes-mode-line 'accented)
   (setq modus-themes-fringes nil)
   (load-theme 'modus-vivendi t))

 ;; (load-theme 'leuven t)

 ;; this modeline is minimal and easy to read. it also does the declutter
 ;; tasks for you
 (use-package telephone-line
   :config
   (setq telephone-line-primary-left-separator 'telephone-line-flat)
   (setq telephone-line-primary-right-separator 'telephone-line-flat)
   (setq telephone-line-secondary-left-separator 'telephone-line-flat)
   (setq telephone-line-secondary-right-separator 'telephone-line-flat)
   (telephone-line-mode 1))

 ;; (use-package doom-modeline
 ;;   :config
 ;;   ;; this is the min value for the default font size
 ;;   (setq doom-modeline-height 24)
 ;;   (setq doom-modeline-buffer-encoding nil)
 ;;   (doom-modeline-mode 1))

 ;; actual real macos title bar that looks good
 (use-package ns-auto-titlebar
   :config
   (when (eq system-type 'darwin) (ns-auto-titlebar-mode))))
