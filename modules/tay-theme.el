(tay/do-if-ext-pkg
 ;; (use-package modus-themes
 ;;   :config
 ;;   (setq modus-themes-bold-constructs t)
 ;;   (setq modus-themes-slanted-constructs t)
 ;;   (setq modus-themes-prompts '(background intense bold))
 ;;   (setq modus-themes-paren-match '(bold intense))
 ;;   (setq modus-themes-headings '((t . (rainbow overline))))
 ;;   (setq modus-themes-fringes nil)
 ;;   (load-theme 'modus-vivendi t))

 ;; (use-package doom-themes
 ;;   :config
 ;;   (load-theme 'doom-one t))

 ;; (use-package doom-modeline
 ;;   :init
 ;;   (doom-modeline-mode 1)
 ;;   :config
 ;;   (setq doom-modeline-height 8)
 ;;   (setq doom-modeline-bar-width 16)
 ;;   (setq doom-modeline-major-mode-icon nil)
 ;;   (setq doom-modeline-buffer-state-icon nil)
 ;;   (setq doom-modeline-buffer-encoding nil))

 (use-package nord-theme
   :config
   (load-theme 'nord t))

 (use-package telephone-line
   :config
   (setq telephone-line-primary-left-separator 'telephone-line-flat)
   (setq telephone-line-secondary-left-separator 'telephone-line-flat)
   (setq telephone-line-primary-right-separator 'telephone-line-flat)
   (setq telephone-line-secondary-right-separator 'telephone-line-flat)
   (telephone-line-mode 1))

 (use-package ns-auto-titlebar
   :config
   (when (eq system-type 'darwin) (ns-auto-titlebar-mode)))

 (use-package diminish))
