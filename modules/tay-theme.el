(tay/do-if-ext-pkg
 (use-package modus-themes
   :config
   (setq modus-themes-bold-constructs t)
   (setq modus-themes-slanted-constructs t)
   (setq modus-themes-prompts '(background intense bold))
   (setq modus-themes-fringes nil)
   (load-theme 'modus-vivendi t))

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
