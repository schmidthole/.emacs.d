;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; THEME
;;
;; color scheme, modeline, titlebar
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(tay/do-if-ext-pkg
 ;; light and dark modus themes are what I keep coming back to for some reason
 (use-package modus-themes
  :init
  (setq modus-themes-slanted-constructs t
        modus-themes-bold-constructs t
        modus-themes-region 'no-extend
        modus-themes-paren-match 'intense-bold
        modus-themes-completions 'opinionated
        modus-themes-prompts 'intense-accented
        modus-themes-mode-line 'accented)

  (modus-themes-load-themes)
  :config
  (modus-themes-load-vivendi))


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
