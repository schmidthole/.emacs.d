;; (setq modus-themes-completions '((selection . (intense accented background))))
;; (setq modus-themes-headings '((t . (background bold rainbow))))
;; (setq modus-themes-mode-line '(borderless moody))
;; (setq modus-themes-prompts '(background intense))
;; (setq modus-themes-lang-checkers '(straight-underline background))
;; (setq modus-themes-subtle-line-numbers t)
;; (load-theme 'modus-vivendi t)

(use-package ef-themes
  :config
  (setq ef-themes-variable-pitch-ui nil)
  (setq ef-themes-to-toggle '(ef-dark ef-light))
  (load-theme 'ef-dark))

(use-package diminish)

(provide 'tay-theme)
