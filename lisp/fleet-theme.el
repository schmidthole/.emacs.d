;;; fleet-theme.el --- Fleet theme using modus-themes overrides -*- lexical-binding: t; -*-

;;; Commentary:
;; Fleet theme colors adapted for modus-themes palette overrides

;;; Code:

(use-package modus-themes
  :ensure nil
  :defer t
  :custom
  (modus-themes-italic-constructs t)
  (modus-themes-bold-constructs t)
  (modus-themes-mixed-fonts nil)
  (modus-themes-prompts '(bold intense))
  (modus-themes-common-palette-overrides
   `((bg-main "#0D0D0D")
     (bg-active "#1C1C1C")
     (fg-main "#EFEFEF")
     (fg-active "#FFFFFF")
     (fg-mode-line-active "#EFEFEF")
     (bg-mode-line-active "#1C1C1C")
     (fg-mode-line-inactive "#5D5D5D")
     (bg-mode-line-inactive "#1C1C1C")
     (border-mode-line-active nil)
     (border-mode-line-inactive nil)
     (bg-tab-bar "#0D0D0D")
     (bg-tab-current bg-main)
     (bg-tab-other "#0D0D0D")
     (fg-prompt "#ECA775")
     (bg-prompt "#0D0D0D")
     (bg-hover-secondary "#353535")
     (bg-completion "#163764")
     (fg-completion "#EFEFEF")
     (bg-region "#2A2A2A")
     (fg-region unspecified)

     (fg-line-number-active "#D1D1D1")
     (fg-line-number-inactive "#5D5D5D")
     (bg-line-number-active "#1C1C1C")
     (bg-line-number-inactive "#0D0D0D")
     (fringe "#0D0D0D")

     (fg-heading-0 "#94C1FA")
     (fg-heading-1 "#94C1FA")
     (fg-heading-2 "#AC9CF9")
     (fg-heading-3 "#D898D8")
     (fg-heading-4 "#94C1FA")

     (fg-prose-verbatim "#D898D8")
     (bg-prose-block-contents "#1C1C1C")
     (fg-prose-block-delimiter "#898989")
     (bg-prose-block-delimiter bg-prose-block-contents)

     (accent-1 "#78D0BD")

     (keyword "#78D0BD")
     (builtin "#78D0BD")
     (comment "#898989")
     (string "#D898D8")
     (fnname "#94C1FA")
     (type "#94C1FA")
     (variable "#EFEFEF")
     (docstring "#898989")
     (constant "#E5C995")
     
     (cursor "#FFFFFF")))
  :config
  (modus-themes-with-colors
    (custom-set-faces
     `(tab-bar ((,c :background "#0D0D0D" :foreground "#EFEFEF")))
     `(tab-bar-tab ((,c :background "#0D0D0D" :underline t)))
     `(tab-bar-tab-group-current ((,c :background "#0D0D0D" :foreground "#EFEFEF" :underline t)))
     `(tab-bar-tab-group-inactive ((,c :background "#0D0D0D" :foreground "#5D5D5D")))
     `(icomplete-first-match ((,c :foreground "#78D0BD" :weight bold)))
     `(completions-highlight ((,c :background "#163764" :foreground "#EFEFEF")))
     `(completions-common-part ((,c :foreground "#78D0BD")))
     `(completions-first-difference ((,c :foreground "#D898D8")))))
  :init
  (load-theme 'modus-vivendi-tinted t))

(provide 'fleet-theme)

;;; fleet-theme.el ends here
