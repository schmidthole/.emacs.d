;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FONT
;;
;; facilities for setting up Fira Code font with ligatures and symbols
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (window-system)
  (set-frame-font "Fira Code"))

(tay/do-if-ext-pkg
 (use-package fira-code-mode
   :config
   (global-fira-code-mode)))
