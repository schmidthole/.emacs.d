;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CLOJURE
;;
;; Packages for clojure dev
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(tay/do-if-ext-pkg
 (use-package cider
   :config
   (setq cider-repl-display-help-banner nil))

 (use-package rainbow-delimiters
   :config
   (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)))
