;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CLOJURE
;;
;; Packages for clojure dev
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(tay/do-if-ext-pkg
 (use-package cider
   :defer
   :config
   (setq cider-repl-display-help-banner nil))

 (use-package rainbow-delimiters
   :defer
   :config
   (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)))
