 (use-package flycheck
   :defer 1.0
   :diminish
   :config
   (add-hook 'clojure-mode-hook #'flycheck-mode))

(use-package flycheck-inline
  :hook (flycheck-mode . turn-on-flycheck-inline))

;; (use-package flycheck-clj-kondo
;;   :defer t)

;; (use-package clojure-mode
;;   :config
;;   (require 'flycheck-clj-kondo))

(provide 'tay-flycheck)
