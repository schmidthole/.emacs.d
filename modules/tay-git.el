;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GIT
;;
;; git tools
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; self-explanatory.... use magit for git
(tay/do-if-ext-pkg
 (use-package magit
   :defer t)

 ;; (use-package git-gutter
 ;;   :config
 ;;   (global-git-gutter-mode t))
 )
