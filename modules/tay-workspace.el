;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; WORKSPACES
;;
;; utils for managing multiple workspaces
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tay/do-if-ext-pkg
 ;; so good for keeping project buffers segrated so we dont lose our mind and
 ;; keep the adhd in check
 (use-package perspective
  :config
  (persp-mode)))
