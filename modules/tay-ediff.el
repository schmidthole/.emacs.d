;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EDIFF
;;
;; custom ediff settings
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; we want our ediff windows to be side by side and without the stupid little
;; extra popup frame
(setq ediff-window-setup-function 'ediff-setup-windows-plain
      ediff-split-window-function 'split-window-horizontally
      ediff-diff-options "-w")

(tay/do-if-ext-pkg
 (use-package ztree
   :config
   (setq ztree-diff-additional-options '("-w" "-i"))))
