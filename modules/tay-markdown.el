;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MARKDOWN
;;
;; md file editing
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tay/do-if-ext-pkg
 (use-package markdown-mode
   :config
   (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))))
