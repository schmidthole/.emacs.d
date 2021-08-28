;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; WEB
;;
;; web language settngs js, html, templating
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq js-indent-level 2)

(tay/do-if-ext-pkg
 ;; basically the standard for html, js, and templating for web stuff
 (use-package web-mode
   :config
   (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
   (add-to-list 'auto-mode-alist '("\\.css?\\'" . web-mode))
   (setq web-mode-enable-engine-detection t)
   (setq web-mode-markup-indent-offset 2)
   (setq web-mode-css-indent-offset 2)
   (setq web-mode-enable-css-colorization t)
   (setq web-mode-enable-auto-pairing t)
   (setq web-mode-enable-current-element-highlight t)
   (setq web-mode-enable-current-column-highlight t))

 (use-package js2-mode
   :config
   (setq js2-basic-indent 2
         js2-basic-offset 2
         js2-auto-indent-p t
         js2-cleanup-whitespace t
         js2-enter-indents-newline t
         js2-indent-on-enter-key t)

   (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode)))

 (use-package prettier-js
   :after js2-mode
   :config
   (add-hook 'js2-mode-hook 'prettier-js-mode)))
