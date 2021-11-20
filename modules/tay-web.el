(setq js-indent-level 2)

(tay/do-if-ext-pkg
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
   (setq web-mode-enable-current-column-highlight t)

   :mode (("\\.js\\'" . web-mode)
	      ("\\.jsx\\'" .  web-mode)
	      ("\\.ts\\'" . web-mode)
	      ("\\.tsx\\'" . web-mode)
	      ("\\.html\\'" . web-mode)))

 (use-package lsp-mode
   :hook (web-mode . lsp-deferred))

 (use-package rjsx-mode
   :config
   (add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))
   (add-hook 'web-mode-hook
             (lambda ()
	           (when (string-equal "jsx" (file-name-extension buffer-file-name))
                 (rjsx-mode))))))
