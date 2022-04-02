(setq js-indent-level 2)

(use-package web-mode
  :defer t
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
  (setq js-indent-level 2)
  (setq web-mode-code-indent-offset 2)
  :mode
  (("\\.js\\'" . web-mode)
   ("\\.jsx\\'" .  web-mode)
   ("\\.tsx\\'" . web-mode)
   ("\\.html\\'" . web-mode)))

(use-package rjsx-mode
  :defer t
  :config
  (add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))
  (add-hook 'web-mode-hook
            (lambda ()
	          (when (string-equal "jsx" (file-name-extension buffer-file-name))
                (rjsx-mode)))))

(provide 'tay-web)
