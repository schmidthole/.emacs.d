(tay/do-if-ext-pkg
 ;; (use-package smartparens
 ;;   :config
 ;;   (smartparens-global-mode))

 (use-package expand-region)

 (use-package smex)

 (use-package mwim)

 (use-package volatile-highlights
   :config
   (vhl/define-extension 'evil 'evil-paste-after 'evil-paste-before
                         'evil-paste-pop 'evil-move)
   (vhl/install-extension 'evil)
   (volatile-highlights-mode t))

 (use-package rainbow-delimiters
   :config
   (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)))
