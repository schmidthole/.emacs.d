(use-package evil
  :diminish
  :init
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-fine-undo t)
  :config
  (setq display-line-numbers-type 'relative)
  (add-hook 'prog-mode-hook 'display-line-numbers-mode)
  (add-hook 'markdown-mode-hook 'display-line-numbers-mode)
  (add-hook 'org-mode-hook 'display-line-numbers-mode)
  (evil-mode 1))

(use-package evil-collection
  :diminish
  :after evil
  :config
  (evil-collection-init)
  (evil-define-key 'normal 'dired-mode-map
                   "gk" 'dired-kill-subdir))

(use-package evil-goggles
  :defer 1.5
  :diminish
  :config
  (setq evil-goggles-duration 0.200)
  (evil-goggles-mode))

(provide 'tay-evil)
