(setq dired-auto-revert-buffer t)
(setq  dired-dwim-target t)
(setq dired-hide-details-hide-symlink-targets nil)
(setq dired-recursive-copies  'always)
(setq dired-recursive-deletes 'always)
(setq dired-create-destination-dirs 'ask)

(add-hook 'dired-load-hook (function (lambda ()
                                       (load "dired-x"))))
(use-package dired-subtree
  :after dired
  :config
  (bind-key "<tab>" #'dired-subtree-toggle dired-mode-map)
  (bind-key "<backtab>" #'dired-subtree-cycle dired-mode-map))

(provide 'tay-dired)
