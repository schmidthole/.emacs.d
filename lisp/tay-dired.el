(setq dired-auto-revert-buffer t)
(setq  dired-dwim-target t)
(setq dired-hide-details-hide-symlink-targets nil)
(setq dired-recursive-copies  'always)
(setq dired-recursive-deletes 'always)
(setq dired-create-destination-dirs 'ask)

(add-hook 'dired-load-hook (function (lambda ()
                                       (load "dired-x"))))

(provide 'tay-dired)
