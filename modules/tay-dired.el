(setq dired-auto-revert-buffer t
      dired-dwim-target t
      dired-hide-details-hide-symlink-targets nil
      dired-recursive-copies  'always
      dired-recursive-deletes 'always
      dired-create-destination-dirs 'ask)

(add-hook 'dired-load-hook (function (lambda ()
                                       (load "dired-x"))))
