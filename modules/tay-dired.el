(setq dired-auto-revert-buffer t
      dired-dwim-target t
      dired-hide-details-hide-symlink-targets nil
      dired-recursive-copies  'always
      dired-recursive-deletes 'always
      dired-create-destination-dirs 'ask)

(add-hook 'dired-load-hook (function (lambda ()
                                       (load "dired-x")
                                       (dired-hide-details-mode))))

(tay/do-if-ext-pkg
 ;; this is a great package that makes dired easier to read
 (use-package diredfl
   :defer 1.2
   :config
   (add-hook 'dired-mode-hook 'diredfl-mode)))
