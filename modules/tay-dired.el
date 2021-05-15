;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DIRED
;;
;; dired settings and packages
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq dired-auto-revert-buffer t
      dired-dwim-target t
      dired-hide-details-hide-symlink-targets nil
      dired-recursive-copies  'always
      dired-recursive-deletes 'always
      dired-create-destination-dirs 'ask)

(add-hook 'dired-load-hook (function (lambda () (load "dired-x"))))

(tay/do-if-ext-pkg
 (use-package diredfl
   :config
   (add-hook 'dired-mode-hook 'diredfl-mode)))
