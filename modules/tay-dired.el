;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DIRED
;;
;; dired settings and packages
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; we basically dont want to have to do too much confirmation with dired
;; so try to just do what is asked. we are also using dwim to copy/move between
;; splits
(setq dired-auto-revert-buffer t
      dired-dwim-target t
      dired-hide-details-hide-symlink-targets nil
      dired-recursive-copies  'always
      dired-recursive-deletes 'always
      dired-create-destination-dirs 'ask)

(add-hook 'dired-load-hook (function (lambda () (load "dired-x"))))

(tay/do-if-ext-pkg
 ;; this is a great package that makes dired easier to read
 (use-package diredfl
   :defer 1.2
   :config
   (add-hook 'dired-mode-hook 'diredfl-mode)))
