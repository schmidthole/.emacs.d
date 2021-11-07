;;
;; spotify emacs app via smudge
;;

(tay/do-if-ext-pkg
 (use-package smudge
   :config
   (setq smudge-oauth2-client-secret "65d7a1b1ea8b4f4fb7f18462febff6b5")
   (setq smudge-oauth2-client-id "723c3d0ca0fa4e4a84a151310618599b")
   (setq smudge-transport 'connect)
   (setq smudge-status-location 'title-bar)
   (define-key smudge-mode-map (kbd "M-i .") 'smudge-command-map)
   (global-smudge-remote-mode)))
