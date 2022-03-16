(tay/do-if-ext-pkg
 (use-package exec-path-from-shell
   :diminish
   :defer 0.5
   :config
   (exec-path-from-shell-copy-env "PATH")))
