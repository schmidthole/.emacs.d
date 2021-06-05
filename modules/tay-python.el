;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PYTHON
;;
;; python language settings
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; basic python settings
(setq python-indent-offset 4
      python-shell-completion-native-enable nil)

(tay/do-if-ext-pkg
 ;; i've tried lsp-mode for python and it just is not that great. I find that
 ;; using flycheck with flake8 and anaconda mode for refs/documentation
 ;; is almost 100% of what is needed
 (use-package flycheck
   :defer
   :init
   (setq-default flycheck-disabled-checkers '(python-pylint))
   (add-hook 'python-mode-hook 'flycheck-mode))

 (use-package anaconda-mode
   :defer
   :init
   (setq anaconda-mode-localhost-address "localhost")
   (add-hook 'python-mode-hook 'anaconda-mode)
   (add-hook 'python-mode-hook 'anaconda-eldoc-mode))

 ;; blacken auto formats our buffer after save
 (use-package blacken
   :defer
   :init
   (setq blacken-line-length 100)
   (add-hook 'python-mode-hook 'blacken-mode)))
