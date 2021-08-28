;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PYTHON
;;
;; python language settings
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; basic python settings
(setq python-indent-offset 4
      python-shell-completion-native-enable nil
      python-shell-interpreter "/Library/Frameworks/Python.framework/Versions/3.7/bin/python3")

(tay/do-if-ext-pkg
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
