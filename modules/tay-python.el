;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PYTHON
;;
;; python language settings
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq python-indent-offset 4
      python-shell-completion-native-enable nil)

(tay/do-if-ext-pkg
 (use-package blacken
  :init
  (setq blacken-line-length 100)
  (add-hook 'python-mode-hook 'blacken-mode)))
