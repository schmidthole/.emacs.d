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
 ;; blacken auto formats our buffer after save
 (use-package blacken
  :init
  (setq blacken-line-length 100)
  (add-hook 'python-mode-hook 'blacken-mode)))
