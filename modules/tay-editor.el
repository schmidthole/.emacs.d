;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EDITOR
;;
;; - smartparens
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(tay/do-if-ext-pkg
 ;; this package makes parens and pairs in general behave like fancier editors
 ;; most of the customization is ripped from doom emacs
 (use-package smartparens
   :config
   (let ((unless-list '(sp-point-before-word-p
                        sp-point-after-word-p
                        sp-point-before-same-p)))
     (sp-pair "'"  nil :unless unless-list)
     (sp-pair "\"" nil :unless unless-list))

   (dolist (brace '("(" "{" "["))
     (sp-pair brace nil
              :post-handlers '(("||\n[i]" "RET") ("| " "SPC"))
              :unless '(sp-point-before-word-p sp-point-before-same-p)))

   (sp-local-pair sp-lisp-modes "(" ")" :unless '(:rem sp-point-before-same-p))

   (require 'smartparens-config)
   (smartparens-global-mode)))
