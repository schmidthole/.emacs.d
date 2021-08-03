;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EDITOR
;;
;; - smartparens
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(tay/do-if-ext-pkg
 ;; this package makes parens and pairs in general behave like fancier editors
 ;; most of the customization is ripped from doom emacs
 ;; (use-package smartparens
 ;;   :config
 ;;   (smartparens-global-mode))

 ;; vim like object selection
 (use-package expand-region)

 ;; keep things indented and aligned
 ;; (use-package aggressive-indent
 ;;   :config
 ;;   (global-aggressive-indent-mode 1)))

 ;; (use-package volatile-highlights
 ;;   :config
 ;;   (vhl/define-extension 'evil 'evil-paste-after 'evil-paste-before
 ;;                         'evil-paste-pop 'evil-move)
 ;;   (vhl/install-extension 'evil)
 ;;   (volatile-highlights-mode t))
)
