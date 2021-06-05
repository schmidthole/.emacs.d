;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IVY
;;
;; ivy/counsel/swiper/avy
;;
;; i realize that everything here is not ivy related but most of the packages
;; are made by the same person and generally allow us to get around better
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tay/do-if-ext-pkg
 ;; ivy/counsel are excellent for selecting from a list of candidates, whether
 ;; it is switching buffers or M-x ing around
 (use-package ivy
  :config
  (setq ivy-sort-max-size 7500)
  (setq ivy-height 17
        ivy-wrap t
        ivy-fixed-height-minibuffer t
        ivy-read-action-format-function #'ivy-read-action-format-columns
        ivy-on-del-error-function #'ignore
        ivy-use-selectable-prompt t
        ivy-count-format "(%d/%d) "
        ivy-display-style 'fancy
        ivy-initial-inputs-alist nil)
  (ivy-mode 1))

(use-package counsel
  :after ivy
  :config
  (counsel-mode))

;; we replace isearch with swiper
(use-package swiper
  :config
  (setq swiper-goto-start-of-match t))

;; ivy rich provides some more info and color to selection buffers
(use-package ivy-rich
  :after counsel
  :config
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
  (setq ivy-rich-parse-remote-buffer nil)
  (setq ivy-rich-path-style 'abbrev)
  (ivy-rich-mode 1))

;; (use-package ivy-posframe
;;   :config
;;   (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-center)))
;;   (ivy-posframe-mode 1))

;; avy allows us to easily jump around buffers by character, line, or whatevs
(use-package avy
  :defer 0.4)

;; adds ivy functionality to any lsp stuff
(use-package lsp-ivy
  :defer
  :commands lsp-ivy-workspace-symbol))
