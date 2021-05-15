;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IVY
;;
;; ivy/counsel/swiper/avy
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tay/do-if-ext-pkg
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

(use-package swiper
  :config
  (setq swiper-goto-start-of-match t))

(use-package ivy-rich
  :after counsel
  :config
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
  (setq ivy-rich-parse-remote-buffer nil)
  (setq ivy-rich-path-style 'abbrev)
  (ivy-rich-mode 1))


(use-package avy
  :defer 0.2)

(use-package lsp-ivy
  :defer t
  :commands lsp-ivy-workspace-symbol))
