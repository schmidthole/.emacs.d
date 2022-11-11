;; unset keys we don't want
(tay/bind-key "M-i" nil)
(tay/bind-key "C-z" nil)
(tay/bind-key "C-x C-z" nil)
(tay/bind-key "C-M-." nil)
(tay/bind-key "C-M-," nil)
(tay/bind-key "C-x C-r" nil)

(tay/bind-key "C-j" 'tay/vi-line-below)
(tay/bind-key "C-o" 'tay/vi-line-above)
(tay/bind-key "M-k" 'tay/kill-line)

(tay/bind-key "C-M-." 'next-buffer)
(tay/bind-key "C-M-," 'previous-buffer)
(tay/bind-key "C-x C-k" 'tay/kill-this-buffer)
(tay/bind-key "M-i k" 'tay/cleanup)

(tay/bind-key "M-i 0" 'toggle-frame-fullscreen)

(tay/bind-key "M-i g" 'magit)

(tay/bind-key "M-o" 'other-window)
(tay/bind-key "M-i s" 'split-window-below)
(tay/bind-key "M-i v" 'split-window-right)

(if tay/vterm
    (tay/bind-key "M-i t" 'tay/vterm-new)
  (progn
    (tay/bind-key "M-i t" 'tay/eshell-new)
    (tay/bind-key "M-i y" 'shell)))

(when tay/fancy-editor
  (tay/bind-key "C-=" 'er/expand-region))

(if tay/ivy
    (progn
      (tay/bind-key "M-i a" 'counsel-ag)
      (tay/bind-key "M-i p" 'counsel-git)
      (tay/bind-key "C-x C-r" 'counsel-recentf)
      ;; (tay/bind-key "C-s" 'swiper)
      ;; (tay/bind-key "C-r" 'swiper-backward)
      ;; (tay/bind-key "C-;" 'avy-goto-char)
      (tay/bind-key "C-'" 'avy-goto-line)
      ;; (tay/bind-key "M-i r" 'anzu-query-replace-regexp)
      ;; (tay/bind-key "C-M-r" 'anzu-query-replace-at-cursor-thing))
      )
  (progn
    (tay/bind-key "M-i a" 'tay/git-grep)
    (tay/bind-key "M-i o" 'occur)
    (tay/bind-key "M-i p" 'project-find-file)))

(tay/bind-key-map isearch-mode-map "C-o" 'isearch-occur)

(tay/bind-key "M-i c" 'org-capture)

(tay/bind-key "M-i i" 'tay/echo-info)

(tay/bind-key "M-i m" 'modus-themes-toggle)

(tay/bind-key "M-i n" (lambda () (interactive) (find-file "~/notes/notes.org")))

(provide 'tay-keybindings)
