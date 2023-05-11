;; unset keys we don't want
(tay/bind-key "M-i" nil)
(tay/bind-key "C-z" nil)
(tay/bind-key "C-x C-z" nil)
(tay/bind-key "C-M-." nil)
(tay/bind-key "C-M-," nil)
(tay/bind-key "C-x C-r" nil)
(tay/bind-key "M-l" nil)
(tay/bind-key "M-c" nil)

(tay/bind-key "C-j" 'tay/vi-line-below)
(tay/bind-key "C-o" 'tay/vi-line-above)
(tay/bind-key "M-k" 'tay/kill-line)
(tay/bind-key "C-," 'tay/go-to-char-backward)
(tay/bind-key "C-." 'tay/go-to-char-forward)

(tay/bind-key "C-x C-k" 'tay/kill-this-buffer)
(tay/bind-key "C-x C-r" 'recentf)
(tay/bind-key "M-i k" 'tay/cleanup)

(tay/bind-key "M-i 0" 'toggle-frame-fullscreen)

(tay/bind-key "M-i m" 'modus-themes-toggle)

(tay/bind-key "M-o" 'other-window)
(tay/bind-key "M-i s" 'split-window-below)
(tay/bind-key "M-i v" 'split-window-right)

(tay/bind-key "M-i t" 'tay/eshell-new)
(tay/bind-key "M-i y" 'shell)

(tay/bind-key "C-=" 'er/expand-region)

(tay/bind-key "M-i n" (lambda () (interactive) (find-file "~/notes/notes.org")))

(provide 'tay-keybindings)
