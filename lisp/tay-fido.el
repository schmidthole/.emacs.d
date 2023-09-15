
(add-hook
 'icomplete-minibuffer-setup-hook
 (lambda () (setq-local completion-styles '(initials flex))))

(fido-vertical-mode 1)

(provide 'tay-fido)
