;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EVIL
;;
;; - evil mode support
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tay/do-if-ext-pkg
 (use-package evil
   :init
   (setq evil-want-keybinding nil)
   (setq evil-want-C-u-scroll t)
   (setq evil-want-fine-undo t)
   :config
   (setq display-line-numbers-type 'relative)
   (add-hook 'prog-mode-hook 'display-line-numbers-mode)
   (evil-mode 1))

 (use-package evil-collection
   :after evil
   :config
   (evil-collection-init)
   (evil-define-key 'normal 'dired-mode-map
     "gk" 'dired-kill-subdir))

 (use-package general
   :after evil
   :config
   (general-define-key
    :states 'normal
    :keymaps 'override
    :prefix "SPC"
    "b" 'switch-to-buffer
    "k" 'tay/kill-this-buffer
    "x" 'execute-extended-command
    "f" 'find-file
    "g" 'magit-status
    "e" 'tay/eshell-new
    "s" 'vc-git-grep
    "c c" 'tay/cleanup
    ;; "c o a" 'org-agenda
    ;; "c o c" 'org-capture
    ;; "1" 'delete-other-windows
    ;; "0" 'delete-window
    ;; "2" 'split-window-below
    ;; "3" 'split-window-right
    ;; "o" 'other-window
    "a s" 'counsel-ag
    "a a" 'ag
    "a f" 'ag-dired
    "w" 'save-buffer))

 (use-package evil-goggles
   :config
   (setq evil-goggles-duration 0.100)
   (evil-goggles-mode)))
