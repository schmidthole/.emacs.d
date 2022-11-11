(setq gc-cons-threshold (* 50 1000 1000))

(setq load-prefer-newer noninteractive)

;; get rid of ui elements immediately so they don't linger
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)

;; disable any sort of startup messaging
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message user-login-name)
(setq initial-major-mode 'org-mode)
(setq initial-scratch-message nil)

;; setup the user's custom settings file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

;; load the directory that contains all custom modules
(setq tay/lisp-modules (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path tay/lisp-modules)
