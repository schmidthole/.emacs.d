(setq gc-cons-threshold (* 50 1000 1000))

(setq load-prefer-newer noninteractive)

;; These functions are taken from the ns-auto-titlebar package
;; The functionality was so simple that it didn't need to be pulled in with use-package
(defun ns-auto-titlebar-set-frame (frame &rest _)
  "Set ns-appearance frame parameter for FRAME to match its background-mode parameter."
  (when (display-graphic-p frame)
    (let ((mode (frame-parameter frame 'background-mode)))
      (modify-frame-parameters frame `((ns-transparent-titlebar . t) (ns-appearance . ,mode))))))

(defun ns-auto-titlebar-set-all-frames (&rest _)
  "Set ns-appearance frame parameter for all frames to match their background-mode parameter."
  (mapc 'ns-auto-titlebar-set-frame (frame-list)))

;; Set a nice transparent titlebar for macos
(when (eq system-type 'darwin)
  (add-hook 'after-init-hook 'ns-auto-titlebar-set-all-frames)
  (add-hook 'after-make-frame-functions 'ns-auto-titlebar-set-frame)
  (advice-add 'frame-set-background-mode :after 'ns-auto-titlebar-set-frame))

;; get rid of ui elements immediately so they don't linger
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)

;; disable any sort of startup messaging
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message user-login-name)
(setq initial-major-mode 'fundamental-mode)
(setq initial-scratch-message nil)

;; setup the user's custom settings file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

;; load the directory that contains all custom modules
(setq tay/lisp-modules (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path tay/lisp-modules)
