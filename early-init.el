;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  ______ ______  __  __  __    __  ______  ______  ______
;; /\__  _/\  __ \/\ \_\ \/\ "-./  \/\  __ \/\  ___\/\  ___\
;; \/_/\ \\ \  __ \ \____ \ \ \-./\ \ \  __ \ \ \___\ \___  \
;;    \ \_\\ \_\ \_\/\_____\ \_\ \ \_\ \_\ \_\ \_____\/\_____\
;;     \/_/ \/_/\/_/\/_____/\/_/  \/_/\/_/\/_/\/_____/\/_____/
;;
;; early
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ____________________________________________________________________________
;;|
;;| garbage collection

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

(add-hook 'after-init-hook
          (lambda ()
            (setq gc-cons-threshold (* 100 1024 1024)
                  gc-cons-percentage 0.1)))

(setq vc-handled-backends '(Git))

(setq frame-resize-pixelwise t
      frame-inhibit-implied-resize t
      frame-title-format
      '(:eval
        (let ((project (project-current)))
          (if project
              (concat "taymacs ("
                      (file-name-nondirectory (directory-file-name (project-root project)))
                      ")")
              (concat "taymacs " (buffer-name))))))

;; ____________________________________________________________________________
;;|
;;| ui elements

(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'tooltip-mode) (tooltip-mode -1))

;; ____________________________________________________________________________
;;|
;;| warnings

(setq warning-minimum-level :error)
(setq warning-suppress-types '((lexical-binding)))

;; ____________________________________________________________________________
;;|
;;| file loads

(setq load-prefer-newer noninteractive)

;; ____________________________________________________________________________
;;|
;;| transparent title bar macos

(defun ns-auto-titlebar-set-frame (frame &rest _)
  "Set ns-appearance frame parameter for FRAME to match its background-mode parameter."
  (when (display-graphic-p frame)
    (let ((mode (frame-parameter frame 'background-mode)))
      (modify-frame-parameters frame `((ns-transparent-titlebar . t) (ns-appearance . ,mode))))))

(defun ns-auto-titlebar-set-all-frames (&rest _)
  "Set ns-appearance frame parameter for all frames to match their background-mode parameter."
  (mapc 'ns-auto-titlebar-set-frame (frame-list)))

;; set a nice transparent titlebar for macos
(when (eq system-type 'darwin)
  (add-hook 'after-init-hook 'ns-auto-titlebar-set-all-frames)
  (add-hook 'after-make-frame-functions 'ns-auto-titlebar-set-frame)
  (advice-add 'frame-set-background-mode :after 'ns-auto-titlebar-set-frame))

;; ____________________________________________________________________________
;;|
;;| shell env

;; set up exec-path from shell early
(defun tay/set-exec-path-from-shell ()
  "set up emacs' `exec-path' and PATH environment from zsh."
  (interactive)
  (let* ((command "zsh -i -l -c 'echo $PATH'")
         (path-from-shell (replace-regexp-in-string
                           "[ \t\n]*$" ""
                           (shell-command-to-string command))))
        (when (and path-from-shell (not (string= path-from-shell "")))
          (setenv "PATH" path-from-shell)
          (setq exec-path (append (split-string path-from-shell path-separator) (list exec-directory)))
          (message "PATH loaded from zsh: %s" path-from-shell))))

(tay/set-exec-path-from-shell)
