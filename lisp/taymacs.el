;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; taymacs functions
;;
;; all the functions that make taymacs work
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun tay/bind-key (keychord func)
  "simply setup a global keybinding"
  (global-set-key (kbd keychord) func))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EXTERNAL PACKAGES
;;
;; Setup external package management. taymacs uses elpa/melpa and use-package
;; for management
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun tay/enable-ext-pkg ()
  "setup elpa/melpa and use-package for management of external modules"
  (require 'package)
  (setq package-check-signature nil)
  (add-to-list 'package-archives
	           '("melpa" . "https://melpa.org/packages/") t)
  (package-initialize)

  (unless package-archive-contents
    (package-refresh-contents))

  ;; setup use-package
  (unless (package-installed-p 'use-package)
    (package-install 'use-package))
  (require 'use-package)
  (setq use-package-verbose t)
  (setq use-package-always-ensure t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MOVEMENT
;;
;; custom opinionated buffer editing functions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun tay/vi-line-below ()
  "make a newline below the current one and move there. this will not break
apart lines like the standard emacs binding"
  (interactive)
  (progn
    (move-end-of-line 1)
    (newline-and-indent)))

(defun tay/vi-line-above ()
  "make a newline above the current one and move there. this will not break apart
line like the standard emacs binding"
  (interactive)
  (progn
    (previous-line)
    (tay/vi-line-below)))

(defun tay/up-chunk ()
  "moves up 20 lines at a time"
  (interactive)
  (previous-line 20))

(defun tay/down-chunk ()
  "moves down 20 lines at a time"
  (interactive)
  (next-line 20))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EDITING
;;
;; custom editing functions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun tay/kill-line ()
  "kill the line regardless of cursor position and move to next line"
  (interactive)
  (beginning-of-line)
  (kill-line)
  (let ((line-contents (buffer-substring-no-properties
                        (line-beginning-position)
                        (line-end-position))))
    (if (string= line-contents "")
        (kill-line)))
  (back-to-indentation))

(defun tay/copy-line ()
  "copy the entire current line"
  (interactive)
  (beginning-of-line)
  (set-mark (point))
  (end-of-line)
  (kill-ring-save mark point))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SEARCH
;;
;; custom search functions to replace ivy, counsel, swiper with built in
;; emacs functionality.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun tay/git-grep (expr)
  "Search for `expr' in all files contained in the current repository"
  (interactive "sSEARCH: ")
  (vc-git-grep expr "*" (vc-root-dir)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EXTRAS
;;
;; extra collected functions for various tasks
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun tay/cleanup ()
  "Cleanup all user buffers and remove splits"
  (interactive)
  (mapc (lambda (s)
	      (if (not (string-prefix-p " " (buffer-name s)))
	          (kill-buffer s)))
	    (buffer-list))
  (delete-other-windows)
  (cd "~/")
  (message "YOU ARE SO CLEAN"))

(defun tay/kill-this-buffer ()
  "kill the current buffer"
  (interactive)
  (kill-buffer (current-buffer)))

(defun tay/echo-info ()
  "Show info in echo area that would often be displayed in the modeline"
  (interactive)
  (message (concat
            ""
            (buffer-name) " | "
            (symbol-name major-mode)  " | "
            (format-time-string "%Y-%m-%d %H:%M"))))

(provide 'taymacs)
