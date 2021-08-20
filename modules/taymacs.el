;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; taymacs functions
;;
;; all the functions that make taymacs work
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; defines an association list of the available taymacs modules and their
;; filenames within the modules dir
(setq tay/module-files-alist '((tay/core . "tay-core.el")
                               (tay/font . "tay-font.el")
                               (tay/editor . "tay-editor.el")
                               (tay/evil . "tay-evil.el")
                               (tay/theme . "tay-theme.el")
                               (tay/path . "tay-path.el")
                               (tay/dired . "tay-dired.el")
                               (tay/ediff . "tay-ediff.el")
                               (tay/org . "tay-org.el")
                               (tay/cc . "tay-cc.el")
                               (tay/python . "tay-python.el")
                               (tay/ivy . "tay-ivy.el")
                               (tay/workspace . "tay-workspace.el")
                               (tay/markdown . "tay-markdown.el")
                               (tay/eshell . "tay-eshell.el")
                               (tay/vterm . "tay-vterm.el")
                               (tay/zen . "tay-zen.el")
                               (tay/web . "tay-web.el")
                               (tay/email . "tay-email.el")
                               (tay/git . "tay-git.el")
                               (tay/json . "tay-json.el")
                               (tay/clojure . "tay-clojure.el")
                               (tay/kotlin . "tay-kotlin.el")
                               (tay/cmake . "tay-cmake.el")
                               (tay/ido . "tay-ido.el")
                               (tay/ag . "tay-ag.el")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MODULE FUNCTIONS
;;
;; helper functions for loading taymacs modules
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun tay/load-module (name)
  "loads a custom module file from the defined dir"
  (let* ((file-location (concat tay/custom-module-path name))
         (module-path (expand-file-name file-location user-emacs-directory)))
    (load module-path 'noerror)))

(defun tay/load-mode (name)
  "loads a custom mode file from the defined dir"
  (let* ((file-location (concat tay/custom-mode-path name))
         (mode-path (expand-file-name file-location user-emacs-directory)))
    (load mode-path)))

(defun tay/load-default-module (symbol)
  "loads a module via symbol name vs. by file name if module assoc dne, then
do nothing"
  (let ((module-file (cdr (assoc symbol tay/module-files-alist))))
    (if module-file
        (tay/load-module module-file))))

(defun tay/load-module-list (modules)
  "loads a list of module symbols via assoc lookup. if a module dne, then
nothing is performed for that entry"
  (dolist (m modules)
    (tay/load-default-module m)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; KEYBINDING FUNCTIONS
;;
;; functions for binding keys
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun tay/global-key (keychord func)
  "simply setup a global keybinding"
  (global-set-key (kbd keychord) func))

(defun tay/global-module-key (keychord func module)
  "setup a global keybinding if a module is enabled"
  (if module
      (global-set-key (kbd keychord) func)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EXTERNAL PACKAGES
;;
;; Setup external package management. taymacs uses elpa/melpa and use-package
;; for management
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun tay/do-if-ext-pkg (routine &rest routines)
  "run the provided routine if external packages are enabled in taymacs"
  (if tay/external-packages
      (progn
        routine
        (dolist (r routines)
          r))))

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
  ;; (tramp-cleanup-all-buffers)
  ;; (tramp-cleanup-all-connections)
  (cd "~/")
  (message "YOU ARE SO CLEAN"))

(defun tay/kill-this-buffer ()
  "kill the current buffer"
  (interactive)
  (kill-buffer (current-buffer)))
