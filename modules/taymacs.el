;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; taymacs functions
;;
;; all the functions that make taymacs work
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; defines an association list of the available taymacs modules and their
;; filenames within the modules dir
(setq tay/module-files-alist '((tay/core . "tay-core.el")
                               (tay/editor . "tay-editor.el")
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
                               (tay/git . "tay-git.el")))

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
