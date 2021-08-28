;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  ______ ______  __  __  __    __  ______  ______  ______
;; /\__  _/\  __ \/\ \_\ \/\ "-./  \/\  __ \/\  ___\/\  ___\
;; \/_/\ \\ \  __ \ \____ \ \ \-./\ \ \  __ \ \ \___\ \___  \
;;    \ \_\\ \_\ \_\/\_____\ \_\ \ \_\ \_\ \_\ \_____\/\_____\
;;     \/_/ \/_/\/_/\/_____/\/_/  \/_/\/_/\/_/\/_____/\/_____/
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; pop that garbage collection on up for increased startup speed
(setq gc-cons-threshold most-positive-fixnum gc-cons-percentage 0.6)
(defvar tdm--file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BOOSTRAP TAYMACS
;;
;; emacs auto generated custom settings and personal fields are loaded first
;; this also loads the functions that are used throughout taymacs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; this defines where all of the modules live for this config
;; THIS MUST BE SET
(setq tay/custom-module-path "modules/")
(setq tay/custom-mode-path "modes/")

;; define paths to our basic settings files
;;
;; - custom: auto generated custom settings file that emacs makes
;; - personal: personal settings such as email, name, etc.
;; - taymacs: base taymacs functions that we need for later
(setq custom-file (expand-file-name "custom.el" user-emacs-directory)
      personal-config (expand-file-name "personal.el" user-emacs-directory)
      taymacs-module (expand-file-name
                      (concat tay/custom-module-path "taymacs.el")
                      user-emacs-directory))

(load custom-file 'noerror)
(load personal-config 'noerror)

;; this sets us up for success for the rest of config. we need this to load
;; without error or everything is done for
(load taymacs-module)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  ______  ______  __   __  ______ __  ______
;; /\  ___\/\  __ \/\ "-.\ \/\  ___/\ \/\  ___\
;; \ \ \___\ \ \/\ \ \ \-.  \ \  __\ \ \ \ \__ \
;;  \ \_____\ \_____\ \_\\"\_\ \_\  \ \_\ \_____\
;;   \/_____/\/_____/\/_/ \/_/\/_/   \/_/\/_____/
;;
;; this section covers all of the module configuration for taymacs. we can
;; enable/disable modules as we wish.
;;
;; - set the `tay/external-packages' var to enable melpa packages
;; - edit the `tay/modules' list to setup the various modules for taymacs
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; set if you wish to use external package repos such as melpa
(setq tay/external-packages t)

;; these are all of the available modules
;;
;; enabling a module will pull in external packages and setup default settings
;; if external packages are disabled, no functionality may be setup in some
;; cases
;;
;; comment out or remove uneeded modules to disable them from loading
;;
(setq tay/modules '(
                    ;; look, feel, and function
                    ;; tay/font
                    tay/editor
                    ;; tay/evil
                    tay/icons
                    tay/theme
                    ;; tay/zen

                    ;; general
                    tay/path
                    tay/dired
                    tay/ediff
                    tay/window
                    tay/undo
                    tay/anzu

                    ;; terms
                    tay/eshell
                    tay/vterm

                    ;; navigation/completion
                    tay/ivy
                    tay/avy
                    tay/company
                    tay/flycheck
                    ;; tay/ido

                    ;; organization
                    ;; tay/workspace

                    ;; languages
                    tay/org
                    tay/cc
                    tay/python
                    tay/markdown
                    tay/web
                    ;; tay/json
                    tay/kotlin
                    ;; tay/clojure
                    tay/cmake

                    ;; applications
                    ;; tay/email
                    tay/git
                    ))

;; you dont really have a choice to load the core module
(tay/load-default-module 'tay/core)

;; enable external package managers if desired above
(if tay/external-packages
    (tay/enable-ext-pkg))

;; load all enabled modules
;;
;; check out each module's source in the directory for specifics on what is
;; being loaded
(tay/load-module-list tay/modules)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  __  __  ______  __  __  ______
;; /\ \/ / /\  ___\/\ \_\ \/\  ___\
;; \ \  _"-\ \  __\\ \____ \ \___  \
;;  \ \_\ \_\ \_____\/\_____\/\_____\
;;   \/_/\/_/\/_____/\/_____/\/_____/
;;
;; this section covers all of the customized key bindings in taymacs. these
;; are all in one place so that it is easier understand and modify.
;;
;; there are a few taymacs helpers that allow us to easily setup keys
;;
;; - `tay/global-key': set a global keybinding
;; - `tay/global-module-key': set a global keybinding if a module is enabled
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tay/global-key "M-i" nil)
(tay/global-key "C-z" nil)
(tay/global-key "C-x C-z" nil)
(tay/global-key "C-j" 'tay/vi-line-below)
(tay/global-key "C-o" 'tay/vi-line-above)
(tay/global-key "<up>" 'tay/up-chunk)
(tay/global-key "<down>" 'tay/down-chunk)
(tay/global-module-key "<left>" 'undo 'tay/undo)
(tay/global-module-key "<right>" 'redo 'tay/redo)
(tay/global-module-key "C-/" 'undo 'tay/undo)
(tay/global-module-key "C-z" 'redo 'tay/redo)
(tay/global-key "C-x C-k" 'tay/kill-this-buffer)
(tay/global-key "M-i k" 'tay/cleanup)
(tay/global-key "M-k" 'tay/kill-line)
(tay/global-module-key "C-=" 'er/expand-region 'tay/editor)
(tay/global-module-key "M-i l" 'tay/avy-select-to-line 'tay/ivy)
(tay/global-key "M-i 0" 'toggle-frame-fullscreen)
;; (tay/global-module-key "C-c o c" 'org-capture 'tay/org)
(tay/global-module-key "M-i g" 'magit 'tay/git)
(tay/global-module-key "M-i a" 'counsel-ag 'tay/ivy)
(tay/global-module-key "M-i p" 'counsel-git 'tay/ivy)
(tay/global-module-key "M-s" 'swiper 'tay/ivy)
(tay/global-module-key "C-;" 'avy-goto-char 'tay/ivy)
(tay/global-module-key "C-'" 'avy-goto-line 'tay/ivy)
(tay/global-module-key "C-c s" 'tay/vterm-new 'tay/vterm)
(tay/global-module-key "C-c e" 'tay/eshell-new 'tay/eshell)
(tay/global-module-key "C-c t" 'modus-themes-toggle 'tay/theme)
;; (tay/global-module-key "C-c p k" 'persp-remove-buffer 'tay/workspace)
;; (tay/global-module-key "C-x b" 'persp-ivy-switch-buffer 'tay/workspace)
;; (tay/global-module-key "C-x C-b" 'persp-ivy-switch-buffer 'tay/workspace)
;; (tay/global-module-key "C-c p s" 'persp-switch 'tay/workspace)
;; (tay/global-module-key "C-c p n" 'persp-next 'tay/workspace)
;; (tay/global-module-key "C-c p p" 'persp-prev 'tay/workspace)
;; (tay/global-module-key "C-c p d" 'persp-kill 'tay/workspace)
;; (tay/global-module-key "C-c z" 'olivetti-mode 'tay/zen)
(tay/global-module-key "C-c m" 'mu4e 'tay/email)
(tay/global-module-key "M-o" 'ace-window 'tay/window)
(tay/global-key "M-i s" 'split-window-below)
(tay/global-key "M-i v" 'split-window-right)
(tay/global-module-key "C-a" 'mwim-beginning-of-code-or-line 'tay/editor)
(tay/global-module-key "C-e" 'mwim-end-of-code-or-line 'tay/editor)
(tay/global-module-key "C-r" 'anzu-query-replace-regexp 'tay/anzu)
(tay/global-module-key "C-M-r" 'anzu-query-replace-at-cursor-thing 'tay/anzu)
(tay/global-module-key "M-i d" 'lsp-find-definition 'tay/cc)
(tay/global-module-key "M-i r" 'lsp-find-references 'tay/cc)

;; cleanup and reset after startup
(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold 16777216 gc-cons-percentage 0.1)))

(add-hook 'emacs-startup-hook
          (lambda () (setq file-name-handler-alist tdm--file-name-handler-alist)))
(put 'upcase-region 'disabled nil)
