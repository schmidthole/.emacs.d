;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
>;;
;;  ______ ______  __  __  __    __  ______  ______  ______
;; /\__  _/\  __ \/\ \_\ \/\ "-./  \/\  __ \/\  ___\/\  ___\
;; \/_/\ \\ \  __ \ \____ \ \ \-./\ \ \  __ \ \ \___\ \___  \
;;    \ \_\\ \_\ \_\/\_____\ \_\ \ \_\ \_\ \_\ \_____\/\_____\
;;     \/_/ \/_/\/_/\/_____/\/_/  \/_/\/_/\/_/\/_____/\/_____/
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; load the core settings and custom functions before external packages
(require 'tay-core)
(require 'taymacs)

;; enable use package and melpa
(tay/enable-ext-pkg)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  ______  ______  __   __  ______ __  ______
;; /\  ___\/\  __ \/\ "-.\ \/\  ___/\ \/\  ___\
;; \ \ \___\ \ \/\ \ \ \-.  \ \  __\ \ \ \ \__ \
;;  \ \_____\ \_____\ \_\\"\_\ \_\  \ \_\ \_____\
;;   \/_____/\/_____/\/_/ \/_/\/_/   \/_/\/_____/
;;
;; this section covers all of the module configuration for taymacs.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

<<<<<<< HEAD
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
                    tay/theme
                    ;; tay/zen

                    ;; general
                    tay/path
                    tay/dired
                    tay/ediff

                    ;; searching
                    tay/ag

                    ;; terms
                    ;; tay/eshell
                    tay/vterm

                    ;; navigation/completion
                    tay/ivy
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
=======
;; load ui elements such as theme, modeline, and font
(require 'tay-font)
(require 'tay-theme)
(require 'tay-path)

;; load basic editor functionality such as search and completion
(require 'tay-editor)
(require 'tay-ivy)
(require 'tay-company)
(require 'tay-flycheck)
(require 'tay-lsp)
(require 'tay-anzu)

;; mode specific packages
(require 'cmake-mode)
(require 'tay-ediff)
(require 'tay-eshell)
(require 'tay-git)
(require 'tay-clojure)
(require 'tay-paredit)
(require 'tay-indent)
(require 'tay-kconfig)
(require 'tay-markdown)
(require 'tay-python)
(require 'tay-yaml)
(require 'tay-typescript)
(require 'tay-web)
(require 'tay-dired)
>>>>>>> simple-miso

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  __  __  ______  __  __  ______
;; /\ \/ / /\  ___\/\ \_\ \/\  ___\
;; \ \  _"-\ \  __\\ \____ \ \___  \
;;  \ \_\ \_\ \_____\/\_____\/\_____\
;;   \/_/\/_/\/_____/\/_____/\/_____/
;;
;; all custom keybindings are contained in a single module
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

<<<<<<< HEAD
;; disable these bindings as they tend to not be wanted or cause loss of work
(tay/global-key "C-z" nil)
(tay/global-key "C-x C-z" nil)

;; more vi like newline keys
(tay/global-key "C-j" 'tay/vi-line-below)
(tay/global-key "C-o" 'tay/vi-line-above)

;; move in chunks
(tay/global-key "<up>" 'tay/up-chunk)
(tay/global-key "<down>" 'tay/down-chunk)

;; helpers for killing things better
(tay/global-key "C-x C-k" 'tay/kill-this-buffer)
(tay/global-key "C-c K" 'tay/cleanup)
(tay/global-key "M-k" 'tay/kill-line)

;; selection functions
(tay/global-module-key "C-=" 'er/expand-region 'tay/editor)

(tay/global-key "M-i" nil)
(tay/global-module-key "M-i l" 'tay/avy-select-to-line 'tay/ivy)

;; go to full screen
;; (tay/global-key "C-c 0" 'toggle-frame-fullscreen)

;; capture a new task
;; (tay/global-module-key "C-c o c" 'org-capture 'tay/org)

;; open git easily
(tay/global-module-key "C-c g" 'magit 'tay/git)

;; ag searching is great, needs silver searcher to be installed
(tay/global-module-key "C-c a a" 'counsel-ag 'tay/ivy)
(tay/global-module-key "C-c a f" 'ag-dired 'tay/ag)
(tay/global-module-key "C-c a s" 'ag 'tay/ag)

;; swiper instead of isearch
;; (tay/global-module-key "C-s" 'swiper 'tay/ivy)
;; (tay/global-module-key "C-r" 'swiper-backward 'tay/ivy)

;; avy for jumping around visible buffer area
(tay/global-module-key "C-;" 'avy-goto-char 'tay/ivy)
(tay/global-module-key "C-'" 'avy-goto-line 'tay/ivy)

;; hotkeys for eshell and vterm
;; these will open a new terminal every time we use them
(tay/global-module-key "C-c s" 'tay/vterm-new 'tay/vterm)
(tay/global-module-key "C-c e" 'tay/eshell-new 'tay/eshell)

;; switch light and dark themes easily
(tay/global-module-key "C-c t" 'modus-themes-toggle 'tay/theme)

;; switch around projects and segregate buffer so we dont get lost
;; (tay/global-module-key "C-c p k" 'persp-remove-buffer 'tay/workspace)
;; (tay/global-module-key "C-x b" 'persp-ivy-switch-buffer 'tay/workspace)
;; (tay/global-module-key "C-x C-b" 'persp-ivy-switch-buffer 'tay/workspace)
;; (tay/global-module-key "C-c p s" 'persp-switch 'tay/workspace)
;; (tay/global-module-key "C-c p n" 'persp-next 'tay/workspace)
;; (tay/global-module-key "C-c p p" 'persp-prev 'tay/workspace)
;; (tay/global-module-key "C-c p d" 'persp-kill 'tay/workspace)

;; toggle zen mode easily in buffer
;; (tay/global-module-key "C-c z" 'olivetti-mode 'tay/zen)

;; open email
;; (tay/global-module-key "C-c m" 'mu4e 'tay/email)

;; cleanup and reset after startup
(add-hook 'emacs-startup-hook
    (lambda () (setq gc-cons-threshold 16777216 gc-cons-percentage 0.1)))
=======
(require 'tay-keybindings)
>>>>>>> simple-miso

(setq gc-cons-threshold (* 2 1000 1000))
