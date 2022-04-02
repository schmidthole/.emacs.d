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

(require 'tay-keybindings)

(setq gc-cons-threshold (* 2 1000 1000))
