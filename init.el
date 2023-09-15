;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
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

;; load ui elements such as theme, modeline, and font
(require 'tay-font)
(require 'tay-theme)
(require 'tay-modeline)
(require 'tay-path)

(require 'tay-fido)

;; shell
(require 'tay-eshell)

;; enable fancy editing features
(require 'tay-editor)

;; completion, linting, goto definition
(require 'tay-company)
(require 'tay-eglot)

(require 'tay-git)

;; mode specific packages
(require 'tay-cc)
(require 'cmake-mode)
(require 'tay-kconfig)
(require 'tay-markdown)
(require 'tay-python)
(require 'tay-yaml)
(require 'tay-typescript)
(require 'tay-web)
(require 'tay-dired)
(require 'tay-org)
(require 'tay-golang)
(require 'tay-dockerfile)

;; all custom keybindings are defined in separate module
(require 'tay-keybindings)

(setq gc-cons-threshold (* 2 1000 1000))
(put 'narrow-to-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
