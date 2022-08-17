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

;; flags to enable extra features as they are wanted/needed
(setq tay/evil nil)
(setq tay/ivy t)
(setq tay/vterm nil)
(setq tay/fancy-editor t)

;; load ui elements such as theme, modeline, and font
(require 'tay-font)
(require 'tay-theme)
(require 'tay-path)

;; evil mode specific setup
(if tay/evil
    (progn
      (require 'tay-evil))
      ;;(require 'tay-smartparens))
  (progn
    (require 'tay-paredit)))

;; enable all of the ivy/counsel/swiper/avy stuff or use vanilla navigation
(if tay/ivy
    (progn
      (require 'tay-ivy)
      (require 'tay-avy)
      (require 'tay-anzu))
  (require 'tay-fido))

;; ;; use vterm or built in eshell
(if tay/vterm
    (require 'tay-vterm)
  (require 'tay-eshell))

;; ;; enable fancy editing features that modify default behavior
(when tay/fancy-editor
  (require 'tay-editor))

;; completion, linting, goto definition
(require 'tay-company)
(require 'tay-flycheck)
(require 'tay-lsp)

;; mode specific packages
;; (require 'tay-cc)
;; (require 'cmake-mode)
(require 'tay-ediff)
(require 'tay-git)
;; (require 'tay-clojure)
;; (require 'tay-indent)
;; (require 'tay-kconfig)
(require 'tay-markdown)
(require 'tay-python)
;; (require 'tay-yaml)
(require 'tay-typescript)
(require 'tay-web)
(require 'tay-dired)
(require 'tay-org)
;; (require 'tay-rust)

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
