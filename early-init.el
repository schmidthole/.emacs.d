;;; early-init.el --- frame appearance -*- lexical-binding: t; -*-

(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

(add-to-list 'default-frame-alist '(font . "JetBrains Mono-12"))

(when (eq system-type 'darwin)
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark)))

;;; early-init.el ends here
