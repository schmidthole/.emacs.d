;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CC
;;
;; Settings for C/C++ language
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq c-default-style "linux"
      c-basic-offset 4)

(tay/do-if-ext-pkg
 (use-package lsp-mode
  :hook
  (c-mode . lsp-deferred)
  :commands
  (lsp lsp-deferred)
  :config
  (setq lsp-clients-clangd-executable "/usr/local/opt/llvm/bin/clangd")))
