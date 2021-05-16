;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CC
;;
;; Settings for C/C++ language
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; sets up cc formatting for us
(setq c-default-style "linux"
      c-basic-offset 4)

(tay/do-if-ext-pkg
 ;; we are using lsp mode with clangd, which ends up being great
 (use-package lsp-mode
  :hook
  (c-mode . lsp-deferred)
  :commands
  (lsp lsp-deferred)
  :config
  ;; you will probably need to modify this path if you are not on macos
  (setq lsp-clients-clangd-executable "/usr/local/opt/llvm/bin/clangd")))
