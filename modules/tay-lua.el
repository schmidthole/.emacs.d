(tay/do-if-ext-pkg
 (use-package fennel-mode
   :config
   (add-to-list 'auto-mode-alist '("\\.fnl\\'" . fennel-mode))
   (defun love-repl ()
     "start a repl for the current love project.
helper function passing `love .` to fennel-repl"
     (interactive)
     (if (not (comint-check-proc fennel-repl--buffer))
         (let* ((cmd "love .")
                (cmdlist (split-string cmd)))
           (set-buffer (apply #'make-comint "Fennel REPL" (car cmdlist) nil (cdr cmdlist)))
           (fennel-repl-mode)
           (setq-local fennel-program cmd)
           (setq inferior-lisp-buffer fennel-repl--buffer)))
     (get-buffer fennel-repl--buffer)))

 (use-package lua-mode
   :config
   (add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
   (add-to-list 'interpreter-mode-alist '("lua" . lua-mode))
   (setq lua-indent-level 2)))
