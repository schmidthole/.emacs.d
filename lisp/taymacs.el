;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; taymacs functions
;;
;; all the functions that make taymacs work
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun tay/bind-key (keychord func)
  "simply setup a global keybinding"
  (global-set-key (kbd keychord) func))

(defun tay/bind-key-map (mode-map keychord func)
  "bind a key for a specific mode map"
  (define-key mode-map (kbd keychord) func))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MOVEMENT
;;
;; custom opinionated buffer editing functions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun tay/vi-line-below ()
  "make a newline below the current one and move there. this will not break
apart lines like the standard emacs binding"
  (interactive)
  (progn
    (move-end-of-line 1)
    (newline-and-indent)))

(defun tay/vi-line-above ()
  "make a newline above the current one and move there. this will not break apart
line like the standard emacs binding"
  (interactive)
  (progn
    (previous-line)
    (tay/vi-line-below)))

(setq tay/last-go-to-char nil)

(defun tay/go-to-char-forward (c)
  (interactive "cCHAR: ")
  (search-forward (char-to-string c) (point-at-eol))
  (setq tay/last-go-to-char c))

(defun tay/go-to-char-backward (c)
  (interactive "cCHAR: ")
  (search-backward (char-to-string c) (point-at-bol))
  (setq tay/last-go-to-char c))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EDITING
;;
;; custom editing functions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun tay/kill-line ()
  "kill the line regardless of cursor position and move to next line"
  (interactive)
  (beginning-of-line)
  (kill-line)
  (let ((line-contents (buffer-substring-no-properties
                        (line-beginning-position)
                        (line-end-position))))
    (if (string= line-contents "")
        (kill-line)))
  (back-to-indentation))

(defun tay/copy-line ()
  "copy the entire current line"
  (interactive)
  (beginning-of-line)
  (set-mark (point))
  (end-of-line)
  (kill-ring-save mark point))

(defun tay/base64-decode-buffer ()
  "decodes a base64 encoded string in the current buffer an replaces it with
the decoded string"
  (interactive)
  (with-current-buffer (current-buffer)
    (base64-decode-region (point-min) (point-max))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EXTRAS
;;
;; extra collected functions for various tasks
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq tay/cleanup-exceptions '("*scratch*"
                               "*Messages*"
                               "*dashboard*"))

(defun tay/cleanup ()
  "Cleanup all user buffers and remove splits"
  (interactive)
  (mapc (lambda (s)
          (let* ((bname (buffer-name s))
                 (not-special-buffer (not (string-prefix-p " " bname)))
                 (not-exception-buffer (not (member bname tay/cleanup-exceptions))))
	        (if (and not-special-buffer
                     not-exception-buffer)
                (progn
                  (message (concat "killing " bname))
	              (kill-buffer s)))))
	    (buffer-list))
  (delete-other-windows)
  (cd "~/")
  (message "YOU ARE SO CLEAN"))

(defun tay/kill-this-buffer ()
  "kill the current buffer"
  (interactive)
  (kill-buffer (current-buffer)))

(defun tay/echo-info ()
  "Show info in echo area that would often be displayed in the modeline"
  (interactive)
  (message (concat
            ""
            (buffer-name) " | "
            (symbol-name major-mode)  " | "
            (format-time-string "%Y-%m-%d %H:%M"))))

(provide 'taymacs)
