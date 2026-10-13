;;; mac-launcher.el --- macos application popup -*- lexical-binding: t; -*-

;;; commentary:
;; launch applications through a standalone minibuffer on the shared server.

;;; code:

(require 'cl-lib)

(defvar mac-launcher-directories
  '("/Applications" "/System/Applications" "~/Applications")
  "directories searched for application bundles.")

(defvar mac-launcher--history nil)

(defun mac-launcher--bundles (directory)
  "find application bundles below directory without entering bundles or links."
  (when (file-readable-p directory)
    (cl-loop for path in (directory-files directory t "\\`[^.]")
             when (file-directory-p path)
             append (cond
                     ((string-suffix-p ".app" path) (list path))
                     ((not (file-symlink-p path))
                      (mac-launcher--bundles path))))))

(defun mac-launcher--applications ()
  "return completion labels paired with full application paths."
  (let* ((paths (delete-dups
                 (cl-mapcan (lambda (directory)
                              (mac-launcher--bundles (expand-file-name directory)))
                            mac-launcher-directories)))
         (names (mapcar #'file-name-base paths)))
    (sort (mapcar (lambda (path)
                    (let ((name (file-name-base path)))
                      (cons (if (> (cl-count name names :test #'equal) 1)
                                (format "%s (%s)" name (file-name-directory path))
                              name)
                            path)))
                  paths)
          (lambda (a b) (string-lessp (downcase (car a)) (downcase (car b)))))))

;;;###autoload
(defun mac-launcher ()
  "show a macos application popup; return non-nil after launching an app."
  (interactive)
  (unless (eq system-type 'darwin)
    (user-error "the application launcher requires macos"))
  (when (active-minibuffer-window)
    (user-error "finish the active minibuffer before opening the launcher"))
  (let ((applications (mac-launcher--applications))
        (resize-mini-frames t)
        (resize-mini-windows t)
        (max-mini-window-height 12)
        frame path)
    (unless applications
      (user-error "no applications found"))
    (condition-case nil
        (unwind-protect
            (progn
              (setq frame (make-frame '((window-system . ns)
                                        (name . "application launcher")
                                        (minibuffer . only)
                                        (width . 72)
                                        (height . 1)
                                        (left . 0.5)
                                        (top . 0.25)
                                        (undecorated . t)
                                        (internal-border-width . 12)
                                        (menu-bar-lines . 0)
                                        (tool-bar-lines . 0)
                                        (vertical-scroll-bars . nil))))
              (select-frame-set-input-focus frame)
              (setq path (cdr (assoc (completing-read "application: " applications
                                                      nil t nil
                                                      'mac-launcher--history)
                                     applications))))
          (when (and frame (frame-live-p frame))
            (delete-frame frame t)))
      (quit nil))
    (when path
      (unless (zerop (call-process "/usr/bin/open" nil nil nil path))
        (user-error "could not launch application"))
      t)))

(provide 'mac-launcher)
;;; mac-launcher.el ends here
