;;; calsync.el --- sync apple calendar to org-mode using apple shortcuts -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Taylor Schmidt

;; Author: Taylor Schmidt <taylor@taytay.lol>
;; Version: 1.0.0
;; Package-Requires: ((emacs "25.1"))

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; sync apple calendars with org mode for easy access in emacs.
;; requires creation of apple shortcut to place calendar event ics files in a know directory.
;;
;; To use:
;; 1. Create an Apple Shortcut that exports events as .ics files to `calsync-ics-dir`.
;; 2. Run `M-x calsync-setup` to hook into `org-agenda-redo`.
;; 3. Refresh your agenda (`g` in *Org Agenda* buffer) to sync.

(defgroup calsync nil
  "calsync"
  :prefix "calsync-"
  :group 'applications)

(defcustom calsync-ics-dir "~/org/events/"
  "directory where apple shortcut places ics events"
  :type 'directory
  :group 'calsync)

(defcustom calsync-ics-master-file "~/org/calendar.ics"
  "master ics file where all events are concatenated"
  :type 'file
  :group 'calsync)

(defcustom calsync-ics-org-file "~/org/calendar.org"
  "master org file where ics conversion outputs to from ical2orgpy"
  :type 'file
  :group 'calsync)

(defcustom calsync-shortcut-name "Snapshot Calendar"
  "name of apple shortcut to export ics events (must be setup to output to `calsync-ics-dir`'')"
  :type 'string
  :group 'calsync)

(defun calsync-run-shortcut ()
  "run the shorcut to export calendar snapshot"
  (let ((command (format "shortcuts run \"%s\"" calsync-shortcut-name)))
    (call-process "shortcuts" nil 0 nil "run" calsync-shortcut-name)))

(defun calsync-concat-ics-files ()
  "Concatenate all .ics files in DIRECTORY into OUTPUT-FILE.
   Removes duplicate VCALENDAR headers/footers to create a valid ICS file."
  (let ((ics-files (directory-files calsync-ics-dir t "\\.ics$"))
        (events '())
        (calendar-properties '()))
    
    ;; Process each ICS file
    (dolist (file ics-files)
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        
        ;; Extract calendar properties from first file only
        (when (null calendar-properties)
          (when (re-search-forward "^BEGIN:VCALENDAR" nil t)
            (let ((start (line-beginning-position)))
              (when (re-search-forward "^BEGIN:VEVENT\\|^BEGIN:VTODO\\|^BEGIN:VJOURNAL" nil t)
                (forward-line -1)
                (setq calendar-properties 
                      (buffer-substring-no-properties start (point)))))))
        
        ;; Extract all events/todos/journals
        (goto-char (point-min))
        (while (re-search-forward "^BEGIN:\\(VEVENT\\|VTODO\\|VJOURNAL\\)" nil t)
          (let ((component-start (line-beginning-position))
                (component-type (match-string 1)))
            (when (re-search-forward (format "^END:%s" component-type) nil t)
              (let ((component-end (line-end-position)))
                (push (buffer-substring-no-properties component-start component-end)
                      events)))))))
    
    ;; Write combined file
    (when ics-files
      (with-temp-file calsync-ics-master-file
        (insert calendar-properties "\n")
        (dolist (event (reverse events))
          (insert event "\n"))
        (insert "END:VCALENDAR\n")))
    
    (message "Concatenated %d ICS files into %s" (length ics-files) calsync-ics-master-file)))

(defun calsync-convert-to-org ()
  "run the ical2orgpy script to convert master ics to org entries"
  (let ((command (format "ical2orgpy %s %s" calsync-ics-master-file calsync-ics-org-file)))
    (call-process "ical2orgpy" nil 0 nil calsync-ics-master-file calsync-ics-org-file)))

(defun calsync-run-pipeline (&rest _)
  ;; remove the ics directory to avoid duplicate adds
  (if (file-directory-p calsync-ics-dir)
      (delete-directory calsync-ics-dir t))
  (make-directory calsync-ics-dir)
  ;; clear the output org file just in case
  (with-temp-file calsync-ics-org-file)

  ;; run the pipeline functions
  (calsync-run-shortcut)
  (calsync-concat-ics-files)
  (calsync-convert-to-org)
  (message "calsync finished..."))

;;;###autoload
(defun calsync-setup ()
  "setup calsync to run on every org redo and put the calendar org file in place"
  (with-temp-file calsync-ics-org-file)
  (advice-add 'org-agenda-redo :before #'calsync-run-pipeline))

(provide 'calsync)

;;; calsync.el ends here
