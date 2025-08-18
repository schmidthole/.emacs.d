(defvar calsync/applescript "tell application \"Calendar\"
    set startDate to date \"%s 12:00:00 AM\"
    set endDate to date \"%s 11:59:59 PM\"
    set calendarList to every calendar
    set allEvents to {}
    repeat with cal in calendarList
        set calendarEvents to (every event of cal whose start date ≥ startDate and start date ≤ endDate)
        set allEvents to allEvents & calendarEvents
    end repeat
    set output to \"\"
    repeat with evt in allEvents
        set output to output & (summary of evt) & \" | \" & (start date of evt) & \" | \" & (end date of evt) & \"\ $$$$\n\"
    end repeat
    return output
end tell")

(defvar calsync/buffer-name "*calsync*")

(defun calsync/process-sentinel (process event)
  (with-current-buffer calsync/buffer-name
    )
  (message "...calsync done"))

(defun calsync/sync-events (start-date end-date)
  (make-process
   :name "calsync"
   :buffer calsync/buffer-name
   :command (list "osascript" "-e" (format calsync/applescript start-date end-date))
   :sentinel #'calsync/process-sentinel)
  (message "start calsync..."))

(calsync/sync-events "August 10, 2025" "August 16, 2025")
