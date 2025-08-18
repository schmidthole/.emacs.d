;;; weather-mode.el --- Display weather forecasts -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Taylor Schmidt

;; Author: Taylor Schmidt <taylor@taytay.lol>
;; URL: https://github.com/schmidthole/weather-mode
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

;; easy weather forecast display in emacs.

;;; Code:
(require 'url)
(require 'json)
(require 'tabulated-list)

(defgroup weather nil
  "weather forecast"
  :group 'applications)

(defcustom weather-location ""
  "default location for weather forecast"
  :type 'string
  :group 'weather)

(defun get-coordinates (location)
  (let* ((url (format "https://nominatim.openstreetmap.org/search?q=%s&format=json&limit=1"
                      (url-hexify-string location)))
	 (response (with-current-buffer (url-retrieve-synchronously url)
		     (goto-char (point-min))
		     (re-search-forward "^$")
		     (json-read-from-string (buffer-substring (point) (point-max)))))
	 (result (elt response 0)))
    (if result
	(cons (string-to-number (cdr (assoc 'lat result)))
	      (string-to-number (cdr (assoc 'lon result))))
      (cons nil nil))))

(defun get-forecast (lat lon)
  (let* ((url (format "https://api.weather.gov/points/%s,%s" lat lon))
	 (response (with-current-buffer (url-retrieve-synchronously url)
		     (goto-char (point-min))
		     (re-search-forward "^$")
		     (json-read-from-string (buffer-substring (point) (point-max)))))
	 (forecast-url (cdr (assoc 'forecast (cdr (assoc 'properties response)))))
	 (forecast (with-current-buffer (url-retrieve-synchronously forecast-url)
		     (goto-char (point-min))
		     (re-search-forward "^$")
		     (json-read-from-string (buffer-substring (point) (point-max)))))
	 (periods (cdr (assoc 'periods (assoc 'properties forecast)))))
    (mapcar
     (lambda (period)
       (list nil
	     (vector (cdr (assoc 'name period))
		     (format "%s°F" (cdr (assoc 'temperature period)))
		     (cdr (assoc 'windSpeed period))
		     (cdr (assoc 'shortForecast period)))))
     periods)))

(defun weather-refresh ()
  "refresh the weather forecast"
  (interactive)
  (weather-display-forecast weather-location))

(defun weather-set-location (location)
  "set the default location for weather forecast"
  (interactive "sEnter location: ")
  (setq weather-location location)
  (customize-save-variable 'weather-location location)
  (message "default weather location set to %s" location))

(defun weather-display-forecast ()
  (with-current-buffer (get-buffer-create "*Weather Forecast*")
    (if (or (null weather-location) (string-empty-p weather-location))
	(setq weather-location (read-string "Enter Location: ")))
    (weather-mode)
    (switch-to-buffer (current-buffer))
    (weather-fetch-forecast)))

(defun weather-fetch-forecast ()
  (interactive)
  (let* ((coordinates (get-coordinates weather-location))
	 (lat (car coordinates))
	 (lon (cdr coordinates)))
    (if (and lat lon)
	(let ((forecast (get-forecast lat lon)))
	  (setq tabulated-list-entries forecast)
	  (tabulated-list-print))
      (message "failed to find location for weather forecast"))))

(defvar-keymap weather-mode-map
  :parent tabulated-list-mode-map
  "C-c s" #'weather-set-location)

;;;###autoload
(define-derived-mode weather-mode tabulated-list-mode "Weather"
  "major mode for displaying weather forecasts"
  (setq tabulated-list-use-header-line t)
  (setq tabulated-list-format [("Period" 20 t)
			       ("Temperature" 15 t)
			       ("Wind Speed" 15 t)
			       ("Forecast" 0 t)])
  (tabulated-list-init-header)
  (add-hook 'tabulated-list-revert-hook 'weather-fetch-forecast))

;;;###autoload
(defun weather-forecast ()
  "display the weather forecast for the default or specified location"
  (interactive)
  (weather-display-forecast))

(provide 'weather-mode)

;;; weather-mode.el ends here
