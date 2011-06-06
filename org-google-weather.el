;;; org-google-weather.el --- Show Google Weather forecasts in Org agenda.

;; Copyright (C) 2010 Julien Danjou

;; Author: Julien Danjou <julien@danjou.info>
;; Keywords: comm

;; This file is NOT part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; This module allows to display the weather forecast fetched from Google in
;; your Org agenda.
;;
;;     Wednesday   8 September 2010
;;       Weather:    Pluie, 12/18 ℃
;;     Thursday    9 September 2010
;;       Weather:    Couverture nuageuse partielle, 11/21 ℃
;;
;; Just add the following in an Org buffer:
;; %%(org-google-weather)
;;
;;; Code:

(require 'google-weather)
(require 'image)
(require 'format-spec)
(require 'solar)

(defgroup org-google-weather nil
  "Google Weather for Org mode."
  :group 'comm
  :group 'org)

(defcustom org-google-weather-location calendar-location-name
  "Default location for org-google-weather."
  :group 'org-google-weather)

(defcustom org-google-weather-format "%i %c, [%l,%h] %s"
  "String to return to describe the weather.
Valid %-sequences are:
  - %i the icon
  - %c means the weather condition
  - %L the supplied location
  - %C the city the weather is for
  - %l the lower temperature
  - %h the higher temperature
  - %s the temperature unit symbol")

(defcustom org-google-weather-cache-time 43200
  "Define for how many seconds we should cache the weather."
  :group 'org-google-weather)

(defcustom org-google-weather-display-icon-p t
  "Display icons."
  :group 'org-google-weather)

(defun org-google-weather-get-icon (url)
  (with-current-buffer
      (google-weather-retrieve-data-raw url org-google-weather-cache-time)
    (goto-char (point-min))
    (unless (search-forward "\n\n" nil t)
      (error "Data not found"))
    (let ((data (buffer-substring (point) (point-max))))
      (kill-buffer (current-buffer))
      data)))

;;;###autoload
(defun org-google-weather (&optional location language)
  "Return Org entry with the weather for LOCATION in LANGUAGE.
If LOCATION is not set, use org-google-weather-location."
  (let* ((location (or location org-google-weather-location))
         (data (ignore-errors
                 (google-weather-get-data location
                                          language
                                          org-google-weather-cache-time)))
         (problem-cause (when data (google-weather-data->problem-cause data)))
         (forecast (when (and (null problem-cause) data)
                     (google-weather-data->forecast-for-date data date))))
    (if problem-cause
        (message "%s: %s" location problem-cause)
      (when forecast
        (let ((condition (cadr (assoc 'condition forecast)))
              (low (cadr (assoc 'low forecast)))
              (high (cadr (assoc 'high forecast)))
              (city (google-weather-data->city data))
              ;; But *they* told me it's just about calling functions!
              (icon (when (display-images-p)
                      (create-image (org-google-weather-get-icon
                                     (cadr (assoc 'icon forecast)))
                                     nil t)))
              (temp-symbol (google-weather-data->temperature-symbol data)))
          (format-spec org-google-weather-format
                       `((?i . ,(if (and icon org-google-weather-display-icon-p)
                                    (propertize "icon"
                                                'display
                                                (append
                                                 icon
                                                 '(:ascent center))
                                                'rear-nonsticky '(display))
                                  ""))
                         (?c . ,condition)
                         (?L . ,location)
                         (?C . ,city)
                         (?l . ,low)
                         (?h . ,high)
                         (?s . ,temp-symbol))))))))

(provide 'org-google-weather)
