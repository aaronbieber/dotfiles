;;; sunshine.el --- Provide weather and forecast information.
;;; Commentary:
;;
;; Use OpenWeatherMap's API to provide current weather and forecast information within Emacs!
;;
;;; Code:
(require 'url)
(require 'url-cache)
(require 'time-date)
(require 'json)

(defun sunshine-get-forecast (query &optional units)
  "Get forecast data from OpenWeatherMap's API.
Provide a location as QUERY and optionally the preferred unit
of measurement as UNITS (e.g. 'metric' or 'imperial')."
  (let* ((url (concat "http://api.openweathermap.org/data/2.5/forecast/daily?q=" query "&mode=json&units=imperial&cnt=7"))
         (buffer (get-buffer-create "*weather"))
         (data (url-retrieve-synchronously url)))
    (with-current-buffer buffer
      (insert data))))

(provide 'sunshine)
;;; sunshine.el ends here
