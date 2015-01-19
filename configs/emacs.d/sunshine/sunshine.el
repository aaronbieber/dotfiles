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

;;; Declaring this is polite, though this var is created later by url-http.
(defvar url-http-end-of-headers)

(defun cdr-for-car-in-list (sequence value)
  "Given SEQUENCE of cons, return the cdr of the cons having car equal to VALUE.
For example, for SEQUENCE ((\"first\" 1) (\"second\" 2)), VALUE \"second\"
returns \"2\"."
  (let ((del (make-symbol "del")))
    (cdr
     (car
      (remove del
              (mapcar (lambda (el)
                        (if (string-equal (car el) value) el del)) sequence))))))

(defun sunshine-extract-response (buf)
  "Extract the JSON response from the buffer returned by url-http.
Provide the buffer as BUF."
  (with-current-buffer buf
    (setq case-fold-search nil)
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "charset=utf-8" nil t)
        (set-buffer-multibyte t)))
    (goto-char url-http-end-of-headers)
    (prog1 (json-read)
      (kill-buffer buf))))

(defun sunshine-get-forecast (query &optional units)
  "Get forecast data from OpenWeatherMap's API.
Provide a location as QUERY and optionally the preferred unit
of measurement as UNITS (e.g. 'metric' or 'imperial')."
  (let* ((url (concat "http://api.openweathermap.org/data/2.5/forecast/daily?q=" query "&mode=json&units=imperial&cnt=7"))
         (buffer (get-buffer-create "*weather"))
         (weather-json-buffer (url-retrieve-synchronously url))
         (weather (with-current-buffer weather-json-buffer
                    (goto-char url-http-end-of-headers)
                    (prog1 (json-read)
                      (kill-buffer)))))
    ;; This just gives you the cnt specified above; 7.
    (cdr-for-car-in-list weather "cnt")
    ))

(provide 'sunshine)
;;; sunshine.el ends here
