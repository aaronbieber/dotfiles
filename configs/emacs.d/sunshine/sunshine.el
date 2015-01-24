;;; sunshine.el --- Provide weather and forecast information.

;; Author: Aaron Bieber

;; Version 0.1

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; 1) Copy this file somewhere in your Emacs `load-path'.  To see what
;;    your `load-path' is, run inside emacs: C-h v load-path<RET>
;;
;; 2) Add the following to your .emacs file:
;;
;;    (require 'sunshine)
;;
;; 3) Configure your location by setting the variable
;;    `sunshine-location'.  You can provide a string, like "New York,
;;    NY" or a ZIP code, like "90210".  This variable is available
;;    through the Customize facility.
;;
;; 4) To display the forecast for your location, call
;;    `sunshine-forecast'.

;;; Open issues:

;; * Try to resize windows more politely (fit-window-to-buffer expands the window below;
;;   it should try to shrink it to compensate, maybe).
;; * Add icons.

;;; Code:

(require 'cl-macs)
(require 'url)
(require 'url-cache)
(require 'time-date)
(require 'json)

;;; Options available for customization.
(defcustom sunshine-buffer-name "*Sunshine*"
  "Name for the Sunshine buffer.  You probably don't need to change this."
  :group 'sunshine
  :type 'string)

(defcustom sunshine-mode-hook nil
  "Hook to run upon entering `sunshine-mode'.
See `run-hooks'."
  :group 'sunshine
  :type 'hook)

(defcustom sunshine-location "New York, NY"
  "The default location for which to retrieve weather.
You can use a city/state value like \"New York, NY\" or a ZIP code like \"06032\"."
  :group 'sunshine
  :type 'string)

(defcustom sunshine-cache-ttl (seconds-to-time 900)
  "How long to keep forecast data cached; sorry, it is a time value.
The default value is 15 minutes (900 seconds)."
  :group 'sunshine
  :type '(repeat integer))

(defvar sunshine-mode-map
  (let ((map (make-sparse-keymap)))
    ;;(suppress-keymap map)
    (define-key map "q" 'sunshine-key-quit)
    map)
  "Get the keymap for the Sunshine window.")

(defun sunshine-forecast ()
  "The main entry into Sunshine; display the forecast in a window."
  (interactive)
  (sunshine-get-forecast sunshine-location))

(defun sunshine-extract-response ()
  "Extract the JSON response from the buffer returned by url-http."
  (when (re-search-forward "^HTTP/.+ 200 OK$" (line-end-position) t)
    (when (search-forward "\n\n" nil t)
      (prog1 (json-read)
        (kill-buffer)))))

(defun sunshine-make-url (location)
  "Make a URL suitable for retrieving the weather for LOCATION."
  (concat "http://api.openweathermap.org/data/2.5/forecast/daily?q="
                      (url-encode-url location)
                      "&mode=json&units=imperial&cnt=5"))

(defun sunshine-get-forecast (location &optional units)
  "Get forecast data from OpenWeatherMap's API.
Provide a LOCATION and optionally the preferred unit
of measurement as UNITS (e.g. 'metric' or 'imperial')."
  (sunshine-prepare-window)
  (let* ((url (sunshine-make-url location)))
    (if (sunshine-cache-expired url)
        (url-retrieve url 'sunshine-retrieved)
      (with-temp-buffer
        (mm-disable-multibyte)
        (url-cache-extract (url-cache-create-filename url))
        (sunshine-retrieved "status")))))

(defun sunshine-retrieved (status)
  "Process the retrieved data; receives STATUS."
  (url-store-in-cache (current-buffer))
  (let ((buf (get-buffer-create sunshine-buffer-name))
        (forecast (sunshine-extract-response)))
    (with-current-buffer buf
      (progn
        (sunshine-draw-forecast
         (sunshine-build-simple-forecast forecast))
        (fit-window-to-buffer (get-buffer-window buf))
        (select-window (get-buffer-window sunshine-buffer-name))))))

(defun sunshine-cache-expired (url)
  "Check cache for URL."
  (cond (url-standalone-mode
         (not (file-exists-p (url-cache-create-filename url))))
        (t (let ((cache-time (url-is-cached url)))
             (if cache-time
                 (time-less-p
                  (time-add
                   cache-time
                   sunshine-cache-ttl)
                  (current-time))
               t)))))

(defun sunshine-build-simple-forecast (forecast)
  "Build a simple, legible forecast from FORECAST.
FORECAST is the raw forecast data resulting from calling json-read on the
forecast results."
  (let* ((citylist (cdr (assoc 'city forecast)))
         (city (cdr (assoc 'name citylist)))
         (country (cdr (assoc 'country citylist))))
    (list
     (cons 'location (concat city ", " country))
     (cons 'days (cl-loop for day across (cdr (assoc 'list forecast)) collect
                         (list
                          (cons 'date (format-time-string "%A, %h. %e" (seconds-to-time (cdr (assoc 'dt day)))))
                          (cons 'desc (cdr (assoc 'main (elt (cdr (assoc 'weather day)) 0))))
                          (cons 'temp (cdr (assoc 'temp day)))
                          (cons 'pressure (cdr (assoc 'pressure day)))))))))

(defun sunshine-key-quit ()
  "Destroy the Sunshine buffer."
  (interactive)
  (kill-buffer (get-buffer "*Sunshine*")))

(defun sunshine-prepare-window ()
  "Create the window and buffer used to display the forecast."
  (let* ((buf (get-buffer-create sunshine-buffer-name))
         (win (or (get-buffer-window sunshine-buffer-name)
                  (split-window-vertically))))
    (set-window-buffer win buf)
    ;; Start the window rather short so it doesn't jarringly change
    ;; size after the download.
    (window-resize-no-error win (- 10 (window-total-height win)))
    (with-current-buffer buf
      (setq buffer-read-only nil)
      (erase-buffer)
      (kill-all-local-variables)
      (set-window-dedicated-p (get-buffer-window buf) t)
      (sunshine-mode)
      (insert "Loading...")
      (setq buffer-read-only t))))

(defun sunshine-draw-forecast (forecast)
  "Draw FORECAST in pretty ASCII."
  (let* (
         (location (cdr (assoc 'location forecast)))
         (days (cdr (assoc 'days forecast)))
         (output-rows
          (cl-loop for day in days
                   collect (cdr (assoc 'date day)) into dates
                   collect (cdr (assoc 'desc day)) into descs
                   collect (format "High: %s" (number-to-string (cdr (assoc 'max (cdr (assoc 'temp day)))))) into highs
                   collect (format "Low:  %s" (number-to-string (cdr (assoc 'min (cdr (assoc 'temp day)))))) into lows
                   finally (return (list
                                    (cons "dates" dates)
                                    (cons "descs" descs)
                                    (cons "highs" highs)
                                    (cons "lows" lows))))))
    (setq buffer-read-only nil)
    (erase-buffer)
    (insert (concat " "
                    ;; Heading, in tall text.
                    (propertize (concat "Forecast for " location)
                                    'font-lock-face '(:foreground "navajo white" :height 1.5))
                    ;; Newline, providing extra space below.
                    (propertize
                     "\n"
                     'line-spacing .5)))
    (while output-rows
      (let* ((wholerow (car output-rows))
             (type (car wholerow))
             (row (cdr wholerow)))
        (while row
          (insert (sunshine-row-type-propertize (sunshine-pad-or-trunc (car row) 20 1) type)
                  (if (and
                       ;; This is not the last column
                       (/= 1 (length row))
                       ;; This is not the dates row
                       (not (equal type "dates")))
                      (propertize "\u2502" 'font-lock-face '(:foreground "gray50"))
                    " "))
          (setq row (cdr row)))
        (insert (sunshine-newline-propertize type))
        (setq output-rows (cdr output-rows))))
    (goto-char 0)
    (setq buffer-read-only t)))

(defun sunshine-newline-propertize (type)
  "Output a newline appropriate for a line of TYPE."
  (if (equal type "dates")
      (propertize "\n"
                  'line-spacing .5)
    "\n"))

(defun sunshine-row-type-propertize (string type)
  "Return STRING with face properties appropriate for TYPE."
  (or (cond ((equal type "dates") (propertize
                                   string
                                   'font-lock-face
                                   '(:weight ultra-bold :foreground "white")))
            ((equal type "descs") string)
            ((equal type "highs") string)
            ((equal type "lows") string))
      string))

(defun sunshine-pad-or-trunc (string column-width &optional pad trunc-string)
  "Pad or truncate STRING to fit in COLUMN-WIDTH.
Optionally, add PAD spaces before and after STRING, and if STRING exceeds the
available width, truncate it to fit, optionally appending TRUNC-STRING."
  (let* ((actual-width (- column-width (if pad (* pad 2) 0)))
         (display-string (if (> (length string) actual-width)
                             ;; If string exceeds size, truncate.
                             (concat (substring string 0 (- actual-width (if trunc-string (length trunc-string) 0))) trunc-string)
                           ;; Otherwise, pad.
                           (concat string (make-string (- actual-width (length string)) ? )))))
    (concat (if pad (make-string pad ? ))
            display-string
            (if pad (make-string pad ? )))))

(defun sunshine-mode ()
  "A major mode for the Sunshine window.

The following keys are available in `sunshine-mode':

  \\{sunshine-mode-map}"
  (interactive)
  (use-local-map sunshine-mode-map)
  (setq truncate-lines t)
  (setq mode-name "Sunshine")
  (setq major-mode 'sunshine-mode)
  (run-mode-hooks 'sunshine-mode-hook))

(provide 'sunshine)
;;; sunshine.el ends here
