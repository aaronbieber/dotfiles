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

;; * Cache the HTTP response for a while.
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
  "Name for the Sunshine buffer."
  :group 'sunshine
  :type 'string)

(defcustom sunshine-mode-hook nil
  "Hook to run upon entering `sunshine-mode'.
See `run-hooks'."
  :group 'sunshine
  :type 'hook)

(defcustom sunshine-location "New York, NY"
  "The default location for which to retrieve weather."
  :group 'sunshine
  :type 'string)

(defcustom sunshine-cache-ttl (seconds-to-time 900)
  "How long to keep forecast data cached; sorry, it is a time value."
  :group 'sunshine
  :type '(repeat integer))

;;; Declaring this is polite, though this var is created later by url-http.
(defvar url-http-end-of-headers)

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
  "Extract the JSON response from the buffer returned by url-http.
Provide the buffer as BUF."
  (goto-char url-http-end-of-headers)
  (prog1 (json-read)
    (kill-buffer)))

(defun sunshine-get-forecast (location &optional units)
  "Get forecast data from OpenWeatherMap's API.
Provide a LOCATION and optionally the preferred unit
of measurement as UNITS (e.g. 'metric' or 'imperial')."
  (sunshine-prepare-window)
  (let* ((url (concat "http://api.openweathermap.org/data/2.5/forecast/daily?q="
                      (url-encode-url location)
                      "&mode=json&units=imperial&cnt=5")))
  (if (sunshine-cache-expired url)
      (url-retrieve url 'sunshine-retrieved)

    ;;(weather-json-buffer (url-retrieve-synchronously url)))
    (apply 'sunshine-retrieved
           (with-temp-buffer
             (mm-disable-multibyte)
             (url-cache-extract (url-cache-create-filename url))
             ;;(sunshine-build-simple-forecast
             ;;(sunshine-extract-response weather-json-buffer))))
             )))))

(defun sunshine-retrieved (status)
  "Process the retrieved data; receives STATUS."
  (let ((buf (get-buffer-create sunshine-buffer-name))
        (fc (sunshine-extract-response)))
    (url-store-in-cache (current-buffer))
    (with-current-buffer buf
      (progn
        (sunshine-draw-forecast
         (sunshine-build-simple-forecast fc))
        (fit-window-to-buffer)))))

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

(defun sunshine-build-simple-forecast (fc-data)
  "Build a simple, legible forecast from FC-DATA.
FC-DATA is the raw forecast data resulting from calling json-read on the
forecast results."
  (cl-loop for day across (cdr (assoc 'list fc-data)) collect
           (list
            (cons 'date (format-time-string "%A %h %e" (seconds-to-time (cdr (assoc 'dt day)))))
            (cons 'desc (cdr (assoc 'main (elt (cdr (assoc 'weather day)) 0))))
            (cons 'temp (cdr (assoc 'temp day)))
            (cons 'pressure (cdr (assoc 'pressure day))))))

(defun sunshine-key-quit ()
  "Destroy the Sunshine buffer."
  (interactive)
  (kill-buffer (get-buffer "*Sunshine*")))

(defun sunshine-prepare-window ()
  "Create the window and buffer used to display the forecast."
  (let* ((buf (get-buffer-create sunshine-buffer-name))
         (win (split-window-vertically)))
    (set-window-buffer win buf)
    (with-current-buffer buf
      (erase-buffer)
      (kill-all-local-variables)
      (set-window-dedicated-p (get-buffer-window buf) t)
      (sunshine-mode)
      (insert "Loading..."))))

(defun sunshine-draw-forecast (forecast)
  "Draw FORECAST in pretty ASCII."
  (erase-buffer)
  (let ((hline (concat "+"
                       (mapconcat 'identity
                                  (cl-loop for i from 1 to 5 collect
                                           (concat (make-string 18 ?-)
                                                   "+")) "")
                       "\n"))
        (output-rows
         (cl-loop for day in forecast
                  collect (cdr (assoc 'date day)) into dates
                  collect (cdr (assoc 'desc day)) into descs
                  collect (format "high: %s" (number-to-string (cdr (assoc 'max (cdr (assoc 'temp day)))))) into highs
                  collect (format "low:  %s" (number-to-string (cdr (assoc 'min (cdr (assoc 'temp day)))))) into lows
                  finally (return (list
                                   (cons "dates" dates)
                                   (cons "descs" descs)
                                   (cons "highs" highs)
                                   (cons "lows" lows))))))
    (setq buffer-read-only nil)
    (while output-rows
      (let* ((wholerow (car output-rows))
             (type (car wholerow))
             (row (cdr wholerow)))
        (while row
          (insert (sunshine-row-type-propertize (sunshine-pad-or-trunc (car row) 18 1) type)
                  (if (/= 1 (length row)) "|" ""))
          (setq row (cdr row)))
        (insert "\n")
        (setq output-rows (cdr output-rows))))
    (goto-char 0)
    (setq buffer-read-only t)))

(defun sunshine-row-type-propertize (string type)
  "Return STRING with face properties appropriate for TYPE."
  (or (cond ((equal type "dates") (propertize
                                  string
                                  'font-lock-face
                                  '(:underline t :weight "ultra-bold" :foreground "DodgerBlue")))
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
  (setq mode-name "Sunshine")
  (setq major-mode 'sunshine-mode)
  (run-mode-hooks 'sunshine-mode-hook))

(provide 'sunshine)
;;; sunshine.el ends here
