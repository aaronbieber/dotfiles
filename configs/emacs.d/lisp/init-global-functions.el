;;; init-global-functions.el --- Global functions mostly used by mappings.
;;; Commentary:
;;; Code:
(defun air-transpose-word-forward ()
  "Transpose word at point one word forward."
  (interactive)
  (transpose-words 1))

(defun air-insert-week-number-heading (&optional bare)
  "Insert current week number/percent heading, unless BARE.

If a single prefix argument is given, the week number as a single
integer will be inserted rather than the full human-readable phrase
with percentage."
  (interactive "P")
  (let* ((week-str (format-time-string "%U"))
         (week-int (string-to-int week-str))
         (percent (truncate (fceiling (* (/ week-int 52.0) 100)))))
    (insert (concat
             (or bare "Week ")
             (number-to-string (string-to-int (format-time-string "%U")))
             (or bare (concat " (" (number-to-string percent) "%)"))))))

(defun air-transpose-word-backward ()
  "Transpose word at point one word backward."
  (interactive)
  (transpose-words -1))

(defun air--pop-to-file (file &optional split)
  "Visit a FILE, either in the current window or a SPLIT."
  (if split
      (find-file-other-window file)
    (find-file file)))

(defun air-open-eshell (arg)
  "Start the Emacs shell.

With one prefix argument ARG, start the shell in the current window.
With two prefix arguments, force the creation of a new session.
With three prefix arguments, create a new session in the current window."
  (interactive "p")
  (let ((same-window (or (= arg 4)
                         (= arg 64)))
        (new-session (or (= arg 16)
                         (= arg 64))))
    (if same-window
        (eshell new-session)
      (let ((buf (eshell new-session)))
        (switch-to-buffer (other-buffer buf))
        (switch-to-buffer-other-window buf)))))

(require 'htmlfontify)
(defun fontify-and-browse ()
  "Fontify the current buffer into HTML, write it to a temp file, and open it in a browser."
  (interactive)
  (let* ((fontified-buffer (hfy-fontify-buffer))
         (temp-file-name (make-temp-file "ff" nil ".html")))
    (with-current-buffer fontified-buffer
      (write-region (point-min) (point-max) temp-file-name))
    (browse-url (concat "file://" temp-file-name))))

(defun chrome-reload (&optional focus)
  "Use osascript to tell Google Chrome to reload.

If optional argument FOCUS is non-nil, give Chrome the focus as well."
  (interactive "P")
  (let ((cmd (concat "osascript -e 'tell application \"Google Chrome\" "
                     "to (reload (active tab of (window 1)))"
                     (if focus " & activate" "")
                     "'")))
    (shell-command cmd "*Reload Chrome")))

(defun load-only-theme (theme)
  "Disable all themes and then load THEME interactively."
  (interactive
   (list
    (completing-read "Load custom theme: "
                             (mapcar 'symbol-name
                                     (custom-available-themes)))))
  (mapcar #'disable-theme custom-enabled-themes)
  (load-theme (intern theme) t)
  (if (fboundp 'powerline-reset)
    (powerline-reset)))

(defun air-cycle-theme (&optional reverse)
  "Load the next (or previous if REVERSE is true) available theme."
  (interactive)
  (if (> (length custom-enabled-themes) 1)
      (message "You cannot cycle themes with more than one theme enabled")
    (let* ((current-theme (car custom-enabled-themes))
           (all-themes (if reverse
                           (reverse (custom-available-themes))
                         (custom-available-themes)))
           (first-theme (car all-themes))
           (go (lambda (theme)
                 (message "Loading %s." (symbol-name theme))
                 (disable-theme current-theme)
                 (load-theme theme)))
           theme)
      (if (catch 'done
            (while (setq theme (pop all-themes))
              (if (and (eq theme current-theme)
                       (setq theme (pop all-themes)))
                  (progn
                    (funcall go theme)
                    (throw 'done nil))))
            t)
          (funcall go first-theme)))))

(defun func-region (func start end)
  "Run FUNC over the region between START and END in current buffer."
  (save-excursion
    (let ((text (delete-and-extract-region start end)))
      (insert (funcall func text)))))

(defun hex-region (start end)
  "Hexify (URL encod) the region between START and END in current buffer."
  (interactive "r")
  (func-region #'url-hexify-string start end))

(defun unhex-region (start end)
  "Unhex (URL decode) the region between START and END in current buffer."
  (interactive "r")
  (func-region #'url-unhex-string start end))

(defun cycle-powerline-separators (&optional reverse)
  "Set Powerline separators in turn.  If REVERSE is not nil, go backwards."
  (interactive)
  (let* ((fn (if reverse 'reverse 'identity))
         (separators (funcall fn '("arrow" "arrow-fade" "slant"
                                   "chamfer" "wave" "brace" "roundstub" "zigzag"
                                   "butt" "rounded" "contour" "curve")))
         (found nil))
    (while (not found)
      (progn (setq separators (append (cdr separators) (list (car separators))))
             (when (string= (car separators) powerline-default-separator)
               (progn (setq powerline-default-separator (cadr separators))
                      (setq found t)
                      (redraw-display)))))))

(defun occur-last-search ()
  "Run `occur` with the last evil search term."
  (interactive)
  ;; Use the appropriate search term based on regexp setting.
  (let ((term (if evil-regexp-search
                  (car-safe regexp-search-ring)
                (car-safe search-ring))))
    ;; If a search term exists, execute `occur` on it.
    (if (> (length term) 0)
        (occur term)
      (message "No term to search for."))))

(defun air-calendar-next-day-of-week (&optional day)
  "Get the date of the next occurrence of DAY.

DAY is an integer where 0 is Sunday and 6 is Saturday.  If DAY is not
given, get the date one week from today."
  (let* ((day (or day
                  (nth 6 (decode-time))))
         (time (decode-time))
         (dow (nth 6 time))
         (day-of-month (nth 3 time))
         (new-dow (if (> day dow)        ; Check what's the new dow's index
                       (- day dow)       ; In the same week
                     (+ (- 7 dow) day))) ; In the next week
         (new-time (encode-time 0 0 0
                                (+ new-dow day-of-month)
                                (nth 4 time)
                                (nth 5 time))))
    new-time))

(defun show-first-occurrence ()
  "Display the location of the word at point's first occurrence in the buffer."
  (interactive)
  (save-excursion
    (let ((search-word (thing-at-point 'symbol t)))
      (goto-char 1)
      (re-search-forward search-word)
      (message (concat
                "L" (number-to-string (line-number-at-pos)) ": "
                (replace-regexp-in-string
                 "[ \t\n]*\\'"
                 ""
                 (thing-at-point 'line t)))))))

(defun selective-display-increase ()
  (interactive)
  (set-selective-display
   (if selective-display (+ selective-display 1) 1)))

(defun selective-display-decrease ()
  (interactive)
  (when selective-display
    (set-selective-display
     (if (< (- selective-display 1) 1)
         nil
       (- selective-display 1)))))

(defun my-align-single-equals ()
  "Align on the first single equal sign."
  (interactive)
  (align-regexp
   (region-beginning) (region-end)
   "\\(\\s-*\\)=" 1 1 nil))

;;; Helpers for narrowing.
(defun narrow-and-set-normal ()
  "Narrow to the region and, if in a visual mode, set normal mode."
  (interactive)
  (narrow-to-region (region-beginning) (region-end))
  (if (string= evil-state "visual")
      (progn (evil-normal-state nil)
             (evil-goto-first-line))))

(defun narrow-to-region-or-subtree ()
  "Narrow to a region, if set, otherwise to an Org subtree, if present."
  (interactive)
  (if (and mark-active
           (not (= (region-beginning) (region-end))))
      (narrow-and-set-normal)
    (if (derived-mode-p 'org-mode)
        (org-narrow-to-subtree))))

(defun air-narrow-dwim ()
  "Narrow to a thing or widen based on context.

Attempts to follow the Do What I Mean philosophy."
  (interactive)
  (if (buffer-narrowed-p)
      (widen)
    (narrow-to-region-or-subtree)))

;;; From http://beatofthegeek.com/2014/02/my-setup-for-using-emacs-as-web-browser.html
(defun wikipedia-search (search-term)
  "Search for SEARCH-TERM on wikipedia"
  (interactive
   (let ((term (if mark-active
                   (buffer-substring (region-beginning) (region-end))
                 (word-at-point))))
     (list (read-string (format "Wikipedia (%s): " term) nil nil term))))
  (w3m-browse-url (concat
               "http://en.m.wikipedia.org/w/index.php?search="
               search-term)))

(defun air--get-vc-root ()
    "Get the root directory of the current VC project.

This function assumes that the current buffer is visiting a file that
is within a version controlled project."
    (require 'vc)
    (vc-call-backend
     (vc-responsible-backend (buffer-file-name))
     'root (buffer-file-name)))

(provide 'init-global-functions)
;;; init-global-functions.el ends here
