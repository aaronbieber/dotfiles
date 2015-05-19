;;; Global functions mostly used by mappings.

(require 'htmlfontify)
(defun fontify-and-browse ()
  "Fontify the current buffer into HTML, write it to a temp file, and open it in a browser."
  (interactive)
  (let* ((fontified-buffer (hfy-fontify-buffer))
         (temp-file-name (make-temp-file "ff" nil ".html")))
    (with-current-buffer fontified-buffer
      (write-region (point-min) (point-max) temp-file-name))
    (browse-url (concat "file://" temp-file-name))))

(defadvice load-theme (after restore-line-numbering)
  "Re-set linum-format after loading themes, which frequently overwrite it."
  (setq linum-format 'my-linum-relative-line-numbers))
(ad-activate 'load-theme)

(defun load-only-theme ()
  "Disable all themes and then load a single theme interactively."
  (interactive)
  (mapcar #'disable-theme custom-enabled-themes)
  (call-interactively 'load-theme))

(defun cycle-powerline-separators (&optional reverse)
  "Set Powerline separators in turn. If REVERSE is not nil, go backwards."
  (let* ((fn (if reverse 'reverse 'identity))
         (separators (funcall fn '("arrow" "arrow-fade" "slant"
                                   "chamfer" "wave" "brace" "roundstub" "zigzag"
                                   "butt" "rounded" "contour" "curve")))
         (found nil))
    (while (not found)
      (progn (setq separators (append (cdr separators) (list (car separators))))
             (when (string= (car separators) powerline-default-separator)
               (progn (setq powerline-default-separator (cadr separators))
                      (setq found t)))))))

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
                 (thing-at-point 'line t)
                 ))))))

(defun switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

;;; Functions for working with the X clipboard, which is very handy.
(defun yank-to-x-clipboard (&optional start end)
  (interactive "r")
  ;; Force the command to output everything to the default output buffer and don't show it.
  (shell-command-on-region start end "/usr/bin/xsel --clipboard -i > /dev/null" nil nil nil nil)
  (deactivate-mark))

(defun put-from-x-clipboard ()
  (interactive)
  (insert (shell-command-to-string "xsel --clipboard")))

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

(defun open-current-line-in-codebase-search ()
  "Go to the current file's current line on the codebase site."
  (interactive)
  (let* ((line-num (number-to-string (line-number-at-pos)))
         (file-path (replace-regexp-in-string (project-root) "" (buffer-file-name)))
         (args (concat "http://dox.wayfair.com/source/xref/php/" file-path "#" line-num)))
    (call-process "xdg-open" nil nil nil args)))

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

(provide 'init-global-functions)
