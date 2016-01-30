;;; narrow-to-mode.el -- Edit a narrowed region with a different major mode.
;;; Commentary:
;;; Code:

(defcustom narrow-to-mode-lang-modes
  '(("markdown" . markdown-mode)
    ("cl" . lisp-interaction-mode)
    ("php" . php-mode))
  "A mapping from markdown language symbols to the modes they should be edited in."
  :group 'narrow-to-mode
  :type '(repeat
          (cons
           (string "Language name")
           (symbol "Major mode"))))

(defcustom narrow-to-mode-default-mode
  'text-mode
  "The default mode to use if a language-appropriate mode cannot be determined."
  :group 'narrow-to-mode
  :type '(symbol))

(defcustom narrow-to-mode-blocks
  '(
    ("^[ \t]*\\(?:~\\{3,\\}\\|`\\{3,\\}\\)\\(.+\\)\n"
     "^[ \t]*\\(?:~\\{3,\\}\\|`\\{3,\\}\\)"
     1)
    )
  "Alist of regexps matching editable blocks.

Each element takes the form
\(START-REGEXP END-REGEXP LANG-RULE)

Where START- and END-REGEXP are patterns matching the start and end of
the block, respectively.

If LANG-RULE is a symbol, that symbol is assumed to be a language
name.

If LANG-RULE is an integer, it is assumed to be the number of a
capture group to pass to `match-string' to get the language (a capture
group within the START-REGEXP).

If the language value with `-mode' appended to it does not resolve to
a bound function, it will be used to look up a mode in
`narrow-to-mode-lang-modes'.  If the symbol doesn't match a key in
that list, the `narrow-to-mode-default-mode' will be used."
  :group 'narrow-to-mode
  :type '(repeat
          (list
           (regexp "Start regexp")
           (regexp "End regexp")
           (choice (integer "Capture group number")
                   (symbol "Language name")))))

(defvar-local ntm-previous-mode nil
  "Mode set before narrowing, restored upon widening.")

(define-minor-mode ntm-edit-mode
  "A minor mode used when editing a narrow-to-mode block."
  nil "NTM-Edit"
  '(((kbd "C-c '") . 'ntm-edit-exit)
    ((kbd "C-c C-k") . 'ntm-edit-abort)))

(defvar ntm-edit-mode-hook nil
  "Hook run when narrow-to-mode has set the block's language mode.

You may want to use this to disable language mode configurations that
don't work well in the snippet view.")

(defun ntm-edit-mode-configure ()
  "Configure the narrow-to-mode edit buffer."
  (add-hook 'kill-buffer-hook
            #'(lambda () (delete-overlay ntm-overlay)) nil 'local))

(add-hook 'ntm-edit-mode-hook 'ntm-edit-mode-configure)

(defsubst ntm-set-local (var value)
  "Make VAR local in current buffer and set it to VALUE."
  (set (make-local-variable var) value))

(defun ntm--make-edit-buffer-name (base-buffer-name lang)
  "Make an edit buffer name from BASE-BUFFER-NAME and LANG."
  (concat "*Narrowed Edit " base-buffer-name "[" lang "]*"))

(defun ntm--get-block-around-point ()
  "Return metadata about block surrounding point.

Return nil if no block is found."
  (save-excursion
    (beginning-of-line)
    (let ((pos (point))
          (blocks narrow-to-mode-blocks)
          block re-start re-end lang-id start end lang)
      (catch 'exit
        (while (setq block (pop blocks))
          (setq re-start (car block)
                re-end (nth 1 block)
                lang-id (nth 2 block))
          (if (or (looking-at re-start)
                  (re-search-backward re-start nil t))
              (progn
                (setq start (match-end 0)
                      lang (cond ((integerp lang-id)
                                  (match-string lang-id))
                                 ((functionp lang-id)
                                  (funcall lang-id))))
                (if (and (and (goto-char (match-end 0))
                              (re-search-forward re-end nil t))
                         (>= (match-beginning 0) pos))
                    (throw 'exit `(,start ,(match-beginning 0) ,lang))))))))))

(defun ntm--get-mode-for-lang (lang)
  "Try to get a mode name from language name LANG.

The assumption is that language `LANG' has a mode `LANG-mode'."
  (let ((mode-name (intern (concat lang "-mode"))))
    (if (fboundp mode-name)
        mode-name
      (if (assoc lang narrow-to-mode-lang-modes)
          (cdr (assoc lang narrow-to-mode-lang-modes))
        narrow-to-mode-default-mode))))

(defun ntm-edit-code-at-point ()
  "Look for a code block at point and, if found, edit it."
  (interactive)
  (let* ((block (ntm--get-block-around-point))
         (pos (point))
         (beg (make-marker))
         (end (copy-marker (make-marker) t))
         edit-point lang code mode ovl edit-buffer vars)
    (if block
        (progn
          (setq beg (move-marker beg (car block))
                end (move-marker end (nth 1 block))
                edit-point (1+ (- pos beg))
                lang (nth 2 block)
                code (buffer-substring-no-properties beg end)
                mode (ntm--get-mode-for-lang lang)
                ovl (make-overlay beg end)
                edit-buffer (generate-new-buffer
                             (ntm--make-edit-buffer-name (buffer-name) lang)))
          (if (string-match-p (rx "\n" string-end) code)
              (setq code (replace-regexp-in-string (rx "\n" string-end) "" code)))
          (overlay-put ovl 'edit-buffer edit-buffer)
          (overlay-put ovl 'face 'secondary-selection)
          (overlay-put ovl :read-only "Please don't.")
          (switch-to-buffer-other-window edit-buffer t)
          (insert code)
          (remove-text-properties (point-min) (point-max)
                                  '(display nil invisible nil intangible nil))
          (condition-case e
              (funcall mode)
            (error
             (message "Language mode `%s' fails with: %S" lang-f (nth 1 e))))
          (ntm-edit-mode)
          (ntm-set-local 'ntm-editor t)
          (ntm-set-local 'ntm-mark-beg beg)
          (ntm-set-local 'ntm-mark-end end)
          (ntm-set-local 'ntm-overlay ovl)
          (ntm-set-local 'header-line-format "Press C-c ' (C-c apostrophe) to save, C-c C-k to abort.")
          (goto-char edit-point)))))

(defun ntm--guard-edit-buffer ()
  "Throw an error if current buffer doesn't look like an edit buffer."
  (unless (bound-and-true-p ntm-editor)
    (error "This is not a narrow-to-mode editor; something is wrong")))

(defun ntm--abandon-edit-buffer (dest-buffer)
  "Trash the edit buffer and switch to DEST-BUFFER.

The edit buffer is expected to be the current buffer."
  (interactive "P")
  (ntm--guard-edit-buffer)
  (let ((buffer (current-buffer)))
    (switch-to-buffer-other-window dest-buffer)
    (delete-other-windows)
    (with-current-buffer buffer
      (set-buffer-modified-p nil))
    (kill-buffer buffer)))

(defun ntm-edit-exit ()
  "Conclude editing, replacing the original text."
  (interactive)
  (ntm--guard-edit-buffer)
  (let ((buffer (current-buffer))
        (code (buffer-string))
        (beg ntm-mark-beg)
        (end ntm-mark-end)
        (edit-point (point))
        (ovl ntm-overlay))
    (if (not (string-match-p "\\n$" code))
        (setq code (concat code "\n")))
    (ntm--abandon-edit-buffer (marker-buffer beg))
    (goto-char beg)
    (undo-boundary)
    (delete-region beg end)
    (insert code)
    (goto-char (1- (+ beg edit-point)))
    (set-marker beg nil)
    (set-marker end nil)))

(defun ntm-edit-abort ()
  "Conclude editing, discarding the edited text."
  (interactive)
  (ntm--guard-edit-buffer)
  (let ((dest-buffer (marker-buffer ntm-mark-beg)))
    (ntm--abandon-edit-buffer dest-buffer)))

(provide 'narrow-to-mode)
;;; narrow-to-mode.el ends here
