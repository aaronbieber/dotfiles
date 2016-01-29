;;; narrow-to-mode.el -- Edit a narrowed region with a different major mode.
;;; Commentary:
;;; Code:

(defcustom ntm-markdown-symbol-mapping
  '(("markdown" . "markdown-mode")
    ("cl" . "lisp-interaction-mode")
    ("php" . "php-mode"))
  "A mapping from markdown language symbols to the modes they should be edited in.")

(defcustom ntm-default-mode
  "text-mode"
  "The default mode to use if a match is not found in the mapping list.")

(defcustom ntm-recenter-on-widen t
  "Upon widening, recenter top to bottom automatically?")

(defcustom ntm-blocks
  '(
    ("^[ \t]*\\(?:~\\{3,\\}\\|`\\{3,\\}\\)\\(.+\\)$"
     "^[ \t]*\\(?:~\\{3,\\}\\|`\\{3,\\}\\)$"
     (lambda ()
       (let* ((lang (match-string 1))
              (lang (cdr (assoc lang ntm-markdown-symbol-mapping))))
         lang)))
    )
  "Alist of regexps matching editable blocks.

Each element takes the form
\(START-REGEXP END-REGEXP LANG-OR-CAPTURE-OR-FUNC)

Where START- and END-REGEXP are patterns matching the start and end of
the block, respectively, and LANG-OR-CAPTURE-OR-FUNC is either a
symbol representing the language the block should be edited in, an
integer indicating the capture group to pass to `match-string' to get
the language, or a function to call to get the language.

The function will only be called if the regexp matches, so you can
rely on the presence of match data.")

(defvar-local ntm-previous-mode nil
  "Mode set before narrowing, restored upon widening.")

(defun ntm-find-markdown-code-block ()
  "Find the extents and type of the code block at point.

Returns a list containing the specified language name, start position,
and end position of the code block, e.g. (\"cl\" 100 150), where the
language is \"cl\" and the block begins at buffer position 100 and
ends at buffer position 150.

Returns nil if a block cannot be found."
  (save-excursion
    (let* ((start (re-search-backward "^\\(?:~\\{3,\\}\\|`\\{3,\\}\\)\\(.+\\)$" (buffer-end -1) t 1))
           (lang (when start (match-string 1)))
           (start (and start (line-end-position)))
           (end (re-search-forward "^\\(?:~\\{3,\\}\\|`\\{3,\\}\\)$" (buffer-end 1) t 1))
           (end (and end (line-beginning-position))))
      (when (and start end)
        (set-text-properties 0 (length lang) nil lang)
        `(,lang ,(1+ start) ,(1- end))))))

(defun ntm-narrow-to-new-mode (start end mode)
  "Narrow to the range defined by START and END and set MODE.

This function will narrow the current buffer to the range defined by
START and END and then set MODE.  The current mode will be saved in a
buffer local variable so that when the widening function is called the
original mode is reset."
  (let ((previous-mode (symbol-name major-mode)))
    (narrow-to-region start end)
    (funcall (intern mode))
    (setq ntm-previous-mode previous-mode)))

(defun ntm-narrow-dwim ()
  "Narrow to a code block, or widen, depending on the situation."
  (interactive)
  (if (and (boundp 'ntm-previous-mode)
           (> (length ntm-previous-mode) 0))
      (ntm--widen)
    (ntm--narrow)))

(defun ntm--narrow ()
  "Look for a code block and, if found, narrow to it and set the mode."
  (deactivate-mark)
  (let* ((block (ntm-find-markdown-code-block))
         (start (cadr block))
         (end (caddr block))
         (lang (car block))
         (mode (or (cdr (assoc lang ntm-markdown-symbol-mapping))
                   ntm-default-mode)))
    (if (and block
             mode)
        (ntm-narrow-to-new-mode start end mode)
      (message "No code block found to narrow to."))))

(defun ntm--widen ()
  "Widen the buffer and restore the previous mode."
    (widen)
    (funcall (intern ntm-previous-mode))
    (when ntm-recenter-on-widen
      (recenter-top-bottom)))

;;; ================================================================================
;;; New buffer code starts here
;;; ================================================================================

(defun ntm-edit-code-at-point ()
  "Look for a code block at point and, if found, edit it."
  (interactive)

  )

(provide 'narrow-to-mode)
;;; narrow-to-mode.el ends here
