;;; narrow-to-mode.el -- Edit a narrowed region with a different major mode.
;;; Commentary:
;;; Code:

(defcustom ntm-markdown-symbol-mapping
  '(("markdown" . "markdown-mode")
    ("cl" . "lisp-interaction-mode")
    ("php" . "php-mode"))
  "A mapping from markdown language symbols to the modes they should be edited in.")

(defcustom narrow-to-code-recenter-on-widen t
  "Upon widening, recenter top to bottom automatically?")

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
    (let* ((start (re-search-backward "^~~~\\(.+\\)$" (buffer-end -1) t 1))
           (lang (when start (match-string 1)))
           (start (and start (line-end-position)))
           (end (re-search-forward "^~~~$" (buffer-end 1) t 1))
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

(defun ntm-narrow-to-code-block-at-point ()
  "Narrow to a code block surrounding point, if one can be found."
  (interactive)
  (deactivate-mark)
  (let* ((block (ntm-find-markdown-code-block))
         (start (cadr block))
         (end (caddr block))
         (lang (car block))
         (mode (cdr (assoc lang ntm-markdown-symbol-mapping))))
    (if (and block
             mode)
        (ntm-narrow-to-new-mode start end mode)
      (message "No code block found to narrow to."))))

(defun ntm-widen (recenter)
  (interactive "P")
  (when (and (boundp 'ntm-previous-mode)
             (length ntm-previous-mode))
    (widen)
    (funcall (intern ntm-previous-mode))
    (when (or recenter narrow-to-code-recenter-on-widen)
      (recenter-top-bottom))))

(provide 'narrow-to-mode)
;;; narrow-to-mode.el ends here
