;;; narrow-to-mode.el -- Edit a region in a separate window, with a different major mode.
;;; Commentary:
;;; Code:

(defun air-find-code-block-at-point ()
  "Find the extents and type of the code block at point.

Returns a list containing the specified language name, start position,
and end position of the code block, e.g. (\"cl\" 100 150), where the
language is \"cl\" and the block begins at buffer position 100 and
ends at buffer position 150.

Returns nil if a block cannot be matched."
  (save-excursion
    (let* ((start (re-search-backward "^~~~\\(.+\\)$" (buffer-end -1) t 1))
           (lang (when start (match-string 1)))
           (start (and start (line-end-position)))
           (end (re-search-forward "^~~~$" (buffer-end 1) t 1))
           (end (and end (line-beginning-position))))
      (when (and start end)
        (set-text-properties 0 (length lang) nil lang)
        `(,lang ,(1+ start) ,(1- end))))))

(defun air-narrow-to-new-mode (start end mode)
  "Narrow to the range defined by START and END and set MODE.

This function will narrow the current buffer to the range defined by
START and END and then set MODE (by calling `MODE-mode').  The current
mode will be saved in a buffer local variable so that when the
widening function is called the original mode is reset."
  (interactive)
  (defvar-local narrow-to-mode-previous-mode major-mode)
  (narrow-to-region start end)
  (funcall (intern (concat mode "-mode"))))

(defcustom narrow-to-code-mode-mapping
  '(("markdown" . "markdown-mode"))
  "A mapping from markdown language symbols to the modes they should be edited in.")

(defun air-narrow-to-code-block-at-point ()
  "Narrow to a code block surrounding point, if one can be found."
  (interactive)
  (deactivate-mark)
  (let* ((block (air-find-code-block-at-point))
         (start (cadr block))
         (end (caddr block))
         (lang (car block))
         (mode (cdr (assoc lang narrow-to-code-mode-mapping))))
    (if (and block
             mode)
        (progn 
          (narrow-to-region start end)
          (funcall (intern mode)))
      (message "No code block found to narrow to."))))


(defun air-narrow-to-thing-with-mode (f mode)
  )

(defun air-edit-code-block-at-point ()
  "Narrow to a code block surround point if one can be found."
  (interactive)
  (deactivate-mark)
  (let ((block (air-find-code-block-at-point)))
    (if (length block)
        (progn
          (setq-local ext-edit-start (cadr block))
          (setq-local ext-edit-end (caddr block))
          (let* ((buffer-name (generate-new-buffer-name "*indirect*"))
                 (buf (temp-buffer-window-setup buffer-name)))
            (pop-to-buffer buf)
            (goto-char (point-min)))))))


(provide 'narrow-to-mode)
;;; narrow-to-mode.el ends here
