;;; narrow-to-mode.el -- Edit a narrowed region with a different major mode.
;;; Commentary:
;;; Code:

(defcustom ntm-markdown-symbol-mapping
  '(("markdown" . "markdown-mode")
    ("cl" . "lisp-interaction-mode")
    ("php" . "php-mode"))
  "A mapping from markdown language symbols to the modes they should be edited in."
  :group 'narrow-to-mode)

(defcustom ntm-default-mode
  "text-mode"
  "The default mode to use if a match is not found in the mapping list."
  :group 'narrow-to-mode)

(defcustom ntm-recenter-on-widen t
  "Upon widening, recenter top to bottom automatically?"
  :group 'narrow-to-mode)

(defcustom ntm-blocks
  '(
    ("^[ \t]*\\(?:~\\{3,\\}\\|`\\{3,\\}\\)\\(.+\\)\n"
     "^[ \t]*\\(?:~\\{3,\\}\\|`\\{3,\\}\\)"
     1)
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
rely on the presence of match data."
  :group 'narrow-to-mode)

(defvar-local ntm-previous-mode nil
  "Mode set before narrowing, restored upon widening.")

(defvar ntm-edit-mode-hook nil
  "Hook run when narrow-to-mode has set the block's language mode.

You may want to use this to disable language mode configurations that
don't work well in the snippet view.")

(define-minor-mode ntm-edit-mode
  "A minor mode used when editing a narrow-to-mode block.")

(defun ntm-edit-mode-configure-buffer ()
  "Configure the narrow-to-mode edit buffer."
  (add-hook 'kill-buffer-hook
            #'(lambda () (delete-overlay ntm-overlay)) nil 'local))
(add-hook 'ntm-edit-mode-hook 'ntm-edit-mode-configure-buffer)

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
         (start (nth 1 block))
         (end (nth 2 block))
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

(defun ntm--make-edit-buffer-name (base-buffer-name lang)
  "Construct the buffer name for a source editing buffer."
  (concat "*Narrowed Edit " base-buffer-name "[" lang "]*"))

(defun ntm--get-block-around-point ()
  "Return metadata about block surrounding point.

Return nil if no block is found."
  (save-excursion
    (beginning-of-line)
    (let ((pos (point))
          (blocks ntm-blocks)
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
      "text-mode")))

(defun ntm-edit-code-at-point ()
  "Look for a code block at point and, if found, edit it."
  (interactive)
  (let* ((block (ntm--get-block-around-point))
         (beg (make-marker))
         (end (copy-marker (make-marker) t))
         lang code mode ovl edit-buffer vars)
    (if block
        (progn
          (setq beg (move-marker beg (car block))
                end (move-marker end (nth 1 block))
                lang (nth 2 block)
                code (buffer-substring-no-properties beg end)
                mode (ntm--get-mode-for-lang lang)
                ovl (make-overlay beg end)
                edit-buffer (generate-new-buffer
                             (ntm--make-edit-buffer-name (buffer-name) lang)))

          ;; Overlay
          (overlay-put ovl 'edit-buffer edit-buffer)
          (overlay-put ovl 'face 'secondary-selection)
          (overlay-put ovl :read-only "Please don't.")

          ;; Buffer setup
          (switch-to-buffer-other-window edit-buffer t)
          (insert code)
          (remove-text-properties (point-min) (point-max)
                                  '(display nil invisible nil intangible nil))
          
          (condition-case e
              (funcall mode)
            (error
             (message "Language mode `%s' fails with: %S" lang-f (nth 1 e))))
          (ntm-edit-mode)

          (set (make-local-variable 'ntm-overlay) ovl)
          (set (make-local-variable 'header-line-format) "Press C-c C-c to save.")
          
          ))))

(provide 'narrow-to-mode)
;;; narrow-to-mode.el ends here
