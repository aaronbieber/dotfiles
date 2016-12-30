;;; periodic-commit-minor-mode.el --- Auto-commit files periodically
;;; Commentary:

;;; Code:
(defgroup pcmm '() "Customizations for Periodic Commit Minor Mode")

(defun pcmm-commit ()
  "Commit all changed files, if possible."
  )

(defun pcmm-handle-save ()
  "Respond to a buffer being saved."
  (when periodic-commit-minor-mode
    (debug)
    (pcmm-commit)))

;;;###autoload
(define-minor-mode periodic-commit-minor-mode
  "Toggle the Periodic Commit minor mode.

When this mode is activated in one or more buffers belonging to a VCS
repository, periodically commit all changes to it."
  :lighter "P.Comm"
  (cond (periodic-commit-minor-mode
         (add-hook 'after-save-hook 'pcmm-handle-save nil t))
        (t
         (remove-hook 'after-save-hook 'pcmm-handle-save t))))

(provide 'periodic-commit-minor-mode)
;;; periodic-commit-minor-mode.el ends here
