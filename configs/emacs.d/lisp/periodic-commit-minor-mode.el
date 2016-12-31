;;; periodic-commit-minor-mode.el --- Auto-commit files periodically

;; Name: periodic-commit-minor-mode
;; Author: Aaron Bieber <aaron@aaronbieber.com>
;; Maintainer: Aaron Bieber <aaron@aaronbieber.com>
;; Package-Requires: ((magit "1.0"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Description:
;;
;; Periodic Commit Minor Mode is a simple tool for automatically
;; committing all changes to a Git repository periodically.  This might be
;; useful if, for example, you use a local Git repository as a safeguard
;; against unwanted data loss or corruption within files that are not
;; part of a project that changes in a predictable, atomic manner (like a
;; software project does).
;;
;; I store all of my Org Mode notes in Dropbox, but if I were to
;; accidentally delete a file's contents, Dropbox would happily sync
;; that with all of my other devices and I would lose that data
;; permanently.  Therefore, using Git locally, within Dropbox, is a
;; nice way to hedge against that possibility.
;;
;; Periodic Commit Minor Mode, when activated, will create an
;; automatic commit of all changes within the current file's
;; repository when:
;;
;; 1. The file is saved, and
;; 2. It has been greater than `pcmm-commit-frequency' since the last commit.
;;
;; The default commit frequency is 30 minutes.
;;
;; Usage:
;;
;; To use Periodic Commit Minor Mode, simply activate it in a buffer
;; that is a part of some repository that you wish to commit to.
;;
;; There are a couple of customizable options, which you can configure
;; through the `customize' facility by running
;; `M-x customize-group RET pcmm RET'.

;;; Code:
(require 'magit)

(defgroup pcmm '()
  "Customizations for Periodic Commit Minor Mode")

(defcustom pcmm-commit-frequency 1800
  "How often to commit upon save, in seconds.

This is the minimum length of time that must pass between commits.
Saving files before this amount of time has elapsed will not trigger a
commit.  You can set this value to zero to commit every time you
save."
  :group 'pcmm
  :type 'integer)

(defcustom pcmm-commit-all t
  "Commit untracked files?

If t, untracked files will be added and committed upon commit.  If
nil, only tracked files will be committed (but all changed files will
be committed no matter what)."
  :group 'pcmm
  :type 'boolean)

(defun pcmm-handle-save ()
  "Respond to a buffer being saved."
  (when periodic-commit-minor-mode
    (pcmm--commit)))

(defun pcmm--commit-overdue-p ()
  "Are we due for a commit in this repository?"
  (> (- (string-to-int (format-time-string "%s"))
        (pcmm--get-log))
     pcmm-commit-frequency))

(defun pcmm--write-log (file)
  "Write the current timestamp to the pcmm log file in FILE."
  (let ((buf (get-buffer-create "*PCMM*")))
    (with-current-buffer buf
      (erase-buffer)
      (if (file-exists-p file)
          (insert (format-time-string "%s"))
        (insert "0"))
      (write-region (point-min) (point-max) file nil 0)
      (string-to-int (buffer-substring (point-min) (point-max))))))

(defun pcmm--read-log (file)
  "Read and return the contents of FILE."
  (let ((buf (get-buffer-create "*PCMM*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert-file-contents file)
      (string-to-int (buffer-substring (point-min) (point-max))))))

(defun pcmm--log-file-p (file)
  "Locate the pcmm log file FILE, return nil if it doesn't exist."
  (if (file-exists-p file)
      file
    nil))

(defun pcmm--get-log ()
  "Find or create a file for storing the last commit timestamp."
  (let* ((root (vc-git-root (buffer-file-name)))
         (log-file-name (expand-file-name ".pcmm" root))
         (log-value (if (pcmm--log-file-p log-file-name)
                       (pcmm--read-log log-file-name)
                      (pcmm--write-log log-file-name))))
    log-value))

(defun pcmm--update-log ()
  "Write the current time into the log file."
  (let* ((root (vc-git-root (buffer-file-name)))
         (log-file-name (expand-file-name ".pcmm" root)))
    (pcmm--write-log log-file-name)))

(defun pcmm--make-commit-message ()
  "Make a pretty and informative commit message."
  (format "Committed automatically at %s"
          (format-time-string "%F %r")))

;;;###autoload
(defun pcmm-commit ()
  "Force a commit of all changed files to the current file's repository.

This is a convenience in case you wish to force an automatic commit
with the auto-generated commit message without waiting for the
`pcmm-commit-frequency' to elapse."
  (interactive)
  (pcmm--commit t))

(defun pcmm--commit (&optional force)
  "Commit all changed files after some interval.

A commit will be made if this function is called from a buffer that is
visiting a file that is already part of a Git repository and this
function has never been called before, or it was last called longer
than `pcmm-commit-frequency' seconds ago.

If FORCE is not nil, a commit will be made and the interval time will
be refreshed, no matter what."
  (interactive)
  (if (or (not (buffer-file-name))
          (not (magit-file-tracked-p (buffer-file-name))))
      (error "Periodic Commit can only work on a saved file in a Git repository")
    (if (or force (pcmm--commit-overdue-p))
        (progn
          (magit-stage-modified pcmm-commit-all)
          (magit-commit (list "-m" (pcmm--make-commit-message)))
          (pcmm--update-log)
          (message "Automatically committed.")))))

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
