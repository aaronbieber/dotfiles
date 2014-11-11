;;; project-root.el --- Find project roots.
;;; Commentary:
;;
;; The majority of this code was borrowed from the fiplr package, which deals
;; with project roots sensibly.
;;
;;; Code:

(defvar project-root-markers '(".git" ".svn" ".hg")
  "Files or directories that indicate the root of a project.")

(defun project-root ()
  "Find the root of the project."
  (let ((cwd (if (buffer-file-name)
                 (directory-file-name
                  (file-name-directory (buffer-file-name)))
               (file-truename "."))))
    (or (project-find-root cwd project-root-markers)
        cwd)))

(defun project-find-root (path root-markers)
  "Tail-recursive part of project-root."
  (let* ((this-dir (file-name-as-directory (file-truename path)))
         (parent-dir (expand-file-name (concat this-dir "..")))
         (system-root-dir (expand-file-name "/")))
    (cond
     ((project-root-p path root-markers) this-dir)
     ((equal system-root-dir this-dir) nil)
     (t (project-find-root parent-dir root-markers)))))

(defun project-anyp (pred seq)
  "True if any value in SEQ matches PRED."
  (catch 'found
    (cl-map nil (lambda (v)
                  (when (funcall pred v)
                    (throw 'found v)))
            seq)))

(defun project-root-p (path root-markers)
  "Predicate to check if the given directory is a project root."
  (let ((dir (file-name-as-directory path)))
    (project-anyp (lambda (marker)
                  (file-exists-p (concat dir marker)))
                root-markers)))

(provide 'project-root)
;;; project-root.el ends here
