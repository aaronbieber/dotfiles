;;; octopress-mode.el --- Lightweight wrapper for Jekyll and Octopress.

;;; Commentary:

;;; Code:

(defvar om-server-process-buffer nil
  "A buffer that will contain the output of the Jekyll server.")

(defvar om-server-process nil
  "A process object that may point to a Jekyll server.")

(defvar octopress-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "q" 'om-quit)
    (define-key map "s" 'om-start-server)
    (define-key map "k" 'om-stop-server)
    (define-key map "N" 'om-new-post)
    (define-key map "n" 'om-new-draft)
    (define-key map "d" 'om-deploy)
    (define-key map "b" 'om-build)
    map)
  "Get the keymap for the Octopress window.")

(defun om-start-server (&optional with-drafts)
  (interactive))
(defun om-stop-server ()
  (interactive))
(defun om-restart-server ()
  (interactive))

(defun om-new-post ()
  (interactive))
(defun om-new-draft ()
  (interactive))
(defun om-deploy ()
  (interactive))
(defun om-build (&optional with-drafts)
  (interactive))

;;; "Private" -- not meant to be used by users.
(defun om--buffer-name-for-type (type)
  "Return a buffer name for the provided type."
  (concat "*om-" type "*"))

(defun om--server-status ()
  (and (processp om-server-process)
       (string= (process-status om-server-process) "run")))

(defun om--server-status-string ()
  (if (om--server-status)
      "Running"
    "Stopped"))

(defun om--prepare-buffer ()
  (let ((buf (get-buffer-create (om--buffer-name-for-type "status"))))
    (with-current-buffer buf
      (setq buffer-read-only nil)
      (erase-buffer)
      (kill-all-local-variables)
      (octopress-mode))
    buf))

(defun om-quit ()
  (interactive)
  (kill-buffer-and-window))

(defun om--get-root ()
  (let ((this-dir (if (and (boundp 'dired-directory) dired-directory)
                      dired-directory
                    (buffer-file-name))))
  (or (vc-find-root this-dir "_config.yml")
      (read-directory-name "Octopress site root: "))))

(defun om--draw-status (buffer status)
  "Draw a display of STATUS in BUFFER.

STATUS is an alist of status names and their printable values."
    (with-current-buffer buffer
      (erase-buffer)
      (insert "Octopress Status\n"
              "\n"
              "Blog root: " om-root "\n"
              "Server:    " (cdr (assoc 'server-status status)) "\n")
      (goto-char (point-min))
      (setq buffer-read-only t)))

(defun om-status ()
  "The main entry point into octopress-mode."
  (interactive)
  (let ((om-buffer (om--prepare-buffer))
        (om-root (om--get-root))
        (status `((server-status . ,(om--server-status-string)))))
    (with-current-buffer om-buffer
      (set (make-local-variable 'om-root) om-root))
    (om--draw-status om-buffer status)
    (pop-to-buffer om-buffer)))

(define-derived-mode octopress-mode nil "Octopress"
  "The major mode for interacting with a Jekyll site.

The following keys are available in `octopress-mode':

  \\{octopress-mode-map}"
  (setq truncate-lines t))

(provide 'octopress-mode)

;;; jekyll-hyde.el ends here
