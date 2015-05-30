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
    (define-key map "g" 'om-refresh-status)
    (define-key map "N" 'om-new-post)
    (define-key map "n" 'om-new-draft)
    (define-key map "d" 'om-deploy)
    (define-key map "b" 'om-build)
    (define-key map "$" 'om-show-server)
    map)
  "Get the keymap for the Octopress status buffer.")

(defvar octopress-server-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "q" 'om-server-quit)
    map)
  "Get the keymap for the Octopress server buffer.")

;;; Customization
(defcustom octopress-posts-directory
  "_posts"
  "Directory containing posts, assumed to begin with /path/to/jekyll-site/"
  :type 'string
  :group 'octopress-mode)

(defcustom octopress-drafts-directory
  "_drafts"
  "Directory containing drafts, assumed to begin with /path/to/jekyll-site/"
  :type 'string
  :group 'octopress-mode)

(defun om-refresh-status ()
  (if (eq (current-buffer) (get-buffer (om--buffer-name-for-type "status")))
      (om--draw-status (current-buffer))))

(defun om-start-server (&optional with-drafts)
  (interactive)
  (om--start-server-process))

(defun om-stop-server ()
  (interactive)
  (om--stop-server-process))

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
(defun om--setup ()
  "Stuff that has to happen before anything else can happen."
  ;; Only set up if we have to...
  (let ((om-buffer (get-buffer (om--buffer-name-for-type "status"))))
    (if (om--buffer-is-configured om-buffer)
        om-buffer
      (let* ((om-buffer (om--prepare-status-buffer))
             (om-root (om--get-root)))
        (if (and om-buffer om-root)
            (progn (with-current-buffer om-buffer
                     (set (make-local-variable 'om-root) om-root))
                om-buffer)
          (progn (kill-buffer om-buffer)
                 nil))))))

(defun om--buffer-is-configured (buffer)
  "Return t if BUFFER is configured properly for Octopress Mode."
  (and (bufferp buffer)
       (let ((vars (buffer-local-variables
                    (get-buffer buffer))))
         (and (assoc 'om-root vars)
              (string= (cdr (assoc 'major-mode vars)) "octopress-mode")))))

(defun om--start-server-process ()
  (let ((om-server-process-buffer (om--prepare-server-buffer)))
    (if (processp (get-buffer-process om-server-process-buffer))
        (message "Server already running!"))
    (setq om-server-process
          (start-process-shell-command
           "octopress-server"
           om-server-process-buffer
           "cd ~/Blog && jekyll serve"))
    (message "Server started!")
    (set-process-sentinel om-server-process 'om--process-sentinel)
    (om--maybe-redraw-status)))

(defun om--stop-server-process ()
  (let ((server-process (get-buffer-process (om--buffer-name-for-type "server"))))
    (if (processp server-process)
        (process-send-string server-process (kbd "C-c")))))

(defun om--buffer-name-for-type (type)
  "Return a buffer name for the provided type."
  (concat "*om-" type "*"))

(defun om--process-sentinel (process event)
  (om--maybe-redraw-status)
  (message "Process `%s' had event `%s'." process event))

(defun om--server-status ()
  (let ((server-process (get-buffer-process (om--buffer-name-for-type "server"))))
    (and (processp server-process)
         (string= (process-status om-server-process) "run"))))

(defun om--server-status-string ()
  (if (om--server-status)
      "Running"
    "Stopped"))

(defun om--prepare-status-buffer ()
  "Create an empty, writable, Octopress Mode buffer and return it."
  (let ((buf (get-buffer-create (om--buffer-name-for-type "status"))))
    (with-current-buffer buf
      (setq buffer-read-only nil)
      (erase-buffer)
      (kill-all-local-variables)
      (octopress-mode))
    buf))

(defun om--prepare-server-buffer ()
  "Create an empty, writable, Octopress Server Mode buffer and return it."
  (let ((buf (get-buffer-create (om--buffer-name-for-type "server"))))
    (if (not (processp (get-buffer-process buf)))
             (with-current-buffer buf
               (setq buffer-read-only nil)
               (erase-buffer)
               (kill-all-local-variables)
               (octopress-server-mode)))
        buf))

(defun om-quit ()
  "Quit the Octopress Mode window, preserving its buffer."
  (interactive)
  (quit-window))

(defun om--get-root ()
  "Attempt to find the root of the Octopress site.

We assume we are running from a buffer editing a file somewhere within the site.
If we are running from some other kind of buffer, or a buffer with no file, the
user will be prompted to enter the path to an Octopress site."
  (let* ((this-dir (if (and (boundp 'dired-directory) dired-directory)
                      dired-directory
                    (if (buffer-file-name (current-buffer))
                        (file-name-directory (buffer-file-name (current-buffer)))))))
    (or (and this-dir
             (expand-file-name (vc-find-root this-dir "_config.yml")))
        (let ((candidate-dir (read-directory-name "Octopress site root: ")))
          (if (file-exists-p (expand-file-name "_config.yml" candidate-dir))
              (expand-file-name candidate-dir)
            (prog2 (message "Could not find _config.yml in `%s'." candidate-dir)
                nil))))))

(defun om--maybe-redraw-status ()
  (let ((status-buffer (get-buffer (om--buffer-name-for-type "status"))))
    (if (bufferp status-buffer)
        (om--draw-status status-buffer))))

(defun om--get-status-data (buffer)
  "Return the status of the Octopress site linked to BUFFER.

This function can only be called after `om-status' has been run and must be
passed the resulting BUFFER."
  (with-current-buffer buffer
    `((posts-count . ,(number-to-string
                       (length
                        (directory-files
                         (expand-file-name octopress-posts-directory om-root)
                         nil
                         "*.md$\\|.*markdown$"))))
      (drafts-count . ,(number-to-string
                        (length
                         (directory-files
                          (expand-file-name octopress-drafts-directory om-root)
                          nil
                          ".*md$\\|.*markdown$"))))
      (server-status . ,(om--server-status-string)))))

(defun om--draw-status (buffer)
  "Draw a display of STATUS in BUFFER.

STATUS is an alist of status names and their printable values."
  (let ((status (om--get-status-data buffer)))
    (with-current-buffer buffer
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert "Octopress Status\n"
              "\n"
              "    Blog root: " om-root "\n"
              "       Server: " (cdr (assoc 'server-status status)) "\n"
              "       Drafts: " (cdr (assoc 'drafts-count status)) "\n"
              "        Posts: " (cdr (assoc 'posts-count status)))
      (goto-char (point-min))
      (setq buffer-read-only t))))

(defun om-status ()
  "The main entry point into octopress-mode."
  (interactive)
  (let ((om-buffer (om--setup)))
    (if om-buffer
        (progn (om--draw-status om-buffer)
               (pop-to-buffer om-buffer)))))

(define-derived-mode octopress-mode nil "Octopress"
  "The major mode for interacting with a Jekyll site.

The following keys are available in `octopress-mode':

  \\{octopress-mode-map}"
  (setq truncate-lines t))

(define-derived-mode octopress-server-mode nil "Octopress[Server]"
  "The major mode for interacting with a Jekyll server process.

The following keys are available in `octopress-server-mode':

  \\{octopress-server-mode-map}"
  (setq truncate-lines t))

(provide 'octopress-mode)

;;; octopress-mode.el ends here
