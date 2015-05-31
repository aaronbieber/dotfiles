;;; octopress-mode.el --- Lightweight wrapper for Jekyll and Octopress.

;;; Commentary:

;;; Code:

(defvar om-server-process-buffer nil
  "A buffer that will contain the output of the Jekyll server.")

(defvar om-server-process nil
  "A process object that may point to a Jekyll server.")

(defvar octopress-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "q" 'om-status-quit)
    (define-key map "s" 'om-start-stop-server)
    (define-key map "g" 'om-refresh-status)
    (define-key map "n" 'om-new-thing)
    (define-key map "d" 'om-deploy)
    (define-key map "b" 'om-build)
    (define-key map "$" 'om-show-server)
    (define-key map "!" 'om-show-process)
    map)
  "Get the keymap for the Octopress status buffer.")

(defvar octopress-server-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "q" 'om-server-quit)
    map)
  "Get the keymap for the Octopress server buffer.")

(defvar octopress-process-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "q" 'om-process-quit)
    map)
  "Get the keymap for the Octopress process buffer.")

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

;;(defcustom octopress-command-new-post)
;;(defcustom octopress-command-new-draft)
;;(defcustom octopress-command-build)
;;(defcustom octopress-command-deploy)

(defun om-refresh-status ()
  (interactive)
  (om--maybe-redraw-status))

(defun om-start-stop-server (&optional with-drafts)
  (interactive)
  (let ((choice (read-char-choice
                 "[s] Start, [d] Start w/drafts, [k] Kill, [q] Abort"
                 '(?s ?d ?k ?q))))
    (cond ((eq choice ?s)
           (progn (message "Starting server normally...")
                  (om--start-server-process)))
          ((eq choice ?d)
           (progn (message "Starting server with drafts...")
                  (om--start-server-process t)))
          ((eq choice ?k)
           (progn (message "Stopping server...")
                  (om--stop-server-process)))
          ((eq choice ?q)
           (message "Aborted.")))))

(defun om-restart-server ()
  (interactive))

(defun om-show-server ()
  (interactive)
  (pop-to-buffer (om--prepare-server-buffer)))

(defun om-show-process ()
  (interactive)
  (pop-to-buffer (om--prepare-process-buffer)))

(defun om-new-thing ()
  (interactive)
  (let ((choice (read-char-choice
                 "[p] Post, [d] Draft, [g] Page, [q] Abort"
                 '(?p ?d ?g ?q))))
    (cond ((eq choice ?p)
           (om--new-post))
          ((eq choice ?d)
           (om--new-draft))
          ((eq choice ?g)
           (om--new-page))
          ((eq choice ?q)
           (message "Aborted.")))))

(defun om-deploy ()
  (interactive))

(defun om-build (&optional with-drafts)
  (interactive))

(defun om-status-quit ()
  "Quit the Octopress Mode window, preserving its buffer."
  (interactive)
  (quit-window))

(defun om-server-quit ()
  "Quit the Octopress Server Mode window, preserving its buffer."
  (interactive)
  (quit-window))

(defun om-process-quit ()
  "Quit the Octopress Process Mode window, preserving its buffer."
  (interactive)
  (quit-window))

;;; "Private" -- not meant to be used by users.
(defun om--setup ()
  "Stuff that has to happen before anything else can happen."
  ;; Only set up if we have to...
  (let ((om-buffer (get-buffer (om--buffer-name-for-type "status"))))
    (if (om--buffer-is-configured om-buffer)
        om-buffer
      (setq om-root (om--get-root))
      (let* ((om-buffer (om--prepare-status-buffer)))
        (if (and om-buffer om-root)
            (progn (with-current-buffer om-buffer
                     (make-local-variable 'om-root))
                om-buffer)
          (progn (kill-buffer om-buffer)
                 nil))))))

(defun om--new-post ()
  (let ((name (read-string "Post name: ")))
    (om--run-octopress-command (concat "octopress new post \"" name "\""))))

(defun om--new-draft ()
  (let ((name (read-string "Draft name: ")))
    (om--run-octopress-command (concat "octopress new draft \"" name "\""))))

(defun om--new-page ()
  (let ((name (read-string "Page name: ")))
    (om--run-octopress-command (concat "octopress new page \"" name "\""))))

(defun om--buffer-is-configured (buffer)
  "Return t if BUFFER is configured properly for Octopress Mode."
  (and (bufferp buffer)
       (let ((vars (buffer-local-variables
                    (get-buffer buffer))))
         (and (assoc 'om-root vars)
              (string= (cdr (assoc 'major-mode vars)) "octopress-mode")))))

(defun om--start-server-process (&optional with-drafts)
  (om--setup)
  (let ((om-server-process-buffer (om--prepare-server-buffer))
        (drafts-opt (if with-drafts " -D" nil)))
    (if (processp (get-buffer-process om-server-process-buffer))
        (message "Server already running!")
      (setq om-server-process
            (start-process-shell-command
             "octopress-server"
             om-server-process-buffer
             (concat "cd " (om--get-root) " && jekyll serve" drafts-opt)))
      (message "Server started!")
      (set-process-sentinel om-server-process 'om--server-sentinel)
      (om--maybe-redraw-status))))

(defun om--stop-server-process ()
  (let ((server-process (get-buffer-process (om--buffer-name-for-type "server"))))
    (if (processp server-process)
        (process-send-string server-process (kbd "C-c")))))

(defun om--buffer-name-for-type (type)
  "Return a buffer name for the provided type."
  (concat "*om-" type "*"))

(defun om--server-sentinel (process event)
  (om--maybe-redraw-status)
  (let ((program (process-name process))
        (event (replace-regexp-in-string "\n$" "" event)))
    (message (concat program " - " event)) ;; @todo @delete
    (cond ((string-prefix-p "finished" event)
           (progn (message "Octopress server has stopped.")
                  (with-current-buffer (om--prepare-server-buffer)
                    (insert "\nServer process ended.")
                    (goto-char (point-max))))))))

(defun om--server-status ()
  (let ((server-process (get-buffer-process (om--buffer-name-for-type "server"))))
    (and (processp server-process)
         (string= (process-status om-server-process) "run"))))

(defun om--server-status-string ()
  (if (om--server-status)
      "Running"
    "Stopped"))

(defun om--prepare-buffer-for-type (type &optional mode-function)
  "Prepare an empty buffer for TYPE and optionally run MODE-FUNCTION."
  (let ((buffer-name (om--buffer-name-for-type type)))
    (if (bufferp buffer-name)
        (get-buffer buffer-name)
      (let ((buf (get-buffer-create buffer-name)))
        (with-current-buffer buf
          (setq buffer-read-only nil)
          (kill-all-local-variables)
          (if (functionp mode-function)
              (funcall mode-function)))
        buf))))

(defun om--prepare-status-buffer ()
  "Return the Octopress Mode (\"status\") buffer.

If the buffer doesn't exist yet, it will be created and prepared."
  (om--prepare-buffer-for-type "status" 'octopress-mode))

(defun om--prepare-server-buffer ()
  "Return the Octopress Server Mode buffer.

If the buffer doesn't exist yet, it will be created and prepared."
  (om--prepare-buffer-for-type "server" 'octopress-server-mode))

(defun om--prepare-process-buffer ()
  "Return the Octopress Process Mode buffer.

If the buffer doesn't exist yet, it will be created and prepared."
  (om--prepare-buffer-for-type "process" 'octopress-process-mode))

(defun om--get-root ()
  "Attempt to find the root of the Octopress site.

We assume we are running from a buffer editing a file somewhere within the site.
If we are running from some other kind of buffer, or a buffer with no file, the
user will be prompted to enter the path to an Octopress site."
  (let ((status-buffer (get-buffer (om--buffer-name-for-type "status")))
        (this-dir (if (and (boundp 'dired-directory) dired-directory)
                      dired-directory
                    (if (buffer-file-name (current-buffer))
                        (file-name-directory (buffer-file-name (current-buffer)))))))
    (if (and (bufferp status-buffer)
             (assoc 'om-root (buffer-local-variables status-buffer))
             (buffer-local-value 'om-root status-buffer))
        (buffer-local-value 'om-root status-buffer)
      (or (and this-dir
               (expand-file-name (vc-find-root this-dir "_config.yml")))
          (let ((candidate-dir (read-directory-name "Octopress site root: ")))
            (if (file-exists-p (expand-file-name "_config.yml" candidate-dir))
                (expand-file-name candidate-dir)
              (prog2 (message "Could not find _config.yml in `%s'." candidate-dir)
                  nil)))))))

(defun om--maybe-redraw-status ()
  "If the status buffer exists, redraw it with current information."
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
      (insert
       (propertize "Octopress Status\n" 'face 'font-lock-constant-face)
       "\n"
       "    Blog root: " om-root "\n"
       "       Server: " (cdr (assoc 'server-status status)) "\n"
       "       Drafts: " (cdr (assoc 'drafts-count status)) "\n"
       "        Posts: " (cdr (assoc 'posts-count status)) "\n"
       "\n"
       (propertize "Commands:\n" 'face 'font-lock-constant-face)
       " " (om--legend-item "n" "New" 18)
       (om--legend-item "s" "Server" 18)
       (om--legend-item "b" "Build" 18)
       (om--legend-item "d" "Deploy" 18) "\n"
       " " (om--legend-item "!" "Show Process" 18)
       (om--legend-item "$" "Show Server" 18)
       (om--legend-item "g" "Refresh" 18)
       (om--legend-item "q" "Quit" 18))
      (goto-char (point-min))
      (setq buffer-read-only t))))

(defun om--legend-item (key label column-width)
  (let ((pad (- column-width (+ (length key) (length label) 2))))
    (concat
     (propertize key 'face 'font-lock-keyword-face) ": "
     label
     (make-string pad ? ))))

(defun om--run-octopress-command (command)
  "Run an Octopress command, sending output to the process buffer.

Returns the process object."
  (om--setup)
  (let ((pbuffer (om--prepare-process-buffer))
        (om-root (om--get-root))
        (command (replace-regexp-in-string "'" "\\\\'" command)))
    (message "Running `%s'..." command)
    (with-current-buffer pbuffer
      (goto-char (point-max))
      (insert (propertize (format "Running `%s'...\n\n" command) 'face 'font-lock-variable-name-face)))
    (let ((process (start-process-shell-command
                    "octopress"
                    pbuffer
                    (concat "cd " om-root " && " command))))
      (set-process-sentinel process 'om--octopress-sentinel)
      process)))

(defun om--octopress-sentinel (process event)
  (let ((program (process-name process))
        (event (replace-regexp-in-string "\n$" "" event)))
    (cond ((string-prefix-p "finished" event)
           (progn (with-current-buffer (om--buffer-name-for-type "process")
                    (insert "--\n")
                    (goto-char (point-max)))
                  (message "Octopress has completed.")))
          ((string-prefix-p "exited" event)
           (message "Octopress exited abnormally; check the process output for information.")))))

(defun om--prop-command (key label)
  "Propertize a command legend item with pretty colors.

Return a propertized string like KEY: LABEL."
  (concat (propertize key 'face 'font-lock-keyword-face) ": " label))

;;;###autoload
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

(define-derived-mode octopress-process-mode nil "Octopress[Process]"
  "The major mode for interacting with Octopress and Jekyll shell commands.

The following keys are available in `octopress-process-mode':

  \\{octopress-server-mode-map}"
  (setq truncate-lines t))

(provide 'octopress-mode)

;;; octopress-mode.el ends here
