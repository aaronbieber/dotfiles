;;; octopress-mode.el --- Lightweight wrapper for Jekyll and Octopress.

;;; Commentary:

;;; Code:

(require 'cl-lib)

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
    (define-key map (kbd "C-n") 'om--move-to-next-heading)
    (define-key map (kbd "C-p") 'om--move-to-previous-heading)
    (define-key map (kbd "<tab>") 'om--maybe-toggle-visibility)
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
                    (goto-char (process-mark process))
                    (let ((inhibit-read-only t))
                      (insert (propertize "\nServer process ended.\n\n" 'face 'font-lock-warning-face))
                      (goto-char (point-max)))))))))

(defun om--server-status ()
  (let ((server-process (get-buffer-process (om--buffer-name-for-type "server"))))
    (and (processp server-process)
         (string= (process-status om-server-process) "run"))))

(defun om--server-status-string ()
  (if (om--server-status)
      "Running"
    "Stopped"))

(defun om--generic-process-filter (proc string)
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (let ((moving (= (point) (process-mark proc))))
        (save-excursion
          ;; Insert the text, advancing the process marker.
          (goto-char (process-mark proc))
          (insert string)
          (set-marker (process-mark proc) (point)))
        (if moving (goto-char (process-mark proc)))))))

(defun om--prepare-buffer-for-type (type &optional mode-function)
  "Prepare an empty buffer for TYPE and optionally run MODE-FUNCTION."
  (let ((buffer-name (om--buffer-name-for-type type)))
    (if (bufferp buffer-name)
        (get-buffer buffer-name)
      (let ((buf (get-buffer-create buffer-name)))
        (with-current-buffer buf
          (setq buffer-read-only t)
          (kill-all-local-variables)
          (if (functionp mode-function)
              (funcall mode-function)))
        buf))))

(defun om--prepare-status-buffer ()
  "Return the Octopress Mode (\"status\") buffer.

If the buffer doesn't exist yet, it will be created and prepared."
  (let ((status-buffer (om--prepare-buffer-for-type "status" 'octopress-mode)))
    (with-current-buffer status-buffer
      (add-to-invisibility-spec 'posts)
      (add-to-invisibility-spec 'drafts))
    status-buffer))

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
               (let ((candidate-dir (vc-find-root this-dir "_config.yml")))
                 (if candidate-dir (expand-file-name candidate-dir) nil)))
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

(defun om--move-to-next-heading ()
  (interactive)
  (goto-char
   (or (save-excursion
         (goto-char (line-end-position))
         (next-single-property-change (point) 'heading))
       (point))))

(defun om--move-to-previous-heading ()
  (interactive)
  (goto-char
   (or (save-excursion
         (goto-char (line-beginning-position))
         (previous-single-property-change (point) 'heading))
       (point)))
  (goto-char (line-beginning-position)))

(defun om--maybe-toggle-visibility ()
  (interactive)
  (save-excursion
    (goto-char (line-beginning-position))
    (let ((hidden (get-text-property (point) 'hidden)))
      (if hidden
          (if (memq hidden buffer-invisibility-spec)
              (remove-from-invisibility-spec hidden)
            (add-to-invisibility-spec hidden)))))
  (redraw-frame))

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
       (propertize " " 'heading t)
       "   Blog root: " om-root "\n"

       (propertize " " 'heading t)
       "      Server: " (cdr (assoc 'server-status status)) "\n"

       (propertize " " 'heading t 'hidden 'drafts)
       "      Drafts: " (cdr (assoc 'drafts-count status)) "\n"

       (propertize " " 'heading t 'hidden 'posts)
       "       Posts: " (cdr (assoc 'posts-count status)) "\n"

       (om--get-posts-display)
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

(defun om--get-posts-display ()
  (let ((post-list "")
        (posts (om--get-posts)))
    (cl-loop for post in posts do
             (setq post-list
                   (concat post-list (make-string 10 ? ) (propertize post 'face 'font-lock-variable-name-face) "\n")))
    (propertize post-list 'invisible 'posts)))

(defun om--legend-item (key label column-width)
  (let ((pad (- column-width (+ (length key) (length label) 2))))
    (concat
     (propertize key 'face 'font-lock-keyword-face) ": "
     label
     (make-string pad ? ))))

(defun om--get-posts ()
  (om--setup)
  (directory-files
   (expand-file-name octopress-posts-directory om-root)
   nil
   "*.md$\\|.*markdown$"))

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
      (set-process-filter process 'om--generic-process-filter)
      process)))

(defun om--octopress-sentinel (process event)
  (let ((program (process-name process))
        (event (replace-regexp-in-string "\n$" "" event)))
    (cond ((string-prefix-p "finished" event)
           (progn (with-current-buffer (om--buffer-name-for-type "process")
                    (let ((inhibit-read-only t))
                      (insert "--\n")
                      (goto-char (point-max))))
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
