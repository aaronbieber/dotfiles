;; init-php.el -- Configuration for PHP

;;; Commentary:

;;; Code:
(defun my-php-lineup-arglist-intro (langelem)
  "Align PHP argument list intro based on LANGELEM."
  (save-excursion
    (goto-char (cdr langelem))
    (vector (+ (current-column) (* 2 c-basic-offset)))))

(defun my-php-lineup-arglist-close (langelem)
  "Align PHP argument list close based on LANGELEM."
  (save-excursion
    (goto-char (c-langelem-pos langelem))
    (vector (current-column))))

(defun my-php-lineup-arglist-cont-nonempty (langelem)
  "Align continued arglist lines to two times the basic offset from LANGELEM."
  (save-excursion
    (goto-char (c-langelem-pos langelem))
    (vector (+ (current-column) (* 2 c-basic-offset)))))

(defun my-php-lineup-statement-cont (langelem)
  "Align PHP continued statements based on LANGELEM."
  (message (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
  (save-excursion
    (back-to-indentation)
    (if (search-forward "->" (line-end-position) t)
        (* 2 c-basic-offset)
      (php-lineup-string-cont langelem))))

(defun air--rbt-run-command (command)
  "Run an rbt COMMAND and send its output to a buffer."
  (let ((rbt-buffer (get-buffer-create "*rbt: command*"))
        (cmd (concat "rbt " command))
        (map (make-sparse-keymap)))
    (with-current-buffer rbt-buffer
      (local-set-key (kbd "q") #'(lambda () (interactive) (kill-buffer (current-buffer))))
      (shell-command cmd rbt-buffer)
      (goto-char (point-min))
      (if (search-forward "http" (point-max) t)
          (let ((url (thing-at-point 'url)))
            (if (yes-or-no-p (format "Browse to `%s'? " url))
                (browse-url url)))))))

(defun air-rbt-post (summary)
  "Post a new review with SUMMARY to Review Board."
  (interactive "MSummary: ")
  (let ((command (concat "post --summary " (shell-quote-argument summary))))
    (message "Running `%s'..." command)
    (air--rbt-run-command command)))

(defun air-rbt-update (id)
  "Update the review with ID on Review Board."
  (interactive "nRequest ID to update: ")
  (let ((command (concat "post -r " (shell-quote-argument (number-to-string id)))))
    (message "Running `%s'..." command)
    (air--rbt-run-command command)))

(defun air--configure-php-mode ()
  "Set up all of my PHP mode preferences."
  (require 'newcomment)
  (setq auto-fill-function 'do-auto-fill)
  (setq flycheck-disabled-checkers '(php-phpmd))

  (when (boundp 'company-backends)
    (setq-local company-backends
                '((company-dabbrev-code php-extras-company))))

  (when (fboundp 'php-extras-eldoc-documentation-function)
    (add-function :before-until (local 'eldoc-documentation-function)
                  'php-extras-eldoc-documentation-function))

  (air-set-php-group)
  (eldoc-mode t)
  (highlight-symbol-mode)
  (turn-on-auto-fill)
  (gtags-mode t)
  (flycheck-mode)
  (yas-minor-mode t))

(defun find-php-functions-in-current-buffer ()
  "Find lines that appear to be PHP functions in the buffer.

This function performs a regexp forward search from the top
\(point-min) of the buffer to the end, looking for lines that
appear to be PHP function declarations.

The return value of this function is a list of cons in which
the car of each cons is the bare function name and the cdr
is the buffer location at which the function was found."
  (save-excursion
    (goto-char (point-min))
    (let (res)
      (save-match-data
        (while (re-search-forward  "^ *\\(public \\|private \\|protected \\|static \\)*?function \\([^{]+\\)" nil t)
          (let* ((fn-name (save-match-data (match-string-no-properties 2)))
                 (fn-location (save-match-data (match-beginning 0))))
            (setq res
                  (append res
                          (list `(,fn-name . ,fn-location)))))))
      res)))

(defvar php-settings-groups
  '(("air"
     (lambda ()
       ;; Use the default PHP style.
       (c-set-style "php")
       (setq-local flycheck-phpcs-standard "PSR2")
       (set-fill-column 85)))
    ("wf"
     (lambda ()
       ;; Provide a style based on "php" that changes a couple of indent behaviors.
       (c-add-style "wf-php"
                    '("php"
                      (c-basic-offset . 2)
                      (c-offsets-alist . ((arglist-intro . my-php-lineup-arglist-intro)
                                          (arglist-close . my-php-lineup-arglist-close)
                                          (arglist-cont-nonempty . my-php-lineup-arglist-cont-nonempty)
                                          (statement-cont . my-php-lineup-statement-cont)
                                          (topmost-intro-cont . my-php-lineup-statement-cont)))))
       ;; Use that style.
       (c-set-style "wf-php")
       (setq-local flycheck-phpcs-standard "CSNStores")
       (set-fill-column 120))))
  "Groups of PHP settings.

Settings are expressed as an alist of settings group names and functions to call
to configure the necessary settings.")

(defun air-set-php-group (&optional php-settings-group)
  "Set all PHP settings appropriate for PHP-SETTINGS-GROUP.

If PHP-SETTINGS-GROUP is given, that group will be used.

If a group is not given, but the file `~/.emacs.d/php-settings-group' exists, is
readable, and contains a string matching one of the available settings groups, that
group will be used.  Otherwise, the first group in `php-settings-groups' will be
used."
  (let* ((group-file (expand-file-name "php-settings-group" user-emacs-directory))
         (group (or php-settings-group
                    (and (file-readable-p group-file)
                         (with-temp-buffer
                           (insert-file-contents group-file)
                           (buffer-substring (point-min) (point-max))))
                    (nth 0 php-settings-groups)))
         (settings (assoc group php-settings-groups)))
    (ignore-errors (funcall (cadr settings)))))

(use-package php-mode
  :mode "\\.php\\'"
  :config
  (add-hook 'php-mode-hook 'air--configure-php-mode))

(provide 'init-php)
;;; init-php.el ends here
