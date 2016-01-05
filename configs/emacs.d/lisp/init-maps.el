;;; init-maps.el -- Provide global key maps

;;; Commentary:
;;; Provide global maps that aren't specific to any mode or package.

;;; Code:
(defun air--pop-to-file (file &optional split)
  "Visit a FILE, either in the current window or a SPLIT."
  (if split
      (find-file-other-window file)
    (find-file file)))

(defun air-pop-to-org-todo (split)
  "Visit my main TODO list, in the current window or a SPLIT."
  (interactive "P")
  (air--pop-to-file "~/Dropbox/org/todo.org" split))

(defun air-pop-to-org-notes (split)
  "Visit my main notes file, in the current window or a SPLIT."
  (interactive "P")
  (air--pop-to-file "~/Dropbox/org/notes.org" split))

(defun air-pop-to-org-vault (split)
  "Visit my encrypted vault file, in the current window or a SPLIT."
  (interactive "P")
  (air--pop-to-file "~/Dropbox/org/vault.gpg" split))

(defun air-pop-to-org-agenda (split)
  "Visit the org agenda, in the current window or a SPLIT."
  (interactive "P")
  (org-agenda-list)
  (when (not split)
    (delete-other-windows)))

(define-key global-map (kbd "C-c c") (lambda () (interactive) (org-capture nil "a")))
(define-key global-map (kbd "C-c t n") 'air-pop-to-org-notes)
(define-key global-map (kbd "C-c t t") 'air-pop-to-org-todo)
(define-key global-map (kbd "C-c t v") 'air-pop-to-org-vault)
(define-key global-map (kbd "C-c t a") 'air-pop-to-org-agenda)
(define-key global-map (kbd "C-x C-q") 'kill-emacs)
(define-key global-map (kbd "C-c C-u") 'insert-char) ;; "u" for Unicode, get it?
(define-key global-map (kbd "C-c l")   'dictionary-lookup-definition)
(define-key global-map (kbd "C-c d f") 'find-name-dired)

;; C-v is "visual block" in normal mode, but use it for "paste" in insert mode.
(when (equal system-type 'darwin)
  (evil-define-key 'insert global-map (kbd "C-v") 'yank))

(provide 'init-maps)
;;; init-maps.el ends here
