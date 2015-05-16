;;; Provide global maps that aren't specific to any mode or package.

(define-key global-map (kbd "C-c t w") (lambda ()
                                         (interactive)
                                         (find-file-other-window "~/Dropbox/org/work.org")))
(define-key global-map (kbd "C-c t h") (lambda ()
                                         (interactive)
                                         (find-file-other-window "~/Dropbox/org/home.org")))
(define-key global-map (kbd "C-x C-q") 'kill-emacs)
(define-key global-map (kbd "C-c C-u") 'insert-char) ;; "u" for Unicode, get it?
(define-key global-map (kbd "C-c l") 'dictionary-lookup-definition)
(define-key global-map (kbd "C-c d f") 'find-name-dired)

;; C-v is "visual block" in normal mode, but use it for "paste" in insert mode.
(when (equal system-type 'darwin)
  (evil-define-key 'insert global-map (kbd "C-v") 'yank))

(provide 'init-maps)
