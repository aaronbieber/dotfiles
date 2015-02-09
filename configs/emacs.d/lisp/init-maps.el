;;; Provide global maps that aren't specific to any mode or package.

(define-key global-map (kbd "C-c t w") (lambda () (interactive) (find-file "~/Dropbox/org/work.org")))
(define-key global-map (kbd "C-c t h") (lambda () (interactive) (find-file "~/Dropbox/org/home.org")))

(when (memq window-system '(mac ns))
  (define-key global-map (kbd "<s-return>") 'toggle-frame-fullscreen))

(provide 'init-maps)
