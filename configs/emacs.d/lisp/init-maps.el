;;; init-maps.el -- Provide global key maps

;;; Commentary:
;;; Provide global maps that aren't specific to any mode or package.

;;; Code:
(define-key global-map (kbd "C-x C-q") 'kill-emacs)
(define-key global-map (kbd "C-c u")   'insert-char) ;; "u" for Unicode, get it?
(define-key global-map (kbd "C-c s")   (lambda () (interactive) (ansi-term "zsh")))
(define-key global-map (kbd "s-e")     'eval-buffer)
(define-key global-map (kbd "s-d")     'eval-defun)
(define-key global-map (kbd "C-}")     'air-cycle-theme)

;; C-v is "visual block" in normal mode, but use it for "paste" in insert mode.
;; Only on Macs, of course.
(when (equal system-type 'darwin)
  (evil-define-key 'insert global-map (kbd "C-v") 'yank))

(provide 'init-maps)
;;; init-maps.el ends here
