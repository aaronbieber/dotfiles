;;; init-twitter.el -- Tweet from Emacs
;;; Commentary:
;;; Code:

(use-package twittering-mode
  :ensure t
  :commands (twit twittering-mode)
  :config
  (add-hook 'twittering-mode-hook
            (lambda ()
              (define-key twittering-mode-map (kbd ",o") 'delete-other-windows)
              (define-key twittering-mode-map (kbd ",b") 'helm-mini))))

(provide 'init-twitter)
;;; init-twitter.el ends here
