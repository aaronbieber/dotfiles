(when (maybe-require-package 'twittering-mode)
  (add-hook 'twittering-mode-hook
            (lambda ()
              (define-key twittering-mode-map (kbd ",o") 'delete-other-windows)
              (define-key twittering-mode-map (kbd ",b") 'helm-mini))))

(provide 'init-twitter)
