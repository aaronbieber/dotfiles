(when (maybe-require-package 'scss-mode)
  (add-hook 'scss-mode-hook (lambda ()
                              (setq-default css-indent-offset 2)))

(provide 'init-scss)
