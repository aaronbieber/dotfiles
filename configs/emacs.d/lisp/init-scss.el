(when (maybe-require-package 'scss-mode)
  (defun scss-settings ()
    (setq-default css-indent-offset 2))

  (add-hook 'scss-mode-hook 'scss-settings))

(provide 'init-scss)
