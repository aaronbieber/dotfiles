(when (and (maybe-require-package 'powerline)
           (maybe-require-package 'powerline-evil))

  (setq powerline-default-separator 'arrow)

  (defface my-mode-line-active
    '((t (:foreground "#B9EDD6" :background "#105C3A")))
    "Active mode line face (left and right edges).")

  (defface my-mode-line-inactive
    '((t (:foreground "#11613D" :background "#082E1D")))
    "Inactive mode line face (left and right edges).")

  (defface my-powerline-active1
    '((t (:foreground "#B9EDD6" :background "#0C422A")))
    "Active primary powerline face.")

  (defface my-powerline-active2
    '((t (:foreground "#B9EDD6" :background "#082E1D")))
    "Active secondary powerline face.")

  (defface my-powerline-inactive1
    '((t (:foreground "#11613D" :background "#082E1D")))
    "Inactive primary powerline face.")

  (defface my-powerline-inactive2
    '((t (:foreground "#11613D" :background "#082E1D")))
    "Inactive secondary powerline face.")

  (defun my-powerline-default-theme ()
    "Setup the default mode-line."
    (interactive)
    (setq-default mode-line-format
                  '("%e"
                    (:eval
                     (let* ((active (powerline-selected-window-active))
                            (mode-line (if active 'my-mode-line-active 'my-mode-line-inactive))
                            (face1 (if active 'my-powerline-active1 'my-powerline-inactive1))
                            (face2 (if active 'my-powerline-active2 'my-powerline-inactive2))
                            (separator-left (intern (format "powerline-%s-%s"
                                                            (powerline-current-separator)
                                                            (car powerline-default-separator-dir))))
                            (separator-right (intern (format "powerline-%s-%s"
                                                             (powerline-current-separator)
                                                             (cdr powerline-default-separator-dir))))
                            (lhs (list (let ((evil-face (powerline-evil-face)))
                                         (if evil-mode
                                              (powerline-raw (powerline-evil-tag) evil-face)
                                              ))
                                       (if evil-mode
                                           (funcall separator-left (powerline-evil-face) mode-line))
                                       ;;(powerline-raw "%*" nil 'l)
                                       ;;(when powerline-display-buffer-size
                                       ;;  (powerline-buffer-size nil 'l))
                                       ;;(when powerline-display-mule-info
                                       ;;  (powerline-raw mode-line-mule-info nil 'l))
                                       (powerline-buffer-id mode-line 'l)
                                       (when (and (boundp 'which-func-mode) which-func-mode)
                                         (powerline-raw which-func-format mode-line 'l))
                                       (powerline-raw " " mode-line)
                                       (funcall separator-left mode-line face1)
                                       (when (boundp 'erc-modified-channels-object)
                                         (powerline-raw erc-modified-channels-object face1 'l))
                                       (powerline-major-mode face1 'l)
                                       (powerline-process face1)
                                       (powerline-minor-modes face1 'l)
                                       (powerline-narrow face1 'l)
                                       (powerline-raw " " face1)
                                       (funcall separator-left face1 face2)
                                       (powerline-vc face2 'r)
                                       (when (bound-and-true-p nyan-mode)
                                         (powerline-raw (list (nyan-create)) face2 'l))))
                            (rhs (list (powerline-raw global-mode-string face2 'r)
                                       (funcall separator-right face2 face1)
                                       (unless window-system
                                         (powerline-raw (char-to-string #xe0a1) face1 'l))
                                       (powerline-raw "%4l" face1 'l)
                                       (powerline-raw ":" face1 'l)
                                       (powerline-raw "%3c" face1 'r)
                                       (funcall separator-right face1 mode-line)
                                       (powerline-raw " " mode-line)
                                       (powerline-raw "%6p" mode-line 'r)
                                       (when powerline-display-hud
                                         (powerline-hud face2 mode-line)))))
                       (concat (powerline-render lhs)
                               (powerline-fill face2 (powerline-width rhs))
                               (powerline-render rhs))))))))
  
(provide 'init-powerline)
