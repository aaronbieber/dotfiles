;;; sanityinc-tomorrow-day-overrides-theme.el --- Override theme!
;;; Commentary:

;;; Code:
(deftheme sanityinc-tomorrow-day-overrides
  "Overrides to be applied over `sanityinc-tomorrow-day'.")

(custom-theme-set-faces
 'sanityinc-tomorrow-day-overrides
 '(org-agenda-date-today ((t (:foreground "gold3"))))
 '(minibuffer-prompt ((t (:background "white")))))

(provide-theme 'sanityinc-tomorrow-day-overrides)
;;; sanityinc-tomorrow-day-overrides-theme.el ends here
