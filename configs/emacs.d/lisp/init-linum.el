;;; Set up relative line numbering to mimic `:set number relativenumber`.
(global-linum-mode t)
(add-hook 'linum-before-numbering-hook 'my-linum-get-format-string)

;;; Stuff for line numbers.
(defun my-linum-get-format-string ()
  (let* ((width (max 4 (1+ (length (number-to-string
                             (count-lines (point-min) (point-max)))))))
         (format (concat "%" (number-to-string width) "d ")))
    (setq my-linum-format-string format)))

(defvar my-linum-current-line-number 0)

(setq linum-format 'my-linum-relative-line-numbers)

(defun my-linum-relative-line-numbers (line-number)
  (let* ( (offset (abs (- line-number my-linum-current-line-number)))
          (linum-display-value (if (= 0 offset)
				   my-linum-current-line-number
				   offset))
        )
    (propertize (format my-linum-format-string linum-display-value) 'face 'linum)))

(defadvice linum-update (around my-linum-update)
  (let ((my-linum-current-line-number (line-number-at-pos)))
    ad-do-it))
(ad-activate 'linum-update)

(provide 'init-linum)
