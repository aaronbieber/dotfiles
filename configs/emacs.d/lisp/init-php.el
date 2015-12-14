;; init-php.el -- Configuration for PHP

;;; Commentary:

;;; Code:
(defun my-php-lineup-arglist-intro (langelem)
  "Align PHP argument list intro based on LANGELEM."
  (save-excursion
    (goto-char (cdr langelem))
    (vector (+ (current-column) (* 2 c-basic-offset)))))

(defun my-php-lineup-arglist-close (langelem)
  "Align PHP argument list close based on LANGELEM."
  (save-excursion
    (goto-char (c-langelem-pos langelem))
    (vector (current-column))))

(defun my-php-lineup-arglist-cont-nonempty (langelem)
  "Align continued arglist lines to two times the basic offset from LANGELEM."
  (save-excursion
    (goto-char (c-langelem-pos langelem))
    (vector (+ (current-column) (* 2 c-basic-offset)))))

(defun my-php-lineup-statement-cont (langelem)
  "Align PHP continued statements based on LANGELEM."
  (message (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
  (save-excursion
    (back-to-indentation)
    (if (search-forward "->" (line-end-position) t)
        (* 2 c-basic-offset)
      (php-lineup-string-cont langelem))))

(defun configure-php-mode ()
  "Set up all of my PHP mode preferences."
  (require 'newcomment)
  (setq comment-auto-fill-only-comments 1)
  (setq auto-fill-function 'do-auto-fill)
  (setq flycheck-disabled-checkers '(php-phpmd))

  (when (boundp 'company-backends)
    (setq-local company-backends
                '((company-dabbrev-code php-extras-company))))

  (when (fboundp 'php-extras-eldoc-documentation-function)
    (add-function :before-until (local 'eldoc-documentation-function)
                  'php-extras-eldoc-documentation-function))

  (c-set-style "wf-php")
  (eldoc-mode t)
  (highlight-symbol-mode)
  (electric-pair-local-mode)
  (turn-on-auto-fill)
  (set-fill-column 120)
  (add-to-list 'write-file-functions 'delete-trailing-whitespace)
  (gtags-mode t)
  (flycheck-mode)
  (yas-minor-mode t))

;; Provide a style based on "php" that changes a couple of indent behaviors.
(c-add-style "wf-php"
             '("php"
               (c-basic-offset . 2)
               (c-offsets-alist . ((arglist-intro . my-php-lineup-arglist-intro)
                                   (arglist-close . my-php-lineup-arglist-close)
                                   (arglist-cont-nonempty . my-php-lineup-arglist-cont-nonempty)
                                   (statement-cont . my-php-lineup-statement-cont)
                                   (topmost-intro-cont . my-php-lineup-statement-cont)))))


(use-package php-mode
  :mode "\\.php\\'"
  :config
  (add-hook 'php-mode-hook 'configure-php-mode))

(provide 'init-php)
;;; init-php.el ends here
