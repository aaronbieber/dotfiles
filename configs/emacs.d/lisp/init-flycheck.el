;;; init-flycheck.el --- Initialize Flycheck
;;; Commentary:
;;; Code:
(use-package let-alist
  :ensure t)

(use-package flycheck
  :ensure t
  :config
  (add-hook 'after-init-hook 'global-flycheck-mode)

  ;; Flycheck mode:
  (add-hook 'flycheck-mode-hook
            (lambda ()
              (when (maybe-require-package 'evil)
                (evil-define-key 'normal flycheck-mode-map (kbd "]e") 'flycheck-next-error)
                (evil-define-key 'normal flycheck-mode-map (kbd "[e") 'flycheck-previous-error))
              (when (maybe-require-package 'evil-leader)
                (evil-leader/set-key (kbd "e") 'flycheck-list-errors))))

  ;; Override default flycheck triggers
  (setq flycheck-emacs-lisp-load-path 'inherit
        flycheck-check-syntax-automatically '(save idle-change mode-enabled)
        flycheck-idle-change-delay 0.8
        flycheck-disabled-checkers '(php-phpmd)
        flycheck-phpcs-standard "CSNStores")

  (setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list))

(provide 'init-flycheck)
;;; init-flycheck.el ends here
