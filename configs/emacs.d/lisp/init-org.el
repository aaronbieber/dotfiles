;;; init-org.el --- Set up Org Mode
;;; Commentary:

;; Basic Org Mode configuration, assuming presence of Evil & Evil Leader.

;;; Code:
(use-package org
  :ensure t
  :defer t
  :commands (org-capture)
  :config
  (setq org-agenda-text-search-extra-files '(agenda-archives))
  (setq org-agenda-files '("~/Dropbox/org/"))
  (setq org-todo-keywords
        '((sequence "☛ TODO" "○ IN-PROGRESS" "⚑ WAITING" "|" "✓ DONE" "✗ CANCELED")))

  (setq org-agenda-files (quote ("~/Dropbox/org/")))
  (setq org-blank-before-new-entry (quote ((heading) (plain-list-item))))
  (setq org-capture-templates
         (quote
          (("a" "My TODO task format." entry
            (file "~/Dropbox/org/todo.org")
            "* ☛ TODO %?
  SCHEDULED: %t"))))
  (setq org-default-notes-file "~/Dropbox/org/todo.org")
  (setq org-directory "~/Dropbox/org")
  (setq org-enforce-todo-dependencies t)
  (setq org-log-done (quote time))
  (setq org-log-redeadline (quote time))
  (setq org-log-reschedule (quote time))

  (evil-leader/set-key-for-mode 'org-mode
    "t"  'org-time-stamp-inactive
    "p"  '(lambda ()
            (interactive)
            (org-insert-property-drawer))
    "d"  'org-deadline
    "s"  'org-schedule
    "a"  'org-agenda
    "ns" 'org-narrow-to-subtree
    "$"  'org-archive-subtree)

  (defun air-org-insert-scheduled-heading ()
    "Insert a new org heading scheduled for today."
    (interactive)
    (call-interactively 'org-insert-todo-heading)
    (org-schedule nil (format-time-string "%Y-%m-%d")))

  (defun air-org-agenda-capture ()
    "Capture a task in agenda mode, using the date at point."
    (interactive)
    (let ((org-overriding-default-time (org-get-cursor-date)))
      (org-capture nil "a")))

  (add-hook 'org-agenda-mode-hook
            (lambda ()
              (define-key org-agenda-mode-map "j"         'org-agenda-next-line)
              (define-key org-agenda-mode-map "k"         'org-agenda-previous-line)
              (define-key org-agenda-mode-map "n"         'org-agenda-next-date-line)
              (define-key org-agenda-mode-map "p"         'org-agenda-previous-date-line)
              (define-key org-agenda-mode-map "c"         'air-org-agenda-capture)
              (define-key org-agenda-mode-map (kbd "RET") 'org-agenda-switch-to)

              ))

  (add-hook 'org-capture-mode-hook
            (lambda ()
              (evil-insert-state)))

  (add-hook 'org-mode-hook
            (lambda ()
              (setq-local my-timer
                          (run-with-idle-timer 1 t
                                               (lambda ()
                                                 (when (and (eq major-mode 'org-mode)
                                                            (and evil-state
                                                                 (not (eq evil-state 'insert)))
                                                            (buffer-file-name)
                                                            (buffer-modified-p))
                                                   (save-buffer)))))
              (define-key org-mode-map (kbd "C-c ,") 'org-time-stamp-inactive)
              (define-key org-mode-map (kbd "C-|") 'air-org-insert-scheduled-heading)
              (define-key org-mode-map (kbd "C-<") 'org-metaleft)
              (define-key org-mode-map (kbd "C->") 'org-metaright)
              (define-key org-mode-map (kbd "C-\\") 'org-insert-heading)
              (define-key org-mode-map (kbd "C-S-j") 'org-priority-down)
              (define-key org-mode-map (kbd "C-S-k") 'org-priority-up)
              (evil-define-key 'normal org-mode-map (kbd "TAB") 'org-cycle)
              (auto-fill-mode)
              (flyspell-mode)
              (org-indent-mode))))

(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(provide 'init-org)
;;; init-org.el ends here
