;;; init-org.el --- Set up Org Mode
;;; Commentary:

;; Basic Org Mode configuration, assuming presence of Evil & Evil Leader.

;;; Code:
(use-package org
  :ensure t
  :defer t
  :commands (org-capture)
  :bind (("C-c c" . air-org-task-capture)
         ("C-c t n" . air-pop-to-org-notes)
         ("C-c t t" . air-pop-to-org-todo)
         ("C-c t v" . air-pop-to-org-vault)
         ("C-c t a" . air-pop-to-org-agenda)
         ("C-c t A" . org-agenda)
         ("C-c f k" . org-search-view)
         ("C-c f t" . org-tags-view))
  :config
  (setq org-agenda-text-search-extra-files '(agenda-archives))
  (setq org-agenda-files '("~/Dropbox/org/"))
  (setq org-todo-keywords
        '((sequence "☛ TODO" "○ IN-PROGRESS" "⚑ WAITING" "|" "✓ DONE" "✗ CANCELED")))

  (setq org-blank-before-new-entry '((heading . t)
                                     (plain-list-item . t)))
  (setq org-capture-templates
        '(("a" "My TODO task format." entry
           (file "todo.org")
           "* ☛ TODO %?
SCHEDULED: %t")))
  (setq org-default-notes-file "~/Dropbox/org/todo.org")
  (setq org-directory "~/Dropbox/org")
  (setq org-enforce-todo-dependencies t)
  (setq org-log-done (quote time))
  (setq org-log-redeadline (quote time))
  (setq org-log-reschedule (quote time))

  (evil-leader/set-key-for-mode 'org-mode
    "t"  'air-org-set-tags
    "p"  'org-set-property
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

  (defun air-org-task-capture ()
    "Capture a task with my default template."
    (interactive)
    (org-capture nil "a"))

  (defun air-org-agenda-capture ()
    "Capture a task in agenda mode, using the date at point."
    (interactive)
    (let ((org-overriding-default-time (org-get-cursor-date)))
      (org-capture nil "a")))

  (defun air-pop-to-org-todo (split)
    "Visit my main TODO list, in the current window or a SPLIT."
    (interactive "P")
    (air--pop-to-file "~/Dropbox/org/todo.org" split))

  (defun air-pop-to-org-notes (split)
    "Visit my main notes file, in the current window or a SPLIT."
    (interactive "P")
    (air--pop-to-file "~/Dropbox/org/notes.org" split))

  (defun air-pop-to-org-vault (split)
    "Visit my encrypted vault file, in the current window or a SPLIT."
    (interactive "P")
    (air--pop-to-file "~/Dropbox/org/vault.gpg" split))

  (defun air-pop-to-org-agenda (split)
    "Visit the org agenda, in the current window or a SPLIT."
    (interactive "P")
    (org-agenda-list nil "today" 'day)
    (when (not split)
      (delete-other-windows)))

  (defun air-org-insert-list-leader-or-self (char)
    (if (= (current-column) 0)
        (insert (concat " " char " "))
      (insert char)))

  (defun air--org-swap-tags (tags)
    "Replace any tags on the current headline with TAGS.

The assumption is that TAGS will be a string conforming to Org Mode's
tag format specifications, or nil to remove all tags."
    (interactive)
    (let ((old-tags (org-get-tags-string))
          (tags (if tags
                    (concat " " tags)
                  "")))
      (save-excursion
        (beginning-of-line)
        (re-search-forward
         (concat "[ \t]*" (regexp-quote old-tags) "[ \t]*$")
         (line-end-position) t)
        (replace-match tags)
        (org-set-tags t))))

  (defun air-org-set-tags (tag)
    "Add TAG if it is not in the list of tags, remove it otherwise.

TAG is chosen interactively from the global tags completion table."
    (interactive
     (list (let ((org-last-tags-completion-table
                  (if (derived-mode-p 'org-mode)
                      (org-uniquify
                       (delq nil (append (org-get-buffer-tags)
                                         (org-global-tags-completion-table))))
                    (org-global-tags-completion-table))))
             (org-icompleting-read
              "Tag: " 'org-tags-completion-function nil nil nil
              'org-tags-history))))
    (let* ((cur-list (org-get-tags))
           (new-tags (mapconcat 'identity
                                (if (member tag cur-list)
                                    (delete tag cur-list)
                                  (append cur-list (list tag)))
                                ":"))
           (new (if (> (length new-tags) 1) (concat " :" new-tags ":")
                  nil)))
      (air--org-swap-tags new)))

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
              ;; Special plain list leader inserts
              (dolist (char '("+" "-"))
                (define-key org-mode-map (kbd char)
                  `(lambda ()
                    (interactive)
                    (air-org-insert-list-leader-or-self ,char))))
              ;; Normal maps
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
