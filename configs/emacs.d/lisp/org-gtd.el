;;; org-gtd.el --- Getting Things Done configuration for Org Mode

;;; Commentary:

;;; Code:

(defun air-org-gtd-refile-targets ()
  "Return a list of GTD files available for refile."
  (directory-files "~/Dropbox/org/gtd" 'full "^[^.].*org$"))

(setq org-agenda-files '("~/Dropbox/org/gtd/inbox.org"
                         "~/Dropbox/org/gtd/tickler.org"
                         "~/Dropbox/org/gtd/team.org"))

(setq org-refile-targets '(("~/Dropbox/org/gtd/inbox.org" :maxlevel . 1)
                           ("~/Dropbox/org/gtd/projects.org" :maxlevel . 1)
                           ("~/Dropbox/org/gtd/someday.org" :maxlevel . 1)
                           ("~/Dropbox/org/gtd/tickler.org" :maxlevel . 1)))

(setq org-refile-targets (list (cons (air-org-gtd-refile-targets) '(:maxlevel . 1))))

(setq org-enforce-todo-dependencies t)
(setq org-agenda-dim-blocked-tasks t)

(setq org-tag-alist '(("@home" . ?h)
                      ("@work" . ?w)
                      ("@anywhere" . ?a)))

(defun air-org-task-capture (&optional vanilla)
  "Capture a task with my default template.

If VANILLA is non-nil, run the standard `org-capture'."
  (interactive "P")
  (if vanilla
      (org-capture)
    (org-capture nil "t")))

(defun air-org-tickler-capture (&optional vanilla)
  "Capture a new scheduled (tickler) item.

If VANILLA is non-nil, run the standard `org-capture'."
  (interactive "P")
  (if vanilla
      (org-capture)
    (org-capture nil "r")))

(defun air-org-pop-to-inbox ()
  "Open the GTD inbox Org file."
  (interactive)
  (find-file "~/Dropbox/org/gtd/inbox.org")
  (org-cycle '(16))
  (goto-char 1)
  (org-evil-motion-forward-heading))

(setq org-capture-templates
      `(("t" "An incoming GTD item." entry
         (file "gtd/inbox.org")
         ,(concat "* %?\n"
                  ":PROPERTIES:\n"
                  ":ORDERED:  t\n"
                  ":END:\n")
         :empty-lines 1)

        ("r" "A Reminder (tickler)." entry
         (file "gtd/tickler.org")
         "* %?\nSCHEDULED: %^t"
         :empty-lines 1)))

(setq org-agenda-custom-commands
      '(("d" "GTD immediate tasks"
         ((todo "TODO" ((org-agenda-skip-function 'air-org-skip-if-habit)
                        (org-agenda-overriding-header "Immediate tasks")))
          (todo "WAITING" ((org-agenda-skip-function 'air-org-skip-if-habit)
                           (org-agenda-overriding-header "Waiting for")))
          (agenda "" ((org-agenda-span 1))))
         ((org-agenda-compact-blocks t)))

        ("l" "Legacy agenda and all TODOs"
         (;; Not-yet-done priority "A" entries (will also display
          ;; non-todo entries).
          (tags "PRIORITY=\"A\""
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                 (org-agenda-overriding-header "High-priority unfinished tasks:")))
          ;; Only todo entries (must be dated to appear in agenda)
          ;; These are usually habits; entries that are marked todo,
          ;; have a date in scope, and do not have a priority of "A".
          (agenda ""
                  ((org-agenda-span 1)
                   (org-agenda-skip-function '(org-agenda-skip-entry-if 'nottodo 'any))))
          ;; Only non-todo entries (still must be dated to appear in
          ;; here). These are things I just want to be aware of,
          ;; like anniversaries, vacations, or other peripheral
          ;; events.
          (agenda ""
                  ((org-agenda-span 1)
                   (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'any))))

          (alltodo ""
                   ((org-agenda-skip-function '(or (air-org-skip-if-habit)
                                                   (air-org-skip-if-priority ?A)
                                                   (org-agenda-skip-if nil '(scheduled deadline))))
                    (org-agenda-overriding-header "ALL normal priority tasks:")))
          ;; Items completed during this work week. My skip function
          ;; here goes through some contortions that may not be
          ;; necessary; it would be faster to simply show "closed in
          ;; the last 7 days". Maybe some other time.
          ;; (todo "DONE"
          ;;       ((org-agenda-skip-function 'air-org-skip-if-not-closed-this-week)
          ;;        (org-agenda-overriding-header "Closed this week:")))
          )
         ((org-agenda-compact-blocks t)))))

(define-key global-map (kbd "C-c c") 'air-org-task-capture)

(provide 'org-gtd)
;;; org-gtd.el ends here
