;;; org-gtd.el --- Getting Things Done configuration for Org Mode

;;; Commentary:

;;; Code:

(defun air-org-gtd-refile-targets ()
  "Return a list of GTD files available for refile."
  (directory-files "~/Dropbox/org/gtd" 'full "^[^.].*org$"))
(setq org-refile-targets (list (cons (air-org-gtd-refile-targets) '(:maxlevel . 1))))

(setq org-agenda-files '("~/Dropbox/org/gtd/inbox.org"
                         "~/Dropbox/org/gtd/team.org"))

(setq org-refile-targets '(("~/Dropbox/org/gtd/inbox.org" :maxlevel . 1)
                           ("~/Dropbox/org/gtd/someday.org" :maxlevel . 1)
                           ("~/Dropbox/org/gtd/tickler.org" :maxlevel . 1)))

(setq org-agenda-time-grid '((daily today require-timed)
                             ""
                             (800 1000 1200 1400 1600)))
(setq org-enforce-todo-dependencies t)
(setq org-agenda-dim-blocked-tasks t)
(setq org-tag-alist '(("@cal" . ?c)
                      ("@home" . ?h)))

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
         ,(concat "* IDEA %?\n"
                  ":PROPERTIES:\n"
                  ":ORDERED:  t\n"
                  ":CREATED:  %u\n"
                  ":END:\n")
         :empty-lines 1)

        ("r" "A Reminder (tickler)." entry
         (file "gtd/tickler.org")
         "* %?\nSCHEDULED: %^t"
         :empty-lines 1)

        ("l" "A link to read later." entry
         (file "gtd/reading.org")
         ,(concat "* TODO %c\n"
                  ":PROPERTIES:\n"
                  ":CREATED:  %u\n"
                  ":END:\n\n"
                  "%i")
         :empty-lines 1)))

(setq org-agenda-custom-commands
      '(("d" "GTD immediate tasks"
         ((todo "TODO"
                ((org-agenda-skip-function '(or (air-org-skip-if-habit)
                                                (org-agenda-skip-if nil '(scheduled))))
                 (org-agenda-overriding-header "Immediate tasks")
                 (org-agenda-files '("~/Dropbox/org/gtd/inbox.org"))))
          (todo "WAITING"
                ((org-agenda-skip-function 'air-org-skip-if-habit)
                 (org-agenda-overriding-header "Waiting for")
                 (org-agenda-files '("~/Dropbox/org/gtd/inbox.org"))))
          (agenda ""
                  ((org-agenda-span 1)
                   (org-agenda-use-time-grid nil)
                   (org-agenda-files '("~/Dropbox/org/gtd/inbox.org"
                                       "~/Dropbox/org/gtd/team.org"
                                       "~/Dropbox/org/gtd/tickler.org"
                                       "~/Dropbox/org/diary.org")))))
         ((org-agenda-compact-blocks t)))

        ("r" "Weekly review"
         ((agenda "" ((org-agenda-span 7)
                      (org-agenda-files '("~/Dropbox/org/gtd/inbox.org"))))
          (stuck "" ((org-stuck-projects
                      '("+LEVEL=1/-DONE-CANCELED"
                        ("TODO" "WAITING" "SOMEDAY") nil ""))
                     (org-agenda-overriding-header (concat "Stuck projects and new items"
                                                           (make-string 72 ?-)))
                     (org-agenda-files '("~/Dropbox/org/gtd/inbox.org"))))
          (todo "SOMEDAY"
                ((org-agenda-overriding-header (concat "Maybe someday is today... "
                                                       (make-string 74 ?-)))))
          (todo "" ((org-agenda-files '("~/Dropbox/org/gtd/reading.org"))
                    (org-agenda-overriding-header (concat "Reading list "
                                                          (make-string 87 ?-))))))
         ((org-agenda-compact-blocks t)))))

(define-key global-map (kbd "C-c c") 'air-org-task-capture)

(provide 'org-gtd)
;;; org-gtd.el ends here
