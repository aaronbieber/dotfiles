;;; org-gtd.el --- Getting Things Done configuration for Org Mode

;;; Commentary:

;;; Code:

(setq org-refile-targets '(("~/Dropbox/org/gtd/projects.org" :maxlevel . 1)
                           ("~/Dropbox/org/gtd/someday.org" :maxlevel . 1)
                           ("~/Dropbox/org/gtd/tickler.org" :maxlevel . 1)))


(setq org-capture-templates
      '(("t" "An incoming GTD item." entry
         (file "gtd/inbox.org")
         "* %?"
         :empty-lines 1)))

(define-key global-map (kbd "C-c c") 'org-capture)

(provide 'org-gtd)
;;; org-gtd.el ends here
