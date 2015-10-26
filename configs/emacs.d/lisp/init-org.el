;;; init-org.el --- Set up Org Mode
;;; Commentary:

;; Basic Org Mode configuration, assuming presence of Evil & Evil Leader.

;;; Code:
(when (and (maybe-require-package 'org)
           (maybe-require-package 'evil-leader)
           (maybe-require-package 'evil))

  (setq org-agenda-text-search-extra-files '(agenda-archives))
  (setq org-agenda-files '("~/Dropbox/org/"))

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

  (evil-define-key 'normal org-mode-map (kbd "C-<") 'org-metaleft)
  (evil-define-key 'normal org-mode-map (kbd "C->") 'org-metaright)
  (evil-define-key 'insert org-mode-map (kbd "C-<") 'org-metaleft)
  (evil-define-key 'insert org-mode-map (kbd "C->") 'org-metaright)

  (add-hook 'org-mode-hook
            (lambda ()
              (define-key org-mode-map (kbd "C-c ,") 'org-time-stamp-inactive)
              (evil-define-key 'normal org-mode-map (kbd "TAB") 'org-cycle)
              (evil-define-key 'normal org-mode-map (kbd "C-\\") 'org-insert-heading)
              (evil-define-key 'insert org-mode-map (kbd "C-\\") 'org-insert-heading)
              (auto-fill-mode)
              (flyspell-mode))))

(provide 'init-org)
;;; init-org.el ends here
