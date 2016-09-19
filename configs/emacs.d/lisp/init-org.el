;;; init-org.el --- Set up Org Mode
;;; Commentary:

;; Basic Org Mode configuration, assuming presence of Evil & Evil Leader.

;; Helper functions
(defun air--org-display-tag (tag &optional focus)
  "Display entries tagged with TAG in a fit window.

Do not make the new window current unless FOCUS is set."
  (org-tags-view nil tag)
  (fit-window-to-buffer)
  (unless focus
    (other-window 1)))

(defun air-org-display-directs (&optional focus)
  "Display entries tagged with `direct'.

Do not make the new window current unless FOCUS is set."
  (interactive "P")
  (air--org-display-tag "direct" focus))

(defun air-org-display-managers (&optional focus)
  "Display entries tagged with `manager'.

Do not make the new window current unless FOCUS is set."
  (interactive "P")
  (air--org-display-tag "manager" focus))

(defun air-org-skip-subtree-if-habit ()
  "Skip an agenda entry if it has a STYLE property equal to \"habit\"."
  (let ((subtree-end (save-excursion (org-end-of-subtree t))))
    (if (string= (org-entry-get nil "STYLE") "habit")
        subtree-end
      nil)))

(defun air-org-skip-subtree-if-priority (priority)
  "Skip an agenda subtree if it has a priority of PRIORITY.

PRIORITY may be one of the characters ?A, ?B, or ?C."
  (let ((subtree-end (save-excursion (org-end-of-subtree t)))
        (pri-value (* 1000 (- org-lowest-priority priority)))
        (pri-current (org-get-priority (thing-at-point 'line t))))
    (if (= pri-value pri-current)
        subtree-end
      nil)))

(defun air--org-global-custom-ids ()
  "Find custom ID fields in all org agenda files."
  (let ((files (org-agenda-files))
        file
        air-all-org-custom-ids)
    (while (setq file (pop files))
      (with-current-buffer (org-get-agenda-file-buffer file)
        (save-excursion
          (save-restriction
            (widen)
            (goto-char (point-min))
            (while (re-search-forward "^[ \t]*:CUSTOM_ID:[ \t]+\\(\\S-+\\)[ \t]*$"
                                      nil t)
              (add-to-list 'air-all-org-custom-ids
                           `(,(match-string-no-properties 1)
                             ,(concat file ":" (number-to-string (line-number-at-pos))))))))))
    air-all-org-custom-ids))

(defun air-org-goto-custom-id ()
  "Go to the location of CUSTOM-ID, or prompt interactively."
  (interactive)
  (let* ((all-custom-ids (air--org-global-custom-ids))
         (custom-id (completing-read
                     "Custom ID: "
                     all-custom-ids)))
    (when custom-id
      (let* ((val (cadr (assoc custom-id all-custom-ids)))
             (id-parts (split-string val ":"))
             (file (car id-parts))
             (line (string-to-int (cadr id-parts))))
        (pop-to-buffer (org-get-agenda-file-buffer file))
        (goto-char (point-min))
        (forward-line line)
        (org-reveal)
        (org-up-element)))))

(defun air-org-insert-custom-id-link ()
  "Insert an Org link to a custom ID selected interactively."
  (interactive)
  (let* ((all-custom-ids (air--org-global-custom-ids))
         (custom-id (completing-read
                     "Custom ID: "
                     all-custom-ids)))
    (when custom-id
      (let* ((val (cadr (assoc custom-id all-custom-ids)))
             (id-parts (split-string val ":"))
             (file (car id-parts))
             (line (string-to-int (cadr id-parts))))
        (org-insert-link nil (concat file "::#" custom-id) custom-id)))))

(defun air-org-nmom-capture-template ()
  "Return a Nine Minutes on Monday weekly agenda template suitable for capture."
  (let* ((deadline-timestamp (format-time-string "<%Y-%m-%d %a>"
                                                 (air-calendar-next-day-of-week 5)))
         (deadline (format "   DEADLINE: %s\n\n" deadline-timestamp)))
    (concat (format "* Week %02d\n\n" (org-days-to-iso-week (org-today)))
            (concat "** ☛ TODO Care: \n" deadline
                    "** ☛ TODO Mastery: \n" deadline
                    "** ☛ TODO Recognition: \n" deadline
                    "** ☛ TODO Purpose: \n" deadline))))

(defun air-org-set-category-property (value)
  "Set the category property of the current item to VALUE."
  (interactive (list (org-read-property-value "CATEGORY")))
  (org-set-property "CATEGORY" value))

(defun air-org-insert-heading (&optional subheading)
  "Insert a heading or a subheading.

If the optional SUBHEADING is t, insert a subheading.  Inserting
headings always respects content."
  (interactive "P")
  (if subheading
      (org-insert-subheading t)
    (org-insert-heading t)))

(defun air-org-insert-scheduled-heading (&optional subheading)
  "Insert a new org heading scheduled for today.

Insert the new heading at the end of the current subtree if
FORCE-HEADING is non-nil."
  (interactive "P")
  (if subheading
      (org-insert-subheading t)
    (org-insert-todo-heading t t))
  (org-schedule nil (format-time-string "%Y-%m-%d")))

(defun air-org-task-capture (&optional vanilla)
  "Capture a task with my default template.

If VANILLA is non-nil, run the standard `org-capture'."
  (interactive "P")
  (if vanilla
      (org-capture)
    (org-capture nil "a")))

(defun air-org-agenda-capture (&optional vanilla)
  "Capture a task in agenda mode, using the date at point.

If VANILLA is non-nil, run the standard `org-capture'."
  (interactive "P")
  (if vanilla
      (org-capture)
    (let ((org-overriding-default-time (org-get-cursor-date)))
      (org-capture nil "a"))))

(defun air-org-agenda-toggle-date (current-line)
  "Toggle `SCHEDULED' and `DEADLINE' tag in the capture buffer."
  (interactive "P")
  (save-excursion
    (let ((search-limit (if current-line
                            (line-end-position)
                          (point-max))))

      (if current-line (beginning-of-line)
        (beginning-of-buffer))
      (if (search-forward "DEADLINE:" search-limit t)
          (replace-match "SCHEDULED:")
        (and (search-forward "SCHEDULED:" search-limit t)
             (replace-match "DEADLINE:"))))))

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
  (org-agenda nil "d")
  (when (not split)
    (delete-other-windows)))

(defun air--org-insert-list-leader-or-self (char)
  "If on column 0, insert space-padded CHAR; otherwise insert CHAR.

This has the effect of automatically creating a properly indented list
leader; like hyphen, asterisk, or plus sign; without having to use
list-specific key maps."
  (if (= (current-column) 0)
      (insert (concat " " char " "))
    (insert char)))

(defun air--org-swap-tags (tags)
  "Replace any tags on the current headline with TAGS.

The assumption is that TAGS will be a string conforming to Org Mode's
tag format specifications, or nil to remove all tags."
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

(defun air-org-goto-first-child ()
  "Goto the first child, even if it is invisible.

Return t when a child was found.  Otherwise don't move point and
return nil."
  (interactive)
  (let ((pos (point))
        (re (concat "^" outline-regexp))
        level)
    (when (condition-case nil (org-back-to-heading t) (error nil))
      (setq level (outline-level))
      (forward-char 1)
      (if (and (re-search-forward re nil t) (> (outline-level) level))
          (progn (goto-char (match-beginning 0)) t)
        (goto-char pos) nil))))


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
           (completing-read
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


;;; Code:
(use-package org
  :ensure t
  :defer t
  :commands (org-capture)
  :bind (("C-c c" .   air-org-task-capture)
         ("C-c l" .   org-store-link)
         ("C-c t n" . air-pop-to-org-notes)
         ("C-c t t" . air-pop-to-org-todo)
         ("C-c t v" . air-pop-to-org-vault)
         ("C-c t a" . air-pop-to-org-agenda)
         ("C-c t A" . org-agenda)
         ("C-c f k" . org-search-view)
         ("C-c f t" . org-tags-view)
         ("C-c f i" . air-org-goto-custom-id))
  :config
  (set-face-attribute 'org-upcoming-deadline nil :foreground "gold1")
  (setq org-agenda-text-search-extra-files '(agenda-archives))
  (setq org-agenda-files '("~/Dropbox/org/"))
  (setq org-todo-keywords
        '((sequence "☛ TODO" "○ IN-PROGRESS" "⚑ WAITING" "|" "✓ DONE" "✗ CANCELED")))
  (setq org-blank-before-new-entry '((heading . t)
                                     (plain-list-item . t)))
  (setq org-capture-templates
        '(("a" "My TODO task format." entry
           (file "todo.org")
           "* ☛ TODO %?\nSCHEDULED: %t")

          ("n" "A (work-related) note." entry
           (file+headline "notes.org" "Work")
           "* %?\n%u\n\n"
           :jump-to-captured t)

          ("w" "Nine Minutes on Monday weekly agenda." entry
           (id "9A6DDE04-90B8-49ED-90B9-A55A0D1E7B28")
           (function air-org-nmom-capture-template))))
  (setq org-default-notes-file "~/Dropbox/org/todo.org")
  (setq org-directory "~/Dropbox/org")
  (setq org-enforce-todo-dependencies t)
  (setq org-log-done (quote time))
  (setq org-log-redeadline (quote time))
  (setq org-log-reschedule (quote time))
  (setq org-agenda-skip-scheduled-if-done t)
  (setq org-insert-heading-respect-content t)
  (setq org-ellipsis " …")
  (setq org-startup-with-inline-images t)
  (setq org-export-initial-scope 'subtree)
  (setq org-use-tag-inheritance nil) ;; Use the list form, which happens to be blank
  (setq org-agenda-custom-commands
        '(("d" "Daily agenda and all TODOs"
           ((tags "PRIORITY=\"A\""
                  ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                   (org-agenda-overriding-header "High-priority unfinished tasks:")))
            (agenda "" ((org-agenda-ndays 1)))
            (alltodo ""
                     ((org-agenda-skip-function '(or (air-org-skip-subtree-if-habit)
                                                     (air-org-skip-subtree-if-priority ?A)))
                      (org-agenda-overriding-header "ALL normal priority tasks:"))))
           ((org-agenda-compact-blocks t)))))

  (evil-leader/set-key-for-mode 'org-mode
    "$"  'org-archive-subtree
    "a"  'org-agenda
    "c"  'air-org-set-category-property
    "d"  'org-deadline
    "ns" 'org-narrow-to-subtree
    "p"  'org-set-property
    "s"  'org-schedule
    "t"  'air-org-set-tags)

  (add-hook 'org-agenda-mode-hook
            (lambda ()
              (setq org-habit-graph-column 50)
              (define-key org-agenda-mode-map "j"          'org-agenda-next-line)
              (define-key org-agenda-mode-map "k"          'org-agenda-previous-line)
              (define-key org-agenda-mode-map "n"          'org-agenda-next-date-line)
              (define-key org-agenda-mode-map "p"          'org-agenda-previous-date-line)
              (define-key org-agenda-mode-map "c"          'air-org-agenda-capture)
              (define-key org-agenda-mode-map "R"          'org-revert-all-org-buffers)
              (define-key org-agenda-mode-map (kbd "RET")  'org-agenda-switch-to)

              (define-prefix-command 'air-org-run-shortcuts)
              (define-key air-org-run-shortcuts "f" (tiny-menu-run-item "org-files"))
              (define-key air-org-run-shortcuts "t" (tiny-menu-run-item "org-things"))
              (define-key air-org-run-shortcuts "c" (tiny-menu-run-item "org-captures"))
              (define-key air-org-run-shortcuts "l" (tiny-menu-run-item "org-links"))
              (define-key org-agenda-mode-map (kbd "\\") air-org-run-shortcuts)))

  (add-hook 'org-capture-mode-hook
            (lambda ()
              (evil-define-key 'insert org-capture-mode-map (kbd "C-d") 'air-org-agenda-toggle-date)
              (evil-define-key 'normal org-capture-mode-map (kbd "C-d") 'air-org-agenda-toggle-date)
              (evil-insert-state)))

  (add-hook 'org-mode-hook
            (lambda ()
              ;; Special plain list leader inserts
              (dolist (char '("+" "-"))
                (define-key org-mode-map (kbd char)
                  `(lambda ()
                    (interactive)
                    (air--org-insert-list-leader-or-self ,char))))

              ;; Normal maps
              (define-key org-mode-map (kbd "C-c d")   (lambda ()
                                                         (interactive) (air-org-agenda-toggle-date t)))
              (define-key org-mode-map (kbd "C-c ,")   'org-time-stamp-inactive)
              (define-key org-mode-map (kbd "C-|")     'air-org-insert-scheduled-heading)
              (define-key org-mode-map (kbd "C-\\")    'air-org-insert-heading)
              (define-key org-mode-map (kbd "s-r")     'org-revert-all-org-buffers)
              (define-key org-mode-map (kbd "C-c C-l") (tiny-menu-run-item "org-links"))

              (define-key org-mode-map (kbd "C-<")                'org-shiftmetaleft)
              (define-key org-mode-map (kbd "C->")                'org-shiftmetaright)

              ;; These are set as evil keys because they conflict with
              ;; existing commands I don't use, or are superseded by
              ;; some evil function that org-mode-map is shadowed by.
              (evil-define-key 'normal org-mode-map (kbd "TAB")   'org-cycle)

              (evil-define-key 'normal org-mode-map (kbd "C-,")   'org-metaleft)
              (evil-define-key 'normal org-mode-map (kbd "C-.")   'org-metaright)

              (evil-define-key 'insert org-mode-map (kbd "C-,")   'org-metaleft)
              (evil-define-key 'insert org-mode-map (kbd "C-.")   'org-metaright)

              (evil-define-key 'normal org-mode-map (kbd "C-S-l") 'org-shiftright)
              (evil-define-key 'normal org-mode-map (kbd "C-S-h") 'org-shiftleft)

              (evil-define-key 'insert org-mode-map (kbd "C-S-l") 'org-shiftright)
              (evil-define-key 'insert org-mode-map (kbd "C-S-h") 'org-shiftleft)

              ;; Navigation
              (define-key org-mode-map (kbd "M-h") 'org-up-element)
              (define-key org-mode-map (kbd "M-j") 'org-forward-heading-same-level)
              (define-key org-mode-map (kbd "M-k") 'org-backward-heading-same-level)
              (define-key org-mode-map (kbd "M-l") 'air-org-goto-first-child)

              ;; Use fill column, but not in agenda
              (setq fill-column 100)
              (when (not (eq major-mode 'org-agenda-mode))
                (visual-line-mode)
                (visual-fill-column-mode))
              (flyspell-mode)
              (org-indent-mode))))

(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  (setq org-bullets-bullet-list '("•")))

(provide 'init-org)
;;; init-org.el ends here
