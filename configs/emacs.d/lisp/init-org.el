;;; init-org.el --- Set up Org Mode
;;; Commentary:

;; Basic Org Mode configuration, assuming presence of Evil & Evil Leader.

;; Defaults
(defcustom air-org-root-location "~/Dropbox/org"
  "The root location where Org files are stored."
  :type 'directory
  :group 'air-org)

;; Helper functions
(defun air-org-insert-first-link ()
  "Insert the first link in `org-stored-links', or do nothing."
  (interactive)
  (let* ((link (car org-stored-links))
         (uri (car link))
         (title (cadr link)))
    (org-insert-link nil uri title)))

(defun air-org-insert-link-dwim ()
  "Attempt to insert a link as the user wanted, without asking.

If the first entry of the kill ring starts with a protocol string,
prompt for link text and insert a link pointing to that
URL.  Otherwise, attempt to insert the first entry in
`org-stored-links'.  If neither is possible, do nothing."
  (interactive)
  (if (string-match "^https?://" (current-kill 0 t))
      (if-let ((url (current-kill 0 t))
               (text (read-string (concat "Text for '"
                                          (substring url 0 (min (length url) 50))
                                          "': "))))
          (org-insert-link nil (current-kill 0 t) text))
    (call-interactively 'air-org-insert-first-link)))

(defun air--alist-key-match-p (list substring)
  "Return t if any key in LIST should contain SUBSTRING."
  (let ((found))
    (dolist (item list)
      (if (string-match-p substring (car item))
          (setq found t)))
    found))

(defun air-org-export-top-subtree ()
  "Export the nearest parent subtree with export options.

By \"with export options\" we mean any entry with a property that
contains \"EXPORT\".

This exports as a Beamer PDF and opens it, because that's what I always do."
  (interactive)
  (let ((failed))
    (save-excursion
      (org-with-limited-levels
       (outline-previous-visible-heading 1)
       (while (and (not failed)
                   (not (air--alist-key-match-p
                         (org-entry-properties) "EXPORT")))
         (or (org-up-heading-safe)
             (setq failed t)))
       (if failed
           (message "No exportable heading found.")
         (org-open-file (org-beamer-export-to-pdf nil t)))))))

(defun air--beamer-bold (contents backend info)
  "Beamer export filter to use the correct LaTeX markup for bold.

CONTENTS BACKEND INFO are required arguments for filter functions."
  (when (eq backend 'beamer)
    (replace-regexp-in-string "\\`\\\\[A-Za-z0-9]+" "\\\\textbf" contents)))

(defun air-org-bulk-copy-headlines (&optional strip-tags)
  "Copy the headline text of the marked headlines in an agenda view.

This function is designed to be called interactively from an agenda
view with marked items.

If STRIP-TAGS is not nil, remove tags and trailing spaces from
the headlines."
  (interactive "P")
  (unless org-agenda-bulk-marked-entries (user-error "No entries are marked"))
  (let ((entries "")
        entry)
    (dolist (entry-marker (reverse org-agenda-bulk-marked-entries))
      (with-current-buffer (marker-buffer entry-marker)
        (save-excursion
          (goto-char (marker-position entry-marker))
          (when (re-search-forward org-heading-regexp (line-end-position) t)
            (setq entry (match-string-no-properties 2))
            (if strip-tags
                (setq entry (replace-regexp-in-string
                             (rx (0+ " ")
                                 (0+ (any alpha ":"))
                                 line-end)
                             "" entry)))
            (setq entries (concat entries entry "\n"))))))
    (if (length entries)
        (kill-new entries)))
  (message "Acted on %s entries%s"
           (length org-agenda-bulk-marked-entries)
           (if org-agenda-persistent-marks
               " (kept marked)" ""))
  (unless org-agenda-persistent-marks
    (org-agenda-bulk-unmark-all)))

(defun air-org-agenda-next-header ()
  "Jump to the next header in an agenda series."
  (interactive)
  (air--org-agenda-goto-header))

(defun air-org-agenda-previous-header ()
  "Jump to the previous header in an agenda series."
  (interactive)
  (air--org-agenda-goto-header t))

(defun air--org-agenda-goto-header (&optional backwards)
  "Find the next agenda series header forwards or BACKWARDS."
  (let ((pos (save-excursion
               (goto-char (if backwards
                              (line-beginning-position)
                            (line-end-position)))
               (let* ((find-func (if backwards
                                     'previous-single-property-change
                                   'next-single-property-change))
                      (end-func (if backwards
                                    'max
                                  'min))
                      (all-pos-raw (list (funcall find-func (point) 'org-agenda-structural-header)
                                         (funcall find-func (point) 'org-agenda-date-header)))
                      (all-pos (cl-remove-if-not 'numberp all-pos-raw))
                      (prop-pos (if all-pos (apply end-func all-pos) nil)))
                 prop-pos))))
    (if pos (goto-char pos))
    (if backwards (goto-char (line-beginning-position)))))

(defun air-org-helm-headings ()
  "Call `helm-org-agenda-files-headings' with a longer list of files."
  (interactive)
  (let ((org-agenda-files (list (expand-file-name "gtd/tasks.org" org-directory)
                                (expand-file-name "gtd/team.org" org-directory))))
    (call-interactively 'helm-org-agenda-files-headings)))

(defun air--org-display-tag (tag &optional focus)
  "Display entries tagged with TAG in a fit window.

Do not make the new window current unless FOCUS is set."
  (let ((org-agenda-files (list (expand-file-name "gtd/tasks.org" org-directory)
                                (expand-file-name "gtd/team.org" org-directory))))
    (org-tags-view nil tag))
  (fit-window-to-buffer)
  (unless focus
    (other-window 1)))

(defun air-org-display-any-tag ()
  "Display entries tagged with a tag selected interactively."
  (interactive)
  (air--org-display-tag (air--org-select-tag)))

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

(defun air-org-display-engineers (&optional focus)
  "Display entries tagged with `engineer'.

Do not make the new window current unless FOCUS is set."
  (interactive "P")
  (air--org-display-tag "engineer" focus))

(defun air--org-get-entry-end (&optional subtree)
  "Get the position of the end of entry at point, or SUBTREE, if not nil."
  (if subtree (save-excursion (org-end-of-subtree t) (point))
    (org-entry-end-position)))

(defun air-org-skip-tag-prefix (prefix &optional unless subtree)
  "Skip entries with tags having string prefix PREFIX.

If UNLESS is not nil, skip entries wihout tags having prefix PREFIX.

Skip the current entry unless SUBTREE is not nil."
       (let* ((end (air--org-get-entry-end subtree))
              (tags (org-get-tags))
              (has-prefix (seq-filter (lambda (tag) (string-prefix-p prefix tag)) tags)))
         (if (org-xor has-prefix unless) end nil)))

(defun air-org-skip-tag (tag &optional subtree)
  "Skip entries having TAG.

Skip only the current entry unless SUBTREE is not nil."
  (let* ((end (air--org-get-entry-end subtree))
         (tags (org-get-tags))
         (has-tag (member tag tags)))
    (if has-tag end nil)))

(defun air-org-skip-if-active (&optional subtree)
  "Skip entries with an :active: tag.

If SUBTREE is not nil, check the entire subtree for `:active:' tags
and skip the subtree if any are found."
  (let ((end (air--org-get-entry-end subtree)))
    (and (re-search-forward (rx bol (+ "*") (*? anychar) ":active:") end t)
         end)))

(defun air--entry-is-active ()
  "Return non-nil if the current entry is not DONE and is marked :active:."
  (and (not (org-entry-is-done-p))
       (member "active" (org-get-tags))))

(defun air-org-skip-if-scheduled (&optional subtree ignore-date)
  "Skip entries that are scheduled.

Skip the current entry unless SUBTREE is not nil, in which case skip
the entire subtree.

If IGNORE-DATE is nil, only skip entries that are scheduled to occur
on a future date (not inclusive of today).  If not nil, skip any entry
with a scheduled date regardless of its value."
  (let ((end (air--org-get-entry-end subtree)))
    (save-excursion
      (catch :skip
        (while (re-search-forward org-scheduled-time-regexp end t)
          (if (or ignore-date
                  (> (org-time-stamp-to-now (match-string 1)) 0))
              (throw :skip end)))))))

(defun air--entry-is-scheduled ()
  "Return non-nil if the current entry has a `SCHEDULED' property."
  (org-agenda-skip-if t '(scheduled)))

(defun air-org-any-entry-below (predicate)
  "Return non-nil if PREDICATE is non-nil for any entry below the current entry."
  (org-back-to-heading t)
  (let ((level (funcall outline-level)))
    (save-excursion
      (catch 'non-nil
        (while (and (progn
                      (outline-next-heading)
                      (> (funcall outline-level) level))
                    (not (eobp)))
          (if (funcall predicate)
              (throw 'non-nil t)))))))

(defun air-org-skip-if-not-closed-today (&optional subtree)
  "Skip entries that were not closed today.

Skip the current entry unless SUBTREE is not nil, in which case skip
the entire subtree."
       (let ((end (air--org-get-entry-end subtree))
             (today-prefix (format-time-string "%Y-%m-%d")))
         (if (save-excursion
               (and (re-search-forward org-closed-time-regexp end t)
                    (string= (substring (match-string-no-properties 1) 0 10) today-prefix)))
             nil
           end)))

(defun air-org-skip-if-not-closed-this-week (&optional subtree)
  "Skip entries that were not closed this week.

Skip the current entry unless SUBTREE is not nil, in which case skip
the entire subtree."
  (let ((end (air--org-get-entry-end subtree)))
    (if (not (save-excursion (re-search-forward org-closed-time-regexp end t)))
        end
      (let* ((now (current-time))
             (closed-time (date-to-time (match-string-no-properties 1)))
             (closed-day (time-to-day-in-year closed-time))
             (closed-year (format-time-string "%Y" closed-time))
             (today-day (time-to-day-in-year now))
             (today-year (format-time-string "%Y" now))
             (today-dow (format-time-string "%w" now))
             (start-day (- today-day
                           (string-to-number today-dow)))
             (end-day (+ today-day
                         (- 6 (string-to-number today-dow)))))
        (if (and (string= closed-year today-year)
                 (>= closed-day start-day)
                 (<= closed-day end-day))
            nil
          end)))))

(defun air-org-skip-if-habit (&optional subtree)
  "Skip an agenda entry (or SUBTREE, if not nil) if it is a habit."
  (let ((end (air--org-get-entry-end subtree)))
    (if (org-is-habit-p)
        end
      nil)))

(defun air-org-skip-if-not-habit (&optional subtree)
  "Skip an agenda entry (or SUBTREE, if not nil) if it is not a habit."
  (let ((end (air--org-get-entry-end subtree)))
    (if (not (org-is-habit-p))
        end
      nil)))

(defun air-org-skip-if-categorized (categories &optional subtree)
  "Skip an agenda entry if it has any category in CATEGORIES.

Skip the current entry unless SUBTREE is not nil."
  (let ((end (air--org-get-entry-end subtree)))
    (if (member (org-get-category) categories)
        end
      nil)))

(defun air-org-skip-if-priority (priority &optional subtree)
  "Skip an agenda item if it has a priority of PRIORITY.

PRIORITY may be one of the characters ?A, ?B, or ?C.

Skips the current entry unless SUBTREE is not nil."
  (let ((end (air--org-get-entry-end subtree))
        (pri-value (* 1000 (- org-lowest-priority priority)))
        (pri-current (org-get-priority (thing-at-point 'line t))))
    (if (= pri-value pri-current)
        end
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
                             ,(list file (line-number-at-pos)))))))))
    air-all-org-custom-ids))

(defun air--org-find-custom-id (custom-id)
  "Return the location of CUSTOM-ID."
  (let* ((all-custom-ids (air--org-global-custom-ids)))
    (let* ((id-parts (cadr (assoc custom-id all-custom-ids)))
           (file (car id-parts))
           (line (cadr id-parts)))
      (with-current-buffer (org-get-agenda-file-buffer file)
        (save-excursion
          (save-restriction
            (widen)
            (goto-char (point-min))
            (forward-line line)
            (org-reveal)
            (org-up-element)
            (list (current-buffer) (point))))))))

(defun air-org-goto-custom-id (&optional split)
  "Go to the location of a custom ID read interactively, maybe in a SPLIT."
  (interactive "P")
  (let* ((all-custom-ids (air--org-global-custom-ids))
         (custom-id (completing-read
                     "Custom ID: "
                     all-custom-ids))
         (id-location (air--org-find-custom-id custom-id)))
    (when id-location
      (let* ((buf (car id-location))
             (loc (cadr id-location)))
        (pop-to-buffer buf (if split t nil))
        (goto-char loc)
        (org-reveal)))))

(defun air-org-insert-custom-id-link ()
  "Insert an Org link to a custom ID selected interactively."
  (interactive)
  (let* ((all-custom-ids (air--org-global-custom-ids))
         (custom-id (completing-read
                     "Custom ID: "
                     all-custom-ids)))
    (when custom-id
      (let* ((id-parts (cadr (assoc custom-id all-custom-ids)))
             (file (car id-parts))
             (line (cadr id-parts)))
        (org-insert-link nil (concat file "::#" custom-id) custom-id)))))

(defun air-org-split-to-topics ()
  "Split the window to a custom ID and related topics entry."
  (interactive)
  (let* ((person-id (completing-read
                     "Custom ID: "
                     (air--org-global-custom-ids)))
         (topics-loc (air--org-find-custom-id "major-topics"))
         (person-loc (air--org-find-custom-id person-id))
         (this-scroll-margin
          (min (max 0 scroll-margin)
               (truncate (/ (window-body-height) 4.0))))
         (original-window (selected-window)))
    (when (and topics-loc person-loc)
      (delete-other-windows)

      (let ((org-agenda-window-setup 'current-window))
        (org-tags-view t person-id))

      (display-buffer (car person-loc)
                      '(display-buffer-at-bottom))

      (with-current-buffer (car person-loc)
        (widen)
        (goto-char (cadr person-loc))
        (search-forward "One-on-one" nil t)
        (if (not (org-at-heading-p))
            (org-back-to-heading))
        (outline-hide-subtree)
        (outline-show-children 2)
        (org-narrow-to-subtree))

      ;; This is the tags buffer because `display-buffer-at-bottom'
      ;; does not select the window
      (fit-window-to-buffer nil nil nil nil nil t)

      (display-buffer (car topics-loc)
                      '(display-buffer-at-bottom))

      (windmove-down)
      (windmove-down)

      (with-current-buffer (car topics-loc)
        (widen)
        (goto-char (cadr topics-loc))
        (search-forward "Current" nil t)
        (if (not (org-at-heading-p))
            (org-back-to-heading))
        (org-show-subtree)
        (org-narrow-to-subtree))

      (fit-window-to-buffer)
      (windmove-up))))


(defun air-org-nmom-capture-template ()
  "Return a Nine Minutes on Monday weekly agenda template suitable for capture."
  (let* ((deadline-timestamp (format-time-string "<%Y-%m-%d %a>"
                                                 (air-calendar-next-day-of-week 5)))
         (deadline (format "DEADLINE: %s\n\n" deadline-timestamp)))
    (concat (format "* Week %02d\n" (org-days-to-iso-week (org-today)))
            "\n"
            "See notes at [[file:~/Dropbox%20(personal)/org/notes.org::*Nine%20Minutes%20on%20Monday][Nine Minutes on Monday]].\n"
            "\n"
            "** TODO Care: \n" deadline
            "** TODO Mastery: \n" deadline
            "** TODO Recognition: \n" deadline
            "** TODO Purpose: \n" deadline)))

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

(defun air-org-insert-thing-dwim (&optional subthing)
  "Insert a heading or list item as appropriate; indent if SUBTHING.

With two prefix arguments, always insert a heading."
       (interactive "P")
       (if (and (org-in-item-p)
                (not (equal subthing '(16))))
           (progn (org-insert-item)
                  (if subthing (org-shiftmetaright)))
         (air-org-insert-heading (equal subthing '(4)))))

(defun air-org-toggle-checkbox-dwim (&optional toggle-presence)
  "Toggle checkbox presence and state, when TOGGLE-PRESENCE, do that."
  (interactive "P")
  (let ((toggle-presence-prefix (if (not (org-at-item-checkbox-p))
                                    '(4)
                                  toggle-presence)))
    (setq current-prefix-arg toggle-presence-prefix)
    (call-interactively 'org-toggle-checkbox)))

(defun air-org-insert-scheduled-heading (&optional subheading)
  "Insert a new org heading scheduled for today.

Insert the new heading at the end of the current subtree if
FORCE-HEADING is non-nil."
  (interactive "P")
  (if subheading
      (org-insert-subheading t)
    (org-insert-todo-heading t t))
  (org-schedule nil (format-time-string "%Y-%m-%d")))

(defun air-org-tickler-capture (&optional vanilla)
  "Capture a new scheduled (tickler) item.

If VANILLA is non-nil, run the standard `org-capture'."
  (interactive "P")
  (if vanilla
      (org-capture)
    (org-capture nil "r")))

(defun air-org-agenda-capture (&optional vanilla)
  "Capture a task in agenda mode, using the date at point.

If VANILLA is non-nil, run the standard `org-capture'."
  (interactive "P")
  (if vanilla
      (org-capture)
    (let ((org-overriding-default-time (org-get-cursor-date)))
      (org-capture nil "t"))))

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

(defun air-org-pop-to-inbox ()
  "Open the GTD inbox Org file."
  (interactive)
  (find-file (expand-file-name "gtd/inbox.org" org-directory))
  (org-cycle '(16))
  (goto-char 1)
  (org-evil-motion-forward-heading))

(defun air-pop-to-org-vault (&optional split)
  "Visit my encrypted vault file, in the current window or a SPLIT."
  (interactive "P")
  (air--pop-to-file (expand-file-name "vault.gpg" org-directory) split))

(defun air-pop-to-org-notes (&optional split)
  "Visit my notes file, in the current window or SPLIT."
  (interactive "P")
  (air--pop-to-file (expand-file-name "notes.org" org-directory) split))

(defun air-pop-to-org-todo (&optional split)
  "Visit my TODO file, in the current window or SPLIT."
  (interactive "P")
  (air--pop-to-file (expand-file-name "gtd/inbox.org" org-directory) split))

(defun air-pop-to-org-agenda-default (&optional nosplit)
  "Pop to the default agenda in the current window unless NOSPLIT."
  (interactive "P")
  (air--pop-to-org-agenda-view "d" nosplit))

(defun air-pop-to-org-agenda-review (&optional nosplit)
  "Pop to the default agenda in a split window unless NOSPLIT."
  (interactive "P")
  (air--pop-to-org-agenda-view "r" nosplit))

(defun air--pop-to-org-agenda-view (key &optional split)
  "Visit the org agenda KEY, in the current window or a SPLIT."
  ;; I don't know why this works, but it works.
  (let ((current-prefix-arg nil))
    (org-agenda nil key))
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

(defun air--org-select-tag ()
  "Interactively select or enter a single tag."
  (let ((org-last-tags-completion-table
         (if (derived-mode-p 'org-mode)
             (org-uniquify
              (delq nil (append (org-get-buffer-tags)
                                (org-global-tags-completion-table))))
           (org-global-tags-completion-table))))
    (completing-read
     "Tag: " 'org-tags-completion-function nil nil nil
     'org-tags-history)))

(defun air-org-agenda-filter-tag-interactive ()
  "Filter the agenda view by a tag selected interactively."
  (interactive)
  (if (derived-mode-p 'org-agenda-mode)
      (let* ((tag (air--org-select-tag))
             (org-tag-alist-for-agenda (list (cons tag ?z))))
        (if tag
            (org-agenda-filter-by-tag nil ?z)))
    (error "Tag filtering only works in an agenda view")))

(defun air-org-set-tags (tag)
  "Add TAG if it is not in the list of tags, remove it otherwise.

TAG is chosen interactively from the global tags completion table."
  (interactive (list (air--org-select-tag)))
  (let* ((cur-list (delq "" (org-get-tags)))
         (new-tags (mapconcat 'identity
                              (if (member tag cur-list)
                                  (delete tag cur-list)
                                (append cur-list (list tag)))
                              ":"))
         (new (if (> (length new-tags) 1) (concat " :" new-tags ":")
                nil)))
    (org-set-tags new)))

(defun air-org-toggle-active-tag ()
  "Toggle the presence of the `active' tag on the current item."
  (interactive)
  (let* ((hdmarker (or (org-get-at-bol 'org-hd-marker)
                       (org-agenda-error)))
         (buffer (marker-buffer hdmarker))
         (pos (marker-position hdmarker))
         (inhibit-read-only t))
    (org-agenda-set-tags "active")
    (with-current-buffer buffer
      (widen)
      (goto-char pos)
      (org-show-context 'agenda))
    (beginning-of-line 1)
    (org-agenda-redo-all)))

;;; Code:
(use-package org
  :ensure t
  :defer t
  :commands (org-capture)
  :bind (("C-c c" .   org-capture)
         ("C-c l" .   org-store-link)
         ("C-c t" .   org-todo)
         ("C-c f k" . org-search-view)
         ("C-c f t" . org-tags-view)
         ("C-c f i" . air-org-goto-custom-id))
  :config
  (require 'org-protocol)
  (setq org-hide-emphasis-markers t)
  (setq org-modules
        '(org-bbdb org-bibtex org-docview org-habit org-info org-w3m))
  (setq org-todo-keywords
        '((sequence "TODO(t)" "IN-PROGRESS(p)" "WAITING(w!)" "|" "DONE(d!)" "CANCELED(c!)")))
  (setq org-blank-before-new-entry '((heading . t)
                                     (plain-list-item . t)))
  (setq org-stuck-projects '("+LEVEL=1/-DONE-TODO-IN\-PROGRESS-WAITING-CANCELED" () ("active") ""))

  (defun air--org-bullet-daily-log-filename ()
    "Return the filename of today's Bullet Journal Daily Log."
    (format-time-string "%Y-%m-%d.org"))

  (defvar summary-annotations
    '("_" "@")
    "Characters used to annotate progressive summarization of content.")

  (defun air--within-annotated-region-p ()
    "Returns non-nil if point is within an annotated region.

Annotated regions are surrounded by one of the characters in `summary-annotations'."
    (interactive)
    (save-excursion
      (forward-char 1)
      (catch 'done
        (mapcar (lambda (c)
                  (let ((start (save-excursion (search-backward-regexp (concat c "\\w") (line-beginning-position) t)))
                        (end (save-excursion (search-forward-regexp (concat "\\w" c) (line-end-position) t))))
                    (if (and start end)
                        (throw 'done (list c start end)))))
                summary-annotations)
        nil)))

  (defun air-promote-summary ()
    "Promote the summarization annotation at point.

If a region is active, apply the lowest level summary
annotation. Else, promote the surrounding summary annotation to the
next level or fail."
    (interactive)
    (if (use-region-p)
        (air--toggle-surrounding-chars (car summary-annotations))
      (let ((annotation (air--within-annotated-region-p)))
        (if (not annotation)
            (user-error "Point is not within an annotated region")
          (let* ((current (car annotation))
                 (next (cadr (member current summary-annotations))))
            (if (not next)
                (user-error "Region is annotated at the highest level"))
            (goto-char (cadr annotation))
            (delete-char 1)
            (insert next)
            (goto-char (1- (caddr annotation)))
            (delete-char 1)
            (insert next))))))

  (defun air-demote-summary ()
    "Demote the summarization annotation at point.

Demote the surrounding summary annotation to the next lowest level or
fail."
    (interactive)
    (if (use-region-p)
        (user-error "Demoting a summary cannot operate on a region")
      (let ((annotation (air--within-annotated-region-p)))
        (if (not annotation)
            (user-error "Point is not within an annotated region")
          (let* ((current (car annotation))
                 (next (cadr (member current (reverse summary-annotations)))))
            (goto-char (cadr annotation))
            (delete-char 1)
            (if next (insert next))
            (goto-char (- (caddr annotation) (if next 1 2)))
            (delete-char 1)
            (if next (insert next)))))))

  (defun air--toggle-surrounding-chars (chars &optional after-chars)
    "Insert CHARS before and after the current region."
    (if (not (use-region-p))
        (user-error "Mark a region before inserting surrounding chars"))
    (if (and after-chars
             (not (= (length chars) (length after-chars))))
        (user-error "CHARS and AFTER-CHARS must be equivalent in length"))
    (let* ((cur-point (point))
           (beg (region-beginning))
           (end (region-end))
           (after-chars (or after-chars chars))
           (start-char (buffer-substring-no-properties beg (+ beg (length chars))))
           (end-char (buffer-substring-no-properties end (+ end (length after-chars)))))
      (if (and (string= start-char chars)
               (string= end-char after-chars))
          (progn
            (goto-char end)
            (delete-char (length after-chars))
            (goto-char (1+ (- beg (length chars))))
            (delete-char (length chars))
            (goto-char (- cur-point (length chars))))
        (progn
          (goto-char end)
          (insert after-chars)
          (goto-char beg)
          (insert chars)
          (goto-char (+ (length chars) cur-point))))))

  (setq org-capture-templates
        `(
          ("m" "Nine Minutes on Monday" entry
           (file "gtd/inbox.org")
           (function air-org-nmom-capture-template)
           :empty-lines 1)

          ("c" "An incoming task." entry
           (file "gtd/tasks.org")
           ,(concat "* TODO %? :active:\n"
                    ":PROPERTIES:\n"
                    ":CREATED:  %u\n"
                    ":END:\n")
           :empty-lines 1)

          ("b" "An incoming backlog item." entry
           (file "gtd/tasks.org")
           ,(concat "* TODO %?\n"
                    ":PROPERTIES:\n"
                    ":CREATED:  %u\n"
                    ":END:\n")
           :empty-lines 1)

          ("n" "A note." entry
           (file "notes.org")
           ,(concat "* %?\n"
                    ":PROPERTIES:\n"
                    ":CREATED:  %u\n"
                    ":END:\n\n"))

          ("r" "A Reminder (tickler)." entry
           (file "gtd/tickler.org")
           "* %?\nSCHEDULED: %^t"
           :empty-lines 1)

          ("l" "Capture links to read later.")

          ("ln" "A link to read later (no selection)." entry
           (file "gtd/reading.org")
           ,(concat "* TODO %:annotation\n"
                    ":PROPERTIES:\n"
                    ":CREATED:  %u\n"
                    ":END:\n\n"
                    "%:link\n\n"
                    "%?")
           :empty-lines 1)

          ("ls" "A link to read later (selection)." entry
           (file "gtd/reading.org")
           ,(concat "* TODO %:annotation\n"
                    ":PROPERTIES:\n"
                    ":CREATED:  %u\n"
                    ":END:\n\n"
                    "%:link\n\n"
                    "%i\n\n"
                    "%?")
           :empty-lines 1)))

  (setq org-directory
        (if (file-directory-p (expand-file-name "~/Dropbox (personal)"))
            "~/Dropbox (personal)/org"
          "~/Dropbox/org"))
  (setq org-default-notes-file (expand-file-name "gtd/inbox.org" org-directory))

  ;; Logging of state changes
  ;; (setq org-log-done (quote time))
  ;; (setq org-log-redeadline (quote time))
  ;; (setq org-log-reschedule (quote time))
  (setq org-log-into-drawer t)

  (setq org-pretty-entities t)
  (setq org-insert-heading-respect-content t)
  (setq org-ellipsis " …")
  (setq org-export-initial-scope 'subtree)
  (setq org-export-with-section-numbers nil)
  (setq org-export-with-toc nil)
  (setq org-use-tag-inheritance nil) ;; Use the list form, which happens to be blank
  (setq org-todo-keyword-faces
        '(("OPEN" . org-done)
          ("PAUSED" . org-upcoming-deadline)))

  ;; Agenda configuration
  (setq org-agenda-text-search-extra-files '(agenda-archives))
  (setq org-agenda-files (list (expand-file-name "gtd/tasks.org" org-directory)
                               (expand-file-name "notes.org" org-directory)
                               (expand-file-name "gtd/team.org" org-directory)
                               (expand-file-name "gtd/reading.org" org-directory)
                               (expand-file-name "gk-roam" org-directory)))
  (setq org-refile-targets `((,(expand-file-name "gtd/tasks.org" org-directory) :maxlevel . 2)
                             (,(expand-file-name "notes.org" org-directory) :maxlevel . 2)
                             (,(expand-file-name "gtd/projects.org" org-directory) :maxlevel . 2)
                             (,(expand-file-name "gtd/backlog.org" org-directory) :maxlevel . 1)
                             (,(expand-file-name "gtd/habits.org" org-directory) :maxlevel . 1)))
  ;; Headings between agenda blocks
  (set-face-attribute 'org-agenda-structure nil
                      :foreground "LightGray"
                      :weight 'bold
                      :underline nil
                      :box nil)
  (setq org-agenda-tags-column -89)
  (setq org-refile-use-outline-path 'file)
  (setq org-refile-allow-creating-parent-nodes 'confirm)
  (setq org-outline-path-complete-in-steps nil)
  (setq org-agenda-time-grid
        (quote
         ((daily today require-timed remove-match)
          (800 1000 1200 1400 1600)
          " │" "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")))
  (setq org-enforce-todo-dependencies t)
  (setq org-agenda-dim-blocked-tasks t)

  ;; Tagging
  (setq org-tag-alist '(("active" . ?a)
                        ("reading" . ?r)))
  (setq org-fast-tag-selection-single-key t)

  (setq org-agenda-skip-scheduled-if-done t)
  (setq org-agenda-hide-tags-regexp "project\\|work\\|home\\|@.*")
  (setq org-habit-today-glyph ?o)
  (setq org-habit-completed-glyph ?*)
  (setq org-habit-show-all-today t)

  (defun air--org-todo-state-change-handler ()
    "Take an action when the TODO state changes.

When the state changes to DONE, remove the `active' tag.

When the state changes to WAITING, add a property WAITING_FROM with
the current timestamp."
    (cond ((and (string= org-state "DONE")
                (not (org-is-habit-p)))
           (org-set-tags-to (delete "active" (delete "" (org-get-tags)))))

          ((string= org-state "WAITING")
           (org-entry-put (point) "WAITING_FROM" (current-time-string)))))

  (defun air--seconds-to-diff (seconds)
    "Convert SECONDS into a human-readable time span."
    (let* ((seconds (floor seconds))
           (days (/ seconds 86400))
           (day-seconds (% seconds 86400))
           (hours (/ day-seconds 3600))
           (hour-seconds (% day-seconds 3600))
           (minutes (/ hour-seconds 60)))
      (concat
       (if (> days 0) (format "%sd" days)
         (format "%sh" hours)))))

  (defun air--full-project-prefix ()
    (let ((outline-list (org-get-outline-path)))
      (concat
       "  "
       (if (> (length outline-list) 1)
           (concat (cadr outline-list) " → ")))))

  (defun air--fixed-project-prefix (&optional width)
    (let* ((outline-list (org-get-outline-path))
           (max-len (or width 20))
           (project (cond ((= (length outline-list) 1)
                           (car outline-list))
                          ((> (length outline-list) 1)
                           (concat (car outline-list)
                                   "→"
                                   (car (last outline-list))))
                          ((org-get-category)
                           (org-get-category))
                          (t "")))
           (waiting-from (org-entry-get (point) "WAITING_FROM"))
           (time-delta (if (and waiting-from
                                (string= (org-get-todo-state) "WAITING"))
                           (format " (%s)" (air--seconds-to-diff
                                            (- (time-to-seconds (current-time))
                                               (time-to-seconds (date-to-time waiting-from)))))
                         ""))
           (total-len (+ (length project)
                         (length time-delta)))
           (substring-index (if (> total-len max-len)
                                (- (length project) (- total-len max-len))
                              (length project)))
           (project-trimmed (concat (string-trim (substring project 0 substring-index))
                                    time-delta)))
      (concat " "
              (make-string (- max-len (length project-trimmed)) 32)
              project-trimmed
              (if (> (length project-trimmed) 0) ": " "  "))))

  (defun air--format-for-meetings-prefix ()
    (let ((id (car (seq-filter (lambda (tag) (string-prefix-p "@" tag)) (org-get-tags)))))
      (if id
          (format "  %17s: " (substring id 0 (min (length id) 10)))
        "                  ")))

  (defun air--org-separating-heading (heading)
    "Print HEADING padded with characters to create a separator."
    (concat heading
            " "
            (make-string (- 80 (length heading)) ? )))

  (defun air--org-all-todo-keywords ()
    "Return a list of all bare TODO keywords."
    (let (kw)
      (dolist (seq org-todo-keywords)
        (setq kw (append kw
                         (org-remove-keyword-keys
                          (delete "|" (cdr seq))))))
      kw))

  (setq org-agenda-custom-commands
        '(("d" "Omnibus agenda"
           ((agenda ""
                    ((org-agenda-span 1)
                     (org-agenda-hide-tags-regexp "active")
                     (org-agenda-prefix-format "%(air--fixed-project-prefix)%?-12t% s")
                     (org-agenda-skip-function '(or (org-agenda-skip-entry-if 'todo '("WAITING"))
                                                    (air-org-skip-tag-prefix "@")
                                                    (air-org-skip-if-habit)))))
            (tags-todo "active-reading/!-WAITING-DONE-CANCELED"
                       ((org-agenda-overriding-header (air--org-separating-heading "Tasks"))
                        (org-agenda-hide-tags-regexp "active")
                        (org-agenda-skip-function '(or (org-agenda-skip-entry-if 'scheduled 'deadline)
                                                       (air-org-skip-tag-prefix "@")))
                        (org-agenda-prefix-format "%(air--fixed-project-prefix)")
                        (org-agenda-sorting-strategy '(todo-state-down priority-down category-up))))
            (todo "TODO"
                  ((org-agenda-overriding-header (air--org-separating-heading "For Meetings"))
                   (org-agenda-skip-function '(or (air-org-skip-tag-prefix "@" t)
                                                  (air-org-skip-if-scheduled t)))
                   (org-agenda-hide-tags-regexp ".")
                   (org-agenda-sorting-strategy '(tag-up))
                   (org-agenda-prefix-format "%(air--format-for-meetings-prefix)")))
            (todo "WAITING"
                  ((org-agenda-overriding-header (air--org-separating-heading "Waiting"))
                   (org-agenda-skip-function 'air-org-skip-if-habit)
                   (org-agenda-hide-tags-regexp "active")
                   (org-agenda-prefix-format "%(air--fixed-project-prefix)")))
            (tags "reading/-DONE" ((org-agenda-overriding-header (air--org-separating-heading "Reading list"))
                                   (org-agenda-hide-tags-regexp "reading\\|active")
                                   (org-agenda-prefix-format "%(air--fixed-project-prefix)")))
            (agenda ""
                    ((org-agenda-overriding-header (air--org-separating-heading "Habits"))
                     (org-agenda-files (list (expand-file-name "gtd/habits.org" org-directory)))
                     (org-agenda-sorting-strategy '(tag-up))
                     (org-agenda-prefix-format "  ")
                     (org-agenda-compact-blocks nil)
                     (org-agenda-time-grid nil)
                     (org-agenda-span 1)
                     (org-agenda-skip-function 'air-org-skip-if-not-habit)))
            (tags "+LEVEL=1"
                  ((org-agenda-overriding-header (air--org-separating-heading "Inactive Projects"))
                   (org-agenda-skip-function '(or
                                               ;; Non-todo top-level headings only
                                               (org-agenda-skip-entry-if 'todo (air--org-all-todo-keywords))
                                               (air-org-skip-if-scheduled t)
                                               (air-org-skip-if-active t)))
                   (org-agenda-files (list (expand-file-name "gtd/tasks.org" org-directory))))))
           ((org-use-property-inheritance t)
            (org-agenda-block-separator "")
            (org-agenda-compact-blocks nil)))

          ("r" "Inbox review"
           ((agenda "" ((org-agenda-span 14)
                        (org-agenda-time-grid nil)
                        (org-agenda-skip-function '(or (org-agenda-skip-entry-if 'todo '("WAITING"))
                                                       (air-org-skip-if-habit)))))
            (todo "TODO"
                  ((org-agenda-overriding-header (air--org-separating-heading "Mobile (REFILE ↓)"))
                   (org-agenda-files (list (expand-file-name "orgzly/inbox.org" org-directory)))))
            (tags-todo "-active-reading+TODO=\"TODO\"|TODO=\"IN-PROGRESS\""
                  ((org-agenda-overriding-header (air--org-separating-heading "Backlog"))
                   (org-agenda-skip-function '(or (air-org-skip-if-scheduled t)
                                                  (air-org-skip-tag-prefix "@")))
                   (org-agenda-prefix-format "%(air--fixed-project-prefix)")))
            (tags "LEVEL=1/DONE"
                  ((org-agenda-overriding-header (air--org-separating-heading "Completed")))))
           ((org-use-property-inheritance t)
            (org-agenda-tag-filter-preset '("-active" "-reading"))
            (org-agenda-block-separator "")
            (org-agenda-compact-blocks nil)))))

  (set-face-attribute 'org-upcoming-deadline nil :foreground "gold1")

  (defun air--org-element-motion (count)
    "Return the bounds of an element; traverse upward COUNT levels."
    (save-excursion
      ;; get to the top of the tree
      (org-with-limited-levels
       (cond ((org-at-heading-p) (beginning-of-line))
             ((org-before-first-heading-p) (user-error "Not in a subtree"))
             (t (outline-previous-visible-heading 1))))

      (decf count)
      (when count (while (and (> count 0) (org-up-heading-safe)) (decf count)))

      ;; extract the beginning and end of the tree
      (let* ((element (org-element-at-point))
             (begin (org-element-property :begin element))
             (end (org-element-property :end element)))
        (list end begin))))

  (defun air--org-inner-element-bounds (count &optional notrim)
    "Return the bounds of the current element's content.

Traverses up the element tree COUNT elements and returns the bounds of
that element's content minus its header and concluding line break.

If NOTRIM is non-nil, include leading blank lines in the content."
    (let* ((outer-points (air--org-element-motion count))
           (outer-begin (cadr outer-points))
           (outer-end (car outer-points))
           (begin (save-excursion
                    (goto-char outer-begin)
                    (next-line)
                    (while (and (not notrim)
                                (< (point) outer-end)
                                (string-match-p "^\\s-*$"
                                                (buffer-substring (line-beginning-position)
                                                                  (line-end-position))))
                      (forward-line 1))
                    (point)))
           (end (save-excursion
                  (goto-char outer-end)
                  (backward-char 1)
                  (while (and (> (point) outer-begin)
                              (string-match-p "^\\s-*$"
                                              (buffer-substring (line-beginning-position)
                                                                (line-end-position))))
                    (forward-line -1))
                  (goto-char (line-end-position))
                  (point))))
      (list end begin)))

  (evil-define-text-object evil-org-outer-element (count &optional beg end type)
    "One whole org element, from headline to final newline."
    :type line
    (air--org-element-motion count))

  (evil-define-text-object evil-org-inner-element (count &optional beg end type)
    "An Org subtree, minus its header and concluding line break.  Uses code from `org-mark-subtree`"
    :type line
    (air--org-inner-element-bounds count))

  (define-key evil-outer-text-objects-map "*" 'evil-org-outer-element)
  (define-key evil-inner-text-objects-map "*" 'evil-org-inner-element)

  (defun air-org-narrow-to-prose-dwim ()
    "Narrow and activate a writing mode.

If the current buffer is visiting an Org Mode file, narrow to the Org
element at point as well. If the view is already narrowed and the
writing mode is already active, undo all of that."
    (interactive)
    (if (and (boundp 'writeroom-mode)
             writeroom-mode)
        (progn (writeroom-mode 0)
               (widen)
               (if (member 'visual-fill-column-mode minor-mode-list)
                  (visual-fill-column-adjust)))
      (if (eq major-mode 'org-mode)
          (let ((bounds (air--org-inner-element-bounds 0 t)))
            (narrow-to-region (cadr bounds) (car bounds))))
      (writeroom-mode t)))

  (defun air-roam-grep (search-term)
    "Search for a string in `org-roam-directory'."
    (interactive (list (deadgrep--read-search-term)))
    (let ((deadgrep-project-root-function (list 'lambda '() org-roam-directory)))
      (deadgrep search-term)))

  (defun air-org-grep (search-term)
    "Search for a string in `org-directory'."
    (interactive (list (deadgrep--read-search-term)))
    (let ((deadgrep-project-root-function (list 'lambda '() org-directory)))
      (deadgrep search-term)))

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
              (define-key org-agenda-mode-map (kbd "@")   'org-agenda-refile)
              (define-key org-agenda-mode-map (kbd "A")   'air-org-toggle-active-tag)
              (define-key org-agenda-mode-map (kbd "a")   'org-agenda-archive)
              (define-key org-agenda-mode-map (kbd "/")   'counsel-grep-or-swiper)
              (define-key org-agenda-mode-map (kbd "H")   'beginning-of-buffer)
              (define-key org-agenda-mode-map (kbd "J")   'air-org-agenda-next-header)
              (define-key org-agenda-mode-map (kbd "K")   'air-org-agenda-previous-header)
              (define-key org-agenda-mode-map (kbd "M")   'org-agenda-bulk-unmark)
              (define-key org-agenda-mode-map (kbd "R")   'org-revert-all-org-buffers)
              (define-key org-agenda-mode-map (kbd "c")   (lambda () (interactive) (org-capture nil "c")))
              (define-key org-agenda-mode-map (kbd "j")   'org-agenda-next-item)
              (define-key org-agenda-mode-map (kbd "k")   'org-agenda-previous-item)
              (define-key org-agenda-mode-map (kbd "u")   'org-agenda-undo)
              (define-key org-agenda-mode-map (kbd "y")   'air-org-bulk-copy-headlines)
              (define-key org-agenda-mode-map (kbd "S")   'org-agenda-schedule)
              (define-key org-agenda-mode-map (kbd "RET") 'org-agenda-switch-to)

              (define-prefix-command 'air-org-run-shortcuts)
              (define-key air-org-run-shortcuts "a" (tiny-menu-run-item "org-agendas"))
              (define-key air-org-run-shortcuts "c" (tiny-menu-run-item "org-captures"))
              (define-key air-org-run-shortcuts "f" (tiny-menu-run-item "org-files"))
              (define-key air-org-run-shortcuts "l" (tiny-menu-run-item "org-links"))
              (define-key air-org-run-shortcuts "t" (tiny-menu-run-item "org-things"))
              (define-key org-agenda-mode-map (kbd "\\") air-org-run-shortcuts)))

  ;; This is the hook called after a TODO or state change note capture
  ;; window has been configured. We use it here simply to enter insert
  ;; mode.
  (add-hook 'org-log-buffer-setup-hook
            (lambda ()
              (evil-insert-state)))

  (add-hook 'org-after-todo-state-change-hook 'air--org-todo-state-change-handler)

  (add-hook 'org-capture-mode-hook
            (lambda ()
              (evil-define-key '(normal insert) org-capture-mode-map (kbd "C-d") 'air-org-agenda-toggle-date)
              (evil-define-key 'normal org-capture-mode-map "+" 'org-priority-up)
              (evil-define-key 'normal org-capture-mode-map "-" 'org-priority-down)
              (evil-define-key '(normal insert) org-capture-mode-map (kbd "C-=" ) 'org-priority-up)
              (evil-define-key '(normal insert) org-capture-mode-map (kbd "C--" ) 'org-priority-down)
              ;; We have to do it like this because `org-capture-mode' is a
              ;; minor mode, thus `evil-emacs-state-modes' has no effect.
              ;; Recommendation from https://emacs.stackexchange.com/questions/10732/start-in-insert-state-based-on-minor-mode
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
              (define-key org-mode-map (kbd "C-c d")      (lambda ()
                                                            (interactive) (air-org-agenda-toggle-date t)))
              (define-key org-mode-map (kbd "C-c ,")      'org-time-stamp-inactive)
              (define-key org-mode-map (kbd "C-|")        'air-org-insert-scheduled-heading)
              (define-key org-mode-map (kbd "<C-return>") 'air-org-insert-thing-dwim)
              (define-key org-mode-map (kbd "C-\\")       'air-org-insert-heading)

              (define-key org-mode-map (kbd "C-<")        'org-shiftmetaleft)
              (define-key org-mode-map (kbd "C->")        'org-shiftmetaright)

              (define-key org-mode-map (kbd "C-c SPC")    'air-org-export-top-subtree)

              (define-key org-mode-map (kbd "s-r")        (tiny-menu-run-item "reverts"))
              (define-key org-mode-map (kbd "C-c C-l")    (tiny-menu-run-item "org-links"))

              ;; These are set as evil keys because they conflict with
              ;; existing commands I don't use, or are superseded by
              ;; some evil function that org-mode-map is shadowed by.
              (defun complete-or-org-cycle ()
                "Call `company-complete' then `org-cycle'."
                (interactive)
                (or (and (looking-back "\\w" (line-beginning-position))
                         (or (org-try-structure-completion)
                             (company-complete)))
                    (org-cycle)))
              (evil-define-key 'normal org-mode-map (kbd "<tab>") 'org-cycle)
              (evil-define-key 'insert org-mode-map (kbd "<tab>") 'complete-or-org-cycle)

              (evil-define-key 'normal org-mode-map (kbd "C-,")   'org-metaleft)
              (evil-define-key 'normal org-mode-map (kbd "C-.")   'org-metaright)

              (evil-define-key 'insert org-mode-map (kbd "C-,")   'org-metaleft)
              (evil-define-key 'insert org-mode-map (kbd "C-.")   'org-metaright)

              (evil-define-key 'normal org-mode-map (kbd "C-S-l") 'org-shiftright)
              (evil-define-key 'normal org-mode-map (kbd "C-S-h") 'org-shiftleft)

              (evil-define-key 'insert org-mode-map (kbd "C-S-l") 'org-shiftright)
              (evil-define-key 'insert org-mode-map (kbd "C-S-h") 'org-shiftleft)

              (evil-define-key 'normal org-mode-map (kbd "[[")    'air-org-toggle-checkbox-dwim)

              (evil-define-key '(normal visual) org-mode-map (kbd "+") 'air-promote-summary)
              (evil-define-key 'normal org-mode-map          (kbd "=") 'air-demote-summary)

              ;; Navigation
              (define-key org-mode-map (kbd "M-h") 'org-up-element)
              (define-key org-mode-map (kbd "M-j") 'org-forward-heading-same-level)
              (define-key org-mode-map (kbd "M-k") 'org-backward-heading-same-level)
              (define-key org-mode-map (kbd "M-l") 'air-org-goto-first-child)

              ;; "gh" goes up a level, and is defined by org-evil-mode.
              ;; "gH" goes to the top level, and is defined by org-evil-mode.
              (evil-define-key 'normal org-mode-map (kbd "gl") 'air-org-goto-first-child)

              ;; Settings for all Org-derived modes
              (setq show-trailing-whitespace t)

              (require 'ox)
              (require 'ox-beamer)
              (require 'ox-md)
              (setf org-export-dispatch-use-expert-ui t)
              (add-to-list 'org-export-filter-bold-functions 'air--beamer-bold)

              ;; Settings for non-agenda modes only
              (when (not (eq major-mode 'org-agenda-mode))
                (ignore-errors
                  (if (buffer-file-name)
                      (and (require 'periodic-commit-minor-mode)
                           (periodic-commit-minor-mode t))))
                (setq fill-column 100)
                (org-evil-mode)
                (visual-line-mode)
                (visual-fill-column-mode)
                (flyspell-mode)
                (org-indent-mode)
                (highlight-regexp "@.*?@" 'hi-blue)))))

(use-package org-evil
  :ensure t
  :config
  (evil-define-minor-mode-key 'normal 'org-evil-heading-mode ":" 'org-set-tags-command)
  (evil-define-minor-mode-key 'normal 'org-evil-heading-mode "t" 'org-todo)
  (evil-define-minor-mode-key 'normal 'org-evil-heading-mode "@" 'org-refile)
  (evil-define-minor-mode-key 'normal 'org-evil-heading-mode "#" 'org-add-note)
  (evil-define-minor-mode-key 'normal 'org-evil-heading-mode "+" 'org-shiftup)
  (evil-define-minor-mode-key 'normal 'org-evil-heading-mode "=" 'org-shiftdown))

(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  (setq org-bullets-bullet-list '("⋄" "•" "◦")))

(use-package ox-clip
  :ensure t
  :defer t
  :config
  (evil-define-key 'visual org-mode-map (kbd "gy") 'ox-clip-formatted-copy))

(use-package gk-roam
  :config
  (setq gk-roam-root-dir "~/Dropbox/org/gk-roam/")
  (setq gk-roam-pub-dir "~/Dropbox/org/gk-roam/site/")
  (setq gk-roam-require-tags nil)
  (global-set-key (kbd "C-c r r") 'gk-roam-index)
  (global-set-key (kbd "C-c r f") 'gk-roam-find)
  (global-set-key (kbd "C-c r i") 'gk-roam-insert-link-dwim)
  (global-set-key (kbd "C-c r u") 'gk-roam-update))

(provide 'init-org)
;;; init-org.el ends here
