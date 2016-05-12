;;; air-menu-run.el --- Run a selected command from one menu.

;;; Commentary:

;; This is meant to be used simiarly to the much more feature-rich
;; "Hydra" package where you can press a single key and a menu will be
;; presented to you, presumably with related options.  Pressing the
;; key associated with an option runs the command linked to that key.

;;; Code:

(defface air-menu-run-heading-face
  '((t (:inherit 'font-lock-string-face)))
  "The menu heading shown in the selection menu for `air-menu-run'."
  :group 'air-menu-run)

(defvar air-menu-run-items
  '(())
  "An alist of menus.
The keys in the alist are simple strings used to reference the menu in
calls to `air-menu-run' and the values are lists with three elements:
A raw character to use as the selection key, such as `?a'; a string to
use in the menu display, and a function to call when that item is
selected.")

(defun air-menu-run (menu)
  "Display the items in MENU and run the selected item."
  (interactive)
  (if (assoc menu air-menu-run-items)
      (let* ((items (append (cadr (assoc menu air-menu-run-items))
                            '((?q "Quit" nil))))
             (prompt (mapconcat (lambda (i)
                                  (concat
                                   (propertize (concat
                                                "[" (char-to-string (nth 0 i)) "] ")
                                               'face 'air-menu-run-heading-face)
                                   (nth 1 i)))
                                items ", "))
             (choices (mapcar (lambda (i) (nth 0 i)) items))
             (choice (read-char-choice prompt choices)))
        (if (and (assoc choice items)
                 (functionp (nth 2 (assoc choice items))))
            (funcall (nth 2 (assoc choice items)))
          (message "Menu aborted.")))))

(provide 'air-menu-run)
;;; air-menu-run ends here
