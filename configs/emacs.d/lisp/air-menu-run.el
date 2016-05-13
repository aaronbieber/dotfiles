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
selected.

The data structure should look like:

'((\"menu-1\" (?a \"First item\" function-to-call-for-item-1)
            (?b \"Second item\" function-to-call-for-item-2))
  (\"menu-2\" (?z \"First item\" function-to-call-for-item-1)
            (?x \"Second item\" function-to-call-for-item-2)))")

(defun air-menu-run (&optional menu)
  "Display the items in MENU and run the selected item.

If MENU is not given, a dynamically generated menu of available menus
is displayed."
  (interactive)
  (if (< (length air-menu-run-items) 1)
      (message "Configure air-menu-run-items first.")
    (let* ((menu (if (assoc menu air-menu-run-items)
                     (cadr (assoc menu air-menu-run-items))
                   (air-menu-of-menus)))
           (title (car menu))
           (items (append (cadr menu)
                          '((?q "Quit" nil))))
           (prompt (concat (propertize (concat title ": ") 'face 'default)
                           (mapconcat (lambda (i)
                                        (concat
                                         (propertize (concat
                                                      "[" (char-to-string (nth 0 i)) "] ")
                                                     'face 'air-menu-run-heading-face)
                                         (nth 1 i)))
                                      items ", ")))
                   (choices (mapcar (lambda (i) (nth 0 i)) items))
                   (choice (read-char-choice prompt choices)))
           (if (and (assoc choice items)
                    (functionp (nth 2 (assoc choice items))))
               (funcall (nth 2 (assoc choice items)))
             (message "Menu aborted.")))))

(defun air-menu-of-menus ()
  "Build menu items for all configured menus.

This allows `air-menu-run' to display an interactive menu of all
configured menus if the caller does not specify a menu name
explicitly."
  (let ((menu-key-char 97))
    `("Menus" ,(mapcar (lambda (i)
                (prog1
                    `(,menu-key-char ,(caadr i) (lambda () (air-menu-run ,(car i))))
                  (setq menu-key-char (1+ menu-key-char))))
              air-menu-run-items))))

(provide 'air-menu-run)
;;; air-menu-run ends here
