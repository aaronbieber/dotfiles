;;; evil-org-rebellion.el --- Key-bindings for evil org-mode rebels

;; Copyright © 2013-2014  Albert Krewinkel
;;
;; Author: Albert Krewinkel <albert+evil@zeitkraut.de>
;; Keywords: evil org rebellion
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; This file is not part of GNU Emacs.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'evil-macros)
(require 'evil)

(evil-define-motion evil-org-end-of-line (count)
  "Move the cursor to the end of the current org line.
If COUNT is given, move COUNT - 1 lines downward first."
  :type inclusive
  (when (looking-at ".$")
    (forward-char))
  (org-end-of-line count)
  (when evil-track-eol
    (setq temporary-goal-column most-positive-fixnum
          this-command 'next-line))
  (unless (evil-visual-state-p)
    (evil-adjust-cursor)
    (when (eolp)
      ;; prevent "c$" and "d$" from deleting blank lines
      (setq evil-this-type 'exclusive))))

;;; Key Bindings
;;; ============
(evil-define-key 'normal org-mode-map
  "\t" 'org-cycle
  "gh" 'outline-up-heading
  "gj" 'org-forward-heading-same-level
  "gk" 'org-backward-heading-same-level
  "gl" 'outline-next-visible-heading
  "go" 'org-open-at-point
  "$"  'evil-org-end-of-line)

(evil-define-keymap evil-org-set-map
  :local t
  (setq evil-org-set-map (make-sparse-keymap))
  (evil-initialize-local-keymaps))


;; Org Agenda
;; ==========
(eval-after-load 'org-agenda
 '(progn
    (evil-set-initial-state 'org-agenda-mode 'normal)
    (evil-define-key 'normal org-agenda-mode-map
      (kbd "<DEL>") 'org-agenda-show-scroll-down
      (kbd "<RET>") 'org-agenda-switch-to
      (kbd "\t") 'org-agenda-goto
      "\C-n" 'org-agenda-next-line
      "\C-p" 'org-agenda-previous-line
      "\C-r" 'org-agenda-redo
      "a" 'org-agenda-archive-default-with-confirmation
      ;b
      "c" 'org-agenda-goto-calendar
      "d" 'org-agenda-day-view
      "e" 'org-agenda-set-effort
      ;f
      "g " 'org-agenda-show-and-scroll-up
      "gG" 'org-agenda-toggle-time-grid
      "gh" 'org-agenda-holidays
      "gj" 'org-agenda-goto-date
      "gJ" 'org-agenda-clock-goto
      "gk" 'org-agenda-action
      "gm" 'org-agenda-bulk-mark
      "go" 'org-agenda-open-link
      "gO" 'delete-other-windows
      "gr" 'org-agenda-redo
      "gv" 'org-agenda-view-mode-dispatch
      "gw" 'org-agenda-week-view
      "g/" 'org-agenda-filter-by-tag
      "h"  'org-agenda-earlier
      "i"  'org-agenda-diary-entry
      "j"  'org-agenda-next-line
      "k"  'org-agenda-previous-line
      "l"  'org-agenda-later
      "m" 'org-agenda-bulk-mark
      "n" nil                           ; evil-search-next
      "o" 'delete-other-windows
      ;p
      "q" 'org-agenda-quit
      "r" 'org-agenda-redo
      "s" 'org-save-all-org-buffers
      "t" 'org-agenda-todo
      "u" 'org-agenda-bulk-unmark
      ;v
      "x" 'org-agenda-exit
      "y" 'org-agenda-year-view
      "z" 'org-agenda-add-note
      "{" 'org-agenda-manipulate-query-add-re
      "}" 'org-agenda-manipulate-query-subtract-re
      "$" 'org-agenda-archive
      "%" 'org-agenda-bulk-mark-regexp
      "+" 'org-agenda-priority-up
      "," 'org-agenda-priority
      "-" 'org-agenda-priority-down
      "." 'org-agenda-goto-today
      "0" 'evil-digit-argument-or-evil-beginning-of-line
      ":" 'org-agenda-set-tags
      ";" 'org-timer-set-timer
      "<" 'org-agenda-filter-by-category
      ">" 'org-agenda-date-prompt
      "?" 'org-agenda-show-the-flagging-note
      "A" 'org-agenda-append-agenda
      "B" 'org-agenda-bulk-action
      "C" 'org-agenda-convert-date
      "D" 'org-agenda-toggle-diary
      "E" 'org-agenda-entry-text-mode
      "F" 'org-agenda-follow-mode
      ;G
      "H" 'org-agenda-holidays
      "I" 'org-agenda-clock-in
      "J" 'org-agenda-next-date-line
      "K" 'org-agenda-previous-date-line
      "L" 'org-agenda-recenter
      "M" 'org-agenda-phases-of-moon
      ;N
      "O" 'org-agenda-clock-out
      "P" 'org-agenda-show-priority
      ;Q
      "R" 'org-agenda-clockreport-mode
      "S" 'org-agenda-sunrise-sunset
      "T" 'org-agenda-show-tags
      ;U
      ;V
      ;W
      "X" 'org-agenda-clock-cancel
      ;Y
      ;Z
      "[" 'org-agenda-manipulate-query-add
      "g\\" 'org-agenda-filter-by-tag-refine
      "]" 'org-agenda-manipulate-query-subtract)))

(provide 'evil-org-rebellion)
