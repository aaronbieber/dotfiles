;;; evil-bbdb-rebellion.el --- Key-bindings for evil bbdb3 rebels

;; Copyright © 2013–2014  Albert Krewinkel
;;
;; Author: Albert Krewinkel <albert+evil@zeitkraut.de>
;; Keywords: evil bbdb rebellion
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

(evil-define-key 'motion bbdb-mode-map
  "\C-k"       'bbdb-delete-field-or-record
  "\C-x\C-s"   'bbdb-save
  "\C-x\C-t"   'bbdb-transpose-fields
  "\d"         'bbdb-prev-field ; DEL
  "\M-d"       'bbdb-dial
  "\t"         'bbdb-next-field ; TAB
  "+"          'bbdb-append-display
  "*"          'bbdb-do-all-records
  ";"          'bbdb-edit-foo
  "?"          'bbdb-help
  "!"          'bbdb-search-invert
  "="          'delete-other-windows
  "a"          'bbdb-add-mail-alias
  "A"          'bbdb-mail-aliases
  "C"          'bbdb-copy-records-as-kill
  "c"          'bbdb-create
  "d"          'bbdb-delete-field-or-record
  "e"          'bbdb-edit-field
  "h"          'bbdb-info
  "i"          'bbdb-insert-field
  "J"          'bbdb-next-field
  "j"          'bbdb-next-record
  "K"          'bbdb-prev-field
  "k"          'bbdb-prev-record
  "m"          'bbdb-mail
  "M"          'bbdb-mail-address
  "N"          'bbdb-next-field
  "n"          'bbdb-next-record
  "o"          'bbdb-omit-record
  "P"          'bbdb-prev-field
  "p"          'bbdb-prev-record
  "s"          'bbdb-save
  "T"          'bbdb-display-records-completely
  "t"          'bbdb-toggle-records-layout
  "u"          'bbdb-browse-url

  ;; Search keys
  "b"          'bbdb
  "/1"         'bbdb-display-records
  "/n"         'bbdb-search-name
  "/o"         'bbdb-search-organization
  "/p"         'bbdb-search-phone
  "/a"         'bbdb-search-address
  "/m"         'bbdb-search-mail
  "/N"         'bbdb-search-xfields
  "/x"         'bbdb-search-xfields
  "/c"         'bbdb-search-changed
  "/d"         'bbdb-search-duplicates
  "\C-xnw"     'bbdb-display-all-records
  "\C-xnd"     'bbdb-display-current-record
  )

(evil-set-initial-state 'bbdb-mode 'motion)

(provide 'evil-bbdb-rebellion)
