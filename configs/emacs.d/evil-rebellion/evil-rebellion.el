;;; evil-rebellion.el --- Key-bindings for evil rebels

;; Copyright © 2013-2014  Albert Krewinkel
;;
;; Author: Albert Krewinkel <albert+evil@zeitkraut.de>
;; Keywords: evil rebellion
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

(eval-when-compile (require 'cl))
(require 'evil)

;; code goes here

(eval-after-load 'org
  '(progn
     (require 'evil-org-rebellion)))

(eval-after-load 'magit
  '(progn
     (require 'evil-magit-rebellion)))

(eval-after-load 'bbdb
  '(progn
     (require 'evil-bbdb-rebellion)))

(require 'evil-visual-rebellion)

(provide 'evil-rebellion)
;;; evil-rebellion.el ends here
