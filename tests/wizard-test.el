;; Copyright (C) 2013-2025  Thierry Banel
;; 
;; org-aggregate is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;; 
;; org-aggregate is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; A Lisp function to record a wizard unit test.
;; - put the cursor somewhere in an Org Mode file,
;; - call M-x orgtbl-aggregate-bench-create
;; - call the wizard with
;;   C-c C-x x aggregate
;; - use the wizard as usual, your key-strokes are recorded while you type,
;; - when done, hit the & key,
;;   this insert a keyboard macro in the Org Mode file,
;; - the keyboard macro can be executed anytime with C-x e
;;   to replicate your actions.

(defun orgtbl-aggregate-bench-create ()
  "Interactively create a bench.
When done, type &."
  (interactive)
  (local-set-key "&" 'orgtbl-aggregate-bench-collect)
  (message "use the wizard, then type & when done")
  (kmacro-start-macro nil))

(defun orgtbl-aggregate-bench-collect ()
  "Called when typing & to close the interactive wizard session.
Do not call it directly."
  (interactive)
  (kmacro-end-macro 1)
  (insert "(execute-kbd-macro (kbd \"\n")
  (insert (key-description (kmacro--keys (kmacro last-kbd-macro))))
  (insert "\"))\n"))
