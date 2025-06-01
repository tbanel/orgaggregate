;;; orgtbl-aggregate.el --- Aggregate an Org Mode table | + | + | into another table  -*- coding:utf-8; lexical-binding: t;-*-
;; Copyright (C) 2013-2025  Thierry Banel

;; Authors:
;;   Thierry Banel tbanelwebmin at free dot fr
;;   Michael Brand michael dot ch dot brand at gmail dot com
;; Contributors:
;;   Eric Abrahamsen
;;   Alejandro Erickson alejandro dot erickson at gmail dot com
;;   Uwe Brauer
;;   Peking Duck
;;   Bill Hunker
;; Package-Requires: ((emacs "26.1"))

;; Version: 1.0
;; Keywords: data, extensions
;; URL: https://github.com/tbanel/orgaggregate/blob/master/README.org

;; orgtbl-aggregate is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; orgtbl-aggregate is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; A new org-mode table is automatically updated,
;; based on another table acting as a data source
;; and user-given specifications for how to perform aggregation.
;;
;; Example:
;; Starting from a source table of activities and quantities
;; (whatever they are) over several days,
;;
;; #+TBLNAME: original
;; | Day       | Color | Level | Quantity |
;; |-----------+-------+-------+----------|
;; | Monday    | Red   |    30 |       11 |
;; | Monday    | Blue  |    25 |        3 |
;; | Tuesday   | Red   |    51 |       12 |
;; | Tuesday   | Red   |    45 |       15 |
;; | Tuesday   | Blue  |    33 |       18 |
;; | Wednesday | Red   |    27 |       23 |
;; | Wednesday | Blue  |    12 |       16 |
;; | Wednesday | Blue  |    15 |       15 |
;; | Thursday  | Red   |    39 |       24 |
;; | Thursday  | Red   |    41 |       29 |
;; | Thursday  | Red   |    49 |       30 |
;; | Friday    | Blue  |     7 |        5 |
;; | Friday    | Blue  |     6 |        8 |
;; | Friday    | Blue  |    11 |        9 |
;;
;; an aggregation is built for each day (because several rows
;; exist for each day), typing C-c C-c
;;
;; #+BEGIN: aggregate :table original :cols "Day mean(Level) sum(Quantity)"
;; | Day       | mean(Level) | sum(Quantity) |
;; |-----------+-------------+---------------|
;; | Monday    |        27.5 |            14 |
;; | Tuesday   |          43 |            45 |
;; | Wednesday |          18 |            54 |
;; | Thursday  |          43 |            83 |
;; | Friday    |           8 |            22 |
;; #+END
;;
;; A wizard can be used:
;; C-c C-x x aggregate
;;
;; Full documentation here:
;;   https://github.com/tbanel/orgaggregate/blob/master/README.org

;;; Requires:
(require 'calc-ext)
(require 'calc-aent)
(require 'calc-alg)
(require 'org)
(require 'org-table)
(require 'thingatpt) ;; just for thing-at-point--read-from-whole-string
(eval-when-compile (require 'cl-lib))
(require 'rx)
(cl-proclaim '(optimize (speed 3) (safety 0)))

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; creating long lists in the right order may be done
;; - by (nconc)  but behavior is quadratic
;; - by (cons) (nreverse)
;; a third way involves keeping track of the last cons of the growing list
;; a cons at the head of the list is used for housekeeping
;; the actual list is (cdr ls)
;;
;; A list with 4 elements:
;; ╭─┬─╮ ╭────┬─╮ ╭────┬─╮ ╭────┬─╮ ╭────┬─╮
;; │◦│◦┼▶┤val1│◦┼▶┤val2│◦┼▶┤val3│◦┼▶┤val4│◦┼▶╴nil
;; ╰┼┴─╯ ╰────┴─╯ ╰────┴─╯ ╰────┴─╯ ╰─┬──┴─╯
;;  │                                 ▲
;;  ╰─────────────────────────────────╯
;;
;; A newly created, empty list
;; ╭─┬─╮
;; │◦│◦┼▶─nil
;; ╰┼┴┬╯
;;  │ ▲
;;  ╰─╯

(defsubst orgtbl-aggregate--list-create ()
  "Create an appendable list."
  (let ((x (cons nil nil)))
    (setcar x x)))

(defmacro orgtbl-aggregate--list-append (ls value)
  "Append VALUE at the end of LS in O(1) time."
  `(setcar ,ls (setcdr (car ,ls) (cons ,value nil))))

(defmacro orgtbl-aggregate--list-get (ls)
  "Return the regular Lisp list from LS."
  `(cdr ,ls))

(defmacro orgtbl-aggregate--pop-simple (place)
  "Like (pop PLACE), but without returning (car PLACE)."
  `(setq ,place (cdr ,place)))

(defmacro orgtbl-aggregate--pop-leading-hline (table)
  "Remove leading hlines from TABLE, if any."
  `(while (not (listp (car ,table)))
     (orgtbl-aggregate--pop-simple ,table)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The function (org-table-to-lisp) have been greatly enhanced
;; in Org Mode version 9.4
;; To benefit from this speedup in older versions of Org Mode,
;; this function is copied here with a slightly different name
;; It has also undergone near 3x speedup,
;; - by not using regexps
;; - achieving the shortest bytecode
;; Furthermore, this version avoids the
;; inhibit-changing-match-data and looking-at
;; incompatibilities between Emacs-27 and Emacs-30

(defun orgtbl-aggregate--table-to-lisp (&optional txt)
  "Convert the table at point to a Lisp structure.
The structure will be a list.  Each item is either the symbol `hline'
for a horizontal separator line, or a list of field values as strings.
The table is taken from the parameter TXT, or from the buffer at point."
  (if txt
      (with-temp-buffer
	(buffer-disable-undo)
        (insert txt)
        (goto-char (point-min))
        (orgtbl-aggregate--table-to-lisp))
    (save-excursion
      (goto-char (org-table-begin))
      (let (table)
        (while (progn (skip-chars-forward " \t")
                      (eq (following-char) ?|))
	  (forward-char)
	  (push
	   (if (eq (following-char) ?-)
	       'hline
	     (let (row)
	       (while (progn (skip-chars-forward " \t")
                             (not (eolp)))
                 (let ((q (point)))
                   (skip-chars-forward "^|\n")
                   (goto-char
                    (let ((p (point)))
                      (unless (eolp) (setq p (1+ p)))
                      (skip-chars-backward " \t" q)
                      (push
                       (buffer-substring-no-properties q (point))
                       row)
                      p))))
	       (nreverse row)))
	   table)
	  (forward-line))
	(nreverse table)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Here is a bunch of useful utilities,
;; generic enough to be detached from the orgtbl-aggregate package.
;; For the time being, they are here.

(defun orgtbl-aggregate--list-local-tables ()
  "Search for available tables in the current file."
  (interactive)
  (let ((tables))
    (save-excursion
      (goto-char (point-min))
      (while (let ((case-fold-search t))
	       (re-search-forward
		(rx bol
		    (* (any " \t")) "#+" (? "tbl") "name:"
		    (* (any " \t")) (group (* not-newline)))
		nil t))
	(push (match-string-no-properties 1) tables)))
    tables))

(defun orgtbl-aggregate--get-table-from-babel (name-or-id)
  "Retrieve an input table as the result of running a Babel block.
The table cells get stringified."
  ;; A user error is generated in case no Babel block is found
  (let ((table (org-babel-ref-resolve name-or-id)))
    (cl-loop
     for row in table
     if (listp row)
     do
     (cl-loop
      for cell on row
      unless (stringp (car cell))
      do (setcar cell (format "%s" (car cell)))))
    table))

(defun orgtbl-aggregate--get-distant-table (name-or-id)
  "Find a table in the current buffer named NAME-OR-ID.
Return it as a Lisp list of lists.
An horizontal line is translated as the special symbol `hline'."
  (unless (stringp name-or-id)
    (setq name-or-id (format "%s" name-or-id)))
  (let (buffer loc)
    (save-excursion
      (goto-char (point-min))
      (if (let ((case-fold-search t))
	    (re-search-forward
	     ;; This concat is automatically done by new versions of rx
	     ;; using "literal". This appeared on june 26, 2019
	     ;; For older versions of Emacs, we fallback to concat
	     (concat
	      (rx bol
		  (* (any " \t")) "#+" (? "tbl") "name:"
		  (* (any " \t")))
	      (regexp-quote name-or-id)
	      (rx (* (any " \t"))
		  eol))
	     nil t))
	  (setq buffer (current-buffer)
		loc (match-beginning 0))
	(let ((id-loc (org-id-find name-or-id 'marker)))
	  (when (and id-loc (markerp id-loc))
	    (setq buffer (marker-buffer id-loc)
		  loc (marker-position id-loc))
	    (move-marker id-loc nil)))))
    (or
     (and buffer
          (with-current-buffer buffer
            (save-excursion
	      (goto-char loc)
	      (forward-line 1)
              (beginning-of-line)
	      (and (re-search-forward
                    (rx
                     point
                     (or
                      (group (1+ "*") " ")
                      (seq
                       (0+ (0+ blank) "#" (0+ any) "\n")
                       (0+ blank) "|")))
                    nil t)
		   (not (match-beginning 1))
	           (orgtbl-aggregate--table-to-lisp)))))
     (orgtbl-aggregate--get-table-from-babel name-or-id))))

(defun orgtbl-aggregate--remove-cookie-lines (table)
  "Remove lines of TABLE which contain cookies.
But do not remove cookies in the header, if any.
The operation is destructive.  But on the other hand,
if there are no cookies in TABLE, TABLE is returned
without any change.
A cookie is an alignment instruction like:
  <l>   left align cells in this column
  <c>   center cells
  <r>   right align
  <15>  make this column 15 characters wide."
  (orgtbl-aggregate--pop-leading-hline table)
  (cl-loop with hline = nil
           for line on table
           if (and hline
                   (cl-loop for cell in (car line)
                            thereis (string-match
                                     (rx bol "<"
                                         (? (any "lcr"))
                                         (* (any "0-9"))
                                         ">" eol)
                                     cell)))
           do (setcar line t)
           if (eq (car line) 'hline)
           do (setq hline t))
  (delq t table))

(defun orgtbl-aggregate--split-string-with-quotes (string)
  "Like (split-string STRING), but with quote protection.
Single and double quotes protect space characters,
and also single quotes protect double quotes
and the other way around."
  (let ((l (length string))
	(start 0)
	(result (orgtbl-aggregate--list-create)))
    (save-match-data
      (while (and (< start l)
		  (string-match
		   (rx
		    (* (any " \f\t\n\r\v"))
		    (group
		     (+ (or
			 (seq ?'  (* (not (any ?')))  ?' )
			 (seq ?\" (* (not (any ?\"))) ?\")
			 (not (any " '\""))))))
		   string start))
	(orgtbl-aggregate--list-append result (match-string 1 string))
	(setq start (match-end 1))))
    (orgtbl-aggregate--list-get result)))

(defun orgtbl-aggregate--colname-to-int (colname table &optional err)
  "Convert the COLNAME into an integer.
COLNAME is a column name of TABLE.
The first column is numbered 1.
COLNAME may be:
- a dollar form, like $5 which is converted to 5
- an alphanumeric name which appears in the column header (if any)
- the special symbol `hline' which is converted into 0
If COLNAME is quoted (single or double quotes),
quotes are removed beforhand.
When COLNAME does not match any actual column,
an error is generated if ERR optional parameter is true
otherwise nil is returned."
  (if (symbolp colname)
      (setq colname (symbol-name colname)))
  (if (string-match
       (rx
	bol
	(or
	 (seq ?'  (group-n 1 (* (not (any ?' )))) ?' )
	 (seq ?\" (group-n 1 (* (not (any ?\")))) ?\"))
	eol)
       colname)
      (setq colname (match-string 1 colname)))
  ;; skip first hlines if any
  (orgtbl-aggregate--pop-leading-hline table)
  (cond ((equal colname "")
	 (and err (user-error "Empty column name")))
	((equal colname "hline")
	 0)
	((string-match (rx bol "$" (group (+ (any "0-9"))) eol) colname)
	 (let ((n (string-to-number (match-string 1 colname))))
	   (if (<= n (length (car table)))
	       n
	     (if err
		 (user-error "Column %s outside table" colname)))))
	((and
          (memq 'hline table)
	  (cl-loop
	   for h in (car table)
	   for i from 1
	   thereis (and (equal h colname) i))))
        (err
	 (user-error "Column %s not found in table" colname))))

(defun orgtbl-aggregate--insert-make-spaces (n spaces-cache)
  "Make a string of N spaces.
Caches results into SPACES-CACHE to avoid re-allocating
again and again the same string."
  (if (< n (length spaces-cache))
      (or (aref spaces-cache n)
	  (aset spaces-cache n (make-string n ? )))
    (make-string n ? )))

;; Time optimization: surprisingly,
;; (insert (concat a b c)) is faster than
;; (insert a b c)
;; Therefore, we build the Org Mode representation of a table
;; as a list of strings which get concatenated into a huge string.
;; This is faster and less garbage-collector intensive than
;; inserting cells one at a time in a buffer.
;;
;; benches:
;; insert a large 3822 rows × 16 columns table
;; - one row at a time or as a whole
;; - with or without undo active
;; repeat 10 times
;;
;; with undo, one row at a time
;;  (3.587732240 40 2.437140552)
;;  (3.474445440 39 2.341087725)
;;
;; without undo, one row at a time
;;  (3.127574093 33 2.001691096)
;;  (3.238456106 33 2.089536034)
;;
;; with undo, single huge string
;;  (3.030763545 30 1.842303196)
;;  (3.012367879 30 1.841319998)
;;
;; without undo, single huge string
;;  (2.499138596 21 1.419285666)
;;  (2.403039955 21 1.338347655)
;;       ▲       ▲      ▲
;;       │       │      ╰──╴CPU time for GC
;;       │       ╰─────────╴number of GC
;;       ╰─────────────────╴overall CPU time

(defun orgtbl-aggregate--elisp-table-to-string (table)
  "Convert TABLE to a string formatted as an Org Mode table.
TABLE is a list of lists of cells.  The list may contain the
special symbol `hline' to mean an horizontal line."
  (let* ((nbcols (cl-loop
		  for row in table
		  maximize (if (listp row) (length row) 0)))
	 (maxwidths  (make-list nbcols 1))
	 (numbers    (make-list nbcols 0))
	 (non-empty  (make-list nbcols 0))
	 (spaces-cache (make-vector 100 nil)))

    ;; compute maxwidths
    (cl-loop for row in table
	     do
	     (cl-loop for cell on row
		      for mx on maxwidths
		      for nu on numbers
		      for ne on non-empty
		      for cellnp = (car cell)
		      do (cond ((not cellnp)
				(setcar cell (setq cellnp "")))
		       	       ((not (stringp cellnp))
		      		(setcar cell (setq cellnp (format "%s" cellnp)))))
		      if (string-match-p org-table-number-regexp cellnp)
		      do (setcar nu (1+ (car nu)))
		      unless (equal cellnp "")
		      do (setcar ne (1+ (car ne)))
		      if (< (car mx) (string-width cellnp))
		      do (setcar mx (string-width cellnp))))

    ;; change meaning of numbers from quantity of cells with numbers
    ;; to flags saying whether alignment should be left (number alignment)
    (cl-loop for nu on numbers
	     for ne in non-empty
	     do
	     (setcar nu (< (car nu) (* org-table-number-fraction ne))))

    ;; creage well padded and aligned cells
    (let ((bits (orgtbl-aggregate--list-create)))
      (cl-loop for row in table
	       do
	       (if (listp row)
		   (cl-loop for cell in row
			    for mx in maxwidths
			    for nu in numbers
			    for pad = (- mx (string-width cell))
                            do
			    (orgtbl-aggregate--list-append bits "| ")
			    (cond
			     ;; no alignment
                             ((<= pad 0)
			      (orgtbl-aggregate--list-append bits cell))
			     ;; left alignment
			     (nu
			      (orgtbl-aggregate--list-append bits cell)
                              (orgtbl-aggregate--list-append
                               bits
                               (orgtbl-aggregate--insert-make-spaces pad spaces-cache)))
			     ;; right alignment
                             (t
			      (orgtbl-aggregate--list-append
                               bits
                               (orgtbl-aggregate--insert-make-spaces pad spaces-cache))
			      (orgtbl-aggregate--list-append bits cell)))
			    (orgtbl-aggregate--list-append bits " "))
		 (cl-loop for bar = "|" then "+"
			  for mx in maxwidths
                          do
			  (orgtbl-aggregate--list-append bits bar)
			  (orgtbl-aggregate--list-append bits (make-string (+ mx 2) ?-))))
	       (orgtbl-aggregate--list-append bits "|\n"))
      ;; remove the last \n because Org Mode re-adds it
      (setcar (car bits) "|")
      (mapconcat
       #'identity
       (orgtbl-aggregate--list-get bits)
       ""))))

(defun orgtbl-aggregate--insert-elisp-table (table)
  "Insert TABLE in current buffer at point.
TABLE is a list of lists of cells.  The list may contain the
special symbol `hline' to mean an horizontal line."
  ;; inactivating jit-lock-after-change boosts performance a lot
  (cl-letf (((symbol-function 'jit-lock-after-change) (lambda (_a _b _c)) ))
    (insert (orgtbl-aggregate--elisp-table-to-string table))))

(defun orgtbl-aggregate--get-header-table (table &optional asstring)
  "Return the header of TABLE as a list of column names.
When ASSTRING is true, the result is a string which concatenates the
names of the columns.  TABLE may be a Lisp list of rows, or the
name or id of a distant table.  The function takes care of
possibly missing headers, and in this case returns a list
of $1, $2, $3... column names.
Actual column names which are not fully alphanumeric are quoted."
  (unless (consp table)
    (setq table (orgtbl-aggregate--get-distant-table table)))
  (orgtbl-aggregate--pop-leading-hline table)
  (let ((header
	 (if (memq 'hline table)
	     (cl-loop for x in (car table)
		      collect
		      (if (string-match
                           (rx bol (+ (in "$._" word)) eol)
                           x)
			  x
			(format "\"%s\"" x)))
	   (cl-loop for _x in (car table)
		    for i from 1
		    collect (format "$%s" i)))))
    (if asstring
	(mapconcat #'identity header " ")
      header)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The venerable Calc is used thoroughly by the Aggregate package.
;; A few bugs were found.
;; They have been fixed in recent versions of Emacs
;; Uncomment the fixes if needed
;(defun math-max-list (a b)
;  (if b
;      (if (or (Math-anglep (car b)) (eq (caar b) 'date)
;	      (and (eq (car (car b)) 'intv) (math-intv-constp (car b)))
;	      (math-infinitep (car b)))
;	  (math-max-list (math-max a (car b)) (cdr b))
;	(math-reject-arg (car b) 'anglep))
;    a))
;
;(defun math-min-list (a b)
;  (if b
;      (if (or (Math-anglep (car b)) (eq (caar b) 'date)
;	      (and (eq (car (car b)) 'intv) (math-intv-constp (car b)))
;	      (math-infinitep (car b)))
;	  (math-min-list (math-min a (car b)) (cdr b))
;	(math-reject-arg (car b) 'anglep))
;    a))
;; End of Calc fixes

;; The *this* variable is accessible to the user.
;; It refers to the aggregated table before it is "printed"
;; into the buffer, so that it can be post-processed.
(defvar *this*)

(defun orgtbl-aggregate--post-process (table post)
  "Post-process the aggregated TABLE according to the :post header.
POST might be:
- a reference to a babel-block, for example:
  :post \"myprocessor(inputtable=*this*)\"
  and somewhere else:
  #+name: myprocessor
  #+begin_src language :var inputtable=
  ...
  #+end_src
- a Lisp lambda with one parameter, for example:
  :post (lambda (table) (append table \\'(hline (\"total\" 123))))
- a Lisp function with one parameter, for example:
  :post my-lisp-function
- a Lisp expression which will be evaluated
  the *this* variable will contain the TABLE
In all those cases, the result must be a Lisp value compliant
with an Org Mode table."
  (cond
   ((null post) table)
   ((functionp post)
    (apply post table ()))
   ((stringp post)
    (let ((*this* table))
      (condition-case err
	  (org-babel-ref-resolve post)
	(error
	 (message "error: %S" err)
         (condition-case err2
	     (orgtbl-aggregate--post-process
              table
              (thing-at-point--read-from-whole-string post))
           (error
            (user-error
             ":post %S ends in an error
- as a Babel block: %s
- not a valid Lisp expression: %s"
             post err err2)))))))
   ((listp post)
    (let ((*this* table))
      (eval post)))
   (t (user-error ":post %S header could not be understood" post))))

(require 'calc-arith)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The Org Table Aggregation package really begins here

(defun orgtbl-aggregate--replace-colnames-nth (table expression)
  "Replace occurrences of column names in Lisp EXPRESSION.
Replacements are forms like (nth N row),
N being the numbering of columns.
Doing so, EXPRESSION is ready to be computed against a TABLE row."
  (cond
   ((listp expression)
    (cons (car expression)
	  (cl-loop for x in (cdr expression)
		   collect
		   (orgtbl-aggregate--replace-colnames-nth table x))))
   ((numberp expression)
    expression)
   (t
    (let ((n (orgtbl-aggregate--colname-to-int expression table)))
      (if n
	  (list 'nth n 'orgtbl-aggregate--row)
	expression)))))

;; dynamic binding
(defvar orgtbl-aggregate--var-keycols)

(cl-defstruct orgtbl-aggregate--outcol
  formula	; user-entered formula to compute output cells
  format	; user-entered formatter of output cell
  sort		; user-entered sorting instruction for output column
  invisible	; user-entered output column invisibility
  name		; user-entered output column name
  formula$	; derived formula with $N instead of input column names
  formula-frux	; derived formula in Calc format with Frux(N) for input columns
  involved	; list of input columns numbers appearing in formula
  key		; is this output column a key-column?
  )

(defun orgtbl-aggregate--parse-col (col table)
  "Parse COL specification into an ORGTBL-AGGREGATE--OUTCOL structure.
COL is a column specification.  It is a string text:
\"formula;formatter;^sorting;<invisible>;'alternate_name'\"
If there is no formatter or sorting or other specifier,
nil is given in place. The other fields of orgtbl-aggregate--OUTCOL are
filled here too, and nowhere else.
TABLE is used to convert a column name
into the column number."
  ;; parse user specification
  (unless (string-match
	   (rx
	    bol
	    (group-n 1
		     (* (or
			 (seq ?'  (* (not (any ?')))  ?' )
			 (seq ?\" (* (not (any ?\"))) ?\")
			 (not (any ";'\"")))))
	    (*
	     ";"
	     (or
	      (seq     (group-n 2 (* (not (any "^;'\"<")))))
	      (seq "^" (group-n 3 (* (not (any "^;'\"<")))))
	      (seq "<" (group-n 4 (* (not (any "^;'\">")))) ">")
	      (seq "'" (group-n 5 (* (not (any "'")))) "'")))
	    eol)
	   col)
    (user-error "Bad column specification: %S" col))
  (let* ((formula   (match-string 1 col))
	 (format    (match-string 2 col))
	 (sort      (match-string 3 col))
	 (invisible (match-string 4 col))
	 (name      (match-string 5 col))

	 ;; list the input column numbers which are involved
	 ;; into formula
	 (involved  nil)

	 ;; create a derived formula in Calc format,
	 ;; where names of input columns are replaced with
	 ;; frux(N)
	 (frux
	  (replace-regexp-in-string
	   (rx
	    (or
	     (seq ?'  (* (not (any ?' ))) ?')
	     (seq ?\" (* (not (any ?\"))) ?\")
	     (seq (+ (any word "_$."))))
	    (? (* space) "("))
	   (lambda (var)
	     (save-match-data ;; save because we are called within a replace-regexp
	       (if (string-match
                    (rx (group (+ (not (any "(")))) (* space) "(") var)
		   (if (member
			(match-string 1 var)
			'("mean" "meane" "gmean" "hmean" "median" "sum"
			  "min" "max" "prod" "pvar" "sdev" "psdev"
			  "corr" "cov" "pcov" "count" "span" "var"))
		       ;; aggregate functions with or without the leading "v"
		       ;; for example, sum(X) and vsum(X) are equivalent
		       (format "v%s" var)
		     var)
		 ;; replace VAR if it is a column name
		 (let ((i (orgtbl-aggregate--colname-to-int
			   var
			   table)))
		   (if i
		       (progn
			 (unless (member i involved)
			   (push i involved))
			 (format "Frux(%s)" i))
		     var)))))
	   formula))

	 ;; create a derived formula where input column names
	 ;; are replaced with $N
	 (formula$
	  (replace-regexp-in-string
	   (rx "Frux(" (+ (any "0-9")) ")")
	   (lambda (var)
	     (save-match-data
	       (string-match
		(rx (group (+ (any "0-9"))))
		var)
	       (format "$%s" (match-string 1 var))))
	   frux))

	 ;; if a formula is just an input column name,
	 ;; then it is a key-grouping-column
	 (key
	  (if (string-match
	       (rx
		bol
		(group
		 (or (seq "'"  (* (not (any "'" ))) "'" )
		     (seq "\"" (* (not (any "\""))) "\"")
		     (+ (any word "_$."))))
		eol)
	       formula)
	      (orgtbl-aggregate--colname-to-int formula table t))))

    (if key (push key orgtbl-aggregate--var-keycols))

    (make-orgtbl-aggregate--outcol
     :formula      formula
     :format       format
     :sort         sort
     :invisible    invisible
     :name         name
     :formula$     formula$
     :formula-frux (math-read-expr frux)
     :involved     involved
     :key          key)))

;; dynamic binding
(defvar orgtbl-aggregate--columns-sorting)

(cl-defstruct orgtbl-aggregate--sorting
  strength
  colnum
  ascending
  extract
  compare)

(defun orgtbl-aggregate--prepare-sorting (aggcols)
  "Create a list of columns to be sorted.
Columns are searched into AGGCOLS.
The resulting list will be used by
`orgtbl-aggregate--columns-sorting'.
The list contains sorting specifications as follows:
  . sorting strength
  . column number
  . ascending descending
  . extract function
  . compare function
- sorting strength is a number telling what column should be
  considered first:
  . lower number are considered first
  . nil are condirered last
- column number is as in the user specification
  1 is the first user specified column
- ascending descending is nil for ascending, t for descending
- extract function converts the input cell (which is a string)
  into a comparable value
- compare function compares two cells and answers nil if
  the first cell must come before the second."
  (cl-loop for col in aggcols
	   for sorting = (orgtbl-aggregate--outcol-sort col)
	   for colnum from 0
	   if sorting
	   do (progn
		(unless (string-match
                         (rx bol
                             (group (any "aAnNtTfF"))
                             (group (* (any num)))
                             eol)
                         sorting)
		  (user-error
                   "Bad sorting specification: ^%s, expecting a/A/n/N/t/T and an optional number"
                   sorting))
		(orgtbl-aggregate--list-append
		 orgtbl-aggregate--columns-sorting
		 (let ((strength
			(if (equal (match-string 2 sorting) "")
			    nil
			  (string-to-number (match-string 2 sorting)))))
		   (pcase (match-string 1 sorting)
		     ("a" (record 'orgtbl-aggregate--sorting strength colnum nil #'identity              #'string-lessp))
		     ("A" (record 'orgtbl-aggregate--sorting strength colnum t   #'identity              #'string-lessp))
		     ("n" (record 'orgtbl-aggregate--sorting strength colnum nil #'string-to-number                 #'<))
		     ("N" (record 'orgtbl-aggregate--sorting strength colnum t   #'string-to-number                 #'<))
		     ("t" (record 'orgtbl-aggregate--sorting strength colnum nil #'orgtbl-aggregate--string-to-time #'<))
		     ("T" (record 'orgtbl-aggregate--sorting strength colnum t   #'orgtbl-aggregate--string-to-time #'<))
		     ((or "f" "F") (user-error "f/F sorting specification not (yet) implemented"))
		     (_ (user-error "Bad sorting specification ^%s" sorting)))))))

  ;; major sorting columns must come before minor sorting columns
  (setq orgtbl-aggregate--columns-sorting
	(sort (orgtbl-aggregate--list-get orgtbl-aggregate--columns-sorting)
	      (lambda (a b)
		(if      (null   (orgtbl-aggregate--sorting-strength a))
		    (and (null   (orgtbl-aggregate--sorting-strength b))
			 (<      (orgtbl-aggregate--sorting-colnum   a)
                                 (orgtbl-aggregate--sorting-colnum   b)))
		  (or    (null   (orgtbl-aggregate--sorting-strength b))
		         (<      (orgtbl-aggregate--sorting-strength a)
                                 (orgtbl-aggregate--sorting-strength b))
			 (and (= (orgtbl-aggregate--sorting-strength a)
                                 (orgtbl-aggregate--sorting-strength b))
			      (< (orgtbl-aggregate--sorting-colnum   a)
                                 (orgtbl-aggregate--sorting-colnum   b)))))))))

;; escape lexical binding to eval user given
;; Lisp expression
(defvar orgtbl-aggregate--row)

(defun orgtbl-aggregate--table-add-group (groups hgroups row aggcond)
  "Add the source ROW to the GROUPS of rows.
If ROW fits a group within GROUPS, then it is added at the end
of this group.
Otherwise a new group is added at the end of GROUPS,
containing this single ROW.
AGGCOND is a formula which is evaluated against ROW.
If nil, ROW is just discarded.
HGROUPS contains the same information as GROUPS, stored in
a hash-table, whereas GROUPS is a Lisp list."
  (and (or (not aggcond)
	   (let ((orgtbl-aggregate--row row))
             ;; this eval need the variable 'orgtbl-aggregate--row
             ;; to have a value
             (eval aggcond)))
       (let ((gr (gethash row hgroups)))
	 (unless gr
	   (setq gr (orgtbl-aggregate--list-create))
	   (puthash row gr hgroups)
	   (orgtbl-aggregate--list-append groups gr))
	 (orgtbl-aggregate--list-append gr row))))

(defun orgtbl-aggregate--read-calc-expr (expr)
  "Interpret EXPR (a string) as either an org date or a calc expression."
  (cond
   ;; nil happens when a table is malformed
   ;; some columns are missing in some rows
   ((not expr) nil)
   ;; empty cell returned as nil,
   ;; to be processed later depending on modifier flags
   ((equal expr "") nil)
   ;; the purely numerical cell case arises very often
   ;; short-circuiting general functions boosts performance (a lot)
   ((and
     (string-match
      (rx bos
	  (? (any "+-")) (* (any "0-9"))
	  (? "." (* (any "0-9")))
	  (? "e" (? (any "+-")) (+ (any "0-9")))
	  eos)
      expr)
     (not (string-match (rx bos (* (any "+-.")) "e") expr)))
    (math-read-number expr))
   ;; Convert an Org-mode date to Calc internal representation
   ((string-match org-ts-regexp0 expr)
    (math-parse-date
     (replace-regexp-in-string
      (rx (* " ") (+ (any "a-z")) (opt ".") (* " "))
      " "
      expr)))
   ;; Convert a duration into a number of seconds
   ((string-match
     (rx bos
	 (group (one-or-more (any "0-9")))
	 ":"
	 (group (any "0-9") (any "0-9"))
	 (? ":" (group (any "0-9") (any "0-9")))
	 eos)
     expr)
    (+
     (* 3600 (string-to-number (match-string 1 expr)))
     (*   60 (string-to-number (match-string 2 expr)))
     (if (match-string 3 expr) (string-to-number (match-string 3 expr)) 0)))
   ;; generic case: symbolic calc expression
   (t
    (math-simplify
     (calcFunc-expand
      (math-read-expr expr))))))

(defun orgtbl-aggregate--hash-test-equal (row1 row2)
  "Are ROW1 & ROW2 equal regarding the key columns?"
  (cl-loop for idx in orgtbl-aggregate--var-keycols
	   always (string= (nth idx row1) (nth idx row2))))

;; Use standard sxhash-equal to hash strings
;; Unfortunately sxhash-equal is weak.
;; So we compensate with multiplications and reminders,
;; while trying to stay within the 2^29 fixnums.
;; see (info "(elisp) Integer Basics")

(defun orgtbl-aggregate--hash-test-hash (row)
  "Compute a hash code for ROW from key columns."
  (let ((h 456542153))
    (cl-loop for idx in orgtbl-aggregate--var-keycols
	     do
             (setq
              h
              (*
               (%
                (*
                 (%
                  (logxor
                   (sxhash-equal (nth idx row))
                   h)
                  53639)
                 9973)
                53633)
               10007)))
    h))

(defun orgtbl-aggregate--create-table-aggregated (table params)
  "Convert the source TABLE into an aggregated table.
The source TABLE is a list of lists of cells.
The resulting table follows the specifications,
found in PARAMS entry :cols, ignoring source rows
which do not pass the filter found in PARAMS entry :cond."
  (orgtbl-aggregate--pop-leading-hline table)
  (define-hash-table-test
    'orgtbl-aggregate--hash-test-name
    #'orgtbl-aggregate--hash-test-equal
    #'orgtbl-aggregate--hash-test-hash)
  (let ((groups (orgtbl-aggregate--list-create))
	(hgroups (make-hash-table :test 'orgtbl-aggregate--hash-test-name))
	(aggcols (plist-get params :cols))
	(aggcond (plist-get params :cond))
	(hline   (plist-get params :hline))
	;; a global variable, passed to the sort predicate
	(orgtbl-aggregate--columns-sorting (orgtbl-aggregate--list-create))
	;; another global variable
	(orgtbl-aggregate--var-keycols))
    (unless aggcols
      (setq aggcols (orgtbl-aggregate--get-header-table table)))
    (if (stringp aggcols)
	(setq aggcols (orgtbl-aggregate--split-string-with-quotes aggcols)))
    (cl-loop for col on aggcols
	     do (setcar col (orgtbl-aggregate--parse-col (car col) table)))
    (when aggcond
      (if (stringp aggcond)
	  (setq aggcond (read aggcond)))
      (setq aggcond
	    (orgtbl-aggregate--replace-colnames-nth table aggcond)))
    (setq hline
	  (cond ((null hline)
		 0)
		((numberp hline)
		 hline)
		((string-match-p (rx bol (or "yes" "t") eol) hline)
		 1)
		((string-match-p (rx bol (or "no" "nil") eol) hline)
		 0)
		((string-match-p "[0-9]+" hline)
		 (string-to-number hline))
		(t
		 (user-error
                  ":hline parameter should be 0, 1, 2, 3, ... or yes, t, no, nil, not %S"
                  hline))))

    ;; special case: no sorting column but :hline 1 required
    ;; then a hidden hline column is added
    (if (and (> hline 0)
	     (cl-loop for col in aggcols
		      never (orgtbl-aggregate--outcol-sort col)))
	(push
	 (orgtbl-aggregate--parse-col "hline;^n;<>" table)
	 aggcols))

    (orgtbl-aggregate--prepare-sorting aggcols)

    ; split table into groups of rows
    (cl-loop with b = 0
	     with bs = "0"
	     for row in
	     (or (cdr (memq 'hline table)) ;; skip header if any
		 table)
	     do
	     (cond ((eq row 'hline)
		    (setq b (1+ b)
			  bs (number-to-string b)))
		   ((listp row)
		    (orgtbl-aggregate--table-add-group
		     groups
		     hgroups
		     (cons bs row)
		     aggcond))))

    (let ((result ;; pre-allocate all resulting rows
	   (cl-loop for _x in (orgtbl-aggregate--list-get groups)
		    collect (orgtbl-aggregate--list-create)))
	  (all-$list
	   (cl-loop for _x in (orgtbl-aggregate--list-get groups)
		    collect
                    (make-vector
                     (1+ (length (car table))) ;; 1+ for hline at 0
                     nil))))

      ;; inactivating those two functions boosts performance
      (cl-letf (((symbol-function 'math-read-preprocess-string) #'identity)
		((symbol-function 'calc-input-angle-units) (lambda (_x) nil)))
	;; do aggregation
	(cl-loop for coldesc in aggcols
		 do
		 (orgtbl-aggregate--compute-sums-on-one-column
                  groups result coldesc all-$list)))

      ;; sort table according to columns described in
      ;; orgtbl-aggregate--columns-sorting
      (if orgtbl-aggregate--columns-sorting ;; are there sorting instructions?
	  (setq result (sort result #'orgtbl-aggregate--sort-predicate)))

      ;; add hlines if requested
      (if (> hline 0)
	  (orgtbl-aggregate--add-hlines result hline))

      (push 'hline result)

      ;; add other lines of the original header, if any;
      ;; this is done only if the aggregated column refers to
      ;; a single source column (either a key column or within
      ;; an aggregated formula)
      (orgtbl-aggregate--pop-leading-hline table)

      (if (memq 'hline table)
          (cl-loop
           for i from (cl-loop
                       for i from -1
                       for x in table
                       until (eq x 'hline)
                       finally return i)
           downto 1
           do (push
               (cons
                nil
                (cl-loop for column in aggcols
                         collect
                         (if (equal (length (orgtbl-aggregate--outcol-involved column)) 1)
                             (let ((n (1- (car (orgtbl-aggregate--outcol-involved column)))))
                               (if (>= n 0)
                                   (nth n (nth i table))
                                 ""))
                           "")))
               result)))

      ;; add the header to the resulting table with column names
      ;; as they appear in :cols but without decorations
      (push
       (cons
        nil
	(cl-loop for column in aggcols
		 collect (or
			  (orgtbl-aggregate--outcol-name column)
                          (replace-regexp-in-string
                           "['\"]" ""
			   (orgtbl-aggregate--outcol-formula column)))))
       result)

      ;; remove invisible columns by modifying the table in-place
      ;; beware! it assumes that the actual list in orgtbl-aggregate--lists
      ;; is pointed to by the cdr of the orgtbl-aggregate--list
      (if (cl-loop for col in aggcols
		   thereis (orgtbl-aggregate--outcol-invisible col))
	  (cl-loop for row in result
		   if (consp row)
		   do (cl-loop for col in aggcols
			       with cel = row
			       if (orgtbl-aggregate--outcol-invisible col)
			       do    (setcdr cel (cddr cel))
			       else do (orgtbl-aggregate--pop-simple cel))))

      ;; change appendable-lists to regular lists
      (cl-loop for row on result
	       if (consp (car row))
	       do (setcar row (orgtbl-aggregate--list-get (car row))))

      result)))

(defun orgtbl-aggregate--sort-predicate (linea lineb)
  "Compares LINEA & LINEB (which are Org Mode table rows)
according to orgtbl-aggregate--columns-sorting instructions.
Return nil if LINEA already comes before LINEB."
  (setq linea (orgtbl-aggregate--list-get linea))
  (setq lineb (orgtbl-aggregate--list-get lineb))
  (cl-loop for col in orgtbl-aggregate--columns-sorting
	   for colnum  = (orgtbl-aggregate--sorting-colnum    col)
	   for desc    = (orgtbl-aggregate--sorting-ascending col)
	   for extract = (orgtbl-aggregate--sorting-extract   col)
	   for compare = (orgtbl-aggregate--sorting-compare   col)
	   for cola = (funcall extract (nth colnum (if desc lineb linea)))
	   for colb = (funcall extract (nth colnum (if desc linea lineb)))
	   thereis (funcall compare cola colb)
	   until   (funcall compare colb cola)))

(defun orgtbl-aggregate--string-to-time (f)
  "Interprete the string F into a duration in minutes.
The code was borrowed from org-table.el."
  (cond ((string-match org-ts-regexp-both f)
	 (float-time
	  (org-time-string-to-time (match-string 0 f))))
	((org-duration-p f) (org-duration-to-minutes f))
	((string-match
          (rx bow (+ (any "0-9")) ":" (= 2 (any "0-9")) eow)
          f)
	 (org-duration-to-minutes (match-string 0 f)))
	(t 0)))

(defun orgtbl-aggregate--add-hlines (result hline)
  "Add hlines to RESULT between different blocks of rows.
HLINE is a small number (1 or 2 or 3, maybe more)
which gives the number of sorted columns to consider
to split rows blocks with hlines.
hlines are added in-place"
  (let ((colnums
	 (cl-loop for col in orgtbl-aggregate--columns-sorting
		  for n from 1 to hline
		  collect (orgtbl-aggregate--sorting-colnum col))))
    (cl-loop for row on result
	     unless
	     (or (null oldrow)
		 (cl-loop for c in colnums
			  always
                          (equal
			   (nth c (orgtbl-aggregate--list-get (car row)))
			   (nth c (orgtbl-aggregate--list-get (car oldrow))))))
	     do (setcdr oldrow (cons 'hline (cdr oldrow)))
	     for oldrow = row)))

(defun orgtbl-aggregate--fmt-settings (fmt)
  "Convert the FMT user-given format.
Result is the FMT-SETTINGS assoc list."
  (let ((fmt-settings (plist-put () :fmt nil)))
    (when fmt
      ;; the following code was freely borrowed from org-table-eval-formula
      ;; not all settings extracted from fmt are used
      (while (string-match
              (rx (group (any "pnfse")) (group (opt "-") (+ (any "0-9"))))
              fmt)
	(let ((c (string-to-char   (match-string 1 fmt)))
	      (n (string-to-number (match-string 2 fmt))))
          (cl-case c
            (?p (setq calc-internal-prec n))
	    (?n (setq calc-float-format (list 'float n)))
	    (?f (setq calc-float-format (list 'fix   n)))
	    (?s (setq calc-float-format (list 'sci   n)))
	    (?e (setq calc-float-format (list 'eng   n)))))
	(setq fmt (replace-match "" t t fmt)))
      (while (string-match "[tTUNLEDRFSuQqCc]" fmt)
        (cl-case (string-to-char (match-string 0 fmt))
          (?t (plist-put fmt-settings :duration t)
	      (plist-put fmt-settings :numbers  t)
	      (plist-put fmt-settings :duration-output-format org-table-duration-custom-format))
          (?T (plist-put fmt-settings :duration t)
	      (plist-put fmt-settings :numbers  t)
	      (plist-put fmt-settings :duration-output-format nil))
          (?U (plist-put fmt-settings :duration t)
	      (plist-put fmt-settings :numbers  t)
	      (plist-put fmt-settings :duration-output-format 'hh:mm))
          (?N (plist-put fmt-settings :numbers  t))
          (?L (plist-put fmt-settings :literal t))
          (?E (plist-put fmt-settings :keep-empty t))
	  (?D (setq calc-angle-mode 'deg))
	  (?R (setq calc-angle-mode 'rad))
	  (?F (setq calc-prefer-frac t))
	  (?S (setq calc-symbolic-mode t))
          (?u (setq calc-simplify-mode 'units))
          (?c (plist-put fmt-settings :debug ?c))
          (?C (plist-put fmt-settings :debug ?C))
          (?q (plist-put fmt-settings :debug ?q))
	  (?Q (plist-put fmt-settings :debug ?Q)))
	(setq fmt (replace-match "" t t fmt)))
      (when (string-match (rx (not (syntax whitespace))) fmt)
	(plist-put fmt-settings :fmt fmt)))
    fmt-settings))

(defmacro orgtbl-aggregate--calc-setting (setting &optional setting0)
  "Retrieve a Calc setting.
The setting comes either from `org-calc-default-modes'
or from SETTING itself.
SETTING0 is a default to use if both fail."
  ;; plist-get would be fine, except that there is no way
  ;; to distinguish a value of nil from no value
  ;; so we fallback to memq
  `(let ((x (memq (quote ,setting) org-calc-default-modes)))
     (if x (cadr x)
       (or ,setting ,setting0))))

(defun orgtbl-aggregate--compute-sums-on-one-column (groups result coldesc all-$list)
  "Apply COLDESC over all GROUPS of rows.
COLDESC is a formula given by the user in :cols,
with an optional format.
Common Calc settings and formats are pre-computed before
actually computing sums, because they are the same for all groups.
RESULT is the list of expected resulting rows.
At the beginning, all rows are empty lists.
A cell is appended to every row at each call of this function."

  ;; within this (let), we locally set Calc settings that must be active
  ;; for all the calls to Calc:
  ;; (orgtbl-aggregate--read-calc-expr) and (math-format-value)
  (let ((calc-internal-prec
 	 (orgtbl-aggregate--calc-setting calc-internal-prec))
	(calc-float-format
  	 (orgtbl-aggregate--calc-setting calc-float-format ))
	(calc-angle-mode
    	 (orgtbl-aggregate--calc-setting calc-angle-mode   ))
	(calc-prefer-frac
   	 (orgtbl-aggregate--calc-setting calc-prefer-frac  ))
	(calc-symbolic-mode
 	 (orgtbl-aggregate--calc-setting calc-symbolic-mode))
	(calc-date-format
   	 (orgtbl-aggregate--calc-setting calc-date-format '(YYYY "-" MM "-" DD " " www (" " hh ":" mm))))
	(calc-display-working-message
         (orgtbl-aggregate--calc-setting calc-display-working-message))
	(fmt-settings nil)
	(case-fold-search nil))

    ;; get that out of the (let) because its purpose is to override
    ;; what the (let) has set
    (setq fmt-settings
          (orgtbl-aggregate--fmt-settings
           (orgtbl-aggregate--outcol-format coldesc)))

    (cl-loop for group in (orgtbl-aggregate--list-get groups)
	     for row in result
	     for $list in all-$list
	     do
	     (orgtbl-aggregate--list-append
	      row
	      (orgtbl-aggregate--compute-one-sum
	       group
	       coldesc
	       fmt-settings
	       $list)))))

(defun orgtbl-aggregate--compute-one-sum (group coldesc fmt-settings $list)
  "Apply a user given formula to one GROUP of input rows.
COLDESC is a structure where several parameters are packed:
see (cl-defstruct orgtbl-aggregate--outcol ...).
Those parameters all describe a single column.
The formula is contained in COLDESC-formula-frux.
Column names have been replaced by Frux(1), Frux(2), Frux(3)... forms.
Those Frux(N) froms are placeholders that will be replaced
by Calc vectors of values extracted from the input table,
in column N.
COLDESC-involved is a list of columns numbers used by COLDESC-formula-frux.
$LIST is a Lisp-vector of Calc-vectors of values from the input table
parsed by Calc. $LIST acts as a cache. When a value is missing, it is
computed, and stored in $LIST. But if there is already a value,
a re-computation is saved.
FMT-SETTINGS are formatter settings computed by
`orgtbl-aggregate--fmt-settings', from user given formatting instructions.
Return an output cell.
When coldesc-key is non-nil, then a key-column is considered,
and a cell from any row in the group is returned."
  (cond
   ;; key column
   ((orgtbl-aggregate--outcol-key coldesc)
    (nth (orgtbl-aggregate--outcol-key coldesc)
	 (car (orgtbl-aggregate--list-get group))))
   ;; do not evaluate, output Calc formula
   ((eq (plist-get fmt-settings :debug) ?c)
    (orgtbl-aggregate--outcol-formula$ coldesc))
   ;; do not evaluate, output Lisp formula
   ((eq (plist-get fmt-settings :debug) ?q)
    (orgtbl-aggregate--outcol-formula-frux coldesc))
   ;; vlist($3) alone, without parenthesis or other decoration
   ((string-match
     (rx bos (? ?v) "list"
	 (* (any " \t")) "(" (* (any " \t"))
	 "$" (group (+ (any "0-9")))
	 (* (any " \t")) ")" (* (any " \t")) eos)
     (orgtbl-aggregate--outcol-formula$ coldesc))
    (mapconcat
     #'identity
     (cl-loop with i =
	      (string-to-number
               (match-string 1 (orgtbl-aggregate--outcol-formula$ coldesc)))
	      for row in (orgtbl-aggregate--list-get group)
	      collect (nth i row))
     ", "))
   (t
    ;; all other cases: handle them to Calc
    (let ((calc-dollar-values-oo
	   (orgtbl-aggregate--make-calc-$-list
	    group
	    fmt-settings
	    (orgtbl-aggregate--outcol-involved coldesc)
	    $list))
	  (calc-command-flags nil)
	  (calc-next-why nil)
	  (calc-language 'flat)
	  (calc-dollar-used 0))
      (let ((ev
	     (orgtbl-aggregate--defrux
	      (orgtbl-aggregate--outcol-formula-frux coldesc)
	      calc-dollar-values-oo
	      (length (orgtbl-aggregate--list-get group)))))

        (cond
         ((eq (plist-get fmt-settings :debug) ?C)
          (math-format-value ev))
         ((eq (plist-get fmt-settings :debug) ?Q)
          (format "%S" ev))
         ((progn
            (setq ev
                  (math-format-value
	           (math-simplify
	            (calcFunc-expand	  ; yes, double expansion
		     (calcFunc-expand  ; otherwise it is not fully expanded
		      (math-simplify
                       ev))))
                   1000))
            (plist-get fmt-settings :fmt))
	  (format (plist-get fmt-settings :fmt) (string-to-number ev)))
	 ((plist-get fmt-settings :duration)
	  (org-table-time-seconds-to-string
	   (string-to-number ev)
	   (plist-get fmt-settings :duration-output-format)))
	 (t ev)))))))

(defun orgtbl-aggregate--defrux (formula-frux calc-dollar-values-oo count)
  "Replace all Frux(N) expressions in FORMULA-FRUX.
Replace with Calc-vectors found in CALC-DOLLAR-VALUES-OO.
Also replace vcount() forms with the actual number of rows
in the current group, given by COUNT."
  (cond
   ((not (consp formula-frux))
    formula-frux)
   ((memq (car formula-frux) '(calcFunc-Frux calcFunc-FRUX))
    (nth (cadr formula-frux) calc-dollar-values-oo))
   ((eq (car formula-frux) 'calcFunc-vcount)
    count)
   (t
    (cl-loop
     for x in formula-frux
     collect (orgtbl-aggregate--defrux x calc-dollar-values-oo count)))))

(defun orgtbl-aggregate--make-calc-$-list (group fmt-settings involved $list)
  "Prepare a list of vectors that will use to replace Frux(N) expressions.
Frux(1) will be replaced by the first element of list,
Frux(2) by the second an so on.
The vectors follow the Calc syntax: (vec a b c ...).
They contain values extracted from rows of the current GROUP.
Vectors are created only for column numbers in INVOLVED.
In FMT-SETTINGS, :keep-empty is a flag to tell whether an empty cell
should be converted to NAN or ignored.
:numbers is a flag to replace non numeric values by 0."
  (cl-loop
   for i in involved
   unless (aref $list i)
   do (aset
       $list i
       (cons 'vec
	     (cl-loop for row in (orgtbl-aggregate--list-get group)
		      collect
		      (orgtbl-aggregate--read-calc-expr (nth i row))))))
  (cl-loop
   for vec across $list
   for i from 0
   collect
   (when (memq i involved)
     (let ((vecc
	    (if (plist-get fmt-settings :keep-empty)
		(cl-loop for x in vec
			 collect (if x x '(var nan var-nan)))
	      (cl-loop for x in vec
		       if x
		       collect x))))
       (if (plist-get fmt-settings :numbers)
	   (cl-loop for x on (cdr vecc)
		    unless (math-numberp (car x))
		    do (setcar x 0)))
       vecc))))

;; aggregation in Push mode

;;;###autoload
(defun orgtbl-to-aggregated-table (table params)
  "Convert the Org Mode TABLE to an aggregated version.

The resulting table contains aggregated material.
Grouping of rows is done for identical values of grouping columns.
For each group, aggregation (sum, mean, etc.) is done for other columns.

The source table must contain sending directives with the following format:
#+ORGTBL: SEND destination orgtbl-to-aggregated-table :cols ... :cond ...

The destination must be specified somewhere in the same file
with a block like this:
  #+BEGIN RECEIVE ORGTBL destination
  #+END RECEIVE ORGTBL destination

PARAMS are parameters given in the #+ORGTBL: SEND line.

:cols     gives the specifications of the resulting columns.
          It is a space-separated list of column specifications.
          Example:
             P Q sum(X) max(X) mean(Y)
          Which means:
             group rows with similar values in columns P and Q,
             and for each group, compute the sum of elements in
             column X, etc.

          The specification for a resulting column may be:
             COL              the name of a grouping column in the source table
             hline            a special name for grouping rows separated
                              by horizontal lines
             count()          give the number of rows in each group
             list(COL)        list the values of the column for each group
             sum(COL)         sum of the column for each group
             sum(COL1*COL2)   sum of the product of two columns for each group
             mean(COL)        average of the column for each group
             mean(COL1*COL2)  average of the product of two columns per group
             meane(COL)       average and estimated error
             hmean(COL)       harmonic average
             gmean(COL)       geometric average
             median(COL)      middle element after sorting them in each group
             max(COL)         largest element of each group
             min(COL)         smallest element of each group
             sdev(COL)        standard deviation (divide by N-1)
             psdev(COL)       population standard deviation (divide by N)
             pvar(COL)        variance of values in each group
             prod(COL)        product of values in each group
             cov(COL1,COL2)   covariance of two columns, per group (div. by N-1)
             pcov(COL1,COL2)  population covariance of two columns (div. by N)
             corr(COL1,COL2)  linear correlation of two columns

:cond     optional
          A lisp expression to filter out rows in the source table.
          When the expression evaluate to nil for a given row of the
          source table, then this row is discarded in the resulting table.
          Example:
             (equal Q \"b\")
          Which means: keep only source rows for which the column Q
          has the value b

Names of columns in the source table may be in the dollar form,
for example use $3 to name the 3th column,
or by its name if the source table have a header.
If all column names are in the dollar form,
the table is supposed not to have a header.
The special column name \"hline\" takes values from zero and up
and is incremented by one for each horizontal line.

Example:
add a line like this one before your table
,#+ORGTBL: SEND aggregatedtable orgtbl-to-aggregated-table \\
           :cols \"sum(X) q sum(Y) mean(Z) sum(X*X)\"
then add somewhere in the same file the following lines:
,#+BEGIN RECEIVE ORGTBL aggregatedtable
,#+END RECEIVE ORGTBL aggregatedtable
Type \\<org-mode-map> & \\[org-ctrl-c-ctrl-c] into your source table

Note:
 This is the \"push\" mode for aggregating a table.
 To use the \"pull\" mode, look at the org-dblock-write:aggregate function.

Note:
 The name `orgtbl-to-aggregated-table' follows the Org Mode standard
 with functions like `orgtbl-to-csv', `orgtbl-to-html'..."
  (interactive)
  (orgtbl-aggregate--elisp-table-to-string
   (orgtbl-aggregate--post-process
    (orgtbl-aggregate--create-table-aggregated table params)
    (plist-get params :post))))

;; aggregation in Pull mode

;;;###autoload
(defun org-dblock-write:aggregate (params)
  "Create a table which is the aggregation of material from another table.
Grouping of rows is done for identical values of grouping columns.
For each group, aggregation (sum, mean, etc.) is done for other columns.

PARAMS contains user parameters given on the #+BEGIN: aggregate line,
as follow:

:table    name of the source table

:cols     gives the specifications of the resulting columns.
          It is a space-separated list of column specifications.
          Example:
             \"P Q sum(X) max(X) mean(Y)\"
          Which means:
             group rows with similar values in columns P and Q,
             and for each group, compute the sum of elements in
             column X, etc.

          The specification for a resulting column may be:
             COL              the name of a grouping column in the source table
             hline            a special name for grouping rows separated
                              by horizontal lines
             count()          number of rows in each group
             list(COL)        list the values of the column for each group
             sum(COL)         sum of the column for each group
             sum(COL1*COL2)   sum of the product of two columns for each group
             mean(COL)        average of the column for each group
             mean(COL1*COL2)  average of the product of two columns per group
             meane(COL)       average along with the estimated error per group
             hmean(COL)       harmonic average per group
             gmean(COL)       geometric average per group
             median(COL)      middle element after sorting them, per group
             max(COL)         largest element of each group
             min(COL)         smallest element of each group
             sdev(COL)        standard deviation (divide by N-1)
             psdev(COL)       population standard deviation (divide by N)
             pvar(COL)        variance per group
             prod(COL)        product per group
             cov(COL1,COL2)   covariance of two columns per group (div. by N-1)
             pcov(COL1,COL2)  population covariance of two columns (div. by N)
             corr(COL1,COL2)  linear correlation of two columns, per group

:cond     optional
          A Lisp expression to filter out rows in the source table.
          When the expression evaluate to nil for a given row of
          the source table, then this row is discarded in the resulting table
          Example:
             (equal Q \"b\")
          Which means: keep only source rows for which the column Q
          has the value b.

Names of columns in the source table may be in the dollar form,
for example $3 to name the 3th column,
or by its name if the source table have a header.
If all column names are in the dollar form,
the table is supposed not to have a header.
The special column name \"hline\" takes values from zero and up
and is incremented by one for each horizontal line.

Example:
- Create an empty dynamic block like this:
  #+BEGIN: aggregate :table originaltable \\
           :cols \"sum(X) Q sum(Y) mean(Z) sum(X*X)\"
  #+END
- Type \\<org-mode-map> & \\[org-ctrl-c-ctrl-c] over the BEGIN line
  this fills in the block with an aggregated table

Note:
 This is the \"pull\" mode for aggregating a table.
 To use the \"push\" mode,
 look at the `orgtbl-to-aggregated-table' function.

Note:
 The name `org-dblock-write:aggregate' is constrained
 by the `org-update-dblock' function."
  (interactive)
  (let ((formula (plist-get params :formula))
	(content (plist-get params :content))
	(post    (plist-get params :post))
	(tblfm nil))
    (if (and content
	     (let ((case-fold-search t))
	       (string-match
		(rx bos
                    (+
                     (* (any " \t")) "#+" (* not-newline) "\n"))
		content)))
	(insert (match-string 0 content)))
    (orgtbl-aggregate--insert-elisp-table
     (orgtbl-aggregate--post-process
      (orgtbl-aggregate--create-table-aggregated
       (orgtbl-aggregate--remove-cookie-lines
        (orgtbl-aggregate--get-distant-table (plist-get params :table)))
       params)
      post))
    (if (and content
	     (let ((case-fold-search t))
	       (string-match
		(rx bol
                    (* (any " \t"))
                    (group "#+tblfm:" (* not-newline)))
		content)))
	(setq tblfm (match-string 1 content)))
    (when (stringp formula)
      (if tblfm
	  (unless (string-match (rx-to-string formula) tblfm)
	    (setq tblfm (format "%s::%s" tblfm formula)))
	(setq tblfm (format "#+TBLFM: %s" formula))))
    (when tblfm
      (end-of-line)
      (insert "\n" tblfm)
      (forward-line -1)
      (let ((org-table-formula-create-columns t))
	(condition-case nil
	    (org-table-recalculate 'iterate)
	  (args-out-of-range nil))))))

;; This variable contains history of user entered
;; :cols and :cond parameters, so that they can be entered
;; again or edited
(defvar orgtbl-aggregate-history-cols ())

;;;###autoload
(defun orgtbl-aggregate-insert-dblock-aggregate ()
  "Wizard to interactively insert an aggregate dynamic block."
  (interactive)
  (let* ((table
	  (completing-read
	   "Table name: "
	   (orgtbl-aggregate--list-local-tables)
	   nil
	   'confirm))
	 (header
	  (condition-case
              _err
              (orgtbl-aggregate--get-header-table table t)
	    (t "$1 $2 $3 $4 ...")))
	 (aggcols
	  (replace-regexp-in-string
	   "\"" "'"
	   (read-string
	    (format "target columns (source columns are: %s): " header)
	    nil 'orgtbl-aggregate-history-cols)))
	 (aggcond
	  (read-string
	   (format
	    "condition (optional lisp function operating on: %s): "
	    header)
	   nil 'orgtbl-aggregate-history-cols))
	 (params (list :name "aggregate" :table table :cols aggcols)))
    (unless (equal aggcond "")
      (nconc params (list :cond (read aggcond))))
    (org-create-dblock params)
    (org-update-dblock)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The Transposition package

(defun orgtbl-aggregate--create-table-transposed (table cols aggcond)
  "Convert the source TABLE to a tranposed version.
TABLE is a list of lists of cells.
COLS gives the source columns that should become rows.
If COLS is nil, all source columns are taken.
AGGCOND is a Lisp expression given bu the user.  It is evaluated
against each row.  If the result is nil, the row is ignored.
If AGGCOND is nil, all source rows are taken."
  (if (stringp cols)
      (setq cols (orgtbl-aggregate--split-string-with-quotes cols)))
  (setq cols
        (if cols
	    (cl-loop for column in cols
		     collect
		     (orgtbl-aggregate--colname-to-int column table t))
          (let ((head table))
	    (orgtbl-aggregate--pop-leading-hline head)
	    (cl-loop for _x in (car head)
		     for i from 1
		     collect i))))
  (if aggcond
      (setq aggcond
            (orgtbl-aggregate--replace-colnames-nth table aggcond)))
  (let ((result (cl-loop for _x in cols collect (list t)))
        (nhline 0))
    (cl-loop for row in table
	     do
	     (if (eq row 'hline)
		 (setq nhline (1+ nhline))
	       (setq row (cons nhline row)))
	     do
	     (when (or (eq row 'hline) (not aggcond) (eval aggcond))
	       (cl-loop
		for spec in cols
		for r in result
		do
		(nconc r (list (if (eq row 'hline) "" (nth spec row)))))))
    (cl-loop for row in result
	     do (orgtbl-aggregate--pop-simple row)
	     collect
	     (if (cl-loop for x in row
			  always (equal "" x))
		 'hline
	       row))))

;;;###autoload
(defun orgtbl-to-transposed-table (table params)
  "Convert the Org Mode TABLE to a transposed version.
Rows become columns, columns become rows.

The source table must contain sending directives with
the following format:
#+ORGTBL: SEND destination orgtbl-to-transposed-table :cols ... :cond ...

PARAMS are the user given parameters found in the #+ORGTBL: SEND line

The destination must be specified somewhere in the same file
with a bloc like this:
  #+BEGIN RECEIVE ORGTBL destination
  #+END RECEIVE ORGTBL destination

:cols     optional, if omitted all source columns are taken.
          Columns specified here will become rows in the result.
          Valid specifications are
          - names as they appear in the first row of the source table
          - $N forms, starting from $1
          - the special hline column which is the numbering of
            blocks separated by horizontal lines in the source table

:cond     optional
          a lisp expression to filter out rows in the source table
          when the expression evaluate to nil for a given row of
          the source table, then this row is discarded in the
          resulting table.
          Example:
             (equal Q \"b\")
          Which means: keep only source rows for which the column Q
          has the value b

Columns in the source table may be in the dollar form,
for example $3 to name the 3th column,
or by its name if the source table have a header.
If all column names are in the dollar form,
the table is supposed not to have a header.
The special column name \"hline\" takes values from zero and up
and is incremented by one for each horizontal line.

Horizontal lines are converted to empty columns,
and the other way around.

The destination must be specified somewhere in the same file
with a block like this:
  #+BEGIN RECEIVE ORGTBL destination_table_name
  #+END RECEIVE ORGTBL destination_table_name

Type \\<org-mode-map> & \\[org-ctrl-c-ctrl-c] in the source
table to re-create the transposed version.

Note:
 This is the \"push\" mode for transposing a table.
 To use the \"pull\" mode, look at the org-dblock-write:transpose function.

Note:
 The name `orgtbl-to-transposed-table' follows the Org Mode standard
 with functions like `orgtbl-to-csv', `orgtbl-to-html'..."
  (interactive)
  (orgtbl-aggregate--elisp-table-to-string
   (orgtbl-aggregate--post-process
    (orgtbl-aggregate--create-table-transposed
     table
     (plist-get params :cols)
     (plist-get params :cond))
    (plist-get params :post))))

;;;###autoload
(defun org-dblock-write:transpose (params)
  "Create a transposed version of an Org Mode table.
Rows become columns, columns become rows.

PARAMS are the user given parameters found in the
#+BEGIN: transpose line

:table    names the source table

:cols     optional, if omitted all source columns are taken.
          Columns specified here will become rows in the result.
          Valid specifications are
          - names as they appear in the first row of the source table
          - $N forms, starting from $1
          - the special hline column which is the numbering of
            blocks separated by horizontal lines in the source table.

:cond     optional
          a Lisp expression to filter out rows in the source table
          when the expression evaluate to nil for a given row of the
          source table, then this row is discarded in the resulting table.
          Example:
             (equal q \"b\")
          Which means: keep only source rows for which the column q
          has the value b.

Columns in the source table may be in the dollar form,
for example $3 to name the 3th column,
or by its name if the source table have a header.
If all column names are in the dollar form,
the table is supposed not to have a header.
The special column name \"hline\" takes values from zero and up
and is incremented by one for each horizontal line.

Horizontal lines are converted to empty columns,
and the other way around.

- Create an empty dynamic block like this:
  #+BEGIN: transpose :table originaltable
  #+END
- Type \\<org-mode-map> & \\[org-ctrl-c-ctrl-c] over the BEGIN line
  this fills in the block with the transposed table

Note:
 This is the \"pull\" mode for transposing a table.
 To use the \"push\" mode, look at the orgtbl-to-transposed-table function.

Note:
 The name `org-dblock-write:transpose' is constrained
 by the `org-update-dblock' function."
  (interactive)
  (let ((formula (plist-get params :formula))
	(content (plist-get params :content))
	(post    (plist-get params :post))
	(tblfm nil))
    (if (and content
	     (let ((case-fold-search t))
	       (string-match
		(rx bos
                    (* (any " \t"))
                    (group "#+" (? "tbl") "name:" (* not-newline)))
		content)))
	(insert (match-string 1 content) "\n"))
    (orgtbl-aggregate--insert-elisp-table
     (orgtbl-aggregate--post-process
      (orgtbl-aggregate--create-table-transposed
       (orgtbl-aggregate--remove-cookie-lines
        (orgtbl-aggregate--get-distant-table (plist-get params :table)))
       (plist-get params :cols)
       (plist-get params :cond))
      post))
    (if (and content
	     (let ((case-fold-search t))
	       (string-match
		(rx bol (* (any " \t")) (group "#+tblfm:" (* not-newline)))
		content)))
	(setq tblfm (match-string 1 content)))
    (when (stringp formula)
      (if tblfm
	  (unless (string-match (rx-to-string formula) tblfm)
	    (setq tblfm (format "%s::%s" tblfm formula)))
	(setq tblfm (format "#+TBLFM: %s" formula))))
    (when tblfm
      (end-of-line)
      (insert "\n" tblfm)
      (forward-line -1)
      (let ((org-table-formula-create-columns t))
	(condition-case nil
	    (org-table-recalculate 'iterate)
	  (args-out-of-range nil))))))

;;;###autoload
(defun orgtbl-aggregate-insert-dblock-transpose ()
  "Wizard to interactively insert a transpose dynamic block."
  (interactive)
  (let* ((table
	  (completing-read
	   "Table name: "
	   (orgtbl-aggregate--list-local-tables)
	   nil
	   'confirm))
	 (header
	  (condition-case _err (orgtbl-aggregate--get-header-table table t)
	    (t "$1 $2 $3 $4 ...")))
	 (aggcols
	  (replace-regexp-in-string
	   "\"" "'"
	   (read-string
	    (format
	     "target columns (empty for all) (source columns are: %s): "
	     header)
	    nil 'orgtbl-aggregate-history-cols)))
	 (aggcond
	  (read-string
	   (format
	    "condition (optional lisp function) (source columns: %s): "
	    header)
	   nil 'orgtbl-aggregate-history-cols))
	 (params (list :name "transpose" :table table)))
    (unless (equal aggcols "")
      (nconc params (list :cols aggcols)))
    (unless (equal aggcond "")
      (nconc params (list :cond (read aggcond))))
    (org-create-dblock params)
    (org-update-dblock)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; wizards

;; Insert a dynamic bloc with the C-c C-x x dispatcher
;;;###autoload
(eval-after-load 'org
  '(when (fboundp #'org-dynamic-block-define) ;; found in Emacs 27.1
     (org-dynamic-block-define "aggregate" #'orgtbl-aggregate-insert-dblock-aggregate)
     (org-dynamic-block-define "transpose" #'orgtbl-aggregate-insert-dblock-transpose)))

(provide 'orgtbl-aggregate)
;;; orgtbl-aggregate.el ends here
