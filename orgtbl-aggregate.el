;;; orgtbl-aggregate.el --- Aggregate an Org Mode table | + | + | into another table  -*- coding:utf-8; lexical-binding: t;-*-
;; Copyright (C) 2013-2026  Thierry Banel

;; Authors:
;;   Thierry Banel tbanelwebmin at free dot fr
;;   Michael Brand michael dot ch dot brand at gmail dot com
;; Contributors:
;;   Eric Abrahamsen, Alejandro Erickson Uwe Brauer, Peking Duck, Bill
;;   Hunker, Dirk Schmitt, Dale Sedivec, falloutphil, Baudilio
;;   Tejerina, Marco Pas, wuqui, Nicolas Viviani, Nils Lehmann,
;;   Shankar Rao, Misohena, Kevin Brubeck Unhammer, Tilmann Singer,
;;   Piotr Panasiuk, Luis Miguel Hernanz, Jason Hemann

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
(require 'org-id)
(require 'thingatpt) ;; just for thing-at-point--read-from-whole-string
(eval-when-compile (require 'cl-lib))
(require 'rx)
(require 'json)
(eval-when-compile
  (cl-proclaim '(optimize (speed 3) (safety 0))))

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

(eval-when-compile

  (defmacro orgtbl-aggregate--list-create ()
    "Create an appendable list."
    `(let ((x (cons nil nil)))
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
  )

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

;; There is no CSV parser bundled with Emacs. In order to avoid a
;; dependency on a package, here is an implementation of a parser.  It
;; is made of the same technology as `orgtbl-aggregate--table-to-lisp'
;; (which is now integrated into the newest versions of Emacs). It is
;; probably as fast as can be in Emacs-Lisp byte-code.

(defun orgtbl-aggregate--csv-to-lisp (header colnames)
  "Convert current buffer in CSV to Lisp.
It recognize cells protected by double quotes, and cells not protected.
When a cell is not protected, blanks are kept.
When a cell is protected, blanks before the first double quote are ignored.
Double double quotes are recognized within a cell double-quoted.
The last line may or may not end in a newline.
Separators are comma, semicolon, or TAB. They can be mixed.
If a row is empty, it is considered as a separator, and translated
to `hline', the Org table horizontal separator.
HEADER non nil means that the first row must be interpreted as a header.
COLNAMES, if not nil, is a list of column names."
  (goto-char (point-min))
  (let (table)
    (while (not (eobp))
      (let (row)
        (while (not (eolp))
          (let ((p (point)))
            (skip-chars-forward " ")
            (if (eq (following-char) ?\")
                (let (dquote)
                  (forward-char 1)
                  (setq p (point))
                  (while
                      (progn
                        (skip-chars-forward "^\"")
                        (forward-char 1)
                        (if (eq (following-char) ?\")
                            (progn (forward-char 1)
                                   (setq dquote t)))))
                  (push
                   (let ((cell
                          (buffer-substring-no-properties p (1- (point)))))
                     (if dquote
                         (string-replace "\"\"" "\"" cell)
                       cell))
                   row)
                  (skip-chars-forward " "))
              (skip-chars-forward "^,;\t\n")
              (push
               (buffer-substring-no-properties p (point))
               row))
            (skip-chars-forward ",;\t" (1+ (point)))))
        (push
         (if row (nreverse row) 'hline)
         table)
        (or (eobp) (forward-char 1))))
    (setq table (nreverse table))
    (if header
        (setcdr table (cons 'hline (cdr table))))
    (if colnames
        (setq table (cons colnames (cons 'hline table))))
    table))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A few rx abbreviations
;; each time a bit of a regexp is used twice or more,
;; it makes sense to define an abbrev

(eval-when-compile ;; not used at runtime

  ;; search for table name, such as:
  ;; #+tablename: mytable
  (rx-define tblname
    (seq bol (* blank) "#+" (? "tbl") "name:" (* blank)))

  ;; skip lines beginning with # in order to reach the start of table
  (rx-define skipmetatable (firstchars)
    (seq point
         (0+ (0+ blank) (? firstchars (0+ any)) "\n")
         (0+ blank) "|"))

  ;; just to get ride of a few parenthesis
  (rx-define notany (&rest list)
    (not (any list)))

  ;; match quoted column names, like
  ;; 'col a' "col b" colc
  (rx-define quotedcolname (&rest bare)
    (or
     (seq ?'  (* (notany ?' )) ?' )
     (seq ?\" (* (notany ?\")) ?\")
     bare))

  ;; match a column name not protected by quotes
  (rx-define nakedname
    (+ (any "$._#@" word)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Here is a bunch of useful utilities,
;; generic enough to be detached from the orgtbl-aggregate package.
;; For the time being, they are here.

(defun orgtbl-aggregate--list-local-tables (file)
  "Search for available tables in FILE.
If FILE is nil, use current buffer."
  (interactive)
  (with-current-buffer
      (if file (find-file-noselect file) (current-buffer))
    (save-excursion
      (goto-char (point-min))
      (let ((case-fold-search t))
        (cl-loop
         while
         (re-search-forward
          (rx tblname (group (*? any)) (* blank) eol)
          nil t)
         collect (match-string-no-properties 1))))))

(defun orgtbl-aggregate--table-from-babel (name-or-id)
  "Retrieve an input table as the result of running a Babel block.
NAME-OR-ID is the usual Org convention for pointing to a distant reference.
Examples: babel, file:babel, file:babel[1:3,2:5], file:babel(p1=…,p2=…)
This function could work also for a table,
but this has already been short-circuited."
  ;; A user error is generated in case no Babel block is found
  (let ((table (org-babel-ref-resolve name-or-id)))
    (and
     table
     (consp table)
     (or (eq (car table) 'hline)
         (consp (car table)))
     table)))

(defun orgtbl-aggregate--table-from-csv (file params)
  "Parse a CSV formatted table located in FILE.
The cell-separator is currently guessed.
Currently, there is no header."
  (let ((header) (colnames))
    (cl-loop
     for p on (cdr (read params))
     do
     (cond
      ((eq (car p) 'header)
       (setq header t))
      ((eq (car p) 'colnames)
       (setq p (cdr p))
       (setq colnames (car p)))
      (t
       (message "parameter %S not recognized" (car p)))))
    (with-temp-buffer
      (insert-file-contents file)
      (orgtbl-aggregate--csv-to-lisp header colnames))))

(defun orgtbl-aggregate--table-from-json (file _params)
  "Parse a JSON formatted table located in FILE.
FILE is a filename with possible relative or absolute path.
Currently, the accepted format is
[[\"COL1\",\"COL2\",…]
 \"hline\"
 [\"VAL11\",\"COL12\",…]
 [\"VAL21\",\"COL22\",…]
 [\"VAL31\",\"COL32\",…]
Numbers do not need to be quoted.
 …"
  (let ((json-object-type 'alist)
        (json-array-type 'list)
        (json-key-type 'string))
    (let ((json (json-read-file file)))
      (cl-loop
       for row in json
       if (stringp row)
       collect (intern row)
       else
       collect (append row ())))))

(defun orgtbl-aggregate--table-from-name (file name)
  "Parse an Org table named NAME in a ditant Org file named FILE.
FILE is a filename with possible relative or absolute path.
If FILE is nil, look in the current buffer."
  (with-current-buffer
      (if file
          (find-file-noselect file)
        (current-buffer))
    (save-excursion
      (goto-char (point-min))
      (when (let ((case-fold-search t))
	      (re-search-forward
	       (rx tblname (literal name) (* blank) eol)
	       nil t))
        (re-search-forward (rx (skipmetatable "#")) nil t)
        (orgtbl-aggregate--table-to-lisp)))))

(defun orgtbl-aggregate--table-from-id (id)
  "Parse a table following a header in a distant Org file.
The header have an ID property equal to ID in a PROPERTY drawer."
  (let ((id-loc (org-id-find id 'marker)))
    (when (and id-loc (markerp id-loc))
      (with-current-buffer (marker-buffer id-loc)
        (save-excursion
          (goto-char (marker-position id-loc))
          (move-marker id-loc nil)
          (and
           (re-search-forward (rx (skipmetatable (any "*#:"))) nil t)
           (orgtbl-aggregate--table-to-lisp)))))))

(defun orgtbl-aggregate--nil-if-empty (field)
  (and
   field
   (not (string-match-p (rx bos (* blank) eos) field))
   field))

(defun orgtbl-aggregate--parse-locator (locator)
  "Parse LOCATOR, a description of where to find the input table.
The result is a vector containing:
[
  FILE   ; optional file where the table/Babel/CSV/JSON may be found
  NAME   ; name of table/Babel denoted by #+name:
  ORGID  ; Org Mode id in a property drawer (exclusive with file+name)
  PARAMS ; optional parameters to pass to babel/CSV/JSON
  SLICE  ; optional slicing of the resultin table, like [0:7]
]
If LOCATOR looks like NAME(params…)[slice] or just NAME, then NAME
is searched in the Org Mode database, and if found it is interpreted
as an Org Id and put in the `orgid' field."
  (unless locator (setq locator ""))
  (unless
      (string-match
       (rx
        bos
        (* space)
        (? (group-n 1 (* (notany ":"))) ":")
        (* space)
        (   group-n 2 (* (notany "[]():")))
        (* space)
        (? (group-n 3 "(" (* any) ")"))
        (* space)
        (? (group-n 4 "[" (* any) "]"))
        (* space)
        eos)
       locator)
    (user-error "Malformed table reference %S" locator))
  (let ((file   (orgtbl-aggregate--nil-if-empty (match-string 1 locator)))
        (name   (orgtbl-aggregate--nil-if-empty (match-string 2 locator)))
        (orgid                                                           )
        (params (orgtbl-aggregate--nil-if-empty (match-string 3 locator)))
        (slice  (orgtbl-aggregate--nil-if-empty (match-string 4 locator))))
    (when (and
           (not file)
           (progn
             (unless org-id-locations (org-id-locations-load))
             (and org-id-locations
	          (hash-table-p org-id-locations)
	          (gethash name org-id-locations))))
      (setq orgid name)
      (setq name nil))
    (vector file name orgid params slice)))

(defun orgtbl-aggregate--assemble-locator (file name orgid params slice)
  "Assemble fields of a locator as a string.
FILE NAME ORGID PARAMS SLICE are the 5 fields composing a locator.
Many of them are optional.
The result is a locator suitable for orgtbl-aggregate and Org Mode."
  (unless params (setq params ""))
  (unless slice  (setq slice  ""))
  (setq file  (orgtbl-aggregate--nil-if-empty file ))
  (setq orgid (orgtbl-aggregate--nil-if-empty orgid))
  (cond
   (orgid (format "%s%s%s"         orgid        params slice))
   (file  (format "%s:%s%s%s" file (or name "") params slice))
   (t     (format "%s%s%s"         name         params slice))))

(defun orgtbl-aggregate-table-from-any-ref (name-or-id)
  "Find a table referenced by NAME-OR-ID.
The reference is all the accepted Org references,
and additionally pointers to CSV or JSON files.
The pointed to object may also be a Babel block, which when executed
returns an Org table. Parameters may be passed to the Babel block
in parenthesis.
A slicing may be applied to the table, to select rows or columns.
The syntax for slicing is like [1:3] or [1:3,2:5].
Return it as a Lisp list of lists.
An horizontal line is translated as the special symbol `hline'."
  (unless (stringp name-or-id)
    (setq name-or-id (format "%s" name-or-id)))
  (let*
      ((struct (orgtbl-aggregate--parse-locator name-or-id))
       (file   (aref struct 0))
       (name   (aref struct 1))
       (orgid  (aref struct 2))
       (params (aref struct 3))
       (slice  (aref struct 4))
       (table
        (cond
         ;; name-or-id = "file:(csv …)"
         ((and file (not name)
               (string-match (rx bos "(csv") params))
          (orgtbl-aggregate--table-from-csv file params))
         ;; name-or-id = "file:(json …)"
         ((and file (not name)
               (string-match (rx bos "(json") params))
          (orgtbl-aggregate--table-from-json file params))
         ;; name-or-id = "34cbc63a-c664-471e-a620-d654b26ffa31"
         ;; pointing to a header in a distant org file, followed by a table
         (orgid
          (orgtbl-aggregate--table-from-id orgid))
         ;; name-or-id = "babel(p=…)" or "file:babel(p=…)"
         ((and params
               (orgtbl-aggregate--table-from-babel
                (if file
                    (format "%s:%s%s" file name params)
                  (format "%s%s" name params)))))
         ;;name-or-id = "table" or "file:table"
         ((orgtbl-aggregate--table-from-name file name))
         ;; name-or-id = "babel" or "file:babel"
         ((orgtbl-aggregate--table-from-babel
           (if file
               (format "%s:%s" file name)
             name)))
         ;; everything failed
         (t
          (user-error
           "Cannot find table or babel block with reference %S"
           name-or-id)))))
      (if slice
          (org-babel-ref-index-list slice table)
        table)))

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
  (cl-loop
   with hline = nil
   for line on table
   if (and hline
           (cl-loop
            for cell in (car line)
            thereis
            (and (stringp cell)
                 (string-match
                  (rx bos "<" (? (any "lcr")) (* digit) ">" eos)
                  cell))))
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
                    (* blank)
                    (group (+ (quotedcolname (notany " '\"")))))
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
	bos
	(or
	 (seq ?'  (group-n 1 (* (notany ?' ))) ?' )
	 (seq ?\" (group-n 1 (* (notany ?\"))) ?\"))
	eos)
       colname)
      (setq colname (match-string 1 colname)))
  ;; skip first hlines if any
  (orgtbl-aggregate--pop-leading-hline table)
  (cond ((string= colname "")
	 (and err (user-error "Empty column name")))
	((string= colname "@#")
	 0)
	((string= colname "hline")
	 (1+ (length (car table))))
	((string-match (rx bos "$" (group (+ digit)) eos) colname)
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
	   thereis (and (string= h colname) i))))
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
		      unless (string= cellnp "")
		      do (setcar ne (1+ (car ne)))
		      if (< (car mx) (string-width cellnp))
		      do (setcar mx (string-width cellnp))))

    ;; change meaning of numbers from quantity of cells with numbers
    ;; to flags saying whether alignment should be left (number alignment)
    (cl-loop for nu on numbers
	     for ne in non-empty
	     do
	     (setcar nu (< (car nu) (* org-table-number-fraction ne))))

    ;; create well padded and aligned cells
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
      (mapconcat #'identity (orgtbl-aggregate--list-get bits)))))

(defun orgtbl-aggregate--recalculate-fast ()
  "Wrapper arround `org-table-recalculate'.
The standard `org-table-recalculate' function is slow because
it must handle lots of cases. Here the table is freshely created,
therefore a lot of special handling and cache updates can be
safely bypassed. Moreover, the alignment of the resulting table
is delegated to orgtbl-aggregate, which is fast.
The result is a speedup up to x6, and a memory consumption
divided by up to 5. It makes a difference for large tables."
  (let ((old (symbol-function 'org-table-goto-column)))
    (cl-letf (((symbol-function 'org-fold-core--fix-folded-region)
               (lambda (_a _b _c)))
              ((symbol-function 'jit-lock-after-change)
               (lambda (_a _b _c)))
              ;; Warning: this org-table-goto-column trick fixes a bug
              ;; in org-table.el around line 3084, when computing
              ;; column-count. The bug prevents single-cell formulas
              ;; creating the cell in some rare cases.
              ((symbol-function 'org-table-goto-column)
               (lambda (n &optional on-delim _force)
                 ;;                            △
                 ;;╭───────────────────────────╯
                 ;;╰╴parameter is forcibly changed to t╶─╮
                 ;;                      ╭───────────────╯
                 ;;                      ▽
                 (funcall old n on-delim t))))
      (condition-case nil
          (org-table-recalculate t t)
        ;;                       △ △
        ;; for all lines╶────────╯ │
        ;; do not re-align╶────────╯
        (args-out-of-range nil)))))

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
    (setq table
          (condition-case _err
              (orgtbl-aggregate-table-from-any-ref table)
            (error
             '(("$1" "$2" "$3" "…") hline)))))
  (orgtbl-aggregate--pop-leading-hline table)
  (let ((header
	 (if (memq 'hline table)
	     (cl-loop for x in (car table)
                      do (setq x (orgtbl-aggregate--cell-to-string x))
		      collect
		      (if (string-match
                           (rx bos nakedname eos)
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
          `(nth ,n orgtbl-aggregate--row)
	expression)))))

(defun orgtbl-aggregate--to-frux (formula table involved)
  "Parse FORMULA replacing column names with Frux(NN).
NN is the column position as it appears in TABLE.
Take into account protection of non-alphanumeric names
by single or double quotes.
Also replace sum, mean, etc. with vsum, vmean, etc.
the v names being understandable by Calc.
INVOLVED is a list to which column numbers of columns
referenced by formula are added."
  (replace-regexp-in-string
   (rx (quotedcolname nakedname) (? (* space) "("))
   (lambda (var)
     (save-match-data ;; save because we are called within a replace-regexp
       (if (string-match
            (rx (group (+ (notany "("))) (* space) "(")
            var)
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
		 (unless
                     (memq i (orgtbl-aggregate--list-get involved))
		   (orgtbl-aggregate--list-append involved i))
		 (format "Frux(%s)" i))
	     var)))))
   formula
   t ;; if nil, Frux is sometimes converted to FRUX
   ))

(defun orgtbl-aggregate--frux-to-$ (frux)
  "Replace all occurences of Frux(NN) by $NN in FRUX"
  (replace-regexp-in-string
   (rx "Frux(" (group (+ digit)) ")")
   (lambda (var)
     (format "$%s" (match-string 1 var))
     )
   frux))

;; dynamic binding
(defvar orgtbl-aggregate--var-keycols)

(cl-defstruct
    (orgtbl-aggregate--outcol
     ;; (:predicate nil) ; worse with this directive
     (:copier nil))
  ;; (formula	nil :readonly t) ;; :readonly has no effect
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
            bos
            (group-n 1 (+ (quotedcolname (notany " ;'\""))))
            (*
             ";"
             (or
              (seq     (group-n 2 (* (notany "^;'\"<"))))
              (seq "^" (group-n 3 (* (notany "^;'\"<"))))
              (seq "<" (group-n 4 (* (notany "^;'\">"))) ">")
              (seq "'" (group-n 5 (* (notany "'"))) "'")))
            eos)
	   col)
    (user-error "Bad column specification: %S" col))
  (let* ((formula   (match-string 1 col))
	 (format    (match-string 2 col))
	 (sort      (match-string 3 col))
	 (invisible (match-string 4 col))
	 (name      (match-string 5 col))

	 ;; list the input column numbers which are involved
	 ;; into formula
	 (involved (orgtbl-aggregate--list-create))

	 ;; create a derived formula in Calc format,
	 ;; where names of input columns are replaced with
	 ;; frux(N)
	 (frux (orgtbl-aggregate--to-frux formula table involved))

	 ;; create a derived formula where input column names
	 ;; are replaced with $N
	 (formula$ (orgtbl-aggregate--frux-to-$ frux))

	 ;; if a formula is just an input column name,
	 ;; then it is a key-grouping-column
	 (key
	  (if (string-match
               (rx bos (group (quotedcolname nakedname)) eos)
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
     :involved     (orgtbl-aggregate--list-get involved)
     :key          key)))

;; dynamic binding
(defvar orgtbl-aggregate--columns-sorting)

(cl-defstruct
    (orgtbl-aggregate--sorting
     ;; (:predicate nil) ; worse with this directive
     (:copier nil))
  ;; (strength  nil :readonly t) ;; :readonly has no effect
  strength        ; the 3 in a user specification like ;^a3
  colnum          ; the number of the output column to sort
  ascending       ; ;^n is ascending, ;^N is descending
  extract         ; extract Lisp function, eg string-to-number for ;^n
  compare         ; comparison Lisp function for 2 cells, eg string< for ;^a
  )

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
	   do
	   (unless (string-match
                    (rx bol
                        (group (any "aAnNtTfF"))
                        (group (* digit))
                        eol)
                    sorting)
	     (user-error
              "Bad sorting specification: ^%s, expecting a/A/n/N/t/T and an optional number"
              sorting))
	   (orgtbl-aggregate--list-append
	    orgtbl-aggregate--columns-sorting
	    (let ((strength
		   (if (string= (match-string 2 sorting) "")
		       nil
		     (string-to-number (match-string 2 sorting)))))
	      (pcase (match-string 1 sorting)
		("a" (record 'orgtbl-aggregate--sorting strength colnum nil #'orgtbl-aggregate--cell-to-string #'string-lessp))
		("A" (record 'orgtbl-aggregate--sorting strength colnum t   #'orgtbl-aggregate--cell-to-string #'string-lessp))
		("n" (record 'orgtbl-aggregate--sorting strength colnum nil #'orgtbl-aggregate--cell-to-number #'<))
		("N" (record 'orgtbl-aggregate--sorting strength colnum t   #'orgtbl-aggregate--cell-to-number #'<))
		("t" (record 'orgtbl-aggregate--sorting strength colnum nil #'orgtbl-aggregate--cell-to-time   #'<))
		("T" (record 'orgtbl-aggregate--sorting strength colnum t   #'orgtbl-aggregate--cell-to-time   #'<))
		((or "f" "F") (user-error "f/F sorting specification not (yet) implemented"))
		(_ (user-error "Bad sorting specification ^%s" sorting))))))

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
   ;; already an integer? return it
   ((integerp expr) expr)
   ;; a floating point? must convert it to Calc
   ((numberp expr) (math-read-number (number-to-string expr)))
   ;; empty cell returned as nil,
   ;; to be processed later depending on modifier flags
   ((string= expr "") nil)
   ;; the purely numerical cell case arises very often
   ;; short-circuiting general functions boosts performance (a lot)
   ((and
     (string-match
      (rx bos
	  (? (any "+-")) (* digit)
	  (? "." (* digit))
	  (? "e" (? (any "+-")) (+ digit))
	  eos)
      expr)
     (not (string-match (rx bos (* (any "+-.")) "e") expr)))
    (math-read-number expr))
   ;; Convert an Org-mode date to Calc internal representation
   ((string-match org-ts-regexp0 expr)
    (math-parse-date
     (replace-regexp-in-string (rx (any "[<>].a-zA-Z")) " " expr)))
   ;; Convert a duration into a number of seconds
   ((string-match
     (rx bos
	 (group (+ digit))
	 ":"
	 (group digit digit)
	 (? ":" (group digit digit))
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
	   always (equal (nth idx row1) (nth idx row2))))

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

(defun orgtbl-aggregate--parse-preprocess (formulas width)
  "Parse the :precompute parameter's value, FORMULAS.
WIDTH is the number of columns of the input table, without
the precomputed columns.
Return a list:
((formula1 . name1) (formula2 . name2) …)"
    ;; formulas is a list of strings
    ;; change it to ((formula1 . name1) (formula2 . name2) …)
    ;; name1 etc. default to a dollar name like "$8"
    (if (stringp formulas)
        (setq formulas
              (split-string
               formulas
               (rx (* space) "::" (* space))
               t)))
    (cl-loop
      for formula in formulas
      for i from (1+ width)
      for dollari = (format "$%s" i)
      collect
      (if (string-match
           (rx bos
               (group-n 1 (+ (notany ";"))) ; formula to compute column
               (*
                ";" (* space) ; maybe something after a semicolon
                (or
                 (seq     (group-n 2 (+ (notany "^;'\"<")))) ; a formatter
                 (seq "'" (group-n 3 (* (notany "'"))) "'") ; column name
                 (seq "")))             ; nothing after semicolon
               (* space)
               eos)
           formula)
          (cons
           (if (match-string 2 formula) ; case formula;formatter
               (format "%s;%s" (match-string 1 formula) (match-string 2 formula))
             (match-string 1 formula))  ; case formula without formatter
           (or (match-string 3 formula) dollari))    ; new column's name
        (cons formula dollari))))

(defun orgtbl-aggregate--enrich-table (table formulas)
  "Enrich TABLE with new columns computed by FORMULAS.
The FORMULAS are supposed to be those used in spreadsheets,
as given after the #+TBLFM: tag.
Actually, FORMULAS are evaluated by Org, not by orgtbl-aggregate."
  (let ((start (point))
        (width (length (car table))))
    (setq
     formulas
     (orgtbl-aggregate--parse-preprocess formulas width))
    (if (memq 'hline table)
        ;; table has a header? add it the names of the new columns
        (cl-loop
         for formula in formulas
         do (nconc (car table) (list (cdr formula))))
      ;; table does not have a header? add one of the form:
      ;; ("$1" "$2" … "$7" "name1" "name2" …)
      (setq
       table
       (cons
        (append
         (cl-loop for i from 1 to width collect (format "$%s" i))
         (cl-loop for formula in formulas collect (cdr formula)))
        (cons 'hline table))))

    (insert "\n#+TBLFM: ")
    (let ((involved (orgtbl-aggregate--list-create)))
      (cl-loop
       for formula in formulas
       for i from (1+ width)
       do
       (insert
        (format
         "::$%s=%s"
         i
         (orgtbl-aggregate--frux-to-$
          (orgtbl-aggregate--to-frux (car formula) table involved))))))
    (forward-line -1)
    (orgtbl-aggregate--insert-elisp-table table)

    ;; ask Org to evaluate the formulas and fill the new columns
    (orgtbl-aggregate--recalculate-fast)
    (prog1
        ;; recover the enriched table a Lisp structure
        (orgtbl-aggregate--table-to-lisp)
      ;; leave the buffer space between #+begin: and #+end:
      ;; as empty as it was prior to entering this function
      (delete-region
       start
       (let ((case-fold-search t))
         (search-forward-regexp (rx bol "#+end:" eol))
         (beginning-of-line)
         (point)))
      (insert "\n")
      (goto-char start))))

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
	(aggcols    (plist-get params :cols))
	(aggcond    (plist-get params :cond))
	(hline      (plist-get params :hline))
        (precompute (plist-get params :precompute))
	;; a global variable, passed to the sort predicate
	(orgtbl-aggregate--columns-sorting (orgtbl-aggregate--list-create))
	;; another global variable
	(orgtbl-aggregate--var-keycols))

    (if precompute
        (setq table (orgtbl-aggregate--enrich-table table precompute)))

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
		((string-match-p (rx bos (or "yes" "t") eos) hline)
		 1)
		((string-match-p (rx bos (or "no" "nil") eos) hline)
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
    (cl-loop
     with nbcols
     = (cl-loop
        for row in table
        maximize (if (listp row) (length row) 0))
     with hline = 0
     for rownum from 1
     for row in
     (or (cdr (memq 'hline table)) ;; skip header if any
	 table)
     do
     (cond
      ((eq row 'hline)
       (setq hline (1+ hline)))
      ((listp row)
       ;; fix too short rows
       (if (< (length row) nbcols)
           (setq row (nconc row (make-list (- nbcols (length row)) ""))))
       (orgtbl-aggregate--table-add-group
	groups
	hgroups
        ;; add rownum at beginning and hline at end
        (cons rownum (nconc row (list hline)))
	aggcond))))

    (let ((result ;; pre-allocate all resulting rows
	   (cl-loop for _x in (orgtbl-aggregate--list-get groups)
		    collect (orgtbl-aggregate--list-create)))
	  (all-$list
	   (cl-loop for _x in (orgtbl-aggregate--list-get groups)
		    collect
                    (make-vector
                     ;; + 2 for rownum at 0 & hline at the end
                     (+ 2 (length (car table)))
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
                         (if (eq (length (orgtbl-aggregate--outcol-involved column)) 1)
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

(defun orgtbl-aggregate--sort-predicate (rowa rowb)
  "Compares ROWA & ROWB (which are Org Mode table rows)
according to orgtbl-aggregate--columns-sorting instructions.
Return nil if ROWA already comes before ROWB."
  (setq rowa (orgtbl-aggregate--list-get rowa))
  (setq rowb (orgtbl-aggregate--list-get rowb))
  (cl-loop for col in orgtbl-aggregate--columns-sorting
	   for colnum  = (orgtbl-aggregate--sorting-colnum    col)
	   for desc    = (orgtbl-aggregate--sorting-ascending col)
	   for extract = (orgtbl-aggregate--sorting-extract   col)
	   for compare = (orgtbl-aggregate--sorting-compare   col)
	   for cella   = (funcall extract (nth colnum (if desc rowb rowa)))
	   for cellb   = (funcall extract (nth colnum (if desc rowa rowb)))
	   thereis (funcall compare cella cellb)
	   until   (funcall compare cellb cella)))

(defun orgtbl-aggregate--cell-to-time (cell)
  "Interprete the string CELL into a duration in minutes.
The code was borrowed from org-table.el."
  (cond
   ((numberp cell) cell)
   ((not (stringp cell))
    (error "cell %S is neither a string nor a number to be converted to time"
           cell))
   ((string-match org-ts-regexp-both cell)
    (float-time
     (org-time-string-to-time (match-string 0 cell))))
   ((org-duration-p cell) (org-duration-to-minutes cell))
   ((string-match
     (rx bow (+ digit) ":" (= 2 digit) eow)
     cell)
    (org-duration-to-minutes (match-string 0 cell)))
   (t 0)))

(defun orgtbl-aggregate--cell-to-number (cell)
  "Convert CELL (a cell in the input table) to a number if it is not already."
  (cond
   ((numberp cell) cell)
   ((stringp cell) (string-to-number cell))
   (t (error "cell %S is not a number neither a string" cell))))

(defun orgtbl-aggregate--cell-to-string (cell)
  "Convert CELL (a cell in the input table) to a string if it is not already."
  (cond
   ((not cell) cell)
   ((stringp cell) cell)
   ((numberp cell) (number-to-string cell))
   ((symbolp cell) (symbol-name cell))
   (t (error "cell %S is not a number neither a string" cell))))

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
              (rx (group (any "pnfse")) (group (? "-") (+ digit)))
              fmt)
	(let ((c (string-to-char   (match-string 1 fmt)))
	      (n (string-to-number (match-string 2 fmt))))
          (cl-case c
            (?p (setq calc-internal-prec n))
	    (?n (setq calc-float-format `(float ,n)))
	    (?f (setq calc-float-format `(fix   ,n)))
	    (?s (setq calc-float-format `(sci   ,n)))
	    (?e (setq calc-float-format `(eng   ,n)))))
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

(eval-when-compile
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
  )

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
	 (* blank) "(" (* blank)
	 "$" (group (+ digit))
	 (* blank) ")" (* blank) eos)
     (orgtbl-aggregate--outcol-formula$ coldesc))
    (mapconcat
     #'identity ;; there is fast path when `identity' is requested
     (cl-loop with i =
	      (string-to-number
               (match-string 1 (orgtbl-aggregate--outcol-formula$ coldesc)))
	      for row in (orgtbl-aggregate--list-get group)
	      collect (orgtbl-aggregate--cell-to-string (nth i row)))
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
   ((eq (car formula-frux) 'calcFunc-Frux)
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

(defun orgtbl-aggregate--table-recalculate (content formula)
  "Update the #+TBLFM: line and recompute all formulas.
The computed table may have formulas which need to be recomputed.
This function adds a #+TBLFM: line at the end of the table.
It merges old formulas (if any) contained in CONTENT,
with new formulas (if any) given in the `formula' directive."
  (let ((tblfm
         ;; Was there already a #+tblfm: line ? Recover it.
         (and content
	      (let ((case-fold-search t))
	        (string-match
	         (rx bol (* blank) (group "#+tblfm:" (* any)))
	         content))
              (match-string 1 content))))
    (if (stringp formula)
        ;; There is a :formula directive. Add it if not already there
        (if tblfm
	    (unless (string-match (regexp-quote formula) tblfm)
	      (setq tblfm (format "%s::%s" tblfm formula)))
	  (setq tblfm (format "#+TBLFM: %s" formula))))

    (when tblfm
      ;; There are formulas. They need to be evaluated.
      (end-of-line)
      (insert "\n" tblfm)
      (forward-line -1)
      (orgtbl-aggregate--recalculate-fast)

      ;; Realign table after org-table-recalculate have changed or added
      ;; some cells. It is way faster to re-read and re-write the table
      ;; through orgtbl-aggregate routines than letting org-mode do the job.
      (let* ((table (orgtbl-aggregate--table-to-lisp))
             (width
              (cl-loop for row in table
                       if (consp row)
                       maximize (length row))))
        (cl-loop
         for row in table
         if (and (consp row) (< (length row) width))
         do (nconc row (make-list (- width (length row)) nil)))
        (delete-region (org-table-begin) (org-table-end))
        (insert (orgtbl-aggregate--elisp-table-to-string table) "\n")))))

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
	(post    (plist-get params :post)))
    (if (and content
	     (let ((case-fold-search t))
	       (string-match
		(rx bos (+ (* blank) "#+" (* any) "\n"))
		content)))
	(insert (match-string 0 content)))
    (orgtbl-aggregate--insert-elisp-table
     (orgtbl-aggregate--post-process
      (orgtbl-aggregate--create-table-aggregated
       (orgtbl-aggregate--remove-cookie-lines
        (orgtbl-aggregate-table-from-any-ref (plist-get params :table)))
       params)
      post))
    (orgtbl-aggregate--table-recalculate content formula)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Wizard

(defun orgtbl-aggregate--alist-get-remove (key alist)
  "A variant of alist-get which removes an entry once read.
ALIST is a list of pairs (key . value).
Search ALIST for a KEY. If found, replace the key in (key . value)
by nil, and return value. If nothing is found, return nil."
  (let ((x (assq key alist)))
    (when x
      (setcar x nil)
      (cdr x))))

;; This variable contains history of user entered
;; :cols and :cond parameters, so that they can be entered
;; again or edited
(defvar orgtbl-aggregate-history-cols ())

(defun orgtbl-aggregate--parse-header-arguments (type)
  "If (point) is on a #+begin: line, parse it, and return an a-list.
TYPE is \"aggregate\" or \"transpose\", or possibly any type of block.
If the line the (point) is on do not match TYPE, return nil."
  (let ((line (buffer-substring-no-properties
               (line-beginning-position)
               (line-end-position)))
        (case-fold-search t))
    (and
     (string-match
      (rx bos (* blank) "#+begin:" (* blank) (group (+ word)))
      line)
     (equal (match-string 1 line) type)
     (cdr (org-babel-parse-header-arguments line t)))))

(defun orgtbl-aggregate--display-help (explain &rest args)
  "Display help for each field the wizard queries.
EXPLAIN is a text in Org Mode to display. It is process
through `format' with replacements in ARGS."
  (let ((docs
         '(
           :isorgid "* Input table locator
The input table may be pointed to by:
- a file and a name,
- an Org Mode identifier"
           :orgid "* Org ID
It is an identifier hidden in a =properties= drawer.
Org Mode globally keeps track of all Ids and knows how to access them.
It is supposed that the ID location is followed by a table or
a Babel block suitable for aggregation."
           :file "* In which file is the table?
The table may be in another file.
Leave answer empty to mean that the table is in the current buffer."
           :name "* The input table may be:
- a regular Org table,
- a Babel block whose output will be the input table.
Org table & Babel block names are available at completion (type ~TAB~).
Leave empty for a CSV or JSON formatted table."
           :params "* Parameters for Babel code block (optional)
** A Babel code block may require specific parameters
Give them here if needed, surrounded by parenthesis. Example:
  ~(size=4,reverse=nil)~
** CSV or JSON formatted tables.
Examples:
  ~(csv header)~ ~(json)~
** Regular Org Mode table
Leave empty."
           :slice "* Slicing (optional)
Slicing is an Org Mode feature allowing to cut the input table.
It applies to any input: Org table, Babel output, CSV, JSON.
Leave empty for no slicing.
** Examples:
- ~mytable[0:5]~     retains only the first 6 rows of the input table
- ~mytable[*,0:1]~   retains only the first 2 columns
- ~mytable[0:5,0:1]~ retains 5 rows and 2 columns"
           :precompute "* Precompute (optional)
The input table may be enriched with additional columns prior to aggregating.
The syntax is the regular Org table spreadsheet formulas for columns,
including formatting.
Additionnaly, the name of new columns can be specified after a semicolumn.
** Available columns
  %s
** Example
  ~quty*10;f1;'q10'~
means:
- add a new column to the input table named ~q10~
- compute it as ~10~ times the ~quty~ input column
- format it with ~f1~, 1 digit after dot"
           :cols "* Target columns
** They may be
- bare input columns, acting as grouping keys,
- formulas in the syntax of Org spreadsheet, like ~vmean()~, ~vsum()~, ~count()~.
** Formatting
Each target column may be followed optionally by semicolon separated parameters:
- alternate name, example ;'alternate-name'
- formatting, examples ~;f2~ ~;%%.2f~
- sorting, examples ~;^a~ ~;^A~ ~;^n~ ~;^N~
- invisibility  ~;<>~
** Available input columns
  %s
** Examples:
  ~vmean(quty);f2~, ~vsum(amount);'total'~"
           :cond "* Filter rows (optional)
Lisp function, lambda, or Babel block to filter out rows.
** Available input columns
  %s
** Example
  ~(>= (string-to-number quty) 3)~
  only rows with cell ~quty~ higher or equal to ~3~ are retained.
  ~(not (equal tag \"dispose\"))~
  rows with cell ~tag~ equal to ~dispose~ are filtered out."
           :hline "* Output horizontal separators level (optional)
- ~0~ or empty means no lines in the output
- ~1~ means separate rows of identical values on 1 column
- ~2~ means separate rows of identical values on 2 columns
- larger values are allowed, but questionably useful.
The columns considered are the sorted ones."
           :post "* Post-process (optional)
The output table may be post-processed prior to printing it
in the current buffer.
The processor may be a Lisp function, a lambda, or a Babel block.
** Example:
  ~(lambda (table) (append table '(hline (banana 42))))~
  two rows are appended at the end of the output table:
  ~hline~ which means horizontal line,
  and a row with two cells."
           :cols-tr "* Target columns
** Optional
If the answer is left empty, all input columns are kept,
in the same order.
** Available input columns
  %s"
           ))
        (main-window (selected-window))
        (help-window (get-buffer-window "*orgtbl-aggregate-help*")))
    (if help-window
        (select-window help-window)
      (setq main-window (split-window nil 16 'above))
      (switch-to-buffer "*orgtbl-aggregate-help*")
      (setq help-window (selected-window)))
    (org-mode)
    (erase-buffer)
    (insert (apply #'format (plist-get docs explain) args))
    (goto-char (point-min))
    (select-window main-window)))

(defun orgtbl-aggregate--dismiss-help ()
  "Hide the wizard help window."
  (let ((help-window (get-buffer-window "*orgtbl-aggregate-help*")))
    (if help-window
        (delete-window help-window))))

(defun orgtbl-aggregate--wizard-query-table (table)
  "Query the 3 fields composing a generalized table: file:name:slice.
If TABLE is not nil, it is decomposed into file:name:slice, and each
of those 3 fields serves as default answer when prompting.
Alternately, file:name may be orgid, an ID which knows its file location."
  (let (file name orgid params slice isorgid)
    (if table
        (let ((struct (orgtbl-aggregate--parse-locator table)))
          (setq file   (aref struct 0))
          (setq name   (aref struct 1))
          (setq orgid  (aref struct 2))
          (setq params (aref struct 3))
          (setq slice  (aref struct 4))))

    (setq
     isorgid
     (cond
      (orgid t)
      (name nil)
      (t
       (orgtbl-aggregate--display-help :isorgid)
       (let ((use-short-answers t))
         (yes-or-no-p "Is the input pointed to by an Org Mode ID? ")))))

    (if isorgid
        (progn
          (orgtbl-aggregate--display-help :orgid)
          (unless org-id-locations (org-id-locations-load))
          (setq orgid
                (completing-read
                 "Org ID: "
                 (hash-table-keys org-id-locations)
                 nil
                 nil ;; user is free to input anything
                 orgid)))

      (orgtbl-aggregate--display-help :file)
      (let ((insert-default-directory nil))
        (setq file
              (orgtbl-aggregate--nil-if-empty
               (read-file-name "File (RET for current buffer): "
                               nil
                               nil
                               nil
                               file))))

      (orgtbl-aggregate--display-help :name)
      (setq name
            (completing-read
             "Table or Babel: "
             (orgtbl-aggregate--list-local-tables file)
             nil
             nil ;; user is free to input anything
             name)))

    (and
     file
     (not params)
     (cond
      ((string-match (rx ".csv"  eos) file)
       (setq params "(csv)"))
      ((string-match (rx ".json" eos) file)
       (setq params "(json)"))))

    (orgtbl-aggregate--display-help :params)
    (setq params
          (read-string
           "Babel parameters (optional): "
           params
           'orgtbl-aggregate-history-cols))

    (orgtbl-aggregate--display-help :slice)
    (setq slice
          (read-string
           "Input slicing (optional): "
           slice
           'orgtbl-aggregate-history-cols))

    (orgtbl-aggregate--assemble-locator file name orgid params slice)))

(defun orgtbl-aggregate--wizard-aggregate-create-update (oldline)
  "Update OLDLINE parameters by interactivly querying user.
OLDLINE is an alist containing parameter-value pairs.
Example: \\'((:table . \"thetable\") (:cols . \"day vsum(quty)\") …)
OLDLINE is supposed to be extracted from an Org Mode block such as:
#+begin: aggregate :table \"thetable\" :cols \"day vsum(quty)\" …
If (point) is not on such a line, OLDLINE is nil.
The function returns a plist which is an updated version of OLDLINE
amended by the user."
  (let ((minibuffer-local-completion-map
         (define-keymap :parent minibuffer-local-completion-map
           "SPC" nil)) ;; allow inserting spaces
        table headerlist header precompute
        aggcols aggcond hline postprocess params)

    (save-window-excursion
      (setq table
            (orgtbl-aggregate--wizard-query-table
             (orgtbl-aggregate--alist-get-remove :table oldline)))

      (setq headerlist
            (orgtbl-aggregate--get-header-table table))

      (setq header
            (mapconcat
             (lambda (x) (format " ~%s~" x))
             headerlist))

      (orgtbl-aggregate--display-help :precompute header)
      (setq precompute
            (read-string
             "Formulas for additional input columns (optional): "
             (orgtbl-aggregate--alist-get-remove :precompute oldline)
             'orgtbl-aggregate-history-cols))

      (when (orgtbl-aggregate--nil-if-empty precompute)
        (setq headerlist
              (append headerlist
                      (cl-loop
                       for pair in
                       (orgtbl-aggregate--parse-preprocess
                        precompute
                        (length headerlist))
                       collect (cdr pair))))
        (setq header
              (mapconcat
               (lambda (x) (format " ~%s~" x))
               headerlist)))

      (orgtbl-aggregate--display-help :cols header)
      (setq aggcols
            (replace-regexp-in-string
             "\"" "'"
             (read-string
              "Target columns & formulas: "
              (orgtbl-aggregate--alist-get-remove :cols oldline)
              'orgtbl-aggregate-history-cols)))

      (orgtbl-aggregate--display-help :cond header)
      (setq aggcond
            (read-string
             "Row filter (optional): "
             (orgtbl-aggregate--alist-get-remove :cond oldline)
             'orgtbl-aggregate-history-cols))

      (orgtbl-aggregate--display-help :hline)
      (setq hline
            (completing-read
             "hline (optional): "
             '("0" "1" "2" "3")
             nil
             'confirm
             (orgtbl-aggregate--cell-to-string
              (orgtbl-aggregate--alist-get-remove :hline oldline))))

      (orgtbl-aggregate--display-help :post)
      (setq postprocess
            (read-string
             "Post process (optional): "
             (orgtbl-aggregate--alist-get-remove :post oldline)
             'orgtbl-aggregate-history-cols))
      )

    (setq params
          (list
           :name "aggregate"
           :table table
           :cols aggcols))
    (if (orgtbl-aggregate--nil-if-empty aggcond)
        (nconc params `(:cond ,(read aggcond))))
    (if (orgtbl-aggregate--nil-if-empty hline)
        (nconc params `(:hline ,hline)))
    (if (orgtbl-aggregate--nil-if-empty precompute)
        (nconc params `(:precompute ,precompute)))
    (if (orgtbl-aggregate--nil-if-empty postprocess)
        (nconc params `(:post ,postprocess)))
    (cl-loop
     for pair in oldline
     if (car pair)
     do (nconc params `(,(car pair) ,(cdr pair))))
    params))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Unfold, Fold
;; Experimental
;; Typing TAB on a line like
;; #+begin aggragate params…
;; unfolds the parameters: a new line for each parameter
;; and a dedicated help & completion for each activated by TAB

;;;###autoload
(defun orgtbl-aggregate-dispatch-TAB ()
  "Type TAB on a line like #+begin: aggregate to activate custom functions.
Actually, any line following this pattern will do:
#+xxxxx: yyyyy
Typing TAB will dispatch to function org-TAB-xxxxx-yyyyy if it exists.
If it does not exist, Org Mode will proceed as usual.
If it exists and returns nil, Org Mode will proceed as usual as well.
It it returns non-nil, the TAB processing will stop there."
  (save-excursion
    (if (and
         (not (bolp))
         (progn
           (end-of-line)
           (not (org-fold-core-folded-p)))
         (progn
           (beginning-of-line)
           (re-search-forward
            (rx
             point
             "#+"
             (group (+ (any "a-z0-9_-")))
             ":"
             (* blank)
             (group (+ (any ":a-z0-9_-")))
             (* blank))
            nil t)))
        (let ((symb
               (intern
                (format
                 "org-TAB-%s-%s"
                 (downcase (match-string-no-properties 1))
                 (downcase (match-string-no-properties 2))))))
          (if (symbol-function symb)
              (funcall symb))))))

(defun org-TAB-begin-aggregate ()
  "Dispatch to unfolding or folding code.
If the line following
  #+begin: aggregate
is an unfoled one of the form:
  #+aggregate: …
then proceed to folding, otherwise unfold."
  (if (save-excursion
        (forward-line 1)
        (beginning-of-line)
        (re-search-forward
         (rx point "#+aggregate:")
         nil t))
      (org-TAB-begin-aggregate-fold)
    (org-TAB-begin-aggregate-unfold)))

(defun org-TAB-begin-aggregate-fold ()
  "Turn all lines of the form #+aggregate: … into a single line.
That is, fold the may lines of the form:
  #+aggregate: param…
into the single line of the form:
  #+begin: aggregate params…
Note that the resulting :table XXX parameter is composed of several
individual parameters."
  (orgtbl-aggregate--dismiss-help)
  (let* ((alist (orgtbl-aggregate-get-all-unfolded)))
    (end-of-line)
    (insert
     " :table \""
     (orgtbl-aggregate--assemble-locator
      (alist-get :file   alist)
      (alist-get :name   alist)
      (alist-get :orgid  alist)
      (alist-get :params alist)
      (alist-get :slice  alist))
     "\"")
    (if (orgtbl-aggregate--nil-if-empty (alist-get :precompute alist))
        (insert
         (format " :precompute \"%s\"" (alist-get :precompute alist))))
    (insert
     (format " :cols \"%s\"" (or (alist-get :cols alist) "")))
    (if (orgtbl-aggregate--nil-if-empty  (alist-get :cond  alist))
        (insert (format " :cond \"%s\""  (alist-get :cond  alist))))
    (if (orgtbl-aggregate--nil-if-empty  (alist-get :hline alist))
        (insert (format " :hline \"%s\"" (alist-get :hline alist))))
    (if (orgtbl-aggregate--nil-if-empty  (alist-get :post  alist))
        (insert (format " :post \"%s\""  (alist-get :post  alist))))
    (forward-line 1)
    (while
        (progn
          (beginning-of-line)
          (re-search-forward (rx point "#+aggregate:") nil t))
      (beginning-of-line)
      (delete-line))
    (forward-line -1)
    t))

(defun org-TAB-begin-aggregate-unfold ()
  "Turn the single line #+begin: aggregate into several lines.
That is, move all parameters in the line
  #+begin: aggregate params…
into several lines, each with a single parameter.
Note that the :table XXX parameter is decomposed into several
individual parameter for an easier reading."
  (let* ((line (orgtbl-aggregate--parse-header-arguments "aggregate"))
         (point (progn (end-of-line) (point)))
         (struct (orgtbl-aggregate--parse-locator
                  (orgtbl-aggregate--alist-get-remove :table line))))
    (insert "\n#+aggregate: :file "   (or (aref struct 0) ""))
    (insert "\n#+aggregate: :name "   (or (aref struct 1) ""))
    (insert "\n#+aggregate: :orgid "  (or (aref struct 2) ""))
    (insert "\n#+aggregate: :params " (or (aref struct 3) ""))
    (insert "\n#+aggregate: :slice "  (or (aref struct 4) ""))
    (insert "\n#+aggregate: :precompute "
            (or (orgtbl-aggregate--alist-get-remove :precompute line) ""))
    (insert "\n#+aggregate: :cols "
            (or (orgtbl-aggregate--alist-get-remove :cols       line) ""))
    (insert "\n#+aggregate: :cond "
            (or (orgtbl-aggregate--alist-get-remove :cond       line) ""))
    (insert "\n#+aggregate: :hline "
            (or (orgtbl-aggregate--alist-get-remove :hline      line) ""))
    (insert "\n#+aggregate: :post "
            (or (orgtbl-aggregate--alist-get-remove :post       line) ""))
    (goto-char point)
    (beginning-of-line)
    (forward-word 2)
    (delete-region (point) point)
    t))

(defun orgtbl-aggregate-get-all-unfolded ()
  "Prepare an a-list of all unfolded parameters."
  (interactive)
  (save-excursion
    (re-search-backward (rx bol "#+begin:") nil t)
    (let ((alist))
      (while
          (progn
            (forward-line 1)
            (re-search-forward
             (rx point "#+aggregate:" (* blank)
               (group (+ (any ":a-z0-9_-")))
               (* blank)
               (group (* any)))
             nil t))
        (push (cons
               (intern (match-string-no-properties 1))
               (match-string-no-properties 2))
              alist))
      (reverse alist))))

(defun orgtbl-aggregate--column-names-from-unfolded ()
  "Return a textual list of column names.
They are computed by looking at the distant table
(an Org table, a Babel block, a CSV, or a JSON)
and recovering its header if any.
If there is no header, $1 $2 $3... is returned."
  (let*
      ((alist (orgtbl-aggregate-get-all-unfolded))
       (table
        (orgtbl-aggregate--assemble-locator
         (alist-get :file   alist)
         (alist-get :name   alist)
         (alist-get :orgid  alist)
         (alist-get :params alist)
         (alist-get :slice  alist))))
    (mapconcat
     (lambda (x) (format " ~%s~" x))
     (orgtbl-aggregate--get-header-table table))))

(defun orgtbl-aggregate--TAB-replace-value (getter)
  "Update a #+aggregate: line
from
  #+aggregate: :tag OLD
to
  #+aggregate: :tag NEW
NEW being the result of executing (GETTER OLD)"
  (let* (;;(insert-default-directory t)
         (start (point))
         (end (progn (end-of-line) (point)))
         (new
          (funcall
           getter
           (buffer-substring-no-properties start end))))
    (when new
      (delete-region start end)
      (delete-horizontal-space)
      (insert " " new))))

(defun org-TAB-aggregate-:file ()
  "Provide help and completion for the #+aggregate: file XXX parameter."
  (orgtbl-aggregate--display-help :file)
  (orgtbl-aggregate--TAB-replace-value
   (lambda (old)
     (read-file-name
      "File: "
      (file-name-directory    old)
      nil
      nil
      (file-name-nondirectory old)))))

(defun org-TAB-aggregate-:name ()
  "Provide help and completion for the #+aggregate: name XXX parameter."
  (orgtbl-aggregate--display-help :name)
  (orgtbl-aggregate--TAB-replace-value
   (lambda (old)
     (completing-read
      "Table or Babel name: "
      (orgtbl-aggregate--list-local-tables
       (orgtbl-aggregate--nil-if-empty
        (alist-get :file (orgtbl-aggregate-get-all-unfolded))))
      nil
      nil ;; user is free to input anything
      old))))

(defun org-TAB-aggregate-:orgid ()
  "Provide help and completion for the #+aggregate: id XXX parameter."
  (orgtbl-aggregate--display-help :orgid)
  (unless org-id-locations (org-id-locations-load))
  (orgtbl-aggregate--TAB-replace-value
   (lambda (old)
     (completing-read
      "Org-ID: "
      (hash-table-keys org-id-locations)
      nil
      nil ;; user is free to input anything
      old))))

(defun org-TAB-aggregate-:params ()
  (orgtbl-aggregate--display-help :params))

(defun org-TAB-aggregate-:slice ()
  (orgtbl-aggregate--display-help :slice))

(defun org-TAB-aggregate-:precompute ()
  (orgtbl-aggregate--display-help :precompute
   (orgtbl-aggregate--column-names-from-unfolded)))
  
(defun org-TAB-aggregate-:cols ()
  (orgtbl-aggregate--display-help :cols
   (orgtbl-aggregate--column-names-from-unfolded)))

(defun org-TAB-aggregate-:cond ()
  (orgtbl-aggregate--display-help :cond
   (orgtbl-aggregate--column-names-from-unfolded)))

(defun org-TAB-aggregate-:hline ()
  "Hitting TAB on #+aggregate: hline N cycles the parameter value.
The cycle is
nothing → 1 → 2 → 3 → nothing."
  (orgtbl-aggregate--display-help :hline)
  (orgtbl-aggregate--TAB-replace-value
   (lambda (old)
     (cond
      ((equal old "" ) "1")
      ((equal old "1") "2")
      ((equal old "2") "3")
      ((equal old "3") "" )
      (t "")))))

(defun org-TAB-aggregate-:post ()
  (orgtbl-aggregate--display-help :post))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun orgtbl-aggregate-insert-dblock-aggregate ()
  "Wizard to interactively insert an aggregate dynamic block."
  (interactive)
  (let* ((oldline (orgtbl-aggregate--parse-header-arguments "aggregate"))
         (params (orgtbl-aggregate--wizard-aggregate-create-update oldline)))
    (when oldline
      (org-mark-element)
      (delete-region (region-beginning) (1- (region-end))))
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
        ;; '(t) or `(t) would be incorrect╶────────▷─╯
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
			  always (string= "" x))
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
	(post    (plist-get params :post)))
    (if (and content
	     (let ((case-fold-search t))
	       (string-match
		(rx bos (+ (* blank) "#+" (* any) "\n"))
		content)))
	(insert (match-string 0 content)))
    (orgtbl-aggregate--insert-elisp-table
     (orgtbl-aggregate--post-process
      (orgtbl-aggregate--create-table-transposed
       (orgtbl-aggregate--remove-cookie-lines
        (orgtbl-aggregate-table-from-any-ref (plist-get params :table)))
       (plist-get params :cols)
       (plist-get params :cond))
      post))
    (orgtbl-aggregate--table-recalculate content formula)))

(defun orgtbl-aggregate--wizard-transpose-create-update (oldline)
  "Update OLDLINE parameters by interactivly querying user.
OLDLINE is an alist containing parameter-value pairs.
Example: \\'((:table . \"thetable\") (:cols . \"day month\") …)
OLDLINE is supposed to be extracted from an Org Mode block such as:
#+begin: transpose :table \"thetable\" :cols \"day month\" …
If (point) is not on such a line, OLDLINE is nil.
The function returns a plist which is an updated version of OLDLINE
amended by the user."
  (let ((minibuffer-local-completion-map
         (define-keymap :parent minibuffer-local-completion-map
           "SPC" nil)) ;; allow inserting spaces
        table headerlist header aggcols aggcond postprocess params)

    (save-window-excursion
      (setq table
            (orgtbl-aggregate--wizard-query-table
             (orgtbl-aggregate--alist-get-remove :table oldline)))

      (setq headerlist
            (orgtbl-aggregate--get-header-table table))

      (setq header
            (mapconcat
             (lambda (x) (format " ~%s~" x))
             headerlist))

      (orgtbl-aggregate--display-help :cols-tr header)
      (setq aggcols
            (replace-regexp-in-string
             "\"" "'"
             (read-string
              "Target columns & formulas: "
              (orgtbl-aggregate--alist-get-remove :cols oldline)
              'orgtbl-aggregate-history-cols)))

      (orgtbl-aggregate--display-help :cond header)
      (setq aggcond
            (read-string
             "Row filter (optional): "
             (orgtbl-aggregate--alist-get-remove :cond oldline)
             'orgtbl-aggregate-history-cols))

      (orgtbl-aggregate--display-help :post)
      (setq postprocess
            (read-string
             "Post process (optional): "
             (orgtbl-aggregate--alist-get-remove :post oldline)
             'orgtbl-aggregate-history-cols))
      )

    (setq params
          (list
           :name "transpose"
           :table table
           :cols aggcols))
    (unless (eq (length aggcond) 0)
      (nconc params `(:cond ,(read aggcond))))
    (unless (eq (length postprocess) 0)
      (nconc params `(:post ,postprocess)))
    (cl-loop
     for pair in oldline
     if (car pair)
     do (nconc params `(,(car pair) ,(cdr pair))))
    params))

;;;###autoload
(defun orgtbl-aggregate-insert-dblock-transpose ()
  "Wizard to interactively insert a transpose dynamic block."
  (interactive)
  (let* ((oldline (orgtbl-aggregate--parse-header-arguments "transpose"))
         (params (orgtbl-aggregate--wizard-transpose-create-update oldline)))
    (when oldline
      (org-mark-element)
      (delete-region (region-beginning) (1- (region-end))))
    (org-create-dblock params)
    (org-update-dblock)))

;; Insert a dynamic bloc with the C-c C-x x dispatcher
;; and activate TAB on #+begin: aggregate ...
;;;###autoload
(eval-after-load 'org
  '(progn
     ;; org-dynamic-block-define found in Emacs 27.1
     (org-dynamic-block-define "aggregate" #'orgtbl-aggregate-insert-dblock-aggregate)
     (org-dynamic-block-define "transpose" #'orgtbl-aggregate-insert-dblock-transpose)))

;; This hook will only work if orgtbl-aggregate is loaded,
;; thus the eval-after-load 'orgtbl-aggregate
;; We do not want this hook to be added to Org Mode if orgtbl-aggregate
;; is not used, thus the eval-after-load 'orgtbl-aggregate
;;;###autoload
(eval-after-load 'orgtbl-aggregate
  '(add-hook 'org-cycle-tab-first-hook #'orgtbl-aggregate-dispatch-TAB))

(provide 'orgtbl-aggregate)
;;; orgtbl-aggregate.el ends here
