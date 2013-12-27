
;;; dict-tree.el --- dictionary data structure package


;; Copyright (C) 2004-2008 Toby Cubitt

;; Author: Toby Cubitt <toby-predictive@dr-qubit.org>
;; Version: 0.11.1
;; Keywords: dictionary, tree
;; URL: http://www.dr-qubit.org/emacs.php


;; This file is NOT part of Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
;; MA 02110-1301, USA.


;;; Commentary:
;;
;; A dictionary is used to store strings, along with arbitrary data
;; associated with each string. As well as basic data insertion,
;; manipulation and retrieval, a dictionary can perform prefix
;; searches on those strings, retrieving all strings with a given
;; prefix in either alphabetical or any other order (see the
;; `dictree-complete' and `dictree-complete-ordered' functions), and
;; is able to cache results in order to speed up those searches. The
;; package also provides persistent storage of the data structures to
;; files.
;;
;; You create a dictionary using `dictree-create', add entries to it
;; using `dictree-insert', lookup entries using `dictree-lookup', find
;; completions of sequences using `dictree-complete', find completions
;; and sort them in any order you speficy using
;; `dictree-complete-ordered', map over it using `dictree-map' and
;; `dictree-mapcar', save it to a file using `dictree-save' or
;; `dictree-write', and load from file it using
;; `dictree-load'. Various other useful functions are also provided.
;;
;; This package uses the ternary search tree package, tstree.el.


;;; Change log:
;;
;; Version 0.11.1
;; * set and restore value of `byte-compile-disable-print-circle' instead of
;;   let-binding it, to avoid warnings when compiling
;; * added `dictree-goto-line' macro to work around `goto-line' bug
;;
;; Version 0.11
;; * modified `dictree-write' so that, by default, both compiled and uncompiled
;;   versions of dictionaries are created when writing dictionaries to file
;; * fixed slow byte-compilation under Emacs 22
;;
;; Version 0.10.2
;; * very minor changes to text of some messages
;;
;; Version 0.10.1
;; * added optional DICTLIST argument to `read-dict', to allow completion from
;;   a restricted set of dictionaries
;;
;; Version 0.10
;; * finally wrote a `dictree-delete' function!
;;
;; Version 0.9.1
;; * fixed bug in `dictree-dump-words-to-buffer' (thanks to Dan Pomohaci
;;   for reporting it)
;; * replaced "word" with "key" in function arguments and docstrings,
;;   since keys don't have to be words
;; * removed "words" from dump functions' names, added TYPE argument in
;;   line with other functions, and made them non-interactive
;; * added COMPARE-FUNCTION argument to `dictree-create', which defaults
;;   to subtraction as before
;; * `dictree-read-line' reads the keys with `read', and no longer evals
;;   the data as this fails for simple, useful cases (e.g. constant lists)
;;
;; Version 0.9
;; * added meta-dictionary functionality
;; * dictionary data can now be referenced by any sequence type, not just
;;   strings
;; * removed cl dependency
;;
;; Note: version 0.8 dictionaries not compatible with version 0.9 and
;;       above
;;
;; Version 0.8.4
;; * fixed small bug in `read-dict'
;;
;; Version 0.8.3
;; * fixed internal function and macro names
;; * changed naming prefix from dict- to dictree- to avoid conflicts
;; * `dict-write' now unloads old name and reloads new
;;
;; Version 0.8.2
;; * added more commentary
;;
;; Version 0.8.1
;; * fixed nasty bug in `dict-map' and `dict-mapcar' caused by dynamic
;;   scoping
;;
;; Version 0.8
;; * changed `dict-map(car)' into functions and made them work with
;;   lookup-only dicts
;; * `dict-insert' now returns the new data value
;; * rewrote cache data structures: data is now wrapped inside a cons
;;   cell, so that cache entries can point to it instead of duplicating
;;   it. This fixes some caching bugs and makes updating cached data when
;;   inserting words much faster
;; * dictionaries (but not lookup-only) can now associate two pieces of
;;   data with each word: normal data, used to rank words returned by
;;   `dict-complete-ordered', and meta-data, not used for ranking
;; * modified functions to work with new caching and meta-data, and added
;;   `dict-set-meta-data' and `dict-lookup-meta-data'
;; * renamed to `dict-tree' to help avoid conflicts with other packages
;;
;; Version 0.7
;; * added `dict-mapcar' macro
;;
;; Version 0.6.2
;; * minor bug fixes
;;
;; Version 0.6.1
;; * minor bug fixes
;;
;; Version 0.6
;; * added dict-size function
;; * added dict-dump-words-to-buffer function
;; * dictionaries now set their names and filenames by doing a library
;;   search for themselves when loaded using require
;; * added `read-dict' minibuffer completion function
;; * interactive commands that read a dictionary name now provide
;;   completion
;;
;; Version 0.5
;; * added dict-dump-words-to-file function
;;
;; Version 0.4
;; * fixed bug in dict-read-line
;;
;; Version 0.3
;; * added dict-map function
;;
;; Version 0.2
;; * added dictionary autosave flag and related functions;
;; * fixed bug preventing dict caches being loaded properly;
;; * explicitly require cl.el;
;;
;; Note: version 0.1 dictionaries not compatible with version 0.2 and
;;       above!
;;
;; Version 0.1
;; * initial release



;;; Code:

(provide 'dict-tree)
(require 'tstree)
(require 'bytecomp)



;;; ================================================================
;;;                Replacements for CL functions

;; copied from cl-extra.el
(defun dictree--subseq (seq start &optional end)
  "Return the subsequence of SEQ from START to END.
If END is omitted, it defaults to the length of the sequence.
If START or END is negative, it counts from the end."
  (if (stringp seq) (substring seq start end)
    (let (len)
      (and end (< end 0) (setq end (+ end (setq len (length seq)))))
      (when (< start 0)
	(setq start (+ start (or len (setq len (length seq))))))
      (cond ((listp seq)
	     (if (> start 0) (setq seq (nthcdr start seq)))
	     (if end
		 (let ((res nil))
		   (while (>= (setq end (1- end)) start)
		     (push (pop seq) res))
		   (nreverse res))
	       (copy-sequence seq)))
	    (t
	     (or end (setq end (or len (length seq))))
	     (let ((res (make-vector (max (- end start) 0) nil))
		   (i 0))
	       (while (< start end)
		 (aset res i (aref seq start))
		 (setq i (1+ i) start (1+ start)))
	       res))))))



;; adapted from cl-seq.el
(defun dictree--merge (list1 list2 predicate)
  "Destructively merge the two lists to produce a new list
sorted according to PREDICATE. The lists are assumed to already
be sorted. The function PREDICATE is passed one entry from each
list, and should return non-nil if the first argument should be
sorted before the second."
  (or (listp list1) (setq list1 (append list1 nil)))
  (or (listp list2) (setq list2 (append list2 nil)))
  (let ((res nil))
    ;; build up result list backwards
    (while (and list1 list2)
      (if (funcall predicate (car list1) (car list2))
	    (push (pop list1) res)
	  (push (pop list2) res)))
    ;; return result, plus any leftover entries (only one of list1 or
    ;; list2 will be non-nil)
    (nconc (nreverse res) list1 list2))
)




;;; ====================================================================
;;;  Internal functions and variables for use in the dictionary package


(defvar dictree-loaded-list nil
  "Stores list of loaded dictionaries.")


(defmacro dictree--name (dict)  ; INTERNAL USE ONLY
  ;; Return the name of dictonary DICT
  `(nth 1 ,dict)
)


(defmacro dictree--set-name (dict name)  ; INTERBAL USE ONLY
  ;; Set the name of dictionary DICT
  `(setcar (cdr ,dict) ,name)
)


(defmacro dictree--filename (dict)  ; INTERNAL USE ONLY.
  ;; Return the filename of dictionary DICT
  `(nth 2 ,dict)
)


(defmacro dictree--set-filename (dict filename)  ; INTERNAL USE ONLY.
  ;; Set the filename of dictionary DICT
  `(setcar (nthcdr 2 ,dict) ,filename)
)


(defmacro dictree--autosave (dict)  ; INTERNAL USE ONLY
  ;; Return the autosave flag of dictionary DICT
  `(nth 3 ,dict))


(defmacro dictree--set-autosave (dict flag)  ; INTERNAL USE ONLY
  ;; Set the autosave flag of dictionary DICT
  `(setcar (nthcdr 3 ,dict) ,flag))


(defmacro dictree--modified (dict)  ; INTERNAL USE ONLY
  ;; Return the modified flag of dictionary DICT
  `(nth 4 ,dict))


(defmacro dictree--set-modified (dict flag)  ; INTERNAL USE ONLY
  ;; Set the modified flag of dictionary DICT
  `(setcar (nthcdr 4 ,dict) ,flag))


(defmacro dictree--lookup-only (dict)  ; INTERNAL USE ONLY.
  ;; Return non-nil if dictionary DICT is lookup-only
  `(nth 5 ,dict))


(defmacro dictree--dict-list (dict)
  ;; Return the list of dictionaries on which meta-dictionary DICT is
  ;; based.
  `(nth 6 ,dict))


(defmacro dictree--set-dict-list (dict tstree)  ; INTERNAL USE ONLY.
  ;; Set the ternary search tree of dictionary DICT.
  `(setcar (nthcdr 6 ,dict) ,tstree))


(defmacro dictree--meta-dict-p (dict)  ; INTERNAL USE ONLY
  ;; Return non-nil if DICT is a meta-dictionary.
  `(not (tstree-p (dictree--dict-list ,dict))))


(defun dictree--tstree (dict)  ; INTERNAL USE ONLY.
  ;; Return the ternary search tree of dictionary DICT.
  (if (dictree--meta-dict-p dict)
      (mapcar (lambda (dic) (dictree--tstree dic)) (nth 6 dict))
    (nth 6 dict)))


(defmacro dictree--set-tstree (dict tstree)  ; INTERNAL USE ONLY.
  ;; Set the ternary search tree of dictionary DICT.
  `(setcar (nthcdr 6 ,dict) ,tstree))


(defmacro dictree--insfun (dict)  ; INTERNAL USE ONLY.
  ;; Return the insert function of dictionary DICT.
  `(nth 7 ,dict))


(defmacro dictree--combfun (dict)  ; INTERNAL USE ONLY.
  ;; Return the combine function of meta-dictionary DICT.
  `(nth 7 ,dict))


(defmacro dictree--rankfun (dict)  ; INTERNAL USE ONLY
  ;; Return the rank function of dictionary DICT.
  `(nth 8 ,dict))


(defmacro dictree--lookup-hash (dict)  ; INTERNAL USE ONLY
  ;; Return the lookup hash table of dictionary DICT
  `(nth 9 ,dict))


(defmacro dictree--set-lookup-hash (dict hash)  ; INTERNAL USE ONLY
  ;; Set the completion hash for dictionary DICT
  `(setcar (nthcdr 9 ,dict) ,hash))


(defmacro dictree--lookup-speed (dict)  ; INTERNAL USE ONLY
  ;; Return the lookup speed of dictionary DICT
  `(nth 10 ,dict))


(defmacro dictree--set-lookup-speed (dict speed)  ; INTERNAL USE ONLY
  ;; Set the lookup speed of dictionary DICT
  `(setcar (nthcdr 10 ,dict) ,speed))


(defmacro dictree--completion-hash (dict)  ; INTERNAL USE ONLY
  ;; Return the completion hash table of dictionary DICT
  `(nth 11 ,dict))


(defmacro dictree--set-completion-hash (dict hash)  ; INTERNAL USE ONLY
  ;; Set the completion hash for dictionary DICT
  `(setcar (nthcdr 11 ,dict) ,hash))


(defmacro dictree--completion-speed (dict)  ; INTERNAL USE ONLY
  ;; Return the completion speed of dictionary DICT
  `(nth 12 ,dict))


(defmacro dictree--set-completion-speed (dict speed)  ; INTERNAL USE ONLY
  ;; Set the lookup speed of dictionary DICT
  `(setcar (nthcdr 12 ,dict) ,speed))


(defmacro dictree--ordered-hash (dict)  ; INTERNAL USE ONLY
  ;; Return the ordered completion hash table of dictionary DICT
  `(nth 13 ,dict))


(defmacro dictree--set-ordered-hash (dict hash)  ; INTERNAL USE ONLY
  ;; Set the completion hash for dictionary DICT
  `(setcar (nthcdr 13 ,dict) ,hash))


(defmacro dictree--ordered-speed (dict)  ; INTERNAL USE ONLY
  ;; Return the ordered completion speed of dictionary DICT
  `(nth 14 ,dict))


(defmacro dictree--set-ordered-speed (dict speed)  ; INTERNAL USE ONLY
  ;; Set the lookup speed of dictionary DICT
  `(setcar (nthcdr 14 ,dict) ,speed))


(defmacro dictree--meta-dict-list (dict)  ; INTERNAL USE ONLY
  ;; Return list of meta-dictionaries which depend on DICT.
  `(nthcdr 15 ,dict))


(defmacro dictree--set-meta-dict-list (dict list)  ; INTERNAL USE ONLY
  ;; Set list of dictionaries on which a meta-dictionary dict is based, or
  ;; the list of meta-dictionaries dependent on dictionary DICT.
  `(setcdr (nthcdr 14 ,dict) ,list))



(defmacro dictree--wrap-data (data &optional meta-data)
  ;; INTERNAL USE ONLY
  ;; wrap the data in a cons cell
  `(cons ,data ,meta-data))


(defmacro dictree--get-data (cell)  ; INTERNAL USE ONLY
  ;; get data component from data cons cell
  `(car ,cell))


(defmacro dictree--set-data (cell data)  ; INTERNAL USE ONLY
  ;; set data component of data cons cell
  `(setcar ,cell ,data))


(defmacro dictree--get-metadata (cell)  ; INTERNAL USE ONLY
  ;; get meta-data component of data cons cell
  `(cdr ,cell))


(defmacro dictree--set-metadata (cell meta-data)  ; INTERNAL USE ONLY
  ;; set meta-data component of data cons cell
  `(setcdr ,cell ,meta-data))



(defmacro dictree--wrap-insfun (insfun)  ; INTERNAL USE ONLY
  ;; return wrapped insfun to deal with data wrapping
  `(lambda (new cell)
     ;; if data doesn't already exist, wrap and return new data
     (if (null cell)
	 (dictree--wrap-data (funcall ,insfun new nil))
       ;; otherhwise, update data cons cell with new data and return it
       (dictree--set-data cell (funcall ,insfun new
					(dictree--get-data cell)))
       cell)))


(defmacro dictree--wrap-rankfun (rankfun)  ; INTERNAL USE ONLY
  ;; return wrapped rankfun to deal with data wrapping
  `(lambda (a b) (funcall ,rankfun
			  (cons (car a) (dictree--get-data (cdr a)))
			  (cons (car b) (dictree--get-data (cdr b))))))


(defmacro dictree--wrap-filter (filter)  ; INTERNAL USE ONLY
  ;; return wrapped filter function to deal with data wrapping
  `(lambda (str data) (funcall ,filter str (dictree--get-data data))))



(defmacro dictree--cache-create (list maxnum)  ; INTERNAL USE ONLY
  ;; Return a completion cache entry
  `(cons ,list ,maxnum))


(defmacro dictree--cache-completions (cache)  ; INTERNAL USE ONLY
  ;; Return the completions list for cache entry CACHE
  `(car ,cache))


(defmacro dictree--cache-maxnum (cache)  ; INTERNAL USE ONLY
  ;; Return the max number of completions returned for cache entry CACHE
  `(cdr ,cache))


(defmacro dictree--set-cache-completions (cache completions)
  ;; INTERNAL USE ONLY
  ;; Set the completions list for cache entry CACHE
  `(setcar ,cache ,completions))


(defmacro dictree--set-cache-maxnum (cache maxnum)  ; INTERNAL USE ONLY
  ;; Set the completions list for cache entry CACHE
  `(setcdr ,cache ,maxnum))



;;; ================================================================
;;;                   Miscelaneous macros

;; `goto-line' without messing around with mark and messages
;; Note: this is a bug in simple.el; there clearly can be a need for
;;       non-interactive calls to goto-line from Lisp code, and
;;       there's no warning about doing this. Yet goto-line *always*
;;       calls push-mark, which usually *shouldn't* be invoked by
;;       Lisp programs, as its docstring warns.
(defmacro dictree-goto-line (line)
  "Goto line LINE, counting from line 1 at beginning of buffer."
  `(progn
     (goto-char 1)
     (if (eq selective-display t)
	 (re-search-forward "[\n\C-m]" nil 'end (1- ,line))
       (forward-line (1- ,line)))))



;;; ================================================================
;;;      The public functions which operate on dictionaries


(defun dictree-p (obj)
  "Return t if OBJ is a dictionary, nil otherwise."
  (eq (car-safe obj) 'DICT)
)


(defun dictree-name (dict)
  "Return dictionary DICT's name."
  (dictree--name dict))


(defun dictree-insert-function (dict)
  "Return the insertion function for dictionary DICT."
  (dictree--insfun dict))


(defun dictree-rank-function (dict)
  "Return the rank function for the dictionary DICT (note: returns nil if
lookup-only is set for the dictionary)."
  (dictree--rankfun dict))



(defun dictree-empty (dict)
  "Return t if the dictionary DICT is empty, nil otherwise."
  (if (dictree--lookup-only dict)
      (= 0 (hash-table-count (dictree--lookup-hash dict)))
    (tstree-empty (dictree--tstree dict)))
)



(defun dictree-create (&optional name filename autosave
				 lookup-speed complete-speed
				 ordered-speed lookup-only
				 compare-function
				 insert-function
				 rank-function
				 unlisted)
  "Create an empty dictionary and return it.

If NAME is supplied, also store it in variable NAME, 

Optional argument FILENAME supplies a directory and file name to
use when saving the dictionary. If the AUTOSAVE flag is non-nil,
then the dictionary will automatically be saved to this file when
it is unloaded or when exiting emacs.

The SPEED settings set the desired speed for the corresponding
dictionary search operations (lookup, completion, ordered
completion), in seconds. If a particular instance of the
operation takes longer than this, the results will be cached in a
hash table. If exactly the same operation is requested
subsequently, it should perform significantly faster. \(Note
\"should\": there's no guarantee!\) The down side is that the
memory or disk space required to store the dictionary grows, and
inserting keys into the dictionary becomes slightly slower, since
the cache has to be synchronized.

All SPEED's default to nil. The values nil and t are special. If
a SPEED is set to nil, no caching is done for that operation. If
it is set to t, everything is cached for that operation \(similar
behaviour can be obtained by setting the SPEED to 0, but it is
better to use t\).

If LOOKUP-ONLY is non-nil, it disables all advanced search
features for the dictionary \(currently, completion\). All the
SPEED settings are ignored, as is the RANK-FUNCTION, and
everything is stored in the lookup cache, even when inserting
data. This is appropriate when a dictionary is only going to be
used for lookup, since it speeds up lookups *and* decreases the
memory required.

Optional argument COMPARE-FUNCTION sets the function used to
compare elements of the keys. It should take two arguments, A and
B, both of the type contained by the sequences used as keys
\(e.g. if the keys will be strings, the function will be passed
two integers, since characters are represented as integers\). It
should return a negative number if A is \"smaller\" than B, a
positive number if A is \"larger\" than B, and 0 if A and B are
\"equal\". It defaults to subtraction, which requires the key
sequences to contain numbers or characters.

Optional argument INSERT-FUNCTION sets the function used to
insert data into the dictionary. It should take two arguments:
the new data, and the data already in the dictionary (or nil if
none exists yet). It should return the data to insert. It
defaults to replacing any existing data with the new data.

Optional argument RANK-FUNCTION sets the function used to rank
the results of the `dictree-complete-ordered' function. It should
take two arguments, each a cons whose car is a key in the
dictionary and whose cdr is the data associated with that key. It
should return non-nil if the first argument is \"better\" than
the second, nil otherwise. It defaults to string comparison of
the keys, ignoring the data \(which is not very useful, since the
`dictree-complete' function already returns completions in
alphabetical order much more efficiently, but at least will never
cause any errors, whatever data is stored!\)

If optional argument UNLISTED is non-nil, the dictionary will not
be added to the list of loaded dictionaries. Note that this will
disable autosaving."

  ;; a dictionary is a list containing:
  ;; ('DICT
  ;;  name
  ;;  filename
  ;;  autosave flag
  ;;  modified flag
  ;;  lookup-only
  ;;  tstree / nil (if lookup-only)
  ;;  insert-function
  ;;  rank-function / nil
  ;;  lookup-hash
  ;;  lookup-speed / nil
  ;;  complete-hash / nil
  ;;  complete-speed / nil
  ;;  ordered-hash / nil
  ;;  ordered-speed / nil
  ;; )
  (let (dict compfun insfun rankfun)

    (if lookup-only
	;; if dict is lookup only, use insert-function since there's no
	;; need to wrap data
	(setq insfun insert-function)
      ;; otherwise, wrap insert-function to deal with data wrapping
      (setq insfun (if insert-function
		       (eval (macroexpand
			      `(dictree--wrap-insfun ,insert-function)))
		     ;; insert-function defaults to "replace"
		     (lambda (a b) a))))

    ;; comparison function defaults to subtraction
    (unless lookup-only
      (setq compfun (if compare-function compare-function '-)))
    
    (unless lookup-only
      (setq rankfun (if rank-function
			(eval (macroexpand
			       `(dictree--wrap-rankfun ,rank-function)))
		      ;; rank-function defaults to comparison of the
		      ;; sequences
		      (eval (macroexpand
			     `(dictree--wrap-rankfun
			       (lambda (a b)
				 (,(tstree-construct-sortfun '-)
				  (car a) (car b)))))))))

    ;; create the dictionary
    (setq dict
	 (if lookup-only
	     ;; lookup-only dictionary
	     (list 'DICT (symbol-name name) filename autosave t t
		   nil insfun nil (make-hash-table :test 'equal)
		   nil nil nil nil nil)

	   ;; normal dictionary
	   (list 'DICT (if name (symbol-name name) "") filename
		 autosave t nil
		 (tstree-create compfun insfun rankfun) insfun rankfun
		 (if lookup-speed (make-hash-table :test 'equal) nil)
		 lookup-speed
		 (if complete-speed (make-hash-table :test 'equal) nil)
		 complete-speed
		 (if ordered-speed (make-hash-table :test 'equal) nil)
		 ordered-speed)))

    ;; store dictionary in variable NAME, add it to loaded list, and
    ;; return it
    (when name (set name dict))
    (unless unlisted
      (push dict dictree-loaded-list)
      (provide name))
    dict)
)




(defun dictree-create-type (name type &optional filename autosave
			      lookup-speed complete-speed ordered-speed)
  "Create an empty dictionary of type TYPE stored in variable
NAME, and return it. Type can be one of dictionary, spell-check,
lookup, or frequency. `dictree-create-type' is a simplified
interface to `dictree-create'.

The \"dictionary\" type is exactly like a normal, paper-based
dictionary: it can associate arbitrary data with any word in the
dictionary. Inserting data for a word will replace any existing
data for that word. All SPEED arguments default to nil.

A \"spell-check\" dictionary stores words, but can not associate
any data with the words. It is appropriate when the dictionary
will only be used for checking if a word is in the
dictionary (e.g. for spell-checking). All SPEED arguments default
to nil.

A \"lookup\" dictionary is like a dictionary-type dictionary, but
can only be used to look up words, not for more advanced
searches (e.g. word completion). This has both speed and memory
benefits. It is appropriate when the more advanced searches are
not required. Any SPEED arguments are ignored.

A \"frequency\" dictionary associates a number with each word in
the dictionary. Inserting new data adds it to the existing
data. It is appropriate, for instance, when storing
word-frequencies\; the `dictree-complete-ordered' function can
then be used to return the most likely completions. All SPEED
arguments default to nil.

See `dictree-create' for more details.


Technicalities:

For the \"dictionary\" type, INSERT-FUNCTION is set to
\"replace\", and RANK-FUNCTION to string comparison of the
words (not very useful, since the `dictree-complete' function
already returns completions sorted alphabetically, and does it
much more efficiently than `dictree-complete-ordered', but at
least it will not cause errors!).

For the \"spell-check\" type, INSERT-FUNCTION is set to a
function that always returns t. RANK-FUNCTION is set to string
comparison of the words.

For the \"lookup\" type, INSERT-FUNCTION is set to \"replace\",
and LOOKUP-ONLY is set to t.

For the \"frequency\" type, INSERT-FUNCTION sums the new and
existing data. Nil is treated as 0. The RANK-FUNCTION is set to
numerical \"greater-than\" comparison of the data."
  
  (let (insfun rankfun lookup-only)
    ;; set arguments based on type
    (cond
     ;; dictionary type
     ((eq type 'dictionary)
      (setq insfun (lambda (a b) a))
      (setq rankfun (lambda (a b) (string< (car a) (car b)))))
     
     ;; spell-check type
     ((eq type 'spell-check)
      (setq insfun (lambda (a b) t))
      (setq rankfun (lambda (a b) (string< (car a) (car b)))))
     
     ;; lookup type
     ((eq type 'lookup)
      (setq insfun (lambda (a b) a))
      (setq rankfun (lambda (a b) (string< (car a) (car b))))
      (setq lookup-only t))
     
     ;; frequency type
     ((eq type 'frequency)
      (setq insfun (lambda (new old)
		     (cond ((and (null new) (null old)) 0)
			   ((null new) old)
			   ((null old) new)
			   (t (+ old new)))))
      (setq rankfun (lambda (a b) (> (cdr a) (cdr b)))))
     )
    
    (dictree-create name filename autosave
		 lookup-speed complete-speed ordered-speed
		 lookup-only nil insfun rankfun))
)



(defun dictree-create-meta-dict (name dictlist &optional filename autosave
				      lookup-speed complete-speed
				      ordered-speed lookup-only
				      combine-function rank-function
				      unlisted)
  "Create a meta-dictionary called NAME, based on dictionaries
in DICTLIST.

COMBINE-FUNCTION is used to combine data from the dictionaries in
DICTLIST. It is passed two cons cells, each of whose car contains
data and whose cdr contains meta-data from the tree. Both cons
cells contain data associated with the same key, but from
different dictionaries. The function should return a cons cell
containing the combined data and meta-data in the car and cdr
respectively.

The other arguments are as for `dictree-create'."

    ;; a meta-dictionary is a list containing:
  ;; ('DICT
  ;;  name
  ;;  filename
  ;;  autosave flag
  ;;  modified flag
  ;;  lookup-only
  ;;  tstree / nil (if lookup-only)
  ;;  combine-function
  ;;  rank-function / nil
  ;;  lookup-hash
  ;;  lookup-speed
  ;;  complete-hash / nil
  ;;  complete-speed / nil
  ;;  ordered-hash / nil
  ;;  ordered-speed / nil
  ;;  dictlist)
  (let (dict combfun rankfun)
    
    ;; wrap rank-function to deal with data wrapping
    (setq combfun combine-function)
    (when rank-function
      (setq rankfun
	    (eval (macroexpand
		   `(dictree--wrap-rankfun ,rank-function)))))

    ;; if any of the dictionaries in DICTLIST are lookup-only, the
    ;; meta-dictionary has to be lookup-only
    (mapc (lambda (dic)
	    (setq lookup-only
		  (or lookup-only (dictree--lookup-only dic))))
	  dictlist)
    
;;     ;; make sure all dictionaries this meta-dict is based on are loaded
;;     (dolist (dic dictlist) (require (dictree--name dic)))
    
    ;; create meta-dictionary
    (setq dict
	  (if lookup-only
	      ;; lookup-only dictionary
	      (list 'DICT (symbol-name name) filename autosave t t
		    dictlist combfun nil
		    (if lookup-speed (make-hash-table :test 'equal) nil)
		    lookup-speed
		    nil nil nil nil)
	    ;; normal dictionary
	    (list 'DICT (symbol-name name) filename autosave t nil
		  dictlist combfun rankfun
		  (if lookup-speed (make-hash-table :test 'equal) nil)
		  lookup-speed
		  (if complete-speed (make-hash-table :test 'equal) nil)
		  complete-speed
		  (if ordered-speed (make-hash-table :test 'equal) nil)
		  ordered-speed)))
    
    ;; add meta-dictionary to lists of meta-dicts for all dictionaries it
    ;; depends on
    (mapc (lambda (dic) (nconc dic (list dict))) dictlist)
    
    ;; store dictionary in variable NAME, add it to loaded list, and
    ;; return it
    (set name dict)
    (unless unlisted
      (push dict dictree-loaded-list)
      (provide name))
    dict)
)




(defun dictree-insert (dict key &optional data insert-function)
  "Insert KEY and DATA into dictionary DICT.
If KEY does not already exist, this creates it. How the data is
inserted depends on the dictionary's insertion function \(see
`dictree-create'\).

The optional INSERT-FUNCTION over-rides the dictionary's own
insertion function. It should take two arguments: the data DATA,
and the data associated with KEY in the dictionary (nil if none
already exists). It should return the data to insert."
  ;; make sure SEQUENCE is a sequence
  (when (not (sequencep key))
    (error "Wrong argument type stringp, %s"
	   (prin1-to-string key)))
  (when (not (dictree-p dict))
    (error "Wrong argument type dictree-p"))
  
  ;; if dictionary is a meta-dictionary, insert key into all the
  ;; dictionaries it's based on
  (if (dictree--meta-dict-p dict)
      (mapc (lambda (dic)
	      (dictree-insert dic key data insert-function))
	    (dictree--dict-list dict))
    
    
    ;; otherwise, dictionary is a normal dictionary...
    (let ((insfun (if insert-function
		      (eval (macroexpand
			     `(dictree--wrap-insfun ,insert-function)))
		    (dictree--insfun dict)))
	  newdata)
      ;; set the dictionary's modified flag
      (dictree--set-modified dict t)
      
      ;; if dictionary is lookup-only, just insert the data in the
      ;; lookup cache
      (if (dictree--lookup-only dict)
	  (let ((lookup-hash (dictree--lookup-hash dict)))
	    (puthash key
		     (setq newdata
			   (funcall insfun data
				    (gethash key lookup-hash)))
		     lookup-hash))
	
	;; otherwise...
	(let ((tstree (dictree--tstree dict)))
	  ;; insert key in dictionary's ternary search tree
	  (setq newdata (tstree-insert tstree key data insfun))
	  ;; update dictionary's caches
	  (dictree-update-cache dict key newdata)
	  ;; update cache's of any meta-dictionaries based on dict
	  (mapc (lambda (dic)
		  (dictree-update-cache dic key newdata))
		(dictree--meta-dict-list dict))))

      ;; return the new data
      (dictree--get-data newdata)))
)



(defun dictree-delete (dict key)
  "Delete KEY from DICT.
Returns non-nil if KEY was deleted, nil if KEY was not in DICT."

  (let (deleted)
    (cond
     ;; if DICT is a meta-dictionary, delete KEY from all dictionaries
     ;; it's based on
     ((dictree--meta-dict-p dict)
      (dolist (dic (dictree--dict-list dict))
	(setq deleted (or deleted (dictree-delete dic key))))
      (dictree--set-modified dict deleted)
      deleted)
     
     ;; if dictionary is lookup-only, just delete KEY from the lookup
     ;; hash
     ((dictree--lookup-only dict)
      (setq deleted (dictree-member-p dict key))
      (when deleted
	(remhash key (dictree--lookup-hash dict))
	(dictree--set-modified dict t))
      deleted)
     
     ;; otherwise...
     (t
      (setq deleted (tstree-delete (dictree--tstree dict) key))
      ;; if key was deleted, have to update the caches
      (when deleted
	(dictree-update-cache dict key nil t)
	(dictree--set-modified dict t))
      deleted)
     ))
)



(defun dictree-lookup (dict key)
  "Return the data associated with KEY in dictionary DICT,
or nil if KEY is not in the dictionary.

Note: this will not distinguish between a non-existent KEY and a
KEY whose data is nil. \(\"spell-check\" type dictionaries
created using `dictree-create-type' store t as the data for every
key to avoid this problem) Use `dictree-member-p' to distinguish
non-existent keys from nil data."
  
  ;; first check the lookup hash for the key
  (let ((data (when (dictree--lookup-speed dict)
		(gethash key (dictree--lookup-hash dict))))
	(combfun (when (dictree--meta-dict-p dict)
		   (dictree--combfun dict)))
	time)
    
    ;; if it wasn't in the lookup hash...
    (unless data
      (cond
       
       ;; if the dictionary is lookup-only and is a meta-dictionary,
       ;; search in the dictionaries it's based on
       ((and (dictree--lookup-only dict) (dictree--meta-dict-p dict))
	(setq time (float-time))
	(mapc (lambda (dic)
		(setq data (funcall (dictree--combfun dict) data
				    (dictree-lookup dic key))))
	      (dictree--dict-list dict))
	(setq time (- (float-time) time))
	
        ;; if the lookup was slower than the dictionary's lookup speed,
        ;; add it to the lookup hash and set the modified flag
	(when (and (dictree--lookup-speed dict)
		   (or (eq (dictree--lookup-speed dict) t)
		       (> time (dictree--lookup-speed dict))))
	  (dictree--set-modified dict t)
	  (puthash key data (dictree--lookup-hash dict))))
       

       ;; if nothing was found in the cache, and the dictionary is not
       ;; lookup-only, look in the ternary search tree
       ((not (dictree--lookup-only dict))
	;; time the lookup
	(setq time (float-time))
	(setq data (tstree-member (dictree--tstree dict) key combfun))
	(setq time (- (float-time) time))
	
	;; if the lookup was slower than the dictionary's lookup speed,
	;; add it to the lookup hash and set the modified flag
	(when (and (dictree--lookup-speed dict)
		   (or (eq (dictree--lookup-speed dict) t)
		       (> time (dictree--lookup-speed dict))))
	  (dictree--set-modified dict t)
	  (puthash key data (dictree--lookup-hash dict))))
       ))
    
    ;; return the data
    (dictree--get-data data))
)



(defun dictree-set-meta-data (dict key meta-data)
  "Set meta-data (data not used to rank keys) for KEY
in dictionary DICT."
  
  (when (not (dictree-p dict))
    (error "Wrong argument type dictree-p"))
  
  ;; set the dictionary's modified flag
  (dictree--set-modified dict t)
    
  ;; if dictionary is lookup-only, refuse!
  (if (dictree--lookup-only dict)
      (error "Lookup-only dictionaries can't contain meta-data")
    ;; otherwise, set key's meta-data
    (dictree--set-metadata
     (tstree-member (dictree--tstree dict) key) meta-data))
)


	
(defun dictree-lookup-meta-data (dict key)
  "Return any meta-data (data not used to rank keys)
associated with KEY in dictionary DICT, or nil if KEY is not in
the dictionary.

Note: this will not distinguish between a non-existent KEY and a
KEY with no meta-data. Use `dictree-member-p' to distinguish
non-existent keys."

  (when (dictree--lookup-only dict)
    (error "Lookup-only dictionaries can't contain meta-data"))
  
  ;; first check the lookup hash for the key
  (let ((data (if (dictree--lookup-speed dict)
		  (gethash key (dictree--lookup-hash dict))
		nil))
	(combfun (when (dictree--meta-dict-p dict)
		   (dictree--combfun dict)))
	time)
    
    ;; if it wasn't in the lookup hash, search in the ternary search tree
    (unless data
      ;; time the lookup
      (let (time)
	(setq time (float-time))
	(setq data (tstree-member (dictree--tstree dict) key combfun))
	(setq time (- (float-time) time))
	
        ;; if the lookup was slower than the dictionary's lookup speed,
        ;; add it to the lookup hash and set the modified flag
	(when (and (dictree--lookup-speed dict)
		   (or (eq (dictree--lookup-speed dict) t)
		       (> time (dictree--lookup-speed dict))))
	  (dictree--set-modified dict t)
	  (puthash key data (dictree--lookup-hash dict)))))
    
    ;; return the meta-data
    (dictree--get-metadata data))
)




(defun dictree-member-p (dict key)
  "Return t if KEY is in dictionary DICT, nil otherwise."

  ;; if DICT is a meta-dictionary, look in dictionaries it's based on
  (cond
   ((dictree--meta-dict-p dict)
    (catch 'found
      (dolist (dic (dictree--dict-list dict))
	(when (dictree-member-p dic key) (throw 'found t)))))
   
   ;; lookup-only, look in lookup hash and use dummy symbol to
   ;; distinguish non-existent keys from those with nil data
   ((dictree--lookup-only dict)
    (if (eq (gethash key (dictree--lookup-hash dict) 'not-in-here)
	    'not-in-here)
	nil t))
   
   ;; otherwise look in the ternary search tree
   (t (tstree-member-p (dictree--tstree dict) key)))
)



(defun dictree-map (function dict &optional type)
  "Apply FUNCTION to all entries in dictionary DICT,
for side-effects only.

FUNCTION will be passed two arguments: a key of type
TYPE ('string, 'vector, or 'list, defaulting to 'vector) from the
dictionary, and the data associated with that key. It is safe to
assume the dictionary entries will be traversed in
\"alphabetical\" order.

If TYPE is 'string, it must be possible to apply the function
`string' to the type used to reference data in the dictionary."
  
  (if (dictree--lookup-only dict)
      (maphash function (dictree--lookup-hash dict))
;;     ;; need to "rename" `function' or we hit a nasty dynamic scoping
;;     ;; problem, since `tstree-map' also binds the symbol `function'
;;     ;; (let ((dictree-map-function function))
      (tstree-map
       `(lambda (key data)
	  (funcall ,function key (dictree--get-data data)))
       (dictree--tstree dict) type));)
)



(defun dictree-mapcar (function dict)
  "Apply FUNCTION to all entries in dictionary DICT,
and make a list of the results.

FUNCTION will be passed two arguments: a key from the
dictionary, and the data associated with that key. It is safe to
assume the dictionary entries will be traversed in alphabetical
order."
  
  (if (dictree--lookup-only dict)
      (let (result)
	(maphash `(lambda function (key data)
		    (cons (,function key data) result))
		 (dictree--lookup-hash dict))
	result)
    ;; need to "rename" `function' or we hit a nasty dynamic scoping
    ;; problem, since `tstree-map' also binds the symbol `function'
    (let ((dictree-map-function function))
      (tstree-map
       (lambda (key data)
	 (funcall dictree-map-function key (dictree--get-data data)))
       (dictree--tstree dict) t t)))
)



(defun dictree-size (dict)
  "Return the number of entries in dictionary DICT."
  (interactive (list (read-dict "Dictionary: ")))

  ;; lookup-only
  (if (dictree--lookup-only dict)
      (if (not (dictree--meta-dict-p dict))
	  ;; normal dictionary
	  (hash-table-size (dictree--lookup-hash dict))
	;; meta-dictionary
	(let ((count 0))
	  (mapc (lambda (dic) (setq count (+ count (dictree-size dic))))
		(dictree--dict-list dict))
	  count))
    ;; non lookup-only
    (let ((count 0))
      (tstree-map (lambda (&rest dummy) (setq count (1+ count)))
		  (dictree--tstree dict))
      (when (interactive-p)
	(message "Dictionary %s contains %d entries"
		 (dictree--name dict) count))
      count))
)



(defun dictree-complete
  (dict sequence &optional maxnum all combine-function filter no-cache)
  "Return an alist containing all completions of SEQUENCE
found in dictionary DICT, along with their associated data, in
the order defined by the dictionary's comparison function (see
`dictree-create'). If no completions are found, return nil.

SEQUENCE can be a single sequence or a list of sequences. If a
list is supplied, completions of all elements in the list are
returned, merged together in a single alist.

The optional numerical argument MAXNUM limits the results to the
first MAXNUM completions. If it is absent or nil, all completions
are included in the returned alist.

DICT can also be a list of dictionaries, in which case
completions are sought in all dictionaries in the list and the
results are merged together, keeping the first MAXNUM. Note that
if a key appears in more than one dictionary, the returned alist
may contain that key more than once. To have multiple
dictionaries treated as a single, combined dictionary, they
should be combined into a meta-dictionary. See
`dict-create-metadict'.

Normally, only the remaining characters needed to complete
SEQUENCE are returned. If the optional argument ALL is non-nil,
the entire completion is returned.

The optional COMBINE-FUNCTION argument overrides a
meta-dictionary's default combine-function. It is ignored if none
of the dictionaries in DICT are meta-dictionaries. See
`dict-create-metadict' for details.

The FILTER argument sets a filter function for the
completions. If supplied, it is called for each possible
completion with two arguments: the completion, and its associated
data. If the filter function returns nil, the completion is not
included in the results.

If the optional argument NO-CACHE is non-nil, it prevents caching
of the result."

  ;; ----- sort out arguments ------
  
  ;; wrap dict in a list if necessary
  (when (dictree-p dict) (setq dict (list dict)))
  
  ;; wrap sequence in a list if necessary
  ;; FIXME: this will fail if SEQUENCE is a list, and tree's reference
  ;;        type is itself a sequence (actually, there might be no way
  ;;        to fully fix this...)
  (when (or (atom sequence)
	    (and (listp sequence) (not (sequencep (car sequence)))))
    (setq sequence (list sequence)))

  
  ;; redefine filter to deal with data wrapping
  (when filter
    (setq filter (eval (macroexpand `(dictree--wrap-filter ,filter)))))
  
  
  ;; ----- search for completions -----
  
  (let (completions cmpl cache time speed combfun)
    ;; search each dictionary in the list
    (dolist (dic dict)
      ;; throw a wobbly if dictionary is lookup-only
      (when (dictree--lookup-only dic)
	(error "Dictionary is lookup-only; completion disabled"))
      ;; get meta-dictionary's combine function
      (when (dictree--meta-dict-p dic)
	(if combine-function
	    (setq combfun combine-function)
	  (setq combfun (dictree--combfun dic))))
      ;; complete each sequence in the list
     (dolist (seq sequence)
	(cond
	 
	 ;; If FILTER or COMBINE-FUNCTION was supplied, look in ternary
	 ;; search tree since we don't cache these custom searches.
	 ((or filter combine-function)
	  (setq cmpl
		(tstree-complete (dictree--tstree dic) seq maxnum
				 combfun filter)))
	 
	 
	 ;; if there's a cached result with enough completions, use it
	 ((and (setq cache
		     (if (dictree--completion-speed dic)
			 (gethash seq (dictree--completion-hash dic))
		       nil))
	       (or (null (dictree--cache-maxnum cache))
		   (and maxnum
			(<= maxnum (dictree--cache-maxnum cache)))))
	  (setq cmpl (dictree--cache-completions cache))
	  ;; drop any excess cached completions
	  (when (and maxnum (> (length cmpl) maxnum))
	    (setcdr (nthcdr (1- maxnum) cmpl) nil)))
	 
	 
	 ;; If nothing was in the cache or the cached result didn't
	 ;; contain enough completions, look in the ternary search tree
	 ;; and time it.
	 (t
	  (setq time (float-time))
	  (setq cmpl
		(tstree-complete (dictree--tstree dic)
				 seq maxnum combfun))
	  (setq time (- (float-time) time))
	  ;; If the completion function was slower than the dictionary's
	  ;; completion speed, add the results to the completion hash and
	  ;; set the dictionary's modified flag.
	  (when (and (not no-cache)
		     (setq speed (dictree--completion-speed dic))
		     (or (eq speed t) (> time speed)))
	    (dictree--set-modified dic t)
	    (puthash seq (dictree--cache-create cmpl maxnum)
		     (dictree--completion-hash dic)))))


	;; ----- construct completion list -----
	
	;; drop prefix from front of the completions if ALL is not set
	(unless all
	  (setq cmpl (mapcar
		      (lambda (s)
			(cons (dictree--subseq (car s) (length seq))
			      (cdr s)))
		      cmpl)))
	;; merge the cached completions with those already found
	(let ((sortfun `(lambda (a b)
			  (,(tstree-construct-sortfun
			     (tstree--tree-cmpfun (dictree--tstree dic)))
			   (car a) (car b)))))
	  (setq completions (dictree--merge completions cmpl sortfun))
	  ;; drop any excess completions
	  (when (and maxnum (> (length completions) maxnum))
	    (setcdr (nthcdr (1- maxnum) completions) nil)))
	))
    
    
    ;; return the completions list, unwrapping the data
    (mapcar (lambda (c) (cons (car c) (dictree--get-data (cdr c))))
	    completions))
)



(defun dictree-complete-ordered
  (dict sequence &optional maxnum all rank-function combine-function
	                   filter no-cache)
  "Return an alist containing all completions of SEQUENCE
found in dictionary DICT, along with their associated data,
sorted according to the rank function. If no completions are found,
return nil.

Note that `dictree-complete' is significantly more efficient than
`dictree-complete-ordered', especially when a MAXNUM is
specified. Always use `dictree-complete' when you don't care
about the ordering of the completions, or you need the
completions ordered according to the dictionary's comparison
function (see `dictree-create').

SEQUENCE can be a single sequence or a list of sequences. If a
list is supplied, completions of all elements in the list are
returned, merged together in a single alist.

The optional numerical argument MAXNUM limits the results to the
\"best\" MAXNUM completions. If it is absent or nil, all
completions are included in the returned alist.

DICT can also be a list of dictionaries, in which case
completions are sought in all dictionaries in the list and the
results are merged together, keeping the \"best\" MAXNUM. Note
that if a key appears in more than one dictionary, the returned
alist may contain that key more than once. To have multiple
dictionaries treated as a single, combined dictionary, they
should be combined into a meta-dictionary. See
`dict-create-metadict'.

Normally, only the remaining characters needed to complete
SEQUENCE are returned. If the optional argument ALL is non-nil,
the entire completion is returned.

The optional argument RANK-FUNCTION over-rides the dictionary's
default rank function (see `dictree-create' for details). The
elements of the returned list are sorted according to this
rank-function, in descending order.

The optional COMBINE-FUNCTION argument overrides a
meta-dictionary's default combine-function. It is ignored if none
of the dictionaries in DICT are meta-dictionaries. See
`dict-create-metadict' for details.

The FILTER argument sets a filter function for the
completions. If supplied, it is called for each possible
completion with two arguments: the completion, and its associated
data. If the filter function returns nil, the completion is not
included in the results.

If the optional argument NO-CACHE is non-nil, it prevents caching
of the result."

  (let (rankfun combfun completions seq cmpl time speed cache)
    ;; wrap dict in a list if necessary
    (when (dictree-p dict) (setq dict (list dict)))

    ;; ----- sort out arguments -----
    
    ;; wrap sequence in a list if necessary
    ;; FIXME: this will fail if SEQUENCE is a list, and tree's reference
    ;;        type is itself a sequence (actually, there might be no way
    ;;        to fully fix this...)
    (when (or (atom sequence)
	      (and (listp sequence) (not (sequencep (car sequence)))))
      (setq sequence (list sequence)))
    
    (if rank-function
	;; redefine supplied rank-function to deal with data wrapping
	(setq rankfun
	      (eval (macroexpand
		     `(dictree--wrap-rankfun ,rank-function))))
      ;; Note: we default to the rank function of first dict in list, and
      ;;       hope it's compatible with the data in the other
      ;;       dictionaries
      (setq rankfun (dictree--rankfun (car dict))))
    
    ;; redefine filter to deal with data wrapping
    (when filter
      (setq filter (eval (macroexpand `(dictree--wrap-filter ,filter)))))
    

    ;; ----- search for completions -----
    
    ;; search each dictionary in the list
    (dolist (dic dict)
      ;; throw a wobbly if dictionary is lookup-only
      (when (dictree--lookup-only dic)
	(error "Dictionary is lookup-only; completion disabled"))
      ;; get meta-dictionary's combine function
      (when (dictree--meta-dict-p dic)
	(if combine-function
	    (setq combfun combine-function)
	  (setq combfun (dictree--combfun dic))))
      ;; complete each sequence in the list
      (dolist (seq sequence)
	(cond
	 
	 ;; If the default rank-function or combine-function have been
	 ;; over-ridden or a filter supplied, look in the ternary search
	 ;; tree since we don't cache these non-default searches.
	 ((or rank-function filter combine-function)
	  (setq cmpl
		(tstree-complete-ordered (dictree--tstree dic)
					 sequence maxnum
					 rankfun combfun filter)))
	 
	 
	 ;; if there's a cached result with enough completions, use it
	 ((and (setq cache (if (dictree--ordered-speed dic)
			       (gethash seq (dictree--ordered-hash dic))
			     nil))
	       (or (null (dictree--cache-maxnum cache))
		   (and maxnum
			(<= maxnum (dictree--cache-maxnum cache)))))
	  (setq cmpl (dictree--cache-completions cache))
	  ;; drop any excess cached completions
	  (when (and maxnum (> (length cmpl) maxnum))
	    (setcdr (nthcdr (1- maxnum) cmpl) nil)))
	 
	 
	 ;; If nothing was in the cache or the cached result didn't
	 ;; contain enough completions, search tree and time the search.
	 (t
	  (setq time (float-time))
	  (setq cmpl (tstree-complete-ordered (dictree--tstree dic)
					      seq maxnum rankfun combfun))
	  (setq time (- (float-time) time))
	  ;; If the completion function was slower than the dictionary's
	  ;; completion speed, add the results to the completion cache and
	  ;; set the dictionary's modified flag.
	  (when (and (not no-cache)
		     (setq speed (dictree--ordered-speed dic))
		     (or (eq speed t) (> time speed)))
	    (dictree--set-modified dic t)
	    (puthash seq (dictree--cache-create cmpl maxnum)
		     (dictree--ordered-hash dic)))))
	
	
	;; ----- construct completion list -----
	
	;; drop prefix from front of the completions if ALL is not set
	(unless all
	  (setq cmpl (mapcar
		      (lambda (s)
			(cons (dictree--subseq (car s) (length seq))
			      (cdr s)))
		      cmpl)))
	;; merge the cached completions with those already found
	(setq completions (dictree--merge completions cmpl rankfun))
	;; drop any excess completions
	(when (and maxnum (> (length completions) maxnum))
	  (setcdr (nthcdr (1- maxnum) completions) nil))
	))
    
    
    ;; return the completions list, unwrapping the data
    (mapcar (lambda (c) (cons (car c) (dictree--get-data (cdr c))))
	    completions))
)



(defun dictree-populate-from-file (dict file)
  "Populate dictionary DICT from the key list in file FILE.

Each line of the file should contain a key, either a string
\(delimeted by \"\), a vector or a list. (Use the escape sequence
\\\" to include a \" in a string.) If a line does not contain a
key, it is silently ignored. The keys should ideally be sorted
\"alphabetically\", as defined by the dictionary's
comparison-function \(see `dictree-create'\).

Each line can optionally include data and meta-data to be
associated with the key, separated from each other and the key by
whitespace.


Technicalities:

The key, data and meta-data are read as lisp expressions using
`read', and are read from the middle outwards, i.e. first the
middle key is read, then the key directly after it, then the key
directly before it, then the one two lines after the middle, and
so on. Assuming the keys in the file are sorted
\"alphabetically\", this helps produce a reasonably efficient
dictionary structure."
  
  (save-excursion
    (let ((buff (generate-new-buffer " *dictree-populate*")))
      ;; insert the key list into a temporary buffer
      (set-buffer buff)
      (insert-file-contents file)
      
      ;; insert the keys starting from the median to ensure a reasonably
      ;; well-balanced tree
      (let* ((lines (count-lines (point-min) (point-max)))
	     (midpt (+ (/ lines 2) (mod lines 2)))
	     entry)
        ;; insert the median key and set the dictionary's modified flag
	(dictree-goto-line midpt)
	(when (setq entry (dictree-read-line))
	  (dictree-insert dict (car entry) (nth 1 entry))
	  (dictree-set-meta-data dict (car entry) (nth 2 entry)))
	(message "Inserting keys in %s...(1 of %d)"
		 (dictree--name dict) lines)
        ;; insert keys successively further away from the median in both
        ;; directions
	(dotimes (i (1- midpt))
	  (dictree-goto-line (+ midpt i 1))
	  (when (setq entry (dictree-read-line))
	    (dictree-insert dict (car entry) (nth 1 entry))
	    (dictree-set-meta-data dict (car entry) (nth 2 entry)))
	  (when (= 49 (mod i 50))
	    (message "Inserting keys in %s...(%d of %d)"
		     (dictree--name dict) (+ (* 2 i) 2) lines))
	  (dictree-goto-line (- midpt i 1))
	  (when (setq entry (dictree-read-line))
	    (dictree-insert dict (car entry) (nth 1 entry))
	    (dictree-set-meta-data dict (car entry) (nth 2 entry))))
	
        ;; if file contains an even number of keys, we still have to add
        ;; the last one
	(when (= 0 (mod lines 2))
	  (dictree-goto-line lines)
	  (when (setq entry (dictree-read-line))
	    (dictree-insert dict (car entry) (nth 1 entry))
	    (dictree-set-meta-data dict (car entry) (nth 2 entry))))
	(message "Inserting keys in %s...done" (dictree--name dict)))
      
      (kill-buffer buff)))
)



;;; FIXME: doesn't fail gracefully if file has invalid format
(defun dictree-read-line ()
  "Return a cons containing the key and data \(if any, otherwise
nil\) at the current line of the current buffer. Returns nil if
line is in wrong format."
  
  (save-excursion
    (let (key data meta-data)
      ;; search for text between quotes "", ignoring escaped quotes \"
      (beginning-of-line)
      (setq key (read (current-buffer)))
      ;; if there is anything after the quoted text, use it as data
      (if (eq (line-end-position) (point))
	  (list key)
	(setq data (read (current-buffer)))
	(if (eq (line-end-position) (point))
	    (list key data)
	  (setq meta-data (read (current-buffer)))
	  ;; return the key and data
	  (list key data meta-data)))
      ))
)



(defun dictree-save-modified (&optional dict ask compilation)
  "Save all modified dictionaries that have a non-nil autosave flag.

If optional argument DICT is a list of dictionaries or a single
dictionary, only save those (even if their autosave flags are not
set). If DICT is non-nil but not a list of dictionaries, save all
dictionaries, irrespective of their autosave flag. Interactively,
this can be set by supplying a prefix argument.

If optional argument ASK is non-nil, ask for confirmation before
saving.

Optional argument COMPILATION determines whether to save the
dictionaries in compiled or uncompiled form. The default is to
save both forms. See `dictree-write'."

  ;; sort out DICT argument
  (cond
   ((dictree-p dict) (setq dict (list dict)))
   ((and (listp dict) (dictree-p (car dict))))
   (dict (setq dict 'all)))
  
  ;; For each dictionary in list / each loaded dictionary, check if dictionary
  ;; has been modified. If so, save it if autosave is on or if saving all
  (dolist (dic (if (or (null dict) (eq dict 'all))
		    dictree-loaded-list
		  dict))
    (when (and (dictree--modified dic)
	       (or (eq dict 'all) (dictree--autosave dic))
	       (or (not ask)
		   (y-or-n-p (format "Save modified dictionary %s? "
				     (dictree--filename dic)))))
      (dictree-save dic compilation)
      (dictree--set-modified dic nil)))
)



(defun dictree-save (dict &optional compilation)
  "Save dictionary DICT to it's associated file.
Use `dictree-write' to save to a different file.

Optional argument COMPILATION determines whether to save the
dictionary in compiled or uncompiled form. The default is to save
both forms. See `dictree-write'."
  
  (let* ((filename (dictree--filename dict)))
    
    ;; if dictionary has no associated file, prompt for one
    (unless (and filename (> (length filename) 0))
      (setq filename
	    (read-file-name
	     (format "Save %s to file (leave blank to NOT save): "
		     (dictree--name dict))))
      (dictree--set-filename dict filename))
    
    ;; if filename is blank, don't save
    (if (string= filename "")
	(message "Dictionary %s NOT saved" (dictree--name dict))
      ;; otherwise write dictionary to file without requiring confirmation
      (dictree-write dict filename t compilation)))
)



(defun dictree-write (dict filename &optional overwrite compilation)
  "Write dictionary DICT to file FILENAME.

If optional argument OVERWRITE is non-nil, no confirmation will
be asked for before overwriting an existing file.

The default is to create both compiled and uncompiled versions of
the dictionary, with extensions .elc and .el respectively (if
FILENAME has either of these extensions, they are stripped off
before proceeding). The compiled version is always used in
preference to the uncomplied version, as it loads
faster. However, only the uncompiled version is portable between
different Emacs versions.

If optional argument COMPILATION is the symbol 'compiled, only
the uncompiled version will be created, whereas if it is the
symbol 'uncompiled, only the uncompiled version will be created.

Interactivley, DICT and FILENAME are read from the minibuffer,
and OVERWRITE is the prefix argument."
  
  (let (dictname buff tmpfile)
    ;; add .el(c) extension to the filename if not already there
    (cond
     ((string= (substring filename -3) ".el")
      (setq filename (substring filename 0 -3)))
     ((string= (substring filename -4) ".elc")
      (setq filename (substring filename 0 -4))))
    
    ;; remove .el(c) extension from filename to create saved dictionary
    ;; name
    (setq dictname (file-name-nondirectory filename))
    
    (save-excursion
      ;; create a temporary file
      (setq buff
	    (find-file-noselect (setq tmpfile (make-temp-file dictname))))
      (set-buffer buff)
      ;; call the appropriate write function to write the dictionary code
      (if (dictree--meta-dict-p dict)
	  (dictree-write-meta-dict-code dict dictname)
	(dictree-write-dict-code dict dictname))
      (save-buffer)
      (kill-buffer buff))

    ;; prompt to overwrite if necessary
    (when (or overwrite
	      (and
	       (or (eq compilation 'compiled)
		   (not (file-exists-p (concat filename ".el"))))
	       (or (eq compilation 'uncompiled)
		   (not (file-exists-p (concat filename ".elc")))))
	      (y-or-n-p
	       (format "File %s already exists. Overwrite? "
		       (concat filename ".el(c)"))))
;      (condition-case nil
	  (progn
	    ;; move the uncompiled version to its final destination
	    (unless (eq compilation 'compiled)
	      (copy-file tmpfile (concat filename ".el") t))
	    ;; byte-compile and move the compiled version to its final
	    ;; destination
	    (unless (eq compilation 'uncompiled)
	      (if (save-window-excursion
		    (let ((restore byte-compile-disable-print-circle)
			  err)
		      (setq byte-compile-disable-print-circle t)
		      (setq err (byte-compile-file tmpfile))
		      (setq byte-compile-disable-print-circle restore)
		      err))
		  (rename-file (concat tmpfile ".elc")
			       (concat filename ".elc") t)
		(error))))
;	(error (error "Error saving %s. Dictionary not saved" dictname)))
      
      ;; if writing to a different name, unload dictionary under old name and
      ;; reload it under new one
      (dictree--set-modified dict nil)
      (unless (string= dictname (dictree--name dict))
	(dictree-unload dict)
	(dictree-load filename)))

    (delete-file tmpfile)
    (message "Dictionary %s saved to %s" dictname filename)
    t)  ; return t to indicate dictionary was successfully saved
)



(defun dictree-load (file)
  "Load a dictionary object from file FILE.
Returns t if successful, nil otherwise."
  (interactive "fDictionary file to load: ")
  
  ;; sort out dictionary name and file name
  (let (dictname dict)
    (when (not (string= (substring file -4) ".elc"))
      (setq file (concat file ".elc")))
    (setq dictname (substring (file-name-nondirectory file) 0 -4))
    
    ;; load the dictionary
    (load file t)
    (setq dict (eval (intern-soft dictname)))
    (when (not (dictree-p dict))
      (beep)
      (error "Error loading dictionary from %s" file))
    
    ;; ensure the dictionary name and file name associated with the
    ;; dictionary match the file it was loaded from
    (dictree--set-filename dict (expand-file-name file))
    (dictree--set-name dict dictname)
    
    ;; make sure the dictionary is in dictree-loaded-list (normally the
    ;; lisp code in the dictionary itself should do that)
    (unless (memq dict dictree-loaded-list)
      (push dict dictree-loaded-list))
    (message (format "Loaded dictionary %s" dictname)))
)



(defun dictree-unload (dict &optional dont-save)
  "Unload dictionary DICT.
If optional argument DONT-SAVE is non-nil, the dictionary will
NOT be saved even if its autosave flag is set."
  (interactive (list (read-dict "Dictionary to unload: ")
		     current-prefix-arg))
  
  ;; if dictionary has been modified, autosave is set and not overidden,
  ;; save it first
  (when (and (dictree--modified dict)
	     (null dont-save)
	     (or (eq (dictree--autosave dict) t)
		 (and (eq (dictree--autosave dict) 'ask)
		      (y-or-n-p
		       (format
			"Dictionary %s modified. Save before unloading? "
			(dictree--name dict))))))
    (dictree-save dict)
    (dictree--set-modified dict nil))
  
  ;; remove dictionary from list of loaded dictionaries and unload it
  (setq dictree-loaded-list (delq dict dictree-loaded-list))
  (unintern (dictree--name dict))
  (message "Dictionary %s unloaded" (dictree--name dict))
)



(defun dictree-dump-to-buffer (dict &optional buffer type)
  "Dump keys and their associated data
from dictionary DICT to BUFFER, in the same format as that used
by `dictree-populate-from-file'. If BUFFER exists, data will be
appended to the end of it. Otherwise, a new buffer will be
created. If BUFFER is omitted, the current buffer is used.

TYPE determines the type of sequence to use to represent the
keys, and should be one of 'string, 'vector or 'list. The default
is 'vector.

Note that if the data does not have a read syntax, the dumped
data can not be used to recreate the dictionary using
`dictree-populate-from-file'."
  
  ;; select the buffer, creating it if necessary
  (if buffer
      (setq buffer (get-buffer-create buffer))
    (setq buffer (current-buffer)))
  (set-buffer buffer)
  
  ;; move point to end of buffer and make sure it's at start of new line
  (goto-char (point-max))
  (unless (= (point) (line-beginning-position))
    (insert "\n"))
  
  ;; dump keys
  (message "Dumping keys from %s to %s..."
	   (dictree--name dict) (buffer-name buffer))
  (let ((count 0) (dictsize (dictree-size dict)))
    (message "Dumping keys from %s to %s...(key 1 of %d)"
	     (dictree--name dict) (buffer-name buffer) dictsize)
    ;; construct dump function
    (let ((dump-func
	   (lambda (key cell)
	     (when (= 99 (mod count 100))
	       (message "Dumping keys from %s to %s...(key %d of %d)"
			(dictree--name dict) (buffer-name buffer)
			(1+ count) dictsize))
	     (insert (prin1-to-string key))
	     (let (data)
	       (when (setq data (dictree--get-data cell))
		 (insert " " (prin1-to-string data)))
	       (when (setq data (dictree--get-metadata cell))
		 (insert " " (prin1-to-string data)))
	       (insert "\n"))
	     (setq count (1+ count)))))
      ;; map dump function over dictionary
      (if (dictree--lookup-only dict)
	  (maphash dump-func (dictree--lookup-hash dict))
	(tstree-map dump-func (dictree--tstree dict) type)))
    (message "Dumping keys from %s to %s...done"
	     (dictree--name dict) (buffer-name buffer)))
  (switch-to-buffer buffer)
)



(defun dictree-dump-to-file (dict filename &optional type overwrite)
  "Dump keys and their associated data
from dictionary DICT to a text file FILENAME, in the same format
as that used by `dictree-populate-from-file'. Prompts to overwrite
FILENAME if it already exists, unless OVERWRITE is non-nil.

TYPE determines the type of sequence to use to represent the
keys, and should be one of 'string, 'vector or 'list. The default
is 'vector.

Note that if the data does not have a read syntax, the dumped
data can not be used to recreate the dictionary using
`dictree-populate-from-file'."
  
  ;; check if file exists, and prompt to overwrite it if necessary
  (if (and (file-exists-p filename)
	   (not overwrite)
	   (not (y-or-n-p
		 (format "File %s already exists. Overwrite? "
			 filename))))
      (message "Key dump cancelled")
    
    (let (buff)
      ;; create temporary buffer, dump keys to it, and save to FILENAME
      (setq buff (generate-new-buffer filename))
      (save-window-excursion
	(dictree-dump-to-buffer dict buff type)
	(write-file filename))
      (kill-buffer buff)))
)





;;; ==================================================================
;;;                   Internal dictionary functions

(defun dictree-update-cache (dict key newdata &optional deleted)
  "Synchronise dictionary DICT's caches,
given that the data associated with KEY has been changed to
NEWDATA, or KEY has been deleted if DELETED is non-nil (NEWDATA
is ignored in that case)."

  (let (seq cache entry cmpl maxnum)
    
    ;; synchronise the lookup cache if dict is a meta-dictionary,
    ;; since it's not done automatically
    (when (and (dictree--meta-dict-p dict)
	       (dictree--lookup-speed dict)
	       (gethash key (dictree--lookup-hash dict)))
      (if deleted
	  (remhash key (dictree--lookup-hash dict))
	(puthash key newdata (dictree--lookup-hash dict))))
    
    
    ;; synchronize the completion hash, if it exists
    (when (dictree--completion-speed dict)
      ;; have to check every possible subsequence that could be cached!
      (dotimes (i (1+ (length key)))
	(setq seq (substring key 0 i))	  
	(when (setq cache (gethash seq (dictree--completion-hash dict)))
	  (setq cmpl (dictree--cache-completions cache))
	  (setq maxnum (dictree--cache-maxnum cache))
	  ;; If key has not been deleted, and is already in the
	  ;; completion list, only update it if dict is a meta-dictionary
	  ;; (since it's not updated automatically).
	  (if (and (not deleted) (setq entry (assoc key cmpl)))
	      (when (dictree--meta-dict-p dict)
		(setcdr entry (dictree-lookup dict key)))
	    ;; Otherwise...
	    ;; (Note: we could avoid looking in the tree by adding the key
	    ;; to the cache list, re-sorting alphabetically, and deleting
	    ;; the last key in the list, but it's probably not worth it,
	    ;; and would deny us the opportunity of shrinking the cache.)
	    (let (time newcmpl)
	      ;; re-complete from the tree
	      (setq time (float-time))
	      (setq newcmpl
		    (tstree-complete (dictree--tstree dict) seq maxnum))
	      (setq time (- (float-time) time))
	      ;; if the lookup still takes too long, update the cache,
	      ;; otherwise delete the cache entry
	      (if (or (eq (dictree--completion-speed dict) t)
		      (> time (dictree--completion-speed dict)))
		  (dictree--set-cache-completions cache newcmpl)
		(remhash seq (dictree--completion-hash dict))))
	    ))))
    
    
    ;; synchronize the ordered completion hash, if it exists
    (when (dictree--ordered-speed dict)
      ;; have to check every possible subsequence that could
      ;; be cached!
      (dotimes (i (1+ (length key)))
	(setq seq (dictree--subseq key 0 i))	  
	(when (setq cache (gethash seq (dictree--ordered-hash dict)))
	  (setq cmpl (dictree--cache-completions cache))
	  (setq maxnum (dictree--cache-maxnum cache))
	  (cond

	   ;; if key was deleted, have to update cache from the tree
	   (deleted
	    (let (time newcmpl)
	      ;; re-complete from the tree
	      (setq time (float-time))
	      (setq newcmpl (tstree-complete-ordered
			     (dictree--tstree dict) seq maxnum))
	      (setq time (- (float-time) time))
	      ;; if the lookup still takes too long, update the cache,
	      ;; otherwise delete the cache entry
	      (if (or (eq (dictree--ordered-speed dict) t)
		      (> time (dictree--ordered-speed dict)))
		  (dictree--set-cache-completions cache newcmpl)
		(remhash seq (dictree--ordered-hash dict)))))
	   
	   ;; if key is in the completion list...
	   ((setq entry (assoc key cmpl))
	    ;; Update the cache entry if dict is a meta-dictionary,
	    ;; since it's not done automatically.
	    (when (dictree--meta-dict-p dict)
	      (setcdr entry
		      (dictree--wrap-data (dictree-lookup dict key))))
	    ;; re-sort the list
	    (dictree--set-cache-completions
	     cache (sort cmpl (dictree--rankfun dict)))
	    (setq cmpl (dictree--cache-completions cache))
	    ;; If key is now at the end of the list, we've no choice but
	    ;; to update from the tree.
	    (when (equal (caar (last cmpl)) key)
	      (let (time newcmpl)
		;; re-complete from the tree
		(setq time (float-time))
		(setq newcmpl (tstree-complete-ordered
			    (dictree--tstree dict) seq maxnum))
		(setq time (- (float-time) time))
		;; if the lookup still takes too long, update the cache,
		;; otherwise delete the cache entry
		(if (or (eq (dictree--ordered-speed dict) t)
			(> time (dictree--ordered-speed dict)))
		    (dictree--set-cache-completions cache newcmpl)
		  (remhash seq (dictree--ordered-hash dict))))))
	   
	   ;; if key isn't in the completion list...
	   (t
	    ;; add key to the end of the list and re-sort
	    (setcdr (last cmpl) (list (cons key newdata)))
	    (dictree--set-cache-completions
	     cache (sort cmpl (dictree--rankfun dict)))
	    (setq cmpl (dictree--cache-completions cache))
	    ;; remove excess completions
	    (when (> (length cmpl) maxnum)
	      (setcdr (nthcdr (1- maxnum) cmpl) nil)))
	   )))))
)



(defun dictree-write-dict-code (dict dictname)
  "Write code for normal dictionary DICT to current buffer,
giving it the name DICTNAME."

  (let (hashcode tmpdict lookup-alist completion-alist ordered-alist)
    
    ;; if the dictionary is lookup only, dump the lookup cache to an alist
    (if (dictree--lookup-only dict)
	(progn
	  (maphash (lambda (key val) (push (cons key val) lookup-alist))
		   (dictree--lookup-hash dict))
	  ;; generate code to reconstruct the lookup hash table
	  (setq hashcode
		(concat
		 "(let ((lookup-hash (make-hash-table :test 'equal)))\n"
		 "  (mapcar (lambda (entry)\n"
		 "    (puthash (car entry) (cdr entry) lookup-hash))\n"
		 "    (dictree--lookup-hash " dictname "))\n"
		 "  (dictree--set-lookup-hash " dictname
		                              " lookup-hash)\n"))
	  ;; generate the structure to save
	  (setq tmpdict (list 'DICT dictname nil
			      (dictree--autosave dict) nil t
			      nil (dictree--insfun dict) nil
			      lookup-alist nil nil nil nil nil)))
      
      
      ;; otherwise, dump caches to alists as necessary and generate code
      ;; to reonstruct the hash tables from the alists
      (let ((lookup-speed (dictree--lookup-speed dict))
	    (completion-speed (dictree--completion-speed dict))
	    (ordered-speed (dictree--ordered-speed dict)))
	
	;; create the lookup alist, if necessary
	(when lookup-speed
	  (maphash
	   (lambda (key val)
	     (push
	      (cons key (cons
			 (mapcar 'car (dictree--cache-completions val))
			 (dictree--cache-maxnum val)))
	      lookup-alist))
	   (dictree--lookup-hash dict))
	  ;; generate code to reconstruct the lookup hash table
	  (setq hashcode
		(concat
		 hashcode
		 "(let ((lookup-hash (make-hash-table :test 'equal))\n"
		 "      (tstree (dictree--tstree " dictname ")))\n"
		 "  (mapc\n"
		 "   (lambda (entry)\n"
		 "     (puthash\n"
		 "      (car entry)\n"
		 "      (dictree--cache-create\n"
		 "       (mapcar\n"
		 "        (lambda (key)\n"
		 "          (cons key (tstree-member tstree key)))\n"
		 "        (dictree--cache-completions (cdr entry)))\n"
		 "       (dictree--cache-maxnum (cdr entry)))\n"
		 "      lookup-hash))\n"
		 "   (dictree--lookup-hash " dictname "))\n"
		 "  (dictree--set-lookup-hash " dictname
		                              " lookup-hash))\n")))
	
	;; create the completion alist, if necessary
	(when completion-speed
	  (maphash
	   (lambda (key val)
	     (push
	      (cons key (cons
			 (mapcar 'car (dictree--cache-completions val))
			 (dictree--cache-maxnum val)))
	      completion-alist))
	   (dictree--completion-hash dict))
	  ;; generate code to reconstruct the completion hash table
	  (setq
	   hashcode
	   (concat
	    hashcode
	    "(let ((completion-hash (make-hash-table :test 'equal))\n"
	    "      (tstree (dictree--tstree " dictname ")))\n"
	    "  (mapc\n"
	    "   (lambda (entry)\n"
	    "     (puthash\n"
	    "      (car entry)\n"
	    "      (dictree--cache-create\n"
	    "       (mapcar\n"
	    "        (lambda (key)\n"
	    "          (cons key (tstree-member tstree key)))\n"
	    "        (dictree--cache-completions (cdr entry)))\n"
	    "       (dictree--cache-maxnum (cdr entry)))\n"
	    "      completion-hash))\n"
	    "   (dictree--completion-hash " dictname "))\n"
	    "  (dictree--set-completion-hash " dictname
	                                     " completion-hash))\n")))
	
	;; create the ordered completion alist, if necessary
	(when ordered-speed
	  (maphash
	   (lambda (key val)
	     (push
	      (cons key (cons
			 (mapcar 'car (dictree--cache-completions val))
			 (dictree--cache-maxnum val)))
	      ordered-alist))
	   (dictree--ordered-hash dict))
	  ;; generate code to reconstruct the ordered hash table
	  (setq hashcode
		(concat
		 hashcode
		 "(let ((ordered-hash (make-hash-table :test 'equal))\n"
		 "      (tstree (dictree--tstree " dictname ")))\n"
		 "  (mapc\n"
		 "   (lambda (entry)\n"
		 "     (puthash\n"
		 "      (car entry)\n"
		 "      (dictree--cache-create\n"
		 "       (mapcar\n"
		 "        (lambda (key)\n"
		 "          (cons key (tstree-member tstree key)))\n"
		 "        (dictree--cache-completions (cdr entry)))\n"
		 "       (dictree--cache-maxnum (cdr entry)))\n"
		 "      ordered-hash))\n"
		 "   (dictree--ordered-hash " dictname "))\n"
		 "  (dictree--set-ordered-hash " dictname
		                               " ordered-hash))\n")))
	
	;; generate the structure to save
	(setq tmpdict (list 'DICT dictname nil
			    (dictree--autosave dict)
			    nil nil
			    (dictree--tstree dict)
			    (dictree--insfun dict)
			    (dictree--rankfun dict)
			    lookup-alist lookup-speed
			    completion-alist completion-speed
			    ordered-alist ordered-speed))
	))
    
    
    ;; write lisp code that generates the dictionary object
    (insert "(provide '" dictname ")\n")
    (insert "(require 'dict-tree)\n")
    (insert "(defvar " dictname " nil \"Dictionary " dictname ".\")\n")
    (insert "(setq " dictname " '" (prin1-to-string tmpdict) ")\n")
    (insert hashcode)
    (insert "(dictree--set-filename " dictname
	    " (locate-library \"" dictname "\"))\n")
    (insert "(unless (memq " dictname " dictree-loaded-list)"
	    " (push " dictname " dictree-loaded-list))\n"))
)




(defun dictree-write-meta-dict-code (dict dictname)
  "Write code for meta-dictionary DICT to current buffer,
giving it the name DICTNAME."

  (let (hashcode tmpdict lookup-alist completion-alist ordered-alist)
    
    ;; dump caches to alists as necessary and generate code to reonstruct
    ;; the hash tables from the alists
    (let ((lookup-speed (dictree--lookup-speed dict))
	  (completion-speed (dictree--completion-speed dict))
	  (ordered-speed (dictree--ordered-speed dict)))
      
      ;; create the lookup alist, if necessary
      (when lookup-speed
	(maphash (lambda (key val)
		   (push (cons key (mapcar 'car val)) lookup-alist))
		 (dictree--lookup-hash dict))
	;; generate code to reconstruct the lookup hash table
	(setq hashcode
	      (concat
	       hashcode
	       "(let ((lookup-hash (make-hash-table :test 'equal)))\n"
	       "  (mapc (lambda (entry)\n"
	       "    (puthash (car entry) (cdr entry) lookup-hash))\n"
	       "    (dictree--lookup-hash " dictname "))\n"
	       "  (dictree--set-lookup-hash " dictname
	                                    " lookup-hash))\n")))
      
      ;; create the completion alist, if necessary
      (when completion-speed
	(maphash (lambda (key val)
		   (push (cons key (mapcar 'car val)) completion-alist))
		 (dictree--completion-hash dict))
	;; generate code to reconstruct the completion hash table
	(setq hashcode
	      (concat
	       hashcode
	       "(let ((completion-hash (make-hash-table :test 'equal)))\n"
	       "  (mapc (lambda (entry)\n"
	       "    (puthash (car entry) (cdr entry) completion-hash))\n"
	       "    (dictree--completion-hash " dictname "))\n"
	       "  (dictree--set-completion-hash " dictname
	                                        " completion-hash))\n")))
      
      ;; create the ordered completion alist, if necessary
      (when ordered-speed
	(maphash (lambda (key val) (push (cons key val) ordered-alist))
		 (dictree--ordered-hash dict))
	;; generate code to reconstruct the ordered hash table
	(setq hashcode
	      (concat
	       hashcode
	       "(let ((ordered-hash (make-hash-table :test 'equal)))\n"
	       "  (mapc (lambda (entry)\n"
	       "    (puthash (car entry) (cdr entry) ordered-hash))\n"
	       "    (dictree--ordered-hash " dictname "))\n"
	       "  (dictree--set-ordered-hash " dictname
	                                     " ordered-hash))\n")))
      
      
      ;; generate the structure to save
      (setq tmpdict
	    (if (dictree--lookup-only dict)
		;; lookup-only meta-dictionary
		(list 'DICT dictname nil (dictree--autosave dict) nil t
		      nil (dictree--combfun dict) nil
		      lookup-alist lookup-speed nil nil nil nil)
	      ;; normal meta-dictionary
	      (list 'DICT dictname nil (dictree--autosave dict) nil nil
		    (mapcar 'dictree-name (dictree--dict-list dict))
		    (dictree--combfun dict) (dictree--rankfun dict)
		    lookup-alist lookup-speed
		    completion-alist completion-speed
		    ordered-alist ordered-speed))))
    
    
    ;; write lisp code that generates the dictionary object
    (insert "(provide '" dictname ")\n")
    (insert "(require 'dict-tree)\n")
    (mapc (lambda (name) (insert "(require '" name ")\n"))
	  (dictree--meta-dict-list tmpdict))
    (insert "(defvar " dictname " nil \"Dictionary " dictname ".\")\n")
    (insert "(setq " dictname " '" (prin1-to-string tmpdict) ")\n")
    (insert "(dictree--set-dict-list\n"
	    " " dictname "\n"
	    " (mapcar (lambda (name) (eval (intern-soft name)))\n"
	    "         (dictree--dict-list " dictname " )))\n")
    (insert hashcode)
    (insert "(dictree--set-filename " dictname
	    " (locate-library \"" dictname "\"))\n")
    (insert "(unless (memq " dictname " dictree-loaded-list)"
	    " (push " dictname " dictree-loaded-list))\n"))
)




(defvar dictree-history nil
  "History list for commands that read an existing ditionary name.")


(defun read-dict (prompt &optional default dictlist)
  "Read the name of a dictionary with completion, and return it.

Prompt with PROMPT. By default, return DEFAULT. If DICTLIST is
supplied, only complete on dictionaries in that list."
  (let (dictnames)
    (mapc (lambda (dict)
	    (unless (or (null (dictree--name dict))
			(member (dictree--name dict) dictnames))
	      (push (list (dictree--name dict)) dictnames)))
	  (or dictlist dictree-loaded-list))
    (eval (intern-soft
	   (completing-read prompt dictnames
			    nil t nil 'dictree-history default))))
)



;; Add the dictree-save-modified function to the kill-emacs-hook to save
;; modified dictionaries when exiting emacs
(add-hook 'kill-emacs-hook 'dictree-save-modified)



;;; dict-tree.el ends here
