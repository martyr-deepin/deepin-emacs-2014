
;;; tstree.el --- ternary search tree package


;; Copyright (C) 2004-2007 Toby Cubitt

;; Author: Toby Cubitt <toby-predictive@dr-qubit.org>
;; Version: 0.7.4
;; Keywords: ternary search tree, tstree
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

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
;; MA 02110-1301, USA.


;;; Commentary:
;;
;; A ternary search tree associates data with keys. The keys can be
;; any ordered sequence of elements: vector, list or string. It stores
;; them in such a way that both storage size and data lookup are
;; reasonably space- and time-efficient, respectively. But more
;; importantly, returning all strings with a given prefix in
;; alphabetical or any other sort-order is also time-efficient.
;;
;; A ternary search tree consists of two cons cells, the first one
;; holding the tag 'TSTREE in the car cell and the second one having
;; the tree in the car and the compare function in the cdr cell. The
;; compare function must take two arguments of the type which is to
;; be stored in the tree and must return a negative value if the
;; first argument is "less than" the second, a positive value if the
;; first argument is "greater than" the second, and zero if the two
;; arguments are "equal".
;;
;; You create a ternary search tree using `tstree-create', add data to
;; it using `tstree-insert', lookup data using `tstree-member', find
;; completions of a sequence and their associated data using
;; `tstree-complete', find completions and sort them in an order you
;; specify using `tstree-complete-ordered', and map over it using
;; `tstree-map' and `tstree-mapcar'. Various other useful functions
;; are also provided.
;;
;; This package uses the ternary heap package heap.el.


;;; Change Log:
;;
;; Version 0.7.4
;; * fixed bug in `tstree-map' so it works when tree contains deleted
;;   data nodes (thanks to Mathias Dahl for noticing this)
;;
;; Version 0.7.3
;; * fixed bug in `tstree-construct-sortfun' that caused keys to be
;;   sorted in reverse order
;;
;; Version 0.7.2
;; * fixed `tstree-complete' and `tstree-complete-ordered' so they work
;;   when tree contains deleted data nodes
;;
;; Version 0.7.1
;; * fixed bugs in `tstree-map', `tstree-complete' and
;;   `tstree-complete-ordered' that resulted in the tree not being
;;   traversed "alphabetically"
;;
;; Version 0.7
;; * finally wrote a `tstree-delete' function!
;; * minor changes to functions that use equal child of a node, to
;;   avoid errors caused by null equal children left by deleting keys
;; * renamed "string" to "key" or "sequence" in function arguments
;;
;; Version 0.6.1
;; * modifications required by changes to heap.el
;;
;; Version 0.6
;; * lists of trees now act as one combined tree when passed as
;;   arguments to most functions
;; * data in a tree can now be referenced by any sequence type
;; * removed cl dependency
;;
;; Version 0.5.2
;; * fixed internal function and macro names
;;
;; Version 0.5.1
;; * added more commentary
;;
;; Version 0.5
;; * completion functions now return lists instead of vectors
;;
;; Version 0.4
;; * removed elib dependency by replacing elib stacks with lists
;;
;; Version 0.3
;; * added `tstree-mapcar' macro
;;
;; Version 0.2.1
;; * fixed bug in `tstree-map' so it doesn't error on empty trees
;;
;; Version 0.2
;; * added `tstree-map' function
;;
;; Version 0.1
;; * initial release



;;; Code:

(provide 'tstree)
(require 'heap)




;;; ================================================================
;;;                Replacements for CL functions

(defmacro tstree--signum (n)  ; INTERNAL USE ONLY
  ;; Return 1 if x is positive, -1 if negativs, 0 if zero.
  `(cond ((> ,n 0) 1) ((< ,n 0) -1) (t 0))
)




;;; ================================================================
;;;  Internal functions for use in the ternary search tree package


(defmacro tstree--tree-root (tree)  ; INTERNAL USE ONLY.
  ;; Return the root node for a ternary search tree.
  `(tstree--node-equal (car (cdr ,tree))))


(defmacro tstree--tree-dummyroot (tree)  ; INTERNAL USE ONLY.
  ;; Return the dummy node of a ternary search tree.
  `(car (cdr ,tree)))


(defmacro tstree--tree-cmpfun (tree)  ; INTERNAL USE ONLY.
  ;; Return the compare function of ternary search tree TREE.
  `(car (cdr (cdr ,tree))))


(defmacro tstree--tree-insfun (tree)  ; INTERNAL USE ONLY.
  ;; Return the insert function of ternary search tree TREE.
  `(car (cdr (cdr (cdr ,tree)))))


(defmacro tstree--tree-rankfun (tree)  ; INTERNAL USE ONLY
  ;; Return the rank function of ternary search tree TREE.
  `(cdr (cdr (cdr (cdr ,tree)))))



(defmacro tstree--node-create (low equal high split)
  ;; INTERNAL USE ONLY.
  ;; Create a TST node from LOW, EQUAL, HIGH and SPLIT.
  ;; Note: If SPLIT is nil, EQUAL stores data rather than a pointer
  `(vector ,low ,equal ,high ,split))


(defmacro tstree--node-p (obj)  ; INTERNAL USE ONLY
  ;; Return t if OBJ is a valid ternary search tree node, nil
  ;; otherwise.
  `(and (vectorp ,obj) (= (length ,obj) 4)))


(defmacro tstree--node-low (node)  ; INTERNAL USE ONLY.
  ;; Return the low pointer of NODE.
  `(aref ,node 0))


(defmacro tstree--node-set-low (node newlow)  ; INTERNAL USE ONLY.
  ;; Set the low pointer of NODE to NEWLOW
  `(aset ,node 0 ,newlow))


(defmacro tstree--node-equal (node)  ; INTERNAL USE ONLY.
  ;; Return the equal pointer of NODE.
  `(aref ,node 1))


(defmacro tstree--node-set-equal (node newequal)  ; INTERNAL USE ONLY.
  ;; Set the equal pointer of NODE to NEWEQUAL
  `(aset ,node 1 ,newequal))


(defmacro tstree--node-high (node)  ; INTERNAL USE ONLY.
  ;; Return the high pointer of NODE.
  `(aref ,node 2))


(defmacro tstree--node-set-high (node newhigh)  ; INTERNAL USE ONLY.
  ;; Set the high pointer of NODE to NEWHIGH
  `(aset ,node 2 ,newhigh))


(defmacro tstree--node-split (node)  ; INTERNAL USE ONLY.
  ;; Return the split value of NODE.
  `(aref ,node 3))


(defmacro tstree--node-set-split (node newsplit)  ; INTERNAL USE ONLY.
  ;; Set the split value of NODE to NEWSPLIT
  `(aset ,node 3 ,newsplit))


(defmacro tstree--node-branch (node d)  ; INTERNAL USE ONLY.
  ;; For D negative, zero, or positive, return the low, equal or high
  ;; pointer of NODE respectively.
  `(aref ,node (1+ (tstree--signum ,d))))


(defmacro tstree--node-set-branch (node d newbranch)
  ;; INTERNAL USE ONLY.
  ;; If D is negative, zero or positive, set the high, equal or low
  ;; value respectively of NODE to NEWBRANCH.
  `(aset ,node (1+ (tstree--signum ,d)) ,newbranch))



(defun tstree--node-find (tree sequence)  ; INTERNAL USE ONLY
  ;; Returns the node corresponding to SEQUENCE, or nil if none found.
  
  (cond
   ;; don't search for nil!
   ((null sequence) nil)
   ;; return root node if searching for an empty sequence
   ((= 0 (length sequence)) (tstree--tree-root tree))
   ;; otherwise search for node corresponding to sequence
   (t (let ((cmpfun (tstree--tree-cmpfun tree))
	    (node (tstree--tree-root tree))
	    (c 0) (chr (elt sequence 0)) (d 0)
	    (len (length sequence)))
	
        ;; as long as we keep finding nodes, keep descending the tree
	(while (and node (< c len))
	  (setq d (funcall cmpfun chr (tstree--node-split node)))
	  (if (= 0 d)
	      (when (< (setq c (1+ c)) len)
		(setq chr (elt sequence c))))
	  (setq node (tstree--node-branch node d)))
	node))
  )
)



(defmacro tstree-construct-sortfun (cmpfun)
  "Construct function to compare keys, based on a CMPFUN
that compares individual elements of that type."
  `(lambda (a b)
     (let (cmp)
       (catch 'compared
	 (dotimes (i (min (length a) (length b)))
	   (setq cmp (funcall ,cmpfun (elt a i) (elt b i)))
	   (cond ((< cmp 0) (throw 'compared t))
		 ((> cmp 0) (throw 'compared nil))))
	 (< (length a) (length b)))))
)




;;; ================================================================
;;;    The public functions which operate on ternary search trees.


(defun tstree-create (&optional compare-function insert-function
				rank-function)
  "Create an empty ternary search tree. If no arguments are
supplied, it creates a tree suitable for storing strings with
numerical data.

The optional COMPARE-FUNCTION sets the comparison function for
the tree. COMPARE-FUNCTION takes two arguments, A and B, and
returns a negative value if A is less than B, zero if A is equal
to B, and a positive value if A is greater than B. It defaults to
subtraction.

The optional INSERT-FUNCTION takes two arguments of the type
stored as data in the tree or nil, and returns the same type. It
defaults to \"replace\". See `tstree-insert'.

The optional RANK-FUNCTION takes two arguments, each a cons whose
car is a sequence referencing data in the tree, and whose cdr is
the data at that reference. It should return non-nil if the first
argument is \"better than\" the second, nil otherwise. It
defaults to numerical comparison of the data using \"greater
than\". Used by `tstree-complete-ordered' to rank completions."

         ;; comparison-function defaults to -
  (let* ((cmpfun (if compare-function compare-function '-))
	 ;; the lambda expression redefines the compare funtion to
	 ;; ensure that all values other than nil are "greater" than
	 ;; nil
	 (cmpfun `(lambda (a b)
		    (cond ((and (null a) (null b)) 0) ((null a) -1)
			  ((null b) 1) (t (,cmpfun a b)))))
	 ;; insert-function defaults to "replace".
	 (insfun (if insert-function insert-function (lambda (a b) a)))
	 ;; rank function defaults to >
	 (rankfun (if rank-function rank-function
		    (lambda (a b) (> (cdr a) (cdr b))))))
  
    (cons 'TSTREE
	  (cons (tstree--node-create nil nil nil t)
		(cons cmpfun
		      (cons insfun rankfun))))
  )
)




(defun tstree-p (obj)
  "Return t if OBJ is a ternary search tree, nil otherwise."
  (eq (car-safe obj) 'TSTREE)
)



(defun tstree-compare-function (tree)
  "Return the comparison function for the ternary search tree TREE."
  (tstree--tree-cmpfun tree)
)



(defun tstree-insert-function (tree)
  "Return the insertion function for the ternary search tree TREE."
  (tstree--tree-insfun tree)
)



(defun tstree-rank-function (tree)
  "Return the rank function for the ternary seach tree TREE."
  (tstree--tree-rankfun tree)
)



(defun tstree-empty (tree)
  "Return t if the ternary search tree TREE is empty, nil otherwise."
  (null (tstree--tree-root tree))
)



(defun tstree-insert (tree key &optional data insert-function)
  "Associate KEY with the result of the TREE's insertion function
called with two arguments: DATA, and the existing data associated
with KEY (or nil if key has no association). KEY must be a
sequence containing the type used to reference data in the tree.

The optional INSERT-FUNCTION over-rides the tree's own insertion
function. It should take two arguments of the type stored as data
in the tree, or nil. The first is the data DATA, the second is
the data associated with KEY, or nil if KEY doesn't yet exist. It
should return the same type. The return value is stored in the
tree."
  
  ;; don't add empty keys to the tree
  (if (= 0 (length key)) nil
    
    (let ((cmpfun (tstree--tree-cmpfun tree))
	  (insfun (if insert-function insert-function
		    (tstree--tree-insfun tree)))
	  (node (tstree--tree-dummyroot tree))
	  (c 0) (chr (elt key 0)) (d 0)
	  (len (length key)) newdata)
      
      ;; as long as we keep finding nodes, keep descending the tree
      (while (and node (tstree--node-branch node d))
	(setq node (tstree--node-branch node d))
	(setq d (funcall cmpfun chr (tstree--node-split node)))
	(when (= 0 d)
	  (if (< (setq c (1+ c)) len)
	      (setq chr (elt key c))
	    ;; if complete key already exists in the tree and
	    ;; we've found the data node, insert new data
	    (if (tstree--node-split node)
		(setq chr nil)  ; not at data node so keep descending
	      (tstree--node-set-equal
	       node (setq newdata
			  (funcall insfun data
				   (tstree--node-equal node))))
	      (setq node nil)))))  ; forces loop to exit
      
      ;; once we've found one node that doesn't exist, must create all
      ;; others
      (while node
	;; create nodes for remainder of key, if any
	(if (< c len)
	    (progn
	      (setq chr (elt key c))
	      (tstree--node-set-branch
	       node d (tstree--node-create nil nil nil chr))
	      (setq node (tstree--node-branch node d))
	      (setq d 0)
	      (setq c (1+ c)))
	  ;; if we've reached end of key, create data node and exit
	  (tstree--node-set-branch
	   node d (tstree--node-create
		   nil (setq newdata (funcall insfun data nil))
		   nil nil))
	  (setq node nil)))  ; fores loop to exit
      
      ;; return the newly inserted data
      newdata)
  )
)




(defun tstree-member (tree key &optional combine-function)
  "Return the data associated with KEY in the tree TREE,
or nil if KEY has no association.

Note: this will not distinguish between a non-existant KEY and
a KEY whose data is nil. Use `tstree-member-p' instead.

If TREE is a list of trees, the return value will be created by
combining data from all trees containing KEY, by calling
COMBINE-FUNCTION on pairs of data. COMBINE-FUNCTION defaults to
the insersion function of the first tree in the list."

  ; wrap tree in a list if not already
  (when (tstree-p tree) (setq tree (list tree)))

  
  (let (data node)
    ;; loop over all trees
    (dotimes (i (length tree))
      
      ;; find first node corresponding to KEY
      (setq node (tstree--node-find (nth i tree) key))
      
      ;; keep following the low branch until we find the data node, or
      ;; can't go any further (Note: no need to deal with deleted data
      ;; nodes specially, since they have null equal nodes anyway and
      ;; will return the right thing, namely nil)
      (while (tstree--node-p node)
	(setq node (if (tstree--node-split node) (tstree--node-low node)
		     (tstree--node-equal node))))
      ;; combine data
      (setq data (if combine-function
		     (funcall combine-function data node)
		   node)))

    ;; return combined data
    data)
)



(defun tstree-member-p (tree key)
  "Return t if KEY is in tree TREE, nil otherwise."
  
  (let ((node (tstree--node-find tree key)))
    ;; keep descending low child until we find data node or can't go any
    ;; further
    (while (tstree--node-p node)
      (setq node
	    (if (tstree--node-split node) (tstree--node-low node)
	      ;; data nodes flagged as deleted don't count (they have a
	      ;; low child set to the symbol 'deleted)
	      (if (eq (tstree--node-low node) 'deleted) nil t))))
    node)
)



;; Deleting keys from a ternary search tree is a messy
;; operation. Basically, either the tree has to be left with redundant
;; nodes, including nodes with null equal children, or the sub-tree below
;; the key needs to be restructured.
;;
;; Possible solutions are either to leave the redundant nodes in the
;; tree, or delete the entire sub-tree then add the keys it contained
;; back again. Both are imperfect: the former because it leaves the tree
;; with redundant nodes that make the tree slightly less efficient; the
;; latter because it could potentially end up recreating almost the
;; entire tree, making it very inefficient.
;;
;; The following function adopts the former solution: it leaves the tree
;; with redundant nodes (though deleting all keys from the tree will
;; result in an empty tree again). Since data nodes can never have a
;; low-child (they have a null split value, and the comparison function
;; ensures everything is larger than nil), deleted data nodes are flagged
;; by setting their low-child cell to the symbol 'deleted.

(defun tstree-delete (tree key)
  "Delete KEY and its associated data from TREE.
Returns non-nil if KEY was deleted, nil if KEY was not in TREE."
  (let ((node (tstree--tree-root tree))
	stack)
    
    ;; as long as we keep finding nodes, keep descending the tree and
    ;; adding the nodes to the stack
    (let ((cmpfun (tstree--tree-cmpfun tree))
	  (c 0) (chr (elt key 0)) (d 0)
	  (len (length key)))
      (push (tstree--tree-dummyroot tree) stack)
      (while (and node (< c len))
	(push node stack)
	(setq d (funcall cmpfun chr (tstree--node-split node)))
	(if (= 0 d)
	    (when (< (setq c (1+ c)) len) (setq chr (elt key c))))
	(setq node (tstree--node-branch node d))))
    ;; keep adding the low branch to the stack until we find the data
    ;; node, or can't go any further
    (while (tstree--node-p node)
      (push node stack)
      (setq node (if (tstree--node-split node) (tstree--node-low node)
		   (tstree--node-equal node))))
    (setq node (pop stack))
    
    (cond
     ;; if KEY is not in TREE, return nil
     ((or (tstree--node-split node)
	  (eq (tstree--node-low node) 'deleted))
      nil)
     ;; if KEY is in TREE, recurse up the stack deleting the nodes,
     ;; until we reach a node that has a branch other than the one
     ;; containing KEY
     (t
      (let (parent)
	(setq parent (car stack))
	(tstree--node-set-equal node nil)
	;; flag data node as deleted, in case it has to be left in the
	;; tree because there are branches below it
	(tstree--node-set-low node 'deleted)
	(while (and parent
		    (or (null (tstree--node-low node))
			(eq (tstree--node-low node) 'deleted))
		    (null (tstree--node-equal node))
		    (null (tstree--node-high node)))
	  (cond
	   ((eq node (tstree--node-low parent))
	    (tstree--node-set-low parent nil))
	   ((eq node (tstree--node-equal parent))
	    (tstree--node-set-equal parent nil))
	   ((eq node (tstree--node-high parent))
	    (tstree--node-set-high parent nil)))
	  (setq node (pop stack))
	  (setq parent (car stack)))
	;; return t to indicate successful deletion
	t))
     ))
)



(defun tstree-map (function tree &optional type mapcar)
  "Apply FUNTION to all elements in the ternary search tree TREE,
for side-effects only.

FUNCTION will be passed two arguments: a key and its associated
data. It is safe to assume the tree will be traversed in
\"lexical\" order (i.e. the order defined by the tree's
comparison function).

If TREE is a list of trees, FUNCTION will be mapped over all trees in
the list, in order.

Optional argument TYPE should be one of the symbols 'string,
'list, or 'vector (default is 'vector). It defines which type of
sequence is passed to FUNCTION. If TYPE is 'string, it must be
possible to apply the function `string' to the type used to
reference data in the tree.

\(If optional argument MAPCAR is non-nil, a list of results of
function calls is returned. Don't use this. Use the
`tstree-mapcar' macro instead\)."

  ;; wrap tree in list if not already
  (when (tstree-p tree) (setq tree (list tree)))
  
  (let (stack str node result accumulate)
    ;; loop over all trees in list
    (dolist (tr tree)
      ;; only do something if tree is not empty
      (when (tstree--tree-root tr)
	;; initialise the stack
	(push (tstree--tree-root tr) stack)
	(cond
	 ((eq type 'string) (push "" stack))
	 ((eq type 'list) (push () stack))
	 (t (push [] stack)))
	
	;; keep going until we've traversed all nodes (stack is empty)
	(while (not (null stack))
	  (setq str (pop stack))
	  (setq node (pop stack))
	  
	  ;; add the high child to the stack, if it exists
	  (when (tstree--node-high node)
	    (push (tstree--node-high node) stack)
	    (push str stack))
	  
	  ;; if we're at a data node that hasn't been flagged as deleted, call
	  ;; FUNCTION, otherwise add the equal child to the stack
	  (if (and (null (tstree--node-split node))
		   (not (eq (tstree--node-low node) 'deleted)))
	      (progn
		(setq result (funcall function str
				      (tstree--node-equal node)))
		(when mapcar
		  (setq accumulate (cons result accumulate))))
	    (when (tstree--node-equal node)
	      (push (tstree--node-equal node) stack)
	      (push (cond
		     ((eq type 'string)
		      (concat str (string (tstree--node-split node))))
		     ((eq type 'list)
		      (append str (list (tstree--node-split node))))
		     (t (vconcat str
				 (vector (tstree--node-split node)))))
		    stack)))
	  
	  ;; add the low child to the stack, if it exists
	  (when (and (tstree--node-low node)
		     (not (eq (tstree--node-low node) 'deleted)))
	    (push (tstree--node-low node) stack)
	    (push str stack))
	  )))
    
    ;; return accumulated list of results (nil if MAPCAR was nil)
    (nreverse accumulate))
)



(defmacro tstree-mapcar (function tree &optional type)
  "Apply FUNTION to all elements in the ternary search tree TREE,
and make a list of the results.

FUNCTION will be passed two arguments: a key and its associated
data. It is safe to assume the tree will be traversed in
\"lexical\" order (i.e. the order defined by the tree's
comparison function).

If TREE is a list of trees, FUNCTION will be mapped over all trees in
the list, in order.

Optional argument TYPE should be one of the symbols 'string,
'list, or 'vector (default is 'vector). It defines which type of
sequence is passed to FUNCTION. If TYPE is 'string, it must be
possible to apply the function `string' to the type used to
reference data in the tree."
  `(tstree-map ,function ,tree ,type t))



(defun tstree-complete
  (tree sequence &optional maxnum combine-function filter)
  "Return an alist containing all completions of SEQUENCE found in
ternary searh tree TREE along with their associated data, in
\"lexical\" order (i.e. the order defined by the tree's
comparison function). If no completions are found, return nil.

If TREE is a list of ternary search trees, it will behave as
though it were a single tree: completions will be sought in all
trees in the list, and if a completion exists in more than one
tree, the data from all the trees will be combined by calling
COMBINE-FUNCTION on pairs of data. COMBINE-FUNCTION defaults to
the first tree's insersion function.

SEQUENCE must be a sequence (vector, list or string) containing
elements of the type used to reference data in the tree, or a
list of such sequences. (If the sequence is a string, it must be
possible to apply the `string' function to the tree's reference
type.) The completions returned in the alist will be sequences of
the same type. If a list of sequences is supplied, completions of
all sequences in the list are included in the returned alist.

The optional integer argument MAXNUM limits the results to the
first MAXNUM completions. Otherwise, all completions are
returned.

The optional COMBINE-FUNCTION argument should take two arguments,
the data associated with the same key in two different trees, and
return the combined data.

The FILTER argument sets a filter function for the
completions. If supplied, it is called for each possible
completion with two arguments: the completion, and its associated
data. If the filter function returns nil, the completion is not
included in the results."
  
  (let (stack completions seq num node data sortfun)
    ;; wrap tree and sequence in lists, if not already lists
    (setq tree (if (tstree-p tree) (list tree) tree))
    (setq sortfun
	  (tstree-construct-sortfun (tstree--tree-cmpfun (car tree))))
    ;; FIXME: this will fail if SEQUENCE is a list, and tree's reference
    ;;        type is itself a sequence (actually, there might be no way
    ;;        to fully fix this...)
    (if (or (atom sequence)
	    (and (listp sequence) (not (sequencep (car sequence)))))
	(setq sequence (list sequence))
      ;; sort sequences in list
      (setq sequence (sort sequence sortfun)))
    
    
    ;; loop over all trees in the list
    (dotimes (i (length tree))
      (setq num 0)
      
      ;; ----- initialise the stack -----
      ;; add initial nodes for each sequence in the sequence list
      (dolist (seq sequence)
	;; if completions exist, add initial node to the stack
	(if (car (push (tstree--node-find (nth i tree) seq) stack))
	    (push seq stack)
	  (pop stack)))
      
      ;; ----- search the tree -----
      ;; Keep going until we've searched all nodes (node stack is
      ;; empty), or have found enough completions.
      (while (and stack (or (null maxnum) (< num maxnum)))
	(setq seq (pop stack))
	(setq node (pop stack))
	
        ;; add the high child to the stack, if it exists
	(when (tstree--node-high node)
	  (push (tstree--node-high node) stack)
	  (push seq stack))
	
        ;; if we're not at a data node, add the equal child to the stack
	(if (tstree--node-split node)
	    (when (tstree--node-equal node)
	      (push (tstree--node-equal node) stack)
	      (push (cond
		     ((stringp seq)
		      (concat seq (string (tstree--node-split node))))
		     ((listp seq)
		      (append seq (list (tstree--node-split node))))
		     (t (vconcat seq
				 (vector (tstree--node-split node)))))
		    stack))
	  ;; if we're at a data node that hasn't been flagged as
	  ;; deleted, and passes the filter, we've found a completion
	  (when (and (not (eq (tstree--node-low node) 'deleted))
		     (or (null filter)
			 (funcall filter seq
				  (tstree--node-equal node))))
	    ;; skip completion if we've already found it in a previous
	    ;; tree
	    (unless (catch 'found
		      (dotimes (j i)
			(when (tstree-member-p (nth j tree) seq)
			  (throw 'found t))))
	      ;; combine data with that from trees later in the list
	      (setq data (tstree--node-equal node))
	      (dotimes (j (- (length tree) i 1))
		(setq data
		      (if combine-function
			  (funcall combine-function data
				   (tstree-member (nth (+ i j 1) tree)
						  seq))
			data)))
	      ;; add the completion to the list
	      (setq completions (cons (cons seq data) completions))
	      (setq num (1+ num))))
	  )
	
	;; add the low child to the stack, if it exists
	(when (and (tstree--node-low node)
		   (not (eq (tstree--node-low node) 'deleted)))
	  (push (tstree--node-low node) stack)
	  (push seq stack))
	))
    
    
    ;; ----- construct the list of completions -----
    ;; if searching across multiple trees, need to sort completions
    (when (> (length tree) 1)
      ;; construct sort function from tree's comparison function
      (let ((cmpl-sortfun `(lambda (a b) (,sortfun (car a) (car b)))))
	;; sort completions
	(setq completions (sort completions cmpl-sortfun))))
    
    ;; discard any excess completions
    (when (and maxnum (> (length completions) maxnum))
      (setcdr (nthcdr (1- maxnum) completions) nil))
    
    ;; return the completions
    (nreverse completions))
)




(defun tstree-complete-ordered
  (tree key &optional maxnum rank-function combine-function filter)
  "Return an alist containing all completions of KEY found in
ternary search tree TREE, along with their associated data. If no
completions are found, return nil.

Note that `tstree-complete' is significantly more efficient than
`tstree-complete-ordered', especially when a maximum number of
completions is specified. Always use `tstree-complete' when you
don't care about the ordering of the completions, or you need the
completions ordered \"alphabetically\".

If TREE is a list of ternary search trees, it will behave as
though it were a single tree: completions will be sought in all
trees in the list, and if a completion exists in more than one
tree, the data from all the trees will be combined by calling
COMBINE-FUNCTION on pairs of data. COMBINE-FUNCTION defaults to
the first tree's insersion function. The completions are ranked
according to the values of the combined data.

KEY must be a sequence (vector, list or string) containing
elements of the type used to reference data in the tree, or a
list of such sequences. (If the sequence is a string, it must be
possible to apply the `string' function to the tree's reference
type.) The completions returned in the alist will be sequences of
the same type. If a list of sequences is supplied, completions of
all sequences in the list are included in the returned alist.

The optional integer argument MAXNUM limits the results to the
\"best\" MAXNUM completions. If nil, all completions are
returned.

The optional argument RANK-FUNCTION over-rides the tree's default
rank function. It should take two arguments, each a cons whose
car is a key, and whose cdr is the data associated with it. It
should return non-nil if the first argument is \"better than\"
the second, nil otherwise. The elements of the returned alist are
sorted according to this rank-function, in descending order.

The optional COMBINE-FUNCTION argument should take two arguments,
the data associated with the same key in two different trees, and
return the combined data.

The FILTER argument sets a filter function for the
completions. If supplied, it is called for each possible
completion with two arguments: the completion (a sequence of the
same type as KEY), and its associated data. If the filter
function returns nil, the completion is not included in the
results."
  
  (let* (stack heap)
    ;; wrap tree and key in lists if necessary
    (when (tstree-p tree) (setq tree (list tree)))
    ;; FIXME: this will fail if KEY is a list, and tree's
    ;;        reference type is itself a sequence
    (when (or (atom key)
	      (and (listp key) (not (sequencep (car key)))))
      (setq key (list key)))

    
    ;; ----- initialise the heap -----
    (let ((rankfun (or rank-function
		       (tstree--tree-rankfun (car tree)))))
      ;; create the heap with a rank-function constructed from the first
      ;; tree in the list
      (setq heap (heap-create `(lambda (a b) (not (,rankfun a b)))
			      (1+ maxnum))))
    
    (let (num seq node data newdata)
      ;; loop over all trees in the list
      (dotimes (i (length tree))
	(setq num 0)
	
	;; ----- initialise the stack -----
	(dolist (seq key)
	  ;; if completions exist, add initial node and key to the
	  ;; stack
	  (if (car (push (tstree--node-find (nth i tree) seq) stack))
	      (push seq stack)
	    (pop stack)))
	
	
	;; ------ search the current tree -----
	;; keep going until we've searched all nodes (node stack is
	;; empty)
	(while stack
	  (setq seq (pop stack))
	  (setq node (pop stack))
	  
	  ;; add the high child to the stack, if it exists
	  (when (tstree--node-high node)
	    (push (tstree--node-high node) stack)
	    (push seq stack))
	  
	  ;; if we're not at a data node, add the equal child to the
	  ;; stack
	  (if (tstree--node-split node)
	      (when (tstree--node-equal node)
		(push (tstree--node-equal node) stack)
		(push (cond
		       ((stringp seq)
			(concat seq (string (tstree--node-split node))))
		       ((listp seq)
			(append seq (list (tstree--node-split node))))
		       (t
			(vconcat seq
				 (vector (tstree--node-split node)))))
		      stack))
	    ;; if we're at a data node that hasn't been flagged as
	    ;; deleted, and passes the filter, we've found a completion
	    (when (and (not (eq (tstree--node-low node) 'deleted))
		       (or (null filter)
			   (funcall filter seq
				    (tstree--node-equal node))))
	      ;; skip completion if we've already found it in a previous
	      ;; tree
	      (unless (catch 'found
			(dotimes (j i)
			  (when (tstree-member-p (nth j tree) seq)
			    (throw 'found t))))
		;; combine data with that from trees later in the list
		(setq data (tstree--node-equal node))
		(dotimes (j (- (length tree) i 1))
		  (setq data
			(if combine-function
			    (funcall combine-function data
				     (tstree-member (nth (+ i j 1) tree)
						    seq))
			  data)))
		;; add the completion to the heap
		(heap-add heap (cons seq data))
		(setq num (1+ num))
		;; If we already have enough completions, delete the
		;; worst one from the heap.
		(when (and maxnum (> num maxnum))
		  (heap-delete-root heap))))
	    )

	  ;; add the low child to the stack, if it exists
	  (when (and (tstree--node-low node)
		     (not (eq (tstree--node-low node) 'deleted)))
	    (push (tstree--node-low node) stack)
	    (push seq stack))
	  )))
    
    
    ;; ----- create the completions vector -----
    ;; repeatedly transfer the worst completion left in the heap to the
    ;; front of the completions vector
    (let (completions cmpl)
      (while (not (heap-empty heap))
	(setq completions
	      (setq cmpl (cons (heap-delete-root heap) completions))))
      ;; return the list of completions
      completions))
)


;;; tstree.el ends here
