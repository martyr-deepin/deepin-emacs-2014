
;;; heap.el --- heap (a.k.a. priority queue) data structure package


;; Copyright (C) 2004-2006 Toby Cubitt

;; Author: Toby Cubitt <toby-predictive@dr-qubit.org>
;; Version: 0.2
;; Keywords: heap, priority queue
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
;; A heap is a form of efficient self-sorting tree (sometimes called a
;; priority queue). In particular, the root node is guaranteed to be
;; the highest-ranked entry in the tree. (The comparison function used
;; for ranking the data can, of course, be freely defined). Therefore
;; repeatedly removing the root node will return the data in order of
;; increasing rank. They are often used as priority queues, for
;; scheduling tasks in order of importance.
;;
;; A heap consists of two cons cells, the first one holding the tag
;; 'HEAP in the car cell and the second one having the heap in the car
;; and the compare function in the cdr cell. The compare function must
;; take two arguments of the type which is to be stored in the heap
;; and must return non-nil or nil. To implement a max-heap, it should
;; return non-nil if the first argument is "greater" than the
;; second. To implement a min-heap, it should return non-nil if the
;; first argument is "less than" the second.
;;
;; You create a heap using `heap-create', add elements to it using
;; `heap-add', delete and return the root of the heap using
;; `heap-delete-root', and modify an element of the heap using
;; `heap-modify'. A number of other convenience functions are
;; also provided.
;;
;; Note that this package implements a ternary heap, since ternary
;; heaps are about 12% more efficient than binary heaps for heaps
;; containing more than about 10 elements. And for very small heaps,
;; the difference is negligible.


;;; Change log:
;;
;; Version 0.2
;; * fixed efficiency issue: vectors are no longer copied all the time
;;   (thanks to Stefan Monnier for pointing out this issue)
;;
;; Version 0.1.5
;; * renamed `vswap' to `heap--vswap'
;; * removed cl dependency
;;
;; Version 0.1.4
;; * fixed internal function and macro names
;;
;; Version 0.1.3
;; * added more commentary
;;
;; Version 0.1.2
;; * moved defmacros before their first use so byte-compilation works
;;
;; Version 0.1.1
;; * added cl dependency
;;
;; version 0.1
;; * initial release



;;; Code:

(provide 'heap)





;;; ================================================================
;;;       Internal functions for use in the heap package


(defmacro heap--vect (heap)   ; INTERNAL USE ONLY
  ;; Return the heap vector.
  `(aref ,heap 1)
)


(defmacro heap--set-vect (heap vect)   ; INTERNAL USE ONLY
  ;; Set the vector containing the heap itself to VECT.
  `(aset ,heap 1 ,vect)
)


(defmacro heap--cmpfun (heap)   ; INTERNAL USE ONLY
  ;; Return the comparison function of a heap.
  `(aref ,heap 2)
)


(defmacro heap--count (heap)   ; INTERNAL USE ONLY
  ;; Return number of items in HEAP
  `(aref ,heap 3)
)


(defmacro heap--set-count (heap count)   ; INTERNAL USE ONLY
  ;; Set number of items in HEAP
  `(aset ,heap 3 ,count)
)


(defmacro heap--size (heap)   ; INTERNAL USE ONLY
  ;; Return size of HEAP
  `(aref ,heap 4)
)


(defmacro heap--set-size (heap size)   ; INTERNAL USE ONLY
  ;; Set size of HEAP
  `(aset ,heap 4 ,size)
)


(defmacro heap--resize (heap)   ; INTERNAL USE ONLY
  ;; Return resize-factor of HEAP
  `(aref ,heap 5)
)



(defun heap--child (heap i)    ; INTERNAL USE ONLY
  ;; Compare the 3 children of element I, and return element reference of
  ;; the smallest/largest (depending on whethen it's a min- or max-heap).
  (let* ((vect (heap--vect heap))
	 (cmpfun (heap--cmpfun heap))
	 (count (heap--count heap))
	 (j nil) (k (* 3 i)))
    ;; Lots of if's in case I has less than three children.
    (if (>= (1+ k) count) nil
      (if (>= (+ 2 k) count) (1+ k)
	(setq j (if (funcall cmpfun (aref vect (1+ k))
			     (aref vect (+ 2 k)))
		    (1+ k) (+ 2 k)))
	(if (>= (+ 3 k) count) j
	  (if (funcall cmpfun (aref vect j) (aref vect (+ 3 k)))
	      j (+ 3 k)))
	)))
)



(defmacro heap--vswap (vect i j)   ; INTERNAL USE ONLY
  ;; Swap elements I and J of vector VECT.
  `(let ((tmp (aref ,vect ,i)))
     (aset ,vect ,i (aref ,vect ,j))
     (aset ,vect ,j tmp) ,vect)
)



(defun heap--sift-up (heap n)   ; INTERNAL USE ONLY
  ;; Sift-up starting from element N of vector belonging to HEAP.
  (let* ((i n) (j nil) (vect (heap--vect heap)) (v (aref vect n)))
    ;; Keep moving element up until it reaches top or is smaller/bigger
    ;; than its parent.
    (while (and (> i 0)
		(funcall (heap--cmpfun heap) v
			 (aref vect (setq j (/ (1- i) 3)))))
      (heap--vswap vect i j)
      (setq i j)))
)



(defun heap--sift-down (heap n)   ; INTERNAL USE ONLY
  ;; Sift-down from element N of the heap vector belonging HEAP.
  (let* ((vect (heap--vect heap))
	(cmpfun (heap--cmpfun heap))
	(i n) (j (heap--child heap i))
	(v (aref vect n)))
    ;; Keep moving the element down until it reaches the bottom of the
    ;; tree or reaches a position where it is bigger/smaller than all its
    ;; children.
    (while (and j (funcall cmpfun (aref vect j) v))
      (heap--vswap vect i j)
      (setq i j)
      (setq j (heap--child heap i)))
  )
)





;;; ================================================================
;;;          The public functions which operate on heaps.


(defun heap-create (compare-function &optional initial-size resize-factor)
  "Create an empty heap with comparison function COMPARE-FUNCTION.

COMPARE-FUNCTION takes two arguments, A and B, and returns
non-nil or nil. To implement a max-heap, it should return non-nil
if A is greater than B. To implemenet a min-heap, it should
return non-nil if A is less than B.

Optional argument INITIAL-SIZE sets the initial size of the heap,
defaulting to 10. Optional argument RESIZE-FACTOR sets the factor
by which the heap's size is increased if it runs out of space, defaulting
to 1.5"
  (unless initial-size (setq initial-size 10))
  (unless resize-factor (setq resize-factor 1.5))
  (vector 'HEAP (make-vector initial-size nil) compare-function
	  0 initial-size resize-factor)
)


(defun heap-copy (heap)
  "Return a copy of heap HEAP."
  (let ((newheap (heap-create (heap--size heap) (heap--cmpfun heap))))
    (heap--set-vect newheap (vconcat (heap--vect heap) []))
    newheap)
)


(defun heap-p (obj)
  "Return t if OBJ is a heap, nil otherwise."
  (and (vectorp obj) (eq (aref obj 0) 'HEAP))
)



(defun heap-empty (heap)
  "Return t if the heap is empty, nil otherwise."
  (= 0 (heap--count heap))
)



(defun heap-size (heap)
  "Return the number of entries in the heap."
  (heap--count heap)
)



(defun heap-compare-function (heap)
  "Return the comparison function for the heap HEAP."
  (heap--cmpfun heap)
)



(defun heap-add (heap data)
  "Add DATA to the heap, and return DATA."
  ;; Add data to bottom of heap and sift-up from bottom.
  (let ((count (heap--count heap))
	(size (heap--size heap))
	(vect (heap--vect heap)))
    ;; if there's no space left, grow the heap
    (if (< count size)
	(aset vect count data)
      (heap--set-vect
       heap (vconcat (heap--vect heap) (vector data)
		     (make-vector
		      (1- (ceiling (* size (1- (heap--resize heap)))))
		      nil)))
      (heap--set-size heap (* 2 size)))
    (setq count (heap--set-count heap (1+ (heap--count heap))))
    (heap--sift-up heap (1- count)))
  ;; return inserted data
  data
)



(defun heap-delete-root (heap)
  "Return the root of the heap and delete it from the heap."
  (let (vect root (count (heap--count heap)))
    
    ;; deal with empty heaps and heaps with just one element
    (if (= count 0) nil
      (setq vect (heap--vect heap))
      (setq root (aref vect 0))
      (heap--set-count heap (1- (heap--count heap)))
      (if (= 1 count) (heap--set-vect heap (make-vector 10 nil))
	;; Delete root, swap last element to top, and sift-down from top.
	(setq vect (heap--vect heap))
	(aset vect 0 (aref vect (1- count)))
	(aset vect (1- count) nil)
	(heap--sift-down heap 0))
      root)
  )
)



(defun heap-modify (heap match-function data)
  "Replace the first heap entry identified by MATCH-FUNCTION
with DATA, if a match exists. Return t if there was a match, nil
otherwise.

The function MATCH-FUNCTION should take one argument of the type
stored in the heap, and return non-nil if it should be modified,
nil otherwise.

Note that only the match highest up the heap is modified."
  
  (let ((vect (heap--vect heap))
	(count (heap--count heap))
	(i 0))
    ;; search vector for the first match
    (while (and (< i count)
		(not (funcall match-function (aref vect i))))
      (setq i (1+ i)))
    ;; if a match was found, modify it
    (if (< i count)
	(let ((olddata (aref vect i)))
	  (aset vect i data)
	  ;; if the new data is greater than old data, sift-up, otherwise
	  ;; sift-down
	  (if (funcall (heap--cmpfun heap) data olddata)
	      (heap--sift-up heap i)
	    (heap--sift-down heap i))
	  t  ; return t if the match was successfully modified
	)
      nil  ; return nil if no match was found
    )
  )
)


;;; heap.el ends here
