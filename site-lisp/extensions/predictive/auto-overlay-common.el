
;;; auto-overlay-common.el --- general overlay functions


;; Copyright (C) 2006 Toby Cubitt

;; Author: Toby Cubitt <toby-predictive@dr-qubit.org>
;; Version: 0.1.1
;; Keywords: automatic, overlays
;; URL: http://www.dr-qubit.org/emacs.php


;; This file is part of the Emacs Automatic Overlays package.
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


;;; Change Log:
;;
;; Version 0.1.1
;; * bugfix in `auto-overlay-local-binding'
;;
;; Version 0.1
;; * initial version split from auto-overlays



;;; Code:


(provide 'auto-overlay-common)


(defun auto-overlays-at-point (&optional point prop-test inactive)
  "Return overlays overlapping POINT (or the point, if POINT is
null). If PROP-TEST is supplied, it should be a list which
specifies a property test with one of the following forms (or a
list of such lists if more than one property test is required):

  (FUNCTION PROPERTY)

  (FUNCTION PROPERTY VALUE)

  (FUNCTION (PROPERTY1 PROPERTY2 ...) (VALUE1 VALUE2 ...))

where PROPERTY indicates an overlay property name (a symbol), and
VALUE indicates an arbitrary value or lisp expression.

For each overlay between START and END, first the values
corresponding to the property names are retrieved from the
overlay, then FUNCTION is called with the properties values
followed by the other values as its arguments. The test is
satisfied if the result is non-nil, otherwise it fails. Tests are
evaluated in order, but only up to the first failure. Only
overlays that satisfy all property tests are returned.

If INACTIVE is non-nil, both active and inactive overlays are
returned (usually inactive ones are ignored).

Note that this function returns any overlay. If you want to
restrict it to auto overlays, include '(identity auto-overlay) in
PROP-TEST."
  (when (null point) (setq point (point)))
  
  (let (overlay-list)
    ;; get overlays overlapping POINT and zero-length overlays at POINT
    (setq overlay-list
	  (auto-overlays-in point point prop-test nil inactive))
    ;; get overlays that end at POINT
    (dolist (o (auto-overlays-in (1- point) point prop-test nil inactive))
      (when (and (< (overlay-start o) point)
		 (= (overlay-end o) point))
	(push o overlay-list)))
    ;; get overlays that start at POINT
    (dolist (o (auto-overlays-in point (1+ point) prop-test nil inactive))
      (when (and (> (overlay-end o) point)
		 (= (overlay-start o) point))
	(push o overlay-list)))
    
    overlay-list)
)



;; FIXME: get rid of INACTIVE argument
(defun auto-overlays-in (start end &optional prop-test within inactive)
  "Return auto overlays overlapping region between START and END.

If PROP-TEST is supplied, it should be a list which specifies a
property test with one of the following forms (or a list of such
lists if more than one property test is required):

  (FUNCTION PROPERTY)

  (FUNCTION PROPERTY VALUE)

  (FUNCTION (PROPERTY1 PROPERTY2 ...) (VALUE1 VALUE2 ...))

where PROPERTY indicates an overlay property name (a symbol), and
VALUE indicates an arbitrary value or lisp expression.

For each overlay between START and END, first the values
corresponding to the property names are retrieved from the
overlay, then FUNCTION is called with the properties values
followed by the other values as its arguments. The test is
satisfied if the result is non-nil, otherwise it fails. Tests are
evaluated in order, but only up to the first failure. Only
overlays that satisfy all property tests are returned.

If WITHIN is non-nil, only overlays entirely within START and END
are returned. If INACTIVE is non-nil, both active and inactive
overlays are returned (usually inactive ones are ignored).

Note that this function returns any overlay. If you want to
restrict it to auto overlays, include '(identity auto-overlay) in
PROP-TEST."

  ;; make sure prop-test is a list of lists, even if there's only one, and
  ;; exclude inactive overlays unless told not to
  (cond
   ((null prop-test)
    (unless inactive (setq prop-test '((null inactive)))))
   ((functionp (car prop-test))
    (if inactive
	(setq prop-test (list prop-test))
      (setq prop-test (list '(null inactive) prop-test))))
   (t
    (unless inactive (setq prop-test (push '(null inactive) prop-test)))))
  
  (let (overlay-list function prop-list value-list result)    
    ;; check properties of each overlay in region
    (dolist (o (overlays-in start end))
      ;; check overlay is entirely within region
      (if (and within
	       (or (< (overlay-start o) start) (> (overlay-end o) end)))
	  (setq result nil)
	
	;; if it is, or we don't care
	(setq result t)
	(catch 'failed
	  ;; check if properties match
	  (dolist (test prop-test)
	    ;; (Note: the whole thing would be neater with something like
	    ;; (apply 'and (map ...)) but 'and is a special form, not a
	    ;; function, so can't be applied)
	    (setq function (nth 0 test))
	    (unless (listp (setq prop-list (nth 1 test)))
	      (setq prop-list (list prop-list)))
	    (setq value-list nil)
	    (unless (or (< (length test) 3)
			(and (setq value-list (nth 2 test))  ; nil isn't list
			     (listp value-list)))
	      (setq value-list (list value-list)))
	    
	    ;; apply the test
	    (setq result
		  (and result
		       (apply function
			      (append (mapcar (lambda (p) (overlay-get o p))
					      prop-list)
				      value-list))))
	    (when (null result) (throw 'failed nil)))))
      
      ;; add overlay to result list if its properties matched
      (when result (push o overlay-list)))
    ;; return result list
    overlay-list)
)



(defun auto-overlay-highest-priority-at-point (&optional point proptest)
  "Return highest priority overlay at POINT (defaults to the point).

If two overlays have the same priority, the innermost one takes
precedence (i.e. the one that begins later, or if they begin at
the same point the one that ends earlier).

See `auto-overlays-at' for ane explanation of the PROPTEST argument."
  
  (unless point (setq point (point)))
  
  ;; get all overlays at point with a non-nil SYMBOL property
  (let* ((overlay-list (auto-overlays-at-point point proptest))
	 (overlay (pop overlay-list))
	 p p1)

    ;; find the highest priority, innermost overlay
    (dolist (o1 overlay-list)
      (setq p (overlay-get overlay 'priority))
      (setq p1 (overlay-get o1 'priority))
      (when (or (and (null p) p1)
		(and p p1 (> p1 p))
		(and (equal p1 p)
		     (or (> (overlay-start o1) (overlay-start overlay))
			 (and (= (overlay-start o1) (overlay-start overlay))
			      (< (overlay-end o1) (overlay-end o1))))))
	(setq overlay o1)))

    ;; return the overlay
    overlay)
)



(defun auto-overlay-local-binding (symbol &optional point)
  "Return \"overlay local \" binding of SYMBOL at POINT,
or the current local binding if there is no overlay
binding. POINT defaults to the point.

An \"overlay local\" binding is creating by giving an overlay a
non-nil value for a property named SYMBOL. If more than one
overlay at POINT has a non-nil SYMBOL property, the value from
the highest priority overlay is returned.

See `auto-overlay-highest-priority-at-point' for a definition of
\"highest priority\"."
  
  (let ((overlay (auto-overlay-highest-priority-at-point
		  point `(identity ,symbol))))
    (if overlay
	(overlay-get overlay symbol)
      (when (boundp symbol) (eval symbol))))
)

;; auto-overlay-common.el ends here
