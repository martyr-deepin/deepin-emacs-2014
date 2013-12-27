
;;; auto-overlay-nested.el --- nested start/end-delimited automatic overlays


;; Copyright (C) 2005-2007 Toby Cubitt

;; Author: Toby Cubitt <toby-predictive@dr-qubit.org>
;; Version: 0.1.7
;; Keywords: automatic, overlays, nested
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
;; Version 0.1.7
;; * added new `auto-overlay-complex-class' property
;; * renamed 'entry-id and 'subentry-id to 'definition-id and 'regexp-id
;;
;; Version 0.1.6
;; * renamed from "nest" to "nested"
;; 
;; Version 0.1.5
;; * set overlay properties straight after creation in `auto-o-make-nest',
;;   rather than leaving it to `auto-overlay-update', in case matching causes
;;   exclusive reparsing, for which properties are already required
;;
;; Version 0.1.4
;; * removed `auto-overlay-functions' and changed to use new interface
;; * renamed from "stack" to "nest"
;;
;; Version 0.1.3
;; * updated to reflect changes in `auto-overlays.el'
;;
;; Version 0.1.2
;; * bug fix to `auto-o-suicide' behaviour, require change to `auto-o-stack'
;;
;; Version 0.1.1
;; * bug fixes
;;
;; Version 0.1
;; * initial version separated off from auto-overlays.el



;;; Code:


(require 'auto-overlays)
(provide 'auto-overlay-nested)


;; set nested overlay parsing and suicide functions, and indicate class
;; requires separate start and end regexps
(put 'nested 'auto-overlay-parse-function 'auto-o-parse-nested-match)
(put 'nested 'auto-overlay-suicide-function 'auto-o-nested-suicide)
(put 'nested 'auto-overlay-complex-class t)



(defun auto-o-parse-nested-match (o-match)
  ;; Perform any necessary updates of auto overlays due to a match for a
  ;; nested regexp.

  (let* ((overlay-stack (auto-o-nested-stack o-match))
	 (o (car overlay-stack)))
    (cond
     ;; if the stack is empty, just create and return a new unmatched overlay
     ((null overlay-stack)
      (auto-o-make-nested o-match 'unmatched))
     
     ;; if appropriate edge of innermost overlay is unmatched, just match it
     ((or (and (eq (auto-o-edge o-match) 'start)
	       (not (auto-o-start-matched-p o)))
	  (and (eq (auto-o-edge o-match) 'end)
	       (not (auto-o-end-matched-p o))))
      (auto-o-match-overlay o o-match)
      ;; return nil since haven't created any new overlays
      nil)
     
     ;; otherwise...
     (t
      ;; create new innermost overlay and add it to the overlay stack
      (push (auto-o-make-nested o-match) overlay-stack)
      ;; sort out the overlay stack
      (auto-o-nested-stack-cascade overlay-stack)
      ;; return newly created overlay
      (car overlay-stack)))
    )
)




(defun auto-o-nested-suicide (o-self)
  ;; Called when match no longer matches. Unmatch the match overlay O-SELF, if
  ;; necessary deleting its parent overlay or cascading the stack.
  
  (let* ((overlay-stack (auto-o-nested-stack o-self))
	(o-parent (car overlay-stack)))
    
    (cond
     ;; if other end of parent is unmatched, just delete parent
     ((not (auto-o-edge-matched-p
	    o-parent
	    (if (eq (auto-o-edge o-self) 'start) 'end 'start)))
      (auto-o-delete-overlay o-parent))

     ;; if parent is the only overlay in the stack...
     ((= (length overlay-stack) 1)
      ;; if we're a start match, make parent start-unmatched
      (if (eq (auto-o-edge o-self) 'start)
	  (auto-o-match-overlay o-parent 'unmatched nil)
	    ;; if we're an end match, make parent end-unmatched
	(auto-o-match-overlay o-parent nil 'unmatched)))
     
      ;; otherwise, unmatch ourselves from parent and cascade the stack
     (t
      (overlay-put o-parent (auto-o-edge o-self) nil)
      (overlay-put o-self 'parent nil)
      (auto-o-nested-stack-cascade overlay-stack))
     ))
)

      


(defun auto-o-make-nested (o-match &optional unmatched)
  ;; Create a nested overlay for match overlay O-MATCH.
  ;; If UNMATCHED is nil, overlay will start and end at O-MATCH.
  ;; If non-nil, overlay will start or end from O-MATCH (depending on whether
  ;; O-MATCH is a 'start or 'end match) and stretch till end or beginning of
  ;; buffer.

  (let (o-new pos)
    ;; create new nested overlay and match it with O-MATCH
    (cond
     ((eq (auto-o-edge o-match) 'start)
      (setq pos (overlay-get o-match 'delim-end))
      (setq o-new (make-overlay pos pos nil nil 'rear-advance))
      (overlay-put o-new 'auto-overlay t)
      (overlay-put o-new 'set-id (overlay-get o-match 'set-id))
      (overlay-put o-new 'definition-id (overlay-get o-match 'definition-id))
      (auto-o-match-overlay o-new o-match 'unmatched))
     
     ((eq (auto-o-edge o-match) 'end)
      (setq pos (overlay-get o-match 'delim-start))
      (setq o-new (make-overlay pos pos nil nil 'rear-advance))
      (overlay-put o-new 'auto-overlay t)
      (overlay-put o-new 'set-id (overlay-get o-match 'set-id))
      (overlay-put o-new 'definition-id (overlay-get o-match 'definition-id))
      (auto-o-match-overlay o-new 'unmatched o-match)))

    ;; return the new overlay
    o-new)
)



(defun auto-o-nested-stack-cascade (overlay-stack)
  ;; Cascade the ends of the overlays in OVERLAY-STACK up or down the stack,
  ;; so as to re-establish a valid stack. It assumes that only the innermost
  ;; is incorrect.
  
  (let ((o (car overlay-stack)) o1)
    (cond
     
     ;; if innermost overlay is start-matched (and presumably
     ;; end-unmatched)...
     ((auto-o-start-matched-p o)
      ;; cascade overlay end matches up through stack until one is left
      (dotimes (i (- (length overlay-stack) 1))
	(setq o (nth i overlay-stack))
	(setq o1 (nth (+ i 1) overlay-stack))
	(auto-o-match-overlay o nil
			      (if (overlay-get o1 'end)
				    (overlay-get o1 'end)
				'unmatched)
			      nil nil 'protect-match))
      ;; if final overlay is start-matched, make it end-unmatched, otherwise
      ;; delete it
      (if (auto-o-start-matched-p o1)
	  ;; FIXME: could postpone re-parsing here in case it can be avoided
	  (auto-o-match-overlay o1 nil 'unmatch nil nil 'protect-match)
	(auto-o-delete-overlay o1 nil 'protect-match)))
     
     
     ;; if innermost overlay is end-matched (and presumably
     ;; start-unmatched)...
     ((auto-o-end-matched-p o)
      ;; cascade overlay start matches up through stack until one is left
      (dotimes (i (- (length overlay-stack) 1))
	(setq o (nth i overlay-stack))
	(setq o1 (nth (+ i 1) overlay-stack))
	(auto-o-match-overlay o (if (overlay-get o1 'start)
				    (overlay-get o1 'start)
				  'unmatched)
			      nil nil nil 'protect-match))
      ;; if final overlay is end-matched, make it start-unmatched, otherwise
      ;; delete it
      (if (auto-o-end-matched-p o1)
	  ;; FIXME: could postpone re-parsing here in case it can be avoided
	  (auto-o-match-overlay o1 'unmatch nil nil nil 'protect-match)
	(auto-o-delete-overlay o1 nil 'protect-match))))
    )
)




(defun auto-o-nested-stack (o-match)
  ;; Return a list of the overlays that overlap and correspond to same entry
  ;; as match overlay O-MATCH, ordered from innermost to outermost. (Assumes
  ;; overlays are correctly stacked.) The parent of O-MATCH is guaranteed to
  ;; come before any other overlay that has exactly the same length (which
  ;; implies they cover identical regions if overlays are correctly
  ;; stacked). For other overlays with identical lengths, the order is
  ;; undefined.
  
  ;; find overlays corresponding to same entry overlapping O-MATCH
  (let ((overlay-stack (auto-overlays-at-point
			(if (eq (auto-o-edge o-match) 'start)
			    (overlay-get o-match 'delim-end)
			  (overlay-get o-match 'delim-start))
			(list '(eq auto-overlay t)
			      (list 'eq 'set-id (overlay-get o-match 'set-id))
			      (list 'eq 'definition-id
				    (overlay-get o-match 'definition-id)))))
	(o-parent (overlay-get o-match 'parent)))
    ;; sort the list by overlay length, i.e. from innermost to outermose
    (sort overlay-stack
	  (lambda (a b)
	    (let ((len-a (- (overlay-end a) (overlay-start a)))
		  (len-b (- (overlay-end b) (overlay-start b))))
	      ;; parent of O-MATCH comes before any other overlay with
	      ;; identical length, otherwise sort by length
	      (if (= len-a len-b) (eq o-parent a) (< len-a len-b)))))
    )
)


;; auto-overlay-nested.el ends here
