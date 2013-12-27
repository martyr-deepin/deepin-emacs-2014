
;;; auto-overlay-flat.el --- flat start/end-delimited automatic overlays


;; Copyright (C) 2006-2008 Toby Cubitt

;; Author: Toby Cubitt <toby-predictive@dr-qubit.org>
;; Version: 0.1.2
;; Keywords: automatic, overlays, flat, unnested
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
;; Version 0.1.2
;; * added new `auto-overlay-complex-class' property
;; * renamed 'entry-id and 'subentry-id to 'definition-id and 'regexp-id
;;
;; Version 0.1.1
;; * set overlay properties straight after creation, rather than leaving it to
;;   `auto-overlay-update', in case matching causes exclusive reparsing, for
;;   which properties are already required
;;
;; Version 0.1
;; * initial version



;;; Code:


(require 'auto-overlays)
(provide 'auto-overlay-flat)


;; set flat overlay parsing and suicide functions, and indicate class requires
;; separate start and end regexps
(put 'flat 'auto-overlay-parse-function 'auto-o-parse-flat-match)
(put 'flat 'auto-overlay-suicide-function 'auto-o-flat-suicide)
(put 'flat 'auto-overlay-complex-class t)



(defun auto-o-parse-flat-match (o-match)
  ;; Perform any necessary updates of auto overlays due to a match for a flat
  ;; regexp.

  (let (o-parent)
    (cond

     ;; if match is for a start regexp...
     ((eq (auto-o-edge o-match) 'start)
      ;; if match is within an existing overlay, ignore match
      (unless (auto-overlays-at-point
	       (overlay-get o-match 'delim-end)  ; FIXME: is this right?
	       `((identity auto-overlay)
		 (eq set-id ,(overlay-get o-match 'set-id))
		 (eq definition-id ,(overlay-get o-match 'definition-id))))

	;; otherwise, look for next end-match...
	(let ((o-end (auto-o-next-flat-match o-match 'end)))
	  (cond
	   ;; if there is one that has a parent, steal start of the parent
	   ;; overlay
	   ((and o-end (overlay-get o-end 'parent))
	    (auto-o-match-overlay (overlay-get o-end 'parent) o-match)
	    nil)  ; return nil since haven't created any overlays

	   ;; if there is one but it's parentless, make a new overlay, match
	   ;; it with O-MATCH and the next end-match, and return it
	   (o-end
	    (let ((pos (overlay-get o-match 'delim-end)))
	      (setq o-parent (make-overlay pos pos nil nil 'rear-advance)))
	    (overlay-put o-parent 'auto-overlay t)
	    (overlay-put o-parent 'set-id (overlay-get o-match 'set-id))
	    (overlay-put o-parent 'definition-id
			 (overlay-get o-match 'definition-id))
	    (auto-o-match-overlay o-parent o-match o-end)
	    o-parent)

	   (t ;; otherwise, make a new, end-unmatched overlay and return it
	    (let ((pos (overlay-get o-match 'delim-end)))
	      (setq o-parent (make-overlay pos pos nil nil 'read-advance))
	      (overlay-put o-parent 'auto-overlay t)
	      (overlay-put o-parent 'set-id (overlay-get o-match 'set-id))
	      (overlay-put o-parent 'definition-id
			   (overlay-get o-match 'definition-id))
	      (auto-o-match-overlay o-parent o-match 'unmatched)
	      o-parent))
	   ))))


     (t ;; if match is for an end regexp...
      ;; if match is within existing overlay with same set-d and definition-id...
      (when (setq o-parent
		(car  ; FIXME: is this right?
		 (auto-overlays-at-point
		  (overlay-get o-match 'delim-start)  ; FIXME: is this right?
		  `((identity auto-overlay)
		    (eq set-id ,(overlay-get o-match 'set-id))
		    (eq definition-id ,(overlay-get o-match 'definition-id))))))

	;; if overlay can simply be re-matched with new end-match, do so
	(let ((o-end (overlay-get o-parent 'end))
	      (o-start (auto-o-next-flat-match o-match 'start)))
	  (if (not (and o-end o-start
			(<= (overlay-get o-start 'delim-end)
			    (overlay-get o-end 'delim-start))))
	      (progn (auto-o-match-overlay o-parent nil o-match) nil)

	    ;; if overlay was end-matched, and there's a start match within
	    ;; existing overlay that will be "unmasked" when end is stolen,
	    ;; create a new overlay between that start match and the end match
	    ;; we're stealing from
	    (auto-o-match-overlay o-parent nil o-match)
	    (let ((pos (overlay-get o-start 'delim-end)))
	      (setq o-parent (make-overlay pos pos nil nil 'read-advance))
	      (overlay-put o-parent 'auto-overlay t)
	      (overlay-put o-parent 'set-id (overlay-get o-match 'set-id))
	      (overlay-put o-parent 'definition-id
			   (overlay-get o-match 'definition-id))
	      (auto-o-match-overlay o-parent o-start o-end))
	    o-parent))  ; return newly created overlay
	))))
)



(defun auto-o-flat-suicide (o-self)
  ;; Called when match no longer matches. Unmatch the match overlay O-SELF,
  ;; re-matching or deleting its parent overlay as necessary.

  (let ((o-parent (overlay-get o-self 'parent)))
    (cond
     ;; if we have no parent, don't need to do anything
     ((null o-parent))

     ;; if we're a start-match...
     ((eq (auto-o-edge o-self) 'start)
      ;; if parent is end-unmatched, delete parent
      (if (null (overlay-get o-parent 'end))
	  (auto-o-delete-overlay o-parent)

	;; otherwise, look for next start match...
	(let ((o-start (auto-o-next-flat-match o-self 'start)))
	  ;; if there is one, match parent with it
	  (if o-start
	      (auto-o-match-overlay o-parent o-start)
	    ;; otherwise, delete parent
	    (auto-o-delete-overlay o-parent)))))


     (t ;; if we're an end-match, look for next end-match...
      (let ((o-start (overlay-get o-parent 'start))
	    (o-end (auto-o-next-flat-match o-self 'end)))
	(cond
	 ;; if there is one, match parent with it
	 (o-end
	  ;; if end-match already has a parent, delete it as its now
	  ;; superfluous (note: no need to parse, since parent overlay will be
	  ;; extended to cover same region anyway)
	  (when (overlay-get o-end 'parent)
	    (auto-o-delete-overlay (overlay-get o-end 'parent) 'no-parse))
	  (auto-o-match-overlay o-parent nil o-end))

	 (t ;; otherwise, make parent end-unmatched
	  (auto-o-match-overlay o-parent nil 'unmatched)))))
     ))
)



(defun auto-o-next-flat-match (o-match edge)
  ;; Find first match overlay for EDGE ('start of 'end) after match overlay
  ;; O-MATCH in buffer, with same set-id and definition-id as O-MATCH.

  ;; get sorted list of matching overlays after O-MATCH
  (let ((o-list
	 (sort (auto-overlays-in
		(overlay-start o-match) (point-max)  ; FIXME: is start right?
		`((identity auto-overlay-match)
		  (eq set-id ,(overlay-get o-match 'set-id))
		  (eq definition-id ,(overlay-get o-match 'definition-id))
		  (,(lambda (set-id definition-id regexp-id edge)
		      (eq (auto-o-entry-edge set-id definition-id regexp-id)
			  edge))
		   (set-id definition-id regexp-id) (,edge))))
	       (lambda (a b) (<= (overlay-start a) (overlay-start b))))))
    ;; if searching for same EDGE as O-MATCH, first overlay in list is always
    ;; O-MATCH itself, so we drop it
    (if (eq (auto-o-edge o-match) edge) (nth 1 o-list) (car o-list)))
)



;;; auto-overlay-flat.el ends here
