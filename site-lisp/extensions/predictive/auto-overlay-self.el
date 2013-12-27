
;;; auto-overlay-self.el --- self-delimited automatic overlays


;; Copyright (C) 2005-2007 Toby Cubitt

;; Author: Toby Cubitt <toby-predictive@dr-qubit.org>
;; Version: 0.2.8
;; Keywords: automatic, overlays, self
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
;; Version 0.2.8
;; * renamed 'entry-id and 'subentry-id to 'definition-id and 'regexp-id
;;
;; Version 0.2.7
;; * fixed bug in `auto-o-parse-self-match' which caused a matched self match
;;   to have null 'parent property if a new self match was created inside an
;;   existing self overlay
;;
;; Version 0.2.6
;; * set overlay properties straight after creation in `auto-o-make-self',
;;   rather than leaving it to `auto-overlay-update', in case matching causes
;;   exclusive reparsing, for which properties are already required
;;
;; Version 0.2.5
;; * removed `auto-overlay-functions' and changed to use new interface
;;
;; Version 0.2.4
;; * fixed(?) bug in auto-o-self-list that caused it to
;;   sometimes miss out the parent overlay itself from the list
;;
;; Version 0.2.3
;; * updated to reflect changes in `auto-overlays.el'
;; * changed `auto-o-self-list' to make it run faster
;;
;; Version 0.2.2
;; * small but important bug fix
;;
;; Version 0.2.1
;; * bug fixes
;;
;; Version 0.2
;; * substantially re-written to postpone cascading until absolutely
;;   necessary, for improved responsiveness
;;
;; Version 0.1
;; * initial version separated off from auto-overlays.el



;;; Code:


(require 'auto-overlays)
(provide 'auto-overlay-self)

(defvar auto-o-pending-self-cascade nil)

;; set self overlay parsing and suicide functions
(put 'self 'auto-overlay-parse-function 'auto-o-parse-self-match)
(put 'self 'auto-overlay-suicide-function 'auto-o-self-suicide)

;; add initialisation and clear functions to hooks
(add-hook 'auto-overlay-load-hook 'auto-o-self-load)
(add-hook 'auto-overlay-unload-hook 'auto-o-self-unload)



(defun auto-o-self-load ()
  ;; Make sure `auto-o-perform-self-cascades' is in `before-change-functions',
  ;; so that any cascading that is required is performed before anything else
  ;; happens.
  (add-hook 'before-change-functions 'auto-o-perform-self-cascades
	    nil t)
  ;; initialise variables
  (setq auto-o-pending-self-cascade nil)
)


(defun auto-o-self-unload ()
  ;; Remove `auto-o-perform-self-cascades' from `before-change-functions'.
  (remove-hook 'before-change-functions 'auto-o-perform-self-cascades t)
)




(defun auto-o-parse-self-match (o-match)
  ;; perform any necessary updates of auto overlays due to a match for a self
  ;; regexp
  
  (let* ((overlay-list (auto-o-self-list o-match))
	 (o (car overlay-list)))
    
    (cond
     ;; if stack is empty, create a new end-unmatched overlay, adding it to
     ;; the list of unascaded overlays (avoids treating it as a special
     ;; case), and return it
     ((null overlay-list)
      (auto-o-make-self o-match nil))
     
     ;; if new delimiter is inside the first existing overlay and existing one
     ;; is end-unmatched, just match it
     ((and (not (overlay-get o 'end))
	   (>= (overlay-get o-match 'delim-start) (overlay-start o)))
      (auto-o-match-overlay o nil o-match 'no-props)
      ;; remove it from the list of uncascaded overlays
      (setq auto-o-pending-self-cascade (delq o auto-o-pending-self-cascade))
      ;; return nil since haven't created any new overlays
      nil)
     
     
     ;; otherwise...
     (t
      (let (o-new)
	;; if the new match is outside existing overlays...
	(if (< (overlay-get o-match 'delim-end) (overlay-start o))
	    ;; create overlay from new match till start of next match, and add
	    ;; it to the list of uncascaded overlays
	    (setq o-new (auto-o-make-self
			 o-match
			 (overlay-get (overlay-get o 'start) 'delim-start)))
	  
	  ;; if the new match is inside an existing overlay...
	  (setq o (pop overlay-list))
	  ;; create overlay from end of existing one till start of the one
	  ;; after (or end of buffer if there isn't one), and add it to the
	  ;; list of uncascaded overlays
	  (setq o-new (auto-o-make-self
		       (overlay-get o 'end)
		       (when overlay-list
			 (overlay-get (overlay-get (car overlay-list) 'start)
				      'delim-start))))
	  ;; match end of existing one with the new match, protecting its old
	  ;; end match which is now matched with start of new one
	  (auto-o-match-overlay o nil o-match 'no-props nil 'protect-match))
      
      ;; return newly created overlay
      o-new))
     ))
)




(defun auto-o-self-suicide (o-self)
  ;; Called when match no longer matches. Unmatch the match overlay O-SELF, if
  ;; necessary deleting its parent overlay or cascading.

  (let ((o-parent (overlay-get o-self 'parent)))
    (cond
     ;; if parent is end-unmatched, delete it from buffer and from list of
     ;; uncascaded overlays
     ((not (auto-o-end-matched-p o-parent))
      (auto-o-delete-overlay o-parent)
      (setq auto-o-pending-self-cascade
	    (delq o-parent auto-o-pending-self-cascade)))

     ;; if we match the end of parent...
     ((eq (overlay-get o-parent 'end) o-self)
      ;; unmatch ourselves from parent and extend parent till next overlay, or
      ;; end of buffer if there is none
      (let ((o (nth 1 (auto-o-self-list o-self))))
	(auto-o-match-overlay
	 o-parent nil (if o (overlay-get (overlay-get o 'start) 'delim-start)
			'unmatched)))
      ;; add parent to uncascaded overlay list
      (push o-parent auto-o-pending-self-cascade))

     ;; if we match the start of parent...
     (t
      (let* ((o-end (overlay-get o-parent 'end))
	     (o (nth 1 (auto-o-self-list o-end))))
	;; unmatch ourselves from parent and "flip"
	(auto-o-match-overlay
	 o-parent o-end
	 (if o (overlay-get (overlay-get o 'start) 'delim-start)
	   'unmatched)))
      ;; add parent to uncascaded overlay list
      (push o-parent auto-o-pending-self-cascade))
     ))
)




(defun auto-o-make-self (o-start &optional end)
  ;; Create a self overlay starting at match overlay O-START.
  ;; If END is a number or marker, the new overlay is end-unmatched and ends
  ;; at the buffer location specified by the number or marker.
  ;; If END is nil, the new overlay is end-unmatched and ends at the end of
  ;; the buffer.
  (let (o-new)
    
    ;; create new overlay (location ensures right things happen when matched)
    (let (pos)
      (cond
       ((overlayp end) (setq pos (overlay-get end 'delim-start)))
       ((number-or-marker-p end) (setq pos end))
       (t (setq pos (point-max))))
      (setq o-new (make-overlay pos pos nil nil 'rear-advance)))
    
    ;; give overlay some basic properties
    (overlay-put o-new 'auto-overlay t)
    (overlay-put o-new 'set-id (overlay-get o-start 'set-id))
    (overlay-put o-new 'definition-id (overlay-get o-start 'definition-id))
    
    ;; if overlay is end-unmatched, add it to the list of uncascaded overlays
    (unless (overlayp end) (push o-new auto-o-pending-self-cascade))
    
    ;; match the new overlay and return it
    (auto-o-match-overlay o-new o-start (if (overlayp end) end nil))
    o-new)
)




(defun auto-o-perform-self-cascades (beg end)
  ;; Perform any necessary self-overlay cascading before the text in the
  ;; buffer is modified. Called from `before-change-functions'.

  ;; check all overlays waiting to be cascaded, from first in buffer to last
  (dolist (o (sort auto-o-pending-self-cascade
		   (lambda (a b) (< (overlay-start a) (overlay-start b)))))
    ;; if buffer modification occurs after the end of an overlay waiting to be
    ;; cascaded, cascade all overlays between it and the modified text
    (when (and (overlay-end o) (< (overlay-end o) end))
      (auto-o-self-cascade (auto-o-self-list (overlay-get o 'start) end))))
)




(defun auto-o-self-cascade (overlay-list)
  ;; "Flip" overlays down through buffer (assumes first overlay in list is
  ;; end-unmatched).
  (when (> (length overlay-list) 1)
    (let ((o (car overlay-list))
	  (o1 (nth 1 overlay-list)))
      
      ;; match first (presumably end-matched) overlay and remove it from list
      (pop overlay-list)
      (auto-o-match-overlay o nil (overlay-get o1 'start) 'no-props)
      ;; remove it from list of uncascaded overlays
      (setq auto-o-pending-self-cascade (delq o auto-o-pending-self-cascade))
      ;; if we've hit an end-unmatched overlay, we can stop cascading
      (if (not (auto-o-end-matched-p o1))
	  (progn
	    (auto-o-delete-overlay o1 nil 'protect-match)
	    (setq auto-o-pending-self-cascade
		  (delq o1 auto-o-pending-self-cascade)))

	;; otherwise, cascade overlay list till one is left or we hit an
	;; end-unmached overlay
	(unless
	    (catch 'stop
	      (dotimes (i (1- (length overlay-list)))
		(setq o (nth i overlay-list))
		(setq o1 (nth (1+ i) overlay-list))
		(auto-o-match-overlay o (overlay-get o 'end)
				      (overlay-get o1 'start)
				      'no-props nil 'protect-match)
		;; if we hit an end-unmatched overlay, we can stop cascading
		(when (not (auto-o-end-matched-p o1))
		  (throw 'stop (progn
				 ;; delete the end-unmatched overlay
				 (auto-o-delete-overlay o1 nil 'protect-match)
				 ;; remove it from uncascaded overlays list
				 (setq auto-o-pending-self-cascade
				       (delq o1 auto-o-pending-self-cascade))
				 ;; return t to indicate cascading ended early
				 t)))))
	  
	  ;; if there's an overlay left, "flip" it so it's end-unmatched and
	  ;; extends to next overlay in buffer, and add it to the list of
	  ;; unmatched overlays
	  (let (pos)
	    (setq o (car (last overlay-list)))
	    (if (setq o1 (nth 1 (auto-o-self-list (overlay-get o 'end))))
		(setq pos (overlay-get (overlay-get o1 'start) 'delim-start))
	      (setq pos (point-max)))
	    (auto-o-match-overlay o (overlay-get o 'end) pos
				  'no-props nil 'protect-match))
	  (push o auto-o-pending-self-cascade)))
      ))
)




;; (defun auto-o-self-list (o-start &optional end)
;;   ;; Return list of self overlays ending at or after match overlay O-START and
;;   ;; starting before or at END, corresponding to same entry as O-START. If END
;;   ;; is null, all overlays after O-START are included.

;;   (when (null end) (setq end (point-max)))
  
;;   (let (overlay-list)
;;     ;; create list of all overlays corresponding to same entry between O-START
;;     ;; and END
;;     (mapc (lambda (o) (when (and (>= (overlay-end o)
;; 				     (overlay-get o-start 'delim-start))
;; 				 (<= (overlay-start o) end))
;; 			(push o overlay-list)))
;; 	  (auto-overlays-in
;; 	   (point-min) (point-max)
;; 	   (list
;; 	    '(identity auto-overlay)
;; 	    (list 'eq 'set-id (overlay-get o-start 'set-id))
;; 	    (list 'eq 'definition-id (overlay-get o-start 'definition-id)))))
;;     ;; sort the list by start position, from first to last
;;     (sort overlay-list
;; 	  (lambda (a b) (< (overlay-start a) (overlay-start b)))))
;; )



(defun auto-o-self-list (o-start &optional end)
  ;; Return list of self overlays ending at or after match overlay O-START and
  ;; starting before or at END, corresponding to same entry as O-START. If END
  ;; is null, all overlays after O-START are included.

  (when (null end) (setq end (point-max)))
  
  (let (overlay-list)
    ;; create list of all overlays corresponding to same entry between O-START
    ;; and END
    (setq overlay-list
	  ;; Note: We subtract 1 from start and add 1 to end to catch overlays
	  ;;       that end at start or start at end. This seems to give the
	  ;;       same results as the old version of `auto-o-self-list'
	  ;;       (above) in all circumstances.
	  (auto-overlays-in
	   (1- (overlay-get o-start 'delim-start)) (1+ end)
	   (list
	    '(identity auto-overlay)
	    (list 'eq 'set-id (overlay-get o-start 'set-id))
	    (list 'eq 'definition-id (overlay-get o-start 'definition-id)))))
    ;; sort the list by start position, from first to last
    (sort overlay-list
	  (lambda (a b) (< (overlay-start a) (overlay-start b)))))
)


;;; auto-overlay-self.el ends here
