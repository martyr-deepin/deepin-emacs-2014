
;;; predictive-auto-overlay-auto-dict.el --- automatic overlays with automatic
;;;                                          dictionary update


;; Copyright (C) 2008 Toby Cubitt

;; Author: Toby Cubitt <toby-predictive@dr-qubit.org>
;; Version: 0.2
;; Keywords: predictive, automatic, overlays, dictionary, auto-dict
;; URL: http://www.dr-qubit.org/emacs.php


;; This file is part of the Emacs Predictive Completion Mode package.
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
;; Version 0.2
;; * moved utility functions from predictive-latex.el
;;
;; Version 0.1.1
;; * add required `require's
;;
;; Version 0.1
;; * initial release




;;; Code:


(require 'auto-overlays)
(require 'auto-overlay-word)
(require 'dict-tree)
(require 'predictive)

(provide 'predictive-auto-overlay-auto-dict)

;; set auto-dict overlay parsing and suicide functions, and indicate class
;; requires separate start and end regexps
(put 'predictive-auto-dict 'auto-overlay-parse-function
     'predictive-parse-auto-dict-match)
(put 'predictive-auto-dict 'auto-overlay-suicide-function
     'predictive-auto-dict-suicide)


(defun predictive-parse-auto-dict-match (o-match)
  ;; Create a new word overlay, and add its contents to a dictionary

  ;; create new word overlay
  (let ((o-new (auto-o-parse-word-match o-match))
	word dict)
    ;; extract word and get dict
    (setq word (buffer-substring-no-properties
		(overlay-get o-match 'delim-start)
		(overlay-get o-match 'delim-end)))
    (setq dict (overlay-get o-new 'auto-dict))
    ;; save word and dict in overlay properties
    (overlay-put o-match 'word word)
    (overlay-put o-match 'auto-dict dict)
    ;; add change function to overlay modification hooks
    (overlay-put o-new 'modification-hooks
		 (cons 'predictive-schedule-auto-dict-update
		       (overlay-get o-new 'modification-hooks)))
    (overlay-put o-new 'insert-in-front-hooks
		 (cons 'predictive-schedule-auto-dict-update
		       (overlay-get o-new 'insert-in-front-hooks)))
    (overlay-put o-new 'insert-behind-hooks
		 (cons 'predictive-schedule-auto-dict-update
		       (overlay-get o-new 'insert-behind-hooks)))
    ;; add word to dictionary
    (unless (dictree-p dict) (setq dict (eval dict)))
    (predictive-add-to-dict dict word 0)
    ;; return the new overlay
    o-new)
)



(defun predictive-auto-dict-suicide (o-match)
  ;; Delete the word overlay, and delete the word from the dictionary

  (let ((word (overlay-get o-match 'word))
	(dict (overlay-get o-match 'auto-dict)))
    ;; delete the overlay
    (auto-o-delete-overlay (overlay-get o-match 'parent))
    ;; delete the word from the dictionary
    (unless (dictree-p dict) (setq dict (eval dict)))
    (dictree-delete dict word))
)



(defun predictive-schedule-auto-dict-update
  (o-self modified &rest unused)
  ;; All auto-dict overlay modification hooks are set to this function, which
  ;; schedules `predictive-auto-dict-update' to run after any suicide
  ;; functions have been called
  (unless modified
    (add-to-list 'auto-o-pending-post-suicide
		 (list 'predictive-auto-dict-update o-self)))
)



(defun predictive-auto-dict-update (o-self)
  ;; Update the auto-dict with new word. Runs after modifications.

  (let ((dict (overlay-get (overlay-get o-self 'start) 'auto-dict))
	word)
    (unless (dictree-p dict) (setq dict (eval dict)))
    ;; delete old word from label dictionary
    (dictree-delete dict (overlay-get (overlay-get o-self 'start) 'word))

    ;; if overlay has not been deleted...
    (when (overlay-buffer o-self)
      ;; extract word
      (setq word (buffer-substring-no-properties
		  (overlay-start o-self) (overlay-end o-self)))
      ;; save label in overlay property
      (overlay-put (overlay-get o-self 'start) 'word word)
      ;; add new label to dictionary
      (predictive-add-to-dict dict word 0)))
)



;;; =================================================================
;;;    Utility functions for automatically generated dictionaries

(defmacro predictive-auto-dict-name (name)
  ;; Return a dictionary name constructed from NAME and the buffer name
  `(intern
    (concat "dict-" ,name "-"
	    (file-name-sans-extension
	     (file-name-nondirectory (buffer-file-name))))))



(defun predictive-load-auto-dict (name)
  "Load/create a NAME dictionary for the current buffer."
  (let ((dict (intern (concat "predictive-" name "-dict")))
	dictname filename)
    (cond
     ;; if buffer is associated with a file...
     ((buffer-file-name)
      (setq dictname (predictive-auto-dict-name name))
      (setq filename
	    (concat (file-name-directory (buffer-file-name))
		    predictive-auxiliary-file-location
		    (symbol-name dictname) ".elc"))
      ;; create directory for dictionary file if necessary
      (predictive-create-auxiliary-file-location)
      ;; if a dictionary isn't loaded, load or create it
      (unless (featurep dictname)
	(if (not (file-exists-p filename))
	    (predictive-create-dict dictname filename)
	  (load filename)
	  (predictive-load-dict dictname)
	  ;; FIXME: probably shouldn't be using an internal dict-tree.el
	  ;;        function
	  (dictree--set-filename (eval dictname) filename)))
      ;; set the NAME dictionary to the loaded/new dictionary
      (set dict (eval dictname)))

     ;; if buffer is not associated with a file,
     (t
      (set dict (predictive-create-dict))
      (setq dict (eval dict))
      ;; FIXME: shouldn't be using internal dict-tree.el functions. Probably
      ;;        need to make `predictive-create-dict' interface more flexible.
      (dictree--set-name dict name)
      (dictree--set-autosave dict nil))
     ))
)



(defun predictive-unload-auto-dict (name)
  "Unload and possibly save the current buffer's NAME dictionary."
  (let ((dict (eval (intern (concat "predictive-" name "-dict")))))
    (dictree-unload (if (dictree-p dict) dict (eval dict))))
)


;;; predictive-auto-overlay-auto-dict.el ends here
