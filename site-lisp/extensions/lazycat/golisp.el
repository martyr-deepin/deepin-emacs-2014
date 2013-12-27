;;; golisp.el --- navigate lisp source code with hyperlinks
;;              _ _           
;;   __ _  ___ | (_)___ _ __  
;;  / _` |/ _ \| | / __| '_ \ 
;; | (_| | (_) | | \__ \ |_) |
;;  \__, |\___/|_|_|___/ .__/ 
;;  |___/              |_|    
;;
;; Copyright (C) 2006  David O'Toole

;; Author: David O'Toole <dto@gnu.org>
;; Keywords: lisp, hypermedia, extensions, tools
;; Version: $Id: golisp.el,v 1.33 2006/10/26 12:43:35 dto Exp dto $	

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Golisp-mode is a small lisp code navigation and indexing tool for
;; GNU Emacs. It is currently under construction. Golisp-mode is
;; inspired in part by the ideas of Eduardo Ochs. 
;;
;;;; Features: 
;;
;; With golisp-mode you may: 
;;
;;  - Mark lisp code comments with plaintext "xref tags" that 
;;    identify a "target"
;;  - Targets may be names of concepts, names of sections within a file,
;;    or names of lisp functions/variables/macros et cetera.
;;  - Navigate through related xrefs with C-&
;;  - Jump from xref to definition of a target (including lisp definitions)
;;    with C-*
;;  - Jump from lisp definition to corresponding xrefs with C-&
;;  - Insert an index of targets into a file
;;
;; You may also analyze and cross-reference all the definitions and
;; references in a lisp file:
;; 
;;  - Pop up a window full of <<>>related links using M-x golisp-show-related.
;;    The first time you do this, there will be a delay while the buffer
;;    is analyzed. 
;;  - Update the analysis after editing, with M-x golisp-analyze
;;
;;;; Installation:
;; 
;; Put golisp.el somewhere in your load-path, and then add (require 'golisp)
;; to your .emacs initialization file. 
;;
;; The default bullets work in ASCII but they don't look very nice.
;; You may need to determine what fancy characters are available in
;; your default Emacs font, and set the appropriate variables:
;;
;; (setq golisp-xref-bullet 342435)
;; (setq golisp-anchor-bullet 343416)
;;
;; Those values work for me in the standard X "fixed" font.
;;
;; NOTE: This program is experimental and unfinished. Please report
;; any problems or feedback to dto@gnu.org
;;
;; The latest version is available from the project home page:
;; http://dto.freeshell.org/notebook/GoLisp.html
;;
;; Eduardo Ochs' website is at http://angg.twu.net/
;;

;; {{index}}
;;
;; <<index>>
;; <<minor mode>>
;; <(variable golisp-map)>
;; <(minor mode golisp-mode)>
;; <(function golisp-enable)>
;; <(function golisp-disable)>
;; <<tags>>
;; <(variable golisp-tag-types)>
;; <(function deftagtype)>
;; <(function golisp-read-tag)>
;; <(function golisp-link-to-anchor)>
;; <(function golisp-is-anchor)>
;; <(function golisp-is-definition)>
;; <(function golisp-find-target)>
;; <(function golisp-find-anchor)>
;; <(function golisp-handle-tag)>
;; <(function golisp-find-all-targets)>
;; <(function golisp-make-toc)>
;; <(function only-within-comment)>
;; <<analysis>>
;; <(variable *golisp-analysis*)>
;; <(variable *golisp-zap-target-buffer*)>
;; <(variable *golisp-definition-symbols*)>
;; <(variable golisp-symbol-reference-regexp)>
;; <(function golisp-explode-symbols)>
;; <(function golisp-analyze-buffer)>
;; <(function golisp-analyze)>
;; <(function golisp-insert-related-links)>
;; <(function golisp-insert-symbol-links)>
;; <(function golisp-show-related)>
;; <<interactive commands>>
;; <(function golisp-insert-toc)>
;; <(function golisp-zap)>
;; <(function golisp-zap-alt)>
;; <<xrefs>>
;; <(variable golisp-xref-regexp)>
;; <(function golisp-read-xref)>
;; <(function golisp-write-xref)>
;; <(function golisp-handle-xref)>
;; <(face golisp-xref-face)>
;; <(variable golisp-xref-face)>
;; <(variable golisp-xref-bullet)>
;; <<plain anchors>>
;; <(variable golisp-anchor-regexp)>
;; <(function golisp-read-anchor)>
;; <(face golisp-anchor-face)>
;; <(variable golisp-anchor-face)>
;; <(variable golisp-anchor-bullet)>
;; <<definitions>>
;; <(variable golisp-def-types)>
;; <(variable golisp-def-names)>
;; <(variable golisp-def-regexp)>
;; <(function golisp-read-def)>
;; <(function golisp-write-def)>
;; <<definition xrefs>>
;; <(variable golisp-defxref-regexp)>
;; <(function golisp-read-defxref)>
;; <(function golisp-write-defxref)>
;; <(function golisp-handle-defxref)>
;; <(face golisp-defxref-name-face)>
;; <(variable golisp-defxref-name-face)>
;; <<fontlocking>>
;; <(face golisp-bullet-face)>
;; <(variable golisp-bullet-face)>
;; <(function golisp-display-link)>
;; <(function golisp-do-font-lock)>




;;; Code:


(require 'cl)


;;;; {{minor mode}}


(defvar golisp-map nil)
;;(setq golisp-map nil)
(when (null golisp-map)
  (setq golisp-map (make-sparse-keymap))
  (define-key golisp-map [(control \&)] 'golisp-zap)
  (define-key golisp-map [(control \*)] 'golisp-zap-alt))
  

(define-minor-mode golisp-mode
  "Navigate lisp source code with hyperlinks."
  nil " Golisp" golisp-map
  (if golisp-mode
      (golisp-enable)
    (golisp-disable)))


(defun golisp-enable ()
  (golisp-do-font-lock 'font-lock-add-keywords)
  (font-lock-fontify-buffer))


(defun golisp-disable ()
  (golisp-do-font-lock 'font-lock-remove-keywords)
  (compose-region (point-min) (point-max) nil 'decompose-region))


;; {{tags}}
;;
;; Tags are text objects embedded in lisp source files. Golisp-mode
;; can read tags, index them, font-lock them, and follow cross
;; references.


(defvar golisp-tag-types nil)
(setq golisp-tag-types nil)


(defun deftagtype (&rest args)
  (push args golisp-tag-types))


(defun golisp-read-tag (string)
  "Try extracting a target from STRING."
  (some (lambda (type)
	  (destructuring-bind (&key reader &allow-other-keys)
	      type
	    (funcall reader string)))
	golisp-tag-types))


(defun golisp-link-to-anchor (string &optional print-anchor-p)
  "Return tag xref for anchor in STRING."
  (let ((target nil)
	(writer-function nil))
    (when (some (lambda (type)
		  (destructuring-bind (&key reader writer &allow-other-keys) type
		    (setf target (funcall reader string))
		    (when target (setf writer-function writer))))
		golisp-tag-types)
      (when writer-function
	(funcall writer-function target string print-anchor-p)))))


(defun golisp-is-anchor (string)
  "Return non-nil when an anchor is on the current line."
  (some (lambda (type)
	  (destructuring-bind (&key reader anchor-p &allow-other-keys)
	      type
	    (when (funcall reader string)
	      anchor-p)))
	golisp-tag-types))


(defun golisp-is-definition (string)
  "Return non-nil when an definition is on the current line."
  (some (lambda (type)
	  (destructuring-bind (&key reader definition-p &allow-other-keys)
	      type
	    (when (funcall reader string)
	      definition-p)))
	golisp-tag-types))

      
(defun golisp-find-target (original-line target &optional anchors-only-p)
  "Find the next link with target TARGET. If ANCHORS-ONLY-P is non-nil, 
find only the anchor."
  ;;
  ;; figure out if there is a target for whatever's on current line
  (let* ((search-for
	  (some (lambda (type)
		  (destructuring-bind 
		      (&key writer reader anchor-p &allow-other-keys)
		      type
		    (let ((string original-line))
		      ;; 			   (buffer-substring-no-properties 
		      ;; 				   (point-at-bol) (point-at-eol))))
		      (when (funcall reader string)
			(message "read from %S" string)
			(funcall writer target original-line anchors-only-p)))))
		golisp-tag-types))
	 ;;
	 ;; writers can return a list of strings.
	 ;; in that case, match any of them. 
	 (search-regexp (if (listp search-for)
			    (regexp-opt search-for)
			  (regexp-quote search-for))))
    (message "%S -> %S" search-for search-regexp)
    ;;
    ;; if so, look for a match
    (when search-regexp
      (when (not (re-search-forward search-regexp nil :noerror))
	(goto-char (point-min))
	(re-search-forward search-regexp nil :noerror)))))

     
(defun golisp-find-anchor (original-line target)
  (golisp-find-target original-line target :anchors-only))

	
(defun golisp-handle-tag (string &optional alternate-p)
  "Try extracting a target from STRING and taking an action on it."
  (some (lambda (type)
	  (destructuring-bind (&key reader handler &allow-other-keys) 
	      type
	    (let ((target (funcall reader string)))
	      (when target
		(funcall handler target string alternate-p)))))
	golisp-tag-types))
		  	  

(defun golisp-find-all-targets ()
  (let ((targets nil))
    (save-excursion
      (goto-char (point-min))
      ;;
      ;; for each line in the buffer, try to extract a target
      ;;
      (while (not (eobp))
	(let ((target (golisp-read-tag (buffer-substring-no-properties
					(point-at-bol)
					(point-at-eol)))))
	  (when (and target (not (member target targets)))
	    (push target targets)))
	;;
	(next-line)))
    (nreverse targets)))
	

(defun golisp-make-toc ()
  (interactive)
  (insert ";; {" "{index}" "}\n;;\n")
  (let ((links nil))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
	(let ((line (buffer-substring-no-properties
		     (point-at-bol) (point-at-eol)))
	      (target nil))
	  (dolist (type golisp-tag-types)
	    (destructuring-bind (&key reader writer anchor-p &allow-other-keys) 
		type
	      ;;
	      ;; only collect anchors
	      (when (and anchor-p
			 (setf target (funcall reader line)))
		(push (concat ";; "
			      (let ((link (funcall writer target line nil)))
				(if (listp link)
				    (car link)
				  link))				     
			      "\n")
		      links)))))
	(next-line))
      (nreverse links))))


(defun only-within-comment (regexp)
  "Wrap a regexp so that it can only match inside a comment."
  (concat "^[[:space:]]*;\\([^\n\<\{\(]*\\)\\(" regexp "\\)[[:space:]]*"))


;;;; {{analysis}}


(defvar *golisp-analysis* nil)
(make-variable-buffer-local '*golisp-analysis*)


(defvar *golisp-zap-target-buffer* nil)
(make-variable-buffer-local '*golisp-zap-target-buffer*)


(defvar *golisp-definition-symbols* nil)
(make-variable-buffer-local '*golisp-definition-symbols*)


(defvar golisp-symbol-reference-regexp 
  "[[:space:]]*\\(\#\'\\|\(\\|\'\\)?\\([^[:space:]]+\\)[[:space:]]*")


(defun golisp-explode-symbols (string)
  "Return a list of all symbol references in STRING."
  (let ((string2 (remove ?\( (remove ?\) string)))
	(pos 0)
	(refs nil))
    (block exploding
      (while (string-match golisp-symbol-reference-regexp string2 pos)
	(setf pos (match-end 2))
	(if pos
	    (push (match-string 2 string2) refs)
	  (return-from exploding))))
    refs))
    

(defun golisp-analyze-buffer () 
  "Attempt to analyze current buffer to determine how tags are
related to one another."
  (message "Analyzing buffer...")
  (when (null *golisp-definition-symbols*)
    (setf *golisp-definition-symbols* (make-hash-table :test 'equal)))
  ;;
  ;; Establish a mapping from targets to lists of the form
  ;; ((target score line-number) (target score line-number) ...)
  ;;
  (lexical-let ((related-from (make-hash-table :test 'equal))
		(related-to (make-hash-table :test 'equal))
		(symbol-references (make-hash-table :test 'equal))
		(symbol-references2 (make-hash-table :test 'equal))
		(current-section nil)
		(current-anchor nil)
		(current-anchor-line nil)
		(current-section-line nil)
		(previous-target nil)
		(anchors nil))
    (labels ((relate (target1 target2 score line)
		     (push (list target2 score (line-number-at-pos) line)
			   (gethash target1 related-from))
		     (push (list target1 score (line-number-at-pos) line)
			   (gethash target2 related-to))))
      (save-excursion
	(goto-char (point-min))
	(while (not (eobp))
	  (let* ((line (buffer-substring-no-properties
			(point-at-bol) (point-at-eol)))
		 (target (golisp-read-tag line))
		 (symbols nil))
	    ;;
	    ;; now analyze whether any links are on current line
	    (if target 
		;;
		;; an anchor can have an effect on many targets
		;;
		(if (golisp-is-anchor line)
		    (progn 
		      (setf current-anchor-line line)
		      ;;
		      ;; is it a definition or a section heading?
		      (if (golisp-is-definition line)
			  ;;
			  ;; it's a definition. relate it to the current section.
			  (progn 
			    (puthash target line *golisp-definition-symbols*)
			    (relate target current-section 100 current-anchor-line)
			    ;; keep track of defs in order they appear
			    (setf current-anchor target)
			    (setf previous-target target))
			;;
			;; it's a section heading. 
			(setf current-section-line line)
			(setf current-anchor target)
			(setf current-section target)))
		  ;;
		  ;; it's an ordinary xref
		  (when (not (equal current-anchor target))
		    (relate target current-anchor 4 current-anchor-line))
		  (when (not (equal current-section target))
		    (relate target current-section 8 current-section-line))
		  ;;
		  (when previous-target
		    (relate target previous-target 1 current-anchor-line)
		    (setf previous-target target)))
	      ;;
	      ;; no links on current line. look at symbol references
	      (setf symbols (golisp-explode-symbols line))
	      (dolist (sym symbols)
		(when current-anchor
		  (push (list current-anchor 9 (line-number-at-pos) current-anchor-line)
			(gethash sym symbol-references))
		  (push (list sym 9 (line-number-at-pos) current-anchor-line)
			(gethash current-anchor symbol-references2))))))
	  
	  (next-line))))
    (message "Analyzing buffer... Done.")
    (values *golisp-definition-symbols*
	    symbol-references 
	    symbol-references2 
	    related-to 
	    related-from)))


(defun golisp-analyze ()
  (interactive)
  (setf *golisp-analysis* (golisp-analyze-buffer)))


(defun golisp-insert-related-links (things)
  (interactive)
  (dolist (thing (remove-duplicates things :test (lambda (i j)
						   (string= (fourth i)
							    (fourth j)))))
    (destructuring-bind (target score line-number line) thing
      (let ((tag (golisp-link-to-anchor line)))
	(insert ";; ")
	(if (listp tag)
	    (insert (car tag))
	  (insert tag))
	(insert "\n")))))


(defun golisp-insert-symbol-links (current-anchor definition-symbols things)
  (interactive)
  (dolist (thing (remove-duplicates things :test (lambda (i j)
						   (string= (first i)
							    (first j)))))
    (destructuring-bind (target score line-number line) thing
      (let ((defline (gethash target definition-symbols)))
	(when defline
	  (let ((tag (golisp-link-to-anchor defline)))
	    (when tag
	      (insert ";; ")
	      (if (listp tag)
		  (insert (car tag))
		(insert tag))
	      (insert "\n"))))))))
    

(defun golisp-show-related ()
  (interactive)
  ;;
  ;; first find out the current anchor 
  (let ((current-anchor nil)
	(line nil)
	(target-buffer (current-buffer)))
    (save-excursion
      (block searching
	(while (not (bobp))
	  (setf line (buffer-substring-no-properties (point-at-bol) (point-at-eol)))
	  (when (golisp-is-anchor line)
	    (setf current-anchor (golisp-read-tag line))
	    (return-from searching))
	  (previous-line))))
    ;; 
    ;; now produce links to related stuff
    (when (null *golisp-analysis*)
      (setf *golisp-analysis* (golisp-analyze-buffer)))
    (multiple-value-bind (definition-symbols
			   symbol-references symbol-references2 
			   related-to related-from)
	*golisp-analysis*
      (let ((buf (get-buffer-create "*golisp-related*")))
	(pop-to-buffer buf)
	(emacs-lisp-mode)
	(golisp-mode 1)
	(delete-region (point-min) (point-max))
	(setq *golisp-zap-target-buffer* target-buffer)
	(let ((things-related-to (gethash current-anchor related-to))
	      (things-related-from (gethash current-anchor related-from))
	      (things-called-from (gethash current-anchor symbol-references))
	      (things-called (gethash current-anchor symbol-references2)))
	  
	  (labels ((sort-things (things)
				(sort things (lambda (i j)
					       (> (car (cdr i))
						  (car (cdr j)))))))
	    (insert ";; " (let ((tag (golisp-link-to-anchor line)))
			    (if (listp tag)
				(car tag)
			      tag))
		    "\n;;\n")
	    (insert ";; In documentation threads:\n")
	    (golisp-insert-related-links (sort-things things-related-from))
	    (insert ";;\n;; Referred to by:\n")
	    (golisp-insert-related-links (sort-things things-called-from))
	    (insert "\n")
	    (insert ";;\n;; Refers to:\n")
	    (golisp-insert-symbol-links current-anchor
					definition-symbols 
					(sort-things things-called))	      
	    (insert "\n;; Points to documentation threads:\n")
	    (golisp-insert-related-links (sort-things things-related-to))))))
    (goto-char (point-min))
    (pop-to-buffer target-buffer)))
		          		

;;;; {{interactive commands}}


(defun golisp-insert-toc ()
  "Insert a table of contents for all anchors in the document."
  (interactive)
  (mapc 'insert (golisp-make-toc)))


(defun golisp-zap (&optional alternate-p)
  "Attempt to handle any tag on the current line."
  (interactive)
  (lexical-let ((line (buffer-substring-no-properties (point-at-bol)
						      (point-at-eol))))
    (if (not *golisp-zap-target-buffer*)
	(golisp-handle-tag line alternate-p)
      ;;
      ;; otherwise, jump and show buffer
      (message "popping")
      (let ((buf *golisp-zap-target-buffer*))
	(select-window (get-buffer-window buf))
	(with-current-buffer buf
	  (message line)
	  (golisp-handle-tag line alternate-p))))))


(defun golisp-zap-alt ()
  "Attempt alternate action on tag on current line."
  (interactive)
  (golisp-zap :alternate))


;;;;; {{xrefs}}


(defvar golisp-xref-regexp "\\(\<\<\\)\\([^>\n]*\\)\\(\>\>\\)"
  "Regular expression to match cross references tags.")


(defun golisp-read-xref (string)
  "Return the target name (if any) of the cross reference in
string STRING."
  (when (string-match golisp-xref-regexp string)
    (match-string 2 string)))


(defun golisp-write-xref (target line anchor-p)
  "Return a string with TARGET formatted as a cross reference."
  (if anchor-p
      (concat "{" "{" target "}" "}")
    (list
     (concat "<" "<" target ">" ">")
     (concat "{" "{" target "}" "}"))))


(defun golisp-handle-xref (target line &optional alternate-p)
  "Find the next occurrence of the TARGET tag. If ALTERNATE-P is
non-nil, find only the anchor for the TARGET."
  (let ((finder (if alternate-p 
		    'golisp-find-anchor 
		  'golisp-find-target)))
    (funcall finder line target)))
  

(defface golisp-xref-face '((t (:foreground "yellow" :bold t :weight bold)))
  "Face for golisp links.")


(defvar golisp-xref-face 'golisp-xref-face)


(defvar golisp-xref-bullet ?>)
;;(setq golisp-xref-bullet 342435)


(deftagtype 
  :name "xref"
  :regexp (only-within-comment golisp-xref-regexp)
  :reader 'golisp-read-xref 
  :writer 'golisp-write-xref
  :handler 'golisp-handle-xref
  :faces '((2 golisp-xref-face))
  :bullet golisp-xref-bullet)


;;;;; {{plain anchors}}
;;
;; These mark top-level sections within a code document. 
;; Other anchor types are possible. 

(defvar golisp-anchor-regexp "\\(\{\{\\)\\([^>\n]*\\)\\(\}\}\\)")


(defun golisp-read-anchor (string)
  (when (string-match golisp-anchor-regexp string)
    (match-string 2 string)))


(defface golisp-anchor-face '((t (:foreground "yellow")))
  "Face for anchor links.")


(defvar golisp-anchor-face 'golisp-anchor-face)


(defvar golisp-anchor-bullet ?@)
;;(setq golisp-anchor-bullet 343416)


(deftagtype
  :name "anchor"
  :regexp (only-within-comment golisp-anchor-regexp)
  :reader 'golisp-read-anchor
  :writer 'golisp-write-xref
  :handler 'golisp-handle-xref
  :faces '((2 golisp-xref-face))
  :bullet golisp-anchor-bullet
  :anchor-p t)


;;;;; {{definitions}}
;;


(defvar golisp-def-types '("defadvice" "defalias" "defgeneric"
			   "defmacro" "defmethod" "defun"
			   "defsetf" "defsubst" "defcondition"
			   "define-derived-mode" "define-minor-mode"
			   "define-method-combination"
			   "define-setf-expander" "define-compiler-macro"
			   "define-symbol-macro" "define-modify-macro"
			   "defconstant" "defvar" "defparameter"
			   "defgroup" "defcustom" "defface"
			   "defclass" "defstruct" "defpackage"
			   "deftype"))

;; these two lists' members should be kept in correspondence

(defvar golisp-def-names '("advice" "alias" "generic function"
			   "macro" "method" "function"
			   "setf function" "substitution" "condition"
			   "derived mode" "minor mode"
			   "method combination"
			   "setf expander" "compiler macro"
			   "symbol macro" "modify macro"
			   "constant" "variable" "parameter"
			   "customization group" "customization variable"
			   "face" "class" "structure" "package" "type"))


(defvar golisp-def-regexp (concat "(" 
				  "\\(" 
				  (regexp-opt golisp-def-types)
				  ;; 				  (mapconcat 'regexp-quote golisp-def-types
				  ;; 					     "\\|")
				  "\\)"
				  " \\([^\n ]*\\)"))


(defun golisp-read-def (string)
  (when (string-match golisp-def-regexp string)
    (match-string 2 string)))


(defun golisp-write-def (target line anchor-p)	
  (string-match golisp-def-regexp line)
  (let ((type (match-string 1 line)))
    (when type
      (let ((pos (position-if (lambda (x)
				(string= type x))
			      golisp-def-types)))
	(when pos
	  (if anchor-p
	      (concat "(" type " " target " ")
	    (list 
	     (concat "<" "(" (nth pos golisp-def-names) " " target ")" ">")
	     (concat "(" type " " target " "))))))))
	

(deftagtype
  :name "def"
  :regexp golisp-def-regexp
  :reader 'golisp-read-def
  :writer 'golisp-write-def
  :handler 'golisp-handle-xref
  :anchor-p t
  :definition-p t)


;;;; {{definition xrefs}}
;;


(defvar golisp-defxref-regexp (concat "\\(" "<" "(" "\\)"
				      "\\("
				      (regexp-opt golisp-def-names)
				      ;; 				      (mapconcat 'regexp-quote golisp-def-names
				      ;; 						 "\\|")
				      "\\)"
				      " \\([^\n ]*\\)" "\\()" ">\\)"))


(defun golisp-read-defxref (string)
  (when (string-match golisp-defxref-regexp string)
    (match-string 3 string)))


(defun golisp-write-defxref (target line anchor-p)
  (let ((type-keys nil))
    (if (string-match golisp-defxref-regexp line)
	(setf type-keys golisp-def-names)
      (when (string-match golisp-def-regexp line)
	(setf type-keys golisp-def-types)))
    (let ((type (match-string 2 line)))
      (when type
	(let ((pos (position-if (lambda (x)
				  (string= type x))
				type-keys)))
	  (when pos
	    (if anchor-p
		(concat "(" (nth pos golisp-def-types) " " target " ")
	      (list (concat "(" (nth pos golisp-def-types) " " target " ")
		    (concat "<" "(" (nth pos golisp-def-names) " "
			    target ")" ">")))))))))
		      

(defun golisp-handle-defxref (target line &optional alternate-p)
  (golisp-handle-xref target line :alternate))
			  

(defface golisp-defxref-name-face '((t (:foreground "aquamarine")))
  "Face for type-name of defxref links.")


(defvar golisp-defxref-name-face 'golisp-defxref-name-face)


(deftagtype
  :name "defxref"
  :regexp (only-within-comment golisp-defxref-regexp)
  :reader 'golisp-read-defxref
  :writer 'golisp-write-defxref
  :handler 'golisp-handle-xref
  :faces '((2 golisp-defxref-name-face) (3 golisp-xref-face))
  :bullet golisp-xref-bullet)


;;;; {{fontlocking}}


(defface golisp-bullet-face '((t (:foreground "yellow")))
  "Face for golisp bullets.")


(defvar golisp-bullet-face 'golisp-bullet-face)


(defun golisp-display-link (beg end bullet-character face)
  ;;
  ;; don't do this more than once at a given location.
  ;; otherwise you get slowdown.
  (when (not (get-text-property beg 'golisp-fontified))
    (add-text-properties beg (+ beg 1) (list 'golisp-fontified t))
    (compose-region beg (+ beg 1) bullet-character)
    (add-text-properties (+ 1 beg) (+ 2 beg)
			 (list 'display " "))
    (add-text-properties (+ 2 beg) (- end 2)
			 (list 'face face))
    (add-text-properties (- end 2) end
			 (list 'invisible t))))
		       

(defun golisp-do-font-lock (func)
  "Add or remove font-lock rules for all tag types."
  (mapc (lambda (type)
	  (destructuring-bind (&key regexp faces bullet &allow-other-keys) type
	    (when faces
	      (labels ((make-rules (faces)
				   (let ((rules nil)
					 (rule nil))
				     (dolist (face-spec faces)
				       (destructuring-bind (subexp face) face-spec
					 (setf rule `(,(+ subexp 2)
						      ,(if (null rules)
							   `(let ((beg (match-beginning 3))
								  (end (match-end 0)))
							      (golisp-display-link beg end 
										   ,bullet ,face)
							      ,face)
							 face) prepend))
					 (push rule rules)))
				     (push `(3 golisp-bullet-face prepend)
					   rules)
				     (nreverse rules))))
		(let ((R (make-rules faces)))
		  (funcall func nil (list `(,regexp ,@R))))))))
	golisp-tag-types))


(provide 'golisp)
;;; golisp.el ends here



