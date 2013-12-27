;;; gnus-BTS.el --- access the Debian Bug Tracking System from Gnus

;; Copyright (C) 2001 Andreas Fuchs <asf@acm.org>

;; Author: Andreas Fuchs
;; Maintainer: Andreas Fuchs <asf@acm.org>
;; Keywords: gnus, Debian, Bug
;; Status: Works in XEmacs (I think >=21)
;; Created: 2001-02-07

;; $Id: gnus-BTS.el,v 1.1.1.1 2003-04-04 20:16:01 lolando Exp $

;; This file is not part of GNU Emacs.

;; gnus-BTS.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; gnus-BTS.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Use this program if you read a lot of debian lists and see many
;; references to the Bug Tracking system in them. It expects to see
;; Bug references in the form of (for example): "#48273", "closes:
;; 238742" or similar.

;;; Code:


(setq anti-bug-special-keywords "reassign\\|merge")
(setq anti-bug-keywords (concat
			 "tags\\|severity\\|retitle\\|close\\|closes:\\|Merged\\|reopen\\|Bug\\|"
			 anti-bug-special-keywords))

(setq anti-bug-prefix " *#?\\|Bugs?\\|#")
(setq anti-bug-number " *\\([0-9]+\\)")
(setq anti-bug-special " +\\([0-9]+\\|[-.A-Za-z0-9]+\\)")

(setq anti-gnus-debian-bug-regexp (concat
				   "\\("
				   "\\("
				   anti-bug-keywords
				   "\\)"
				   anti-bug-prefix
				   "\\)"
				   anti-bug-number))

(setq anti-gnus-debian-reassign-or-merge-regexp
      (concat
       "\\("
       anti-bug-special-keywords
       "\\)"
       anti-bug-number
       anti-bug-special))

(setq anti-gnus-debian-reassign-regexp "reassigned from package `\\([^']*\\)' to `\\([^']*\\)'")
(setq anti-gnus-debian-bug-BTS-regexp "^ *\\([0-9]+\\)")

(defun anti-browse-debpkg-or-bug (thing)
  (interactive "i")
  (require 'thingatpt)
  (let* ((the-thing (if (null thing)
			(thing-at-point 'sexp)
		      thing))
	 (bugp (string-match "[0-9]+$" the-thing))
	 (bug-or-feature (if bugp
			     (progn
			       (string-match "^[^0-9]*\\([0-9]+\\)$" the-thing)
			       (match-string 1 the-thing))
			   the-thing))
	 (url (if bugp
		  "http://bugs.debian.org/cgi-bin/bugreport.cgi?bug="
		"http://cgi.debian.org/cgi-bin/search_packages.pl?&searchon=names&version=all&release=all&keywords=")))
    (browse-url (concat url bug-or-feature))))

(defvar in-debian-group-p nil)
(add-hook 'gnus-select-article-hook
	  (lambda ()
	    (setq in-debian-group-p (string-match "debian"
						  (gnus-group-real-name
						   gnus-newsgroup-name)))))

(defvar in-debian-devel-announce-group-p nil)
(add-hook 'gnus-select-article-hook
	  (lambda ()
	    (setq in-debian-devel-announce-group-p
		  (string-match "debian.devel.announce"
				(gnus-group-real-name
				 gnus-newsgroup-name)))))

(defun anti-buttonize-debian (regexp num predicate)
  (add-to-list 'gnus-button-alist
	       (list regexp
		     num
		     predicate
		     'anti-browse-debpkg-or-bug
		     num)))

(add-hook
 'gnus-article-mode-hook    ; only run once, as soon as the article buffer has been created.
 (lambda ()
   (anti-buttonize-debian anti-gnus-debian-bug-regexp 3
			  'in-debian-group-p)
   (anti-buttonize-debian anti-gnus-debian-reassign-or-merge-regexp 3
			  'in-debian-group-p)
   (anti-buttonize-debian anti-gnus-debian-bug-BTS-regexp 1
			  'in-debian-devel-announce-group-p)
   
   (anti-buttonize-debian anti-gnus-debian-reassign-regexp 1
			  'in-debian-group-p)
   (anti-buttonize-debian anti-gnus-debian-reassign-regexp 2
			  'in-debian-group-p)))

(provide 'gnus-BTS)
