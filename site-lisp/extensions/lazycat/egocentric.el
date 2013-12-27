;;; @(#) egocentric.el --- highlight your name inside emacs buffers

;;; @(#) $Id: egocentric.el,v 1.1.1.1 2003/04/04 20:15:59 lolando Exp $

;; This file is *NOT* part of GNU Emacs.

;; Copyright (C) 2001 by Benjamin Drieu
;; Author:	 Benjamin Drieu <bdrieu@april.org>
;; Maintainer:	 Benjamin Drieu <bdrieu@april.org>
;; Created:	 2001-04-23
;; Keywords: convenience

;; LCD Archive Entry:
;; egocentric|Benjamin Drieu|bdrieu@april.org|
;; Highlight occurences of your name in buffers|
;; 23-Apr-2001|$Revision: 1.1.1.1 $|~/misc/egocentric.el|

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This package highlights occurrences of your own name and/or
;; nickname.  Quite useful for daily kibozing.

;; Main purpose is to be used within your favourite Emacs mailer.  To
;; use egocentric.el with Gnus, simply use the following inside your
;; Gnus init file.
;;
;; (add-hook 'gnus-article-prepare-hook 'egocentric-mode)
;; (autoload 'egocentric-mode "egocentric"
;;           "Highlight your name or various keywords in buffers")

;;; To do:

;; - take care of all sorts of accents
;; - if $NAME isn't set, get it from other sources


;;; History:
;; 

;;; Code:

(provide 'egocentric)

;; Various customization

(defgroup egocentric nil
  "Highlight your name in arbitrary buffers."
  :group 'files
  :group 'convenience)

(defcustom egocentric-additional-keywords nil
  "*Additionnal keywords to highlight added by user."
  :type 'list
  :group 'egocentric)

(defcustom egocentric-additional-regexps nil
  "*Additionnal regexps to highlight added by user."
  :type 'list
  :group 'egocentric)

(defcustom egocentric-accents-translation-alist
  `(("é" . "\\(e\\|é\\|=E9\\)")
    ("è" . "\\(e\\|è\\|=E8\\)")
    ("ê" . "\\(e\\|ê\\|=EA\\)")
    ("ï" . ,(concat "\\(i\\|ï\\|=EF\\)"))) ; [TODO] contribute here ;-)
  "Translation from accents to ''generic'' regexps."
  :type 'list
  :group 'egocentric)


(defvar egocentric-mode nil
  "*Egocentric mode.  Highlights your name and additional keywords in arbitrary buffers.")
(make-variable-buffer-local 'egocentric-mode)

(defface egocentric-face
  '((((class grayscale) (background light)) (:background "DimGray" :underline t))
    (((class grayscale) (background dark)) (:background "LightGray" :underline t))
    (((class color) (background light)) (:background "Cyan" :foreground "Red" :underline t))
    (((class color) (background dark)) (:background "Purple4" :foreground "Yellow" :underline t))
    (t (:bold t :underline t)))
  "Face used to highlight occurences of your name in `egocentric-mode'."
  :group 'font-lock-highlighting-faces)

(defvar egocentric-face 'egocentric-face
  "Face name to use for occurences of your name in `egocentric-mode'.")

(defvar egocentric-overlay-list nil
  "List of overlays used to highlight occurences of your name in `egocentric-mode'.")
(make-local-variable 'egocentric-overlay-list)

(defvar egocentric-regexp-list nil
  "Regexp used to check whether a word has to be highlighted.
Automagically generated once since only schizophrenics are supposed to
change their name at run time.  Use `egocentric-update-regexp-list' to
update it manually")

(defvar egocentric-old-point nil
  "Used to check old point when point moved in a ''egocentriced'' buffer.
This is definitively *gruuuuuik*")


;; Mode line stuff
(or (assoc 'egocentric-mode minor-mode-alist)
    (setq minor-mode-alist
	  (cons '(egocentric-mode " Ego") minor-mode-alist)))


;;;###autoload
(defun egocentric-mode (&optional arg)
  "Toggle egocentric mode.
Optional argument ARG is an optional boolean controling whether egocentric-mode should be turned on/off."
  (interactive "P")
  
  (let ((old-egocentric-mode egocentric-mode))
    ;; Mark the mode as on or off.
    (setq egocentric-mode (not (or (and (null arg) egocentric-mode)
				   (<= (prefix-numeric-value arg) 0))))
    ;; Do the real work.
    (unless (eq egocentric-mode old-egocentric-mode)
      (if egocentric-mode
	  (egocentric-mode-on)
      (egocentric-mode-off)))))


;;;###autoload
(defun egocentric-mode-on ()
  "Turn Egocentric mode on."
  (interactive)
  (make-local-hook 'post-command-hook)
  (add-hook 'post-command-hook (function egocentric-post-command-hook) t t)
  (egocentric-update-regexp-list)
  (egocentric-insinuate egocentric-regexp-list)
  (run-hooks 'egocentric-mode-hook))


;;;###autoload
(defun egocentric-mode-off ()
  "Turn Egocentric mode off."
  (interactive)
  (remove-hook 'post-command-hook (function egocentric-post-command-hook) t)
  (egocentric-delete-all-overlays)
  (setq egocentric-mode nil))


;;;###autoload
(defun egocentric-update-regexp-list ()
  "Update ``egocentric-regexp-list'' from $USER and $NAME variables."
  (interactive)
  (setq egocentric-regexp-list (egocentric-make-regexp-list)))


(defun egocentric-make-regexp-list ()
  "Build a regexp list from USER and NAME environment variables.
It assumes that the NAME environment variable is set to your actual
name, like NAME=\"Benjamin Drieu\".  This is not standard but some
systems use it."
  (append
   (mapcar
    (lambda (word)
      (concat "\\<" (downcase (egocentric-unac-word word)) "\\>"))
    (let ((user (getenv "USER"))
	  (name (getenv "NAME")))
      (append
       (when (and user (not (member user egocentric-additional-keywords)))
	 (list user))
       (when (and name
		  (not (equal name user)))
	 (split-string name))
       egocentric-additional-keywords)))
   egocentric-additional-regexps))


(defun egocentric-unac-word (word)
  "Wipe out accents from a WORD.  Call `egocentric-unac-word-1' as a ''recursor''."
  (egocentric-unac-word-1 word egocentric-accents-translation-alist))


(defun egocentric-unac-word-1 (word list)
  "''Resursor'' for `egocentric-unac-word'.
Argument WORD is passed from it caller.
Argument LIST is an alist of regexps/replacements."
  (cond
   ((null list) word)
   ((string-match (caar list) word)
    (egocentric-unac-word-1 (egocentric-replace-all word (caar list) (cdar list))
			    (cdr list)))
   ((egocentric-unac-word-1 word (cdr list)))))


(defun egocentric-replace-all (word from to)
  "Quick-n-dirty implementation of `replace-regexp'.
`replace-regexp' doesn't really work like it should or like I understand it.
I know ... i know ... this may sounds like using a hammer to squash
grasshoppers.
Argument WORD is the word to replace.
Argument FROM is a letter to remplace by TO."
  (egocentric-replace-all-1 (split-string word "") from to))


(defun egocentric-replace-all-1 (word from to)
  "''Resursor'' for `egocentric-replace-all'.
Argument WORD is passed from `egocentric-replace-all'.
Occurences or argument FROM are replaced by TO."
  (cond
   ((null word) "")
   ((equal (car word) from)
    (concat to
	    (egocentric-replace-all-1 (cdr word) from to)))
   ((concat (car word)
	    (egocentric-replace-all-1 (cdr word) from to)))))


(defun egocentric-post-command-hook ()
  "Function called as `post-command-hook' in ''egocentriced'' buffers."
  (if (not (equal (point) egocentric-old-point))
      (progn
	(if (not (null egocentric-old-point))
	    (egocentric-check-at egocentric-old-point))
	(setq egocentric-old-point (point))))
  (egocentric-check-at (point)))


(defun egocentric-check-at (pos)
  "Check whether word at POS (defaulted to point) is to be highlighted."
  (save-excursion
    (if (not (equal (point) pos))
	(goto-char pos))
    (let ((word (thing-at-point 'symbol)))
      (if word
	  (if (egocentric-word-is-keyword word)
	      (egocentric-highlight-word pos)
	    (if (egocentric-overlay-at pos)
	      (egocentric-unhighlight-at pos))
	    (if (egocentric-overlay-at (1- pos))
		(egocentric-unhighlight-at (1- pos))))))))


(defun egocentric-word-is-keyword (word)
  "Check wether WORD is a keyword to be highlighted."
  (if (stringp word)
      (egocentric-word-is-keyword-1 word egocentric-regexp-list)))


(defun egocentric-word-is-keyword-1 (word list)
  "''Recursor'' for `egocentric-word-is-keyword'.
Argument WORD is passed from caller.
Argument LIST is the list of keywords to compare against."
  (cond
   ((null list) nil)
   ((not (null (string-match (car list) word))))
   ((egocentric-word-is-keyword-1 word (cdr list)))))


(defun egocentric-highlight-word (&optional pos)
  "Actually build and put a cute overlay at POS (defaulted to point)."
  (save-excursion
    (when (not (equal pos (point)))
      (goto-char pos))
    (let ((begin (re-search-backward "\\<" nil t))
	  (end (re-search-forward "\\>" nil t)))
      (if (not (egocentric-overlay-at begin))
	  (let ((ovr (make-overlay begin end nil t nil)))
	    (setq egocentric-overlay-list (cons ovr egocentric-overlay-list))
	    (overlay-put ovr 'face 'egocentric-face))
	(if (not (egocentric-overlay-at end))
	    (move-overlay (egocentric-overlay-at begin) begin end))))))


(defun egocentric-overlay-at (&optional pos)
  "Return true when there is already an egocentric overlay at POS (defaulted to point)."
  (let ((overlays-at-point (overlays-at (or pos (point)))))
    (if (not (null overlays-at-point))
	(egocentric-overlay-at-1 overlays-at-point))))


(defun egocentric-overlay-at-1 (overlay-list)
  "''Recursor'' of `egocentric-overlay-at'.
Argument OVERLAY-LIST is the list of overlays at POS as passed by `egocentric-overlay-at'."
  (cond
   ((null overlay-list) nil)
   ((egocentric-overlay-p (car overlay-list)) (car overlay-list))
   ((egocentric-overlay-at-1 (cdr overlay-list)))))


(defun egocentric-overlay-p (overlay)
  "Return true when OVERLAY is an egocentric overlay."
  (if (memq overlay egocentric-overlay-list) t))

	
(defun egocentric-unhighlight-at (&optional pos)
  "Remove egocentric overlays at POS (defaulted to point)."
  (let ((overlays (overlays-at (or pos (point)))))
    (while (consp overlays)
      (if (egocentric-overlay-p (car overlays))
	  (delete-overlay (car overlays)))
      (setq overlays (cdr overlays)))))


(defun egocentric-insinuate (regexp-list)
  "Highlight egocentric keywords present in that buffer.
Argument REGEXP-LIST is the list of regexps to use."
  (if (null regexp-list)
      nil
    (progn
      (save-excursion
	(beginning-of-buffer)
	(while (re-search-forward (car regexp-list) nil t)
	  (egocentric-highlight-word (match-beginning 0))))
      (egocentric-insinuate (cdr regexp-list)))))


(defun egocentric-delete-all-overlays ()
  "Delete all egocentric overlays."
  (let ((l (overlays-in (point-min) (point-max))))
    (while (consp l)
      (progn
	(if (egocentric-overlay-p (car l))
	    (delete-overlay (car l)))
	(setq l (cdr l))))))

;;; egocentric.el ends here
