;;; ls-lisp+.el --- Enhancements of standard library `ls-lisp.el'.
;; 
;; Filename: ls-lisp+.el
;; Description: Enhancements of standard library `ls-lisp.el'.
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 2008, Drew Adams, all rights reserved.
;; Created: Fri Feb 29 10:54:37 2008 (Pacific Standard Time)
;; Version: 20.0
;; Last-Updated: Tue Mar  4 14:27:26 2008 (Pacific Standard Time)
;;           By: dradams
;;     Update #: 99
;; URL: http://www.emacswiki.org/cgi-bin/wiki/ls-lisp+.el
;; Keywords: internal, extensions, local, files, dired
;; Compatibility: GNU Emacs 20.x, GNU Emacs 21.x, GNU Emacs 22.x
;; 
;; Features that might be required by this library:
;;
;;   `files+', `ls-lisp', `misc-fns', `strings', `thingatpt',
;;   `thingatpt+'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; 
;;    Enhancements of standard library `ls-lisp.el'.
;;
;;  If you use MS Windows, MS-DOS, or MacOS, then you will likely want
;;  to use library `ls-lisp.el' and this library, to use an Emacs Lisp
;;  only definition of `insert-directory'.
;;
;;  Do not load this library directly.  Instead, load library
;;  `files+.el', which will load this.  Both of these libraries
;;  redefine `insert-directory' so that the second header line
;;  includes the number of files and directories in the directory.
;;  Files `.' and `..' are excluded from the count, but all other
;;  directories listed are included.
;;
;;  The second header line thus becomes this, in Emacs 22:
;;
;;    files 276 space used  27359 available 56238272
;;
;;  or this, in Emacs 20 and 21:
;;
;;    files 276 total 27359
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;; 2008/03/04 dadams
;;     insert-directory: Use two separate tooltips.
;; 2008/03/02 dadams
;;     insert-directory: Add total files in dir: shown/total.
;;       Added tooltip, mouse-face.  Bind RET, mouse-2 locally to
;;       dired(-mouse)-describe-listed-directory.
;; 2008/02/29 dadams
;;     Created.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:

(require 'ls-lisp)
(require 'files+) ;; count-dired-files, dired-describe-listed-directory,
                  ;; dired-mouse-describe-listed-directory

;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun insert-directory (file switches &optional wildcard full-directory-p)
  "Insert directory listing for FILE, formatted according to SWITCHES.
Leaves point after the inserted text.
SWITCHES may be a string of options, or a list of strings.
Optional third arg WILDCARD means treat FILE as shell wildcard.
Optional fourth arg FULL-DIRECTORY-P means file is a directory and
switches do not contain `d', so that a full listing is expected.

This version of the function comes from `ls-lisp.el'.
If the value of `ls-lisp-use-insert-directory-program' is non-nil then
it works exactly like the version from `files.el' and runs a directory
listing program whose name is in the variable
`insert-directory-program'; if also WILDCARD is non-nil then it runs
the shell specified by `shell-file-name'.  If the value of
`ls-lisp-use-insert-directory-program' is nil then it runs a Lisp
emulation.

The Lisp emulation does not run any external programs or shells.  It
supports ordinary shell wildcards if `ls-lisp-support-shell-wildcards'
is non-nil; otherwise, it interprets wildcards as regular expressions
to match file names.  It does not support all `ls' switches -- those
that work are: A a c i r S s t u U X g G B C R n and F partly."
  (if ls-lisp-use-insert-directory-program
      (funcall original-insert-directory
	       file switches wildcard full-directory-p)
    ;; We need the directory in order to find the right handler.
    (let ((handler (find-file-name-handler (expand-file-name file)
					   'insert-directory))
	  (orig-file file)
	  wildcard-regexp)
      (if handler
	  (funcall handler 'insert-directory file switches
		   wildcard full-directory-p)
	;; Remove --dired switch
	(if (string-match "--dired " switches)
	    (setq switches (replace-match "" nil nil switches)))
	;; Convert SWITCHES to a list of characters.
	(setq switches (delete ?- (append switches nil)))
	;; Sometimes we get ".../foo*/" as FILE.  While the shell and
	;; `ls' don't mind, we certainly do, because it makes us think
	;; there is no wildcard, only a directory name.
	(if (and ls-lisp-support-shell-wildcards
		 (string-match "[[?*]" file)
		 ;; Prefer an existing file to wildcards, like
		 ;; dired-noselect does.
		 (not (file-exists-p file)))
	    (progn
	      (or (not (eq (aref file (1- (length file))) ?/))
		  (setq file (substring file 0 (1- (length file)))))
	      (setq wildcard t)))
	(if wildcard
	    (setq wildcard-regexp
		  (if ls-lisp-support-shell-wildcards
		      (wildcard-to-regexp (file-name-nondirectory file))
		    (file-name-nondirectory file))
		  file (file-name-directory file))
	  (if (memq ?B switches) (setq wildcard-regexp "[^~]\\'")))
	(condition-case err
	    (ls-lisp-insert-directory
	     file switches (ls-lisp-time-index switches)
	     wildcard-regexp full-directory-p)
	  (invalid-regexp
	   ;; Maybe they wanted a literal file that just happens to
	   ;; use characters special to shell wildcards.
	   (if (equal (cadr err) "Unmatched [ or [^")
	       (progn
		 (setq wildcard-regexp (if (memq ?B switches) "[^~]\\'")
		       file (file-relative-name orig-file))
		 (ls-lisp-insert-directory
		  file switches (ls-lisp-time-index switches)
		  nil full-directory-p))
	     (signal (car err) (cdr err)))))
	(save-excursion
	  (goto-char (point-min))
          (while (re-search-forward "^total" nil t)
            (beginning-of-line)
            (insert "files " (number-to-string (save-match-data
                                                 (count-dired-files)))
                    "/" (number-to-string
                         (- (length (directory-files default-directory
                                                     nil nil t)) 2))
                    " ")
            (goto-char (point-min))
            (re-search-forward "^files [0-9]+/[0-9]+ \\(total\\)" nil t)
            (replace-match "space used" nil nil nil 1)
            (let ((available (and (fboundp 'get-free-disk-space)
                                  (get-free-disk-space ".")))
                  (map (make-sparse-keymap)))
              (define-key map [mouse-2]
                'dired-mouse-describe-listed-directory)
              (define-key map "\r" 'dired-describe-listed-directory)
              (when available (end-of-line) (insert " available "
                                                    available))
              (add-text-properties
               (save-excursion (beginning-of-line) (+ 2 (point)))
               (1- (match-beginning 1))
               `(mouse-face highlight keymap ,map
                 help-echo "Files shown / total files in directory \
\[RET, mouse-2: more info]"))
              (add-text-properties
               (match-beginning 1)
               (save-excursion (end-of-line) (point))
               `(mouse-face highlight keymap ,map
                 help-echo "Kbytes used in directory, Kbytes \
available on disk [RET, mouse-2: more info]")))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'ls-lisp+)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ls-lisp+.el ends here
