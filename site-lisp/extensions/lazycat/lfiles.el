;; -*- auto-recompile: t -*-
;;; lfiles.el --- get filelist from loadpath etc. etc. etc.
;; Time-stamp: <2002-06-21 13:44:21 deego>
;; Copyright (C) 2002 D. Goel
;; Copyright (C) 2002 Free Software Foundation, Inc.
;; Emacs Lisp Archive entry
;; Filename: lfiles.el
;; Package: lfiles
;; Author: D. Goel <deego@glue.umd.edu>
;; Version: 1.5d
;; Author's homepage: http://www.glue.umd.edu/~deego
;; For latest version: 

(defvar lfiles-home-page
  "http://www.glue.umd.edu/~deego/emacspub/lisp-mine/lfiles")



;; This file is NOT (yet) part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.


;; See also:


;; Quick start:
(defvar lfiles-quick-start
  "Help..."
  )

(defun lfiles-quick-start ()
  "Provides electric help regarding `lfiles-quick-start'."
  (interactive)
  (with-electric-help
   '(lambda () (insert lfiles-quick-start) nil) "*doc*"))

;;; Introduction:
;; Stuff that gets posted to gnu.emacs.sources
;; as introduction
(defvar lfiles-introduction
  "
Lfiles returns a list of files in your load-path.. and related stuff.

More generally: It returns a list of all files satisfying a certain
predicate you choose, from lfiles-path, and lfiles-before-path
lfiles-after-path and lfiles-descend-path --> the latter is attached
last of all.  If lfiles-path is set to nil, then your load-path is
automatically used.  lfiles-path can have several formats--- see its
documentation.  The default predicate will filter for a
customizable regexp..  \(Please note that if lfiles-descend-path is
non-nil you should have find-lisp and \(my\) find-utils installed.
Each path in lfiles-descend-path is assumed to mean: Keep descending
this path and find all files..\) When descending, links are not
descended unless lfiles-descend-links-p is true.  The
lfiles-descend-path is not implemented as of writing of this doc.  If
lfiles-remove-duplicates \(not yet implemented\) is t, lfiles checks
if any the new file it is adding is included in the list.  See also
lfiles-after-path-function \(not yet implemented\) , which is nil by
default.


It also provides certain other functions depending on what other
features you have installed

* lfiles-igrep:  will do an igrep on the list it generates. 

"
  )

;;;###autoload
(defun lfiles-introduction ()
  "Provides electric help regarding `lfiles-introduction'."
  (interactive)
  (with-electric-help
   '(lambda () (insert lfiles-introduction) nil) "*doc*"))

;;; Commentary:
(defvar lfiles-commentary
  "As of now, no read-permission checks are done.. 
Thus, lack of read-permissions will probbaly result in error which may
perhapss help identify files which need to have their read-permission changed..."
  )

(defun lfiles-commentary ()
  "Provides electric help regarding `lfiles-commentary'."
  (interactive)
  (with-electric-help
   '(lambda () (insert lfiles-commentary) nil) "*doc*"))

;;; History:

;;; Bugs:

;;; New features:
(defvar lfiles-new-features
  "Help..."
  )

(defun lfiles-new-features ()
  "Provides electric help regarding `lfiles-new-features'."
  (interactive)
  (with-electric-help
   '(lambda () (insert lfiles-new-features) nil) "*doc*"))

;;; TO DO:
(defvar lfiles-todo
  "Help..."
  )

(defun lfiles-todo ()
  "Provides electric help regarding `lfiles-todo'."
  (interactive)
  (with-electric-help
   '(lambda () (insert lfiles-todo) nil) "*doc*"))

(defvar lfiles-version "1.5d")

;;==========================================
;;; Code:

(defcustom lfiles-before-load-hooks nil ".")
(defcustom lfiles-after-load-hooks nil "")
(run-hooks 'lfiles-before-load-hooks)


(defcustom lfiles-predicate-function 'lfiles-predicate "")

(defcustom lfiles-path nil 
  "
Can be nil or a list of paths, or a function-name. 

If it is nil, then load-path is used whenever lfiles needs to use
lfiles-path.  If a list of paths, then that is used.

If a funtion-name, then that function is called to return the list of
paths.  Thus, one function you may find useful is \(lfiles-nil\) :\)

")

(defcustom lfiles-before-path nil "")
(defcustom lfiles-after-path nil "")
(defcustom lfiles-additional-path nil "")


(defun lfiles-nil ()
  nil)

(defvar lfiles-current-files nil
  "Will store the current status.. to help debug... coz lfiles can be
huge...")

;; main function
;;;###autoload
(defun lfiles ()
  (setq lfiles-current-files
        (lfiles-get-files 
         (lfiles-get-full-path)))
  lfiles-current-files)

(defun lfiles-get-full-path ()
  (append
   lfiles-before-path
   (if (null lfiles-path)
       load-path
     (if (listp lfiles-path)
         lfiles-path
       (funcall lfiles-path)))
   lfiles-after-path))


;;;###autoload
(defun lfiles-get-files (path)
  (if (null path) 
      nil
    (apply 'append
           (mapcar 'lfiles-directory-files path))))



;;;###autoload
(defun lfiles-directory-files  (dir)
  (if (and (file-exists-p dir) (file-directory-p dir))
      (let ((files (directory-files dir 'full)))
        (apply 'append
               (mapcar
                '(lambda (arg)
                   (if (file-regular-p arg) (list arg) nil))
                files)))
    nil))



;;TODO

(defun lfiles-predicate (file)
  (string-match lfiles-regexp file))



(defcustom lfiles-regexp "" "")



(provide 'lfiles)
(run-hooks 'lfiles-after-load-hooks)



;;; lfiles.el ends here
