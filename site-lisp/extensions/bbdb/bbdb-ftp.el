;;; -*- Mode:Emacs-Lisp -*-

;;; This file is an addition to the Insidious Big Brother Database
;;; (aka BBDB), copyright (c) 1991, 1992 Jamie Zawinski
;;; <jwz@netscape.com>.
;;;
;;; The Insidious Big Brother Database is free software; you can redistribute
;;; it and/or modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation; either version 1, or (at your
;;; option) any later version.
;;;
;;; BBDB is distributed in the hope that it will be useful, but WITHOUT ANY
;;; WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;;; details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.


;;; This file was written by Ivan Vazquez <ivan@haldane.bu.edu>

;;; $Id: bbdb-ftp.el,v 1.61 2005/07/23 15:02:48 waider Exp $

;;; This file adds the ability to define ftp-sites in a BBDB, much the same
;;; way one adds a regular person's name to the BBDB.  It also defines the
;;; bbdb-ftp command which allows you to ftp a site that is in a bbdb-record.
;;; You must have either EFS or ange-ftp in order to use this code.  Ange-ftp
;;; is available at archive.cis.ohio-state.edu in the
;;; /pub/gnu/emacs/elisp-archive/packages directory.  EFS ships with XEmacs.

;;; Note that Ftp Site BBDB entries differ from regular entries by the
;;; fact that the Name Field must have the ftp site preceeded by the
;;; bbdb-ftp-site-name-designator-prefix.  This defaults to "Ftp Site:"
;;; BBDB Ftp Site entries also have two new fields added, the
;;; ftp-dir slot, and the ftp-user slot.  These are added to the notes
;;; alist part of the bbdb-records, the original bbdb-record structure
;;; remains untouched.

;;; The following user-level commands are defined for use:
;;;

;;; bbdb-ftp - Use ange-ftp to open an ftp-connection to a BBDB
;;;            record's name.  If this command is executed from the
;;;            *BBDB* buffer, ftp the site of the record at point;
;;;            otherwise, it prompts for an ftp-site.

;;; bbdb-create-ftp-site -
;;;            Add a new ftp-site entry to the bbdb database; prompts
;;;            for all relevant info using the echo area, inserts the
;;;            new record in the db, sorted alphabetically.

;;; The package can be installed by compiling and adding the following
;;; two lines to your .emacs.

;;; (autoload 'bbdb-ftp                 "bbdb-ftp"  "Ftp BBDB Package" t)
;;; (autoload 'bbdb-create-ftp-site     "bbdb-ftp"  "Ftp BBDB Package" t)

(require 'bbdb)

;; There must be a better way
(if (featurep 'efs-cu)
    (require 'efs)
    (require 'ange-ftp))

(defcustom bbdb-default-ftp-user "anonymous"
  "*The default login to use when ftp-ing."
  :group 'bbdb-utilities-ftp
  :type 'string)

(defcustom bbdb-default-ftp-dir "/"
  "*The default directory to open when ftp-ing."
  :group 'bbdb-utilities-ftp
  :type 'string)

(defcustom bbdb-ftp-site-name-designator-prefix "Ftp Site: "
  "*The prefix that all ftp sites in the bbdb will have in their name field."
  :group 'bbdb-utilities-ftp
  :type 'string)

(defmacro defun-bbdb-raw-notes-accessor (slot)
  "Expands into an accessor function for slots in the notes alist."
  (let ((fn-name (intern (concat "bbdb-record-" (symbol-name slot)))))
    (list 'defun fn-name (list 'record)
      (list 'cdr
        (list 'assoc (list 'quote slot)
              (list 'bbdb-record-raw-notes 'record))))))

(defun-bbdb-raw-notes-accessor ftp-dir)
(defun-bbdb-raw-notes-accessor ftp-user)

(defun bbdb-record-ftp-site (record)
  "Accessor Function. Returns the ftp-site field of the BBDB record or nil."
  (let* ((name (bbdb-record-name record))
     (ftp-pfx-regexp (concat bbdb-ftp-site-name-designator-prefix " *"))
     (ftp-site
      (and (string-match ftp-pfx-regexp name)
           (substring name (match-end 0)))))
    ftp-site))

(defun remove-leading-whitespace (string)
  "Remove any spaces or tabs from only the start of the string."
  (let ((space-char-code (string-to-char " "))
    (tab-char-code   ?\t)
    (index 0))
    (if string
    (progn
      (while (or (char-equal (elt string index) space-char-code)
             (char-equal (elt string index) tab-char-code))
        (setq index (+ index 1)))
      (substring string index))
      nil)))

;;;###autoload
(defun bbdb-ftp (bbdb-record &optional which)
  "Use ange-ftp to open an ftp-connection to a BBDB record's name.
If this command is executed from the *BBDB* buffer, ftp the site of
the record at point; otherwise, it prompts for an ftp-site."
  (interactive (list (bbdb-get-record "Visit (FTP): ")
                     (or current-prefix-arg 0)))
  (if (bbdb-record-ftp-site bbdb-record)
      (bbdb-ftp-internal bbdb-record)
      (find-file-other-window
       (read-string "fetch: " (bbdb-get-field bbdb-record 'ftp which)))))

(defun bbdb-ftp-internal (bbdb-record)
  (let* ((site (bbdb-record-ftp-site bbdb-record))
         (dir  (or (bbdb-record-ftp-dir bbdb-record) bbdb-default-ftp-dir))
         (user (or (bbdb-record-ftp-user bbdb-record) bbdb-default-ftp-user))
         (file-string (concat "/" user "@" site ":" dir )))
    (if bbdb-inside-electric-display
        (bbdb-electric-throw-to-execute (list 'bbdb-ftp-internal bbdb-record)))
    (if site
        (find-file-other-window file-string)
      (error "Not an ftp site.  Check bbdb-ftp-site-name-designator-prefix"))))

(defun bbdb-read-new-ftp-site-record ()
  "Prompt for and return a completely new BBDB record that is
specifically an ftp site entry.  Doesn't insert it in to the database
or update the hashtables, but does insure that there will not be name
collisions."
  (bbdb-records) ; make sure database is loaded
  (if bbdb-readonly-p (error "The Insidious Big Brother Database is read-only."))
  (let (site dir user)
    (bbdb-error-retry
     (progn
       (setq site (bbdb-read-string "Ftp Site: "))
       ;; try and parse it out, in case the user typed in things like
       ;; "ftp://user@site/directory/ or /user@site/directory
       (if (string-match
            "^\\([Ff][Tt][Pp]://\\|/\\)?\\([^@/]@\\)?\\([^/]+\\)\\(/[^/].*\\)?"
            site)
           (setq user (if (match-beginning 2)
                          (substring site (match-beginning 2)
                                     (match-end 2)))
                 dir (if (match-beginning 4)
                         (substring site (match-beginning 4)
                                    (match-end 4)))
                 site (substring site (match-beginning 3)
                                 (match-end 3)))
         (if (string-match "/" site)
             (error "%s doesn't look like a valid site name." site)))
       (setq site (concat bbdb-ftp-site-name-designator-prefix site))
       (if (and bbdb-no-duplicates-p
        (bbdb-gethash (downcase site)))
        (error "%s is already in the database" site))))
    (let* ((dir (or dir (bbdb-read-string "Ftp Directory: "
                                           bbdb-default-ftp-dir)))
           (user (or user (bbdb-read-string "Ftp Username: "
                                            bbdb-default-ftp-user)))
           (company (bbdb-read-string "Company: "))
           (notes (bbdb-read-string "Additional Comments: "))
           (names  (bbdb-divide-name site))
           (firstname (car names))
           (lastname (nth 1 names)))
      (if (string= user bbdb-default-ftp-user) (setq user nil))
      (if (string= company "") (setq company nil))
      (if (or (string= dir bbdb-default-ftp-dir) (string= dir ""))
          (setq dir nil))
      (if (string= notes "")   (setq notes nil))

      (let ((record
         (vector firstname lastname nil company nil nil nil
             (append
              (if notes (list (cons 'notes notes)) nil)
              (if dir   (list (cons 'ftp-dir dir)) nil)
              (if user  (list (cons 'ftp-user user)) nil))
             (make-vector bbdb-cache-length nil))))
    record))))

;;;###autoload
(defun bbdb-create-ftp-site (record)
  "Add a new ftp-site entry to the bbdb database.
Prompts for all relevant info using the echo area,
inserts the new record in the db, sorted alphabetically."
  (interactive (list (bbdb-read-new-ftp-site-record)))
  (bbdb-invoke-hook 'bbdb-create-hook record)
  (bbdb-change-record record t)
  (bbdb-display-records (list record)))

(provide 'bbdb-ftp)
