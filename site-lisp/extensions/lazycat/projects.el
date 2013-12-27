;;; projects.el -- Project-based buffer name management

;; Copyright 1998 Naggum Software
;; Copyright 2003 Peter S Galbraith <psg@debian.org>
;;  (Erik, please contact me for copyright assignment back to you.  -psg)

;; Author: Erik Naggum <erik@naggum.no>
;; Keywords: internal

;; This file is not part of GNU Emacs, but distributed under the same
;; conditions as GNU Emacs, and is useless without GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Managing a large number of buffers that visit files in many directories
;; (such as both local and remote copies of sources) can be confusing when
;; there are files with similar or even identical names and the buffers end
;; up being named foobar.cl<19> or like unintuitiveness.  This package
;; introduces the concept of PROJECT ROOTS that allow the programmer to
;; define what looks suspiciously like logical pathname hosts from Common
;; Lisp and get abbreviated yet meaningful buffer names in the modeline.

;; Commands include PROJECT-ADD, which takes a project name and a directory
;; (which conveniently defaults to the current directory), PROJECT-REMOVE
;; (which completes on existing projects), and PROJECT-LIST, which lists the
;; current projects in a rudimentary table.  PROJECT-UPDATE-BUFFER-NAMES is
;; called automatically when either PROJECT-ADD or PROJECT-REMOVE changes
;; the project list, but may also be called by the user as a command.

;; Variables include PROJECT-ROOT-ALIST, which contains the list of current
;; projects and their root directories, and two variables that control the
;; naming of buffers: PROJECT-BUFFER-NAME-DIRECTORY-LIMIT, the uppper limit
;; on the number of characters in the last few directory elements in the
;; pathname that makes up the buffer name and
;; PROJECT-BUFFER-NAME-DIRECTORY-PREFIX, the string prepended to buffer
;; names that would be too long.

;; Internal functions include PROJECT-BUFFER-NAME, which computes the
;; buffer name from the filename argument, PROJECT-ROOT-ALIST, which
;; computes a sorted list of projects on their directories and maintains a
;; cache because this operation is expensive, and a redefinition of the
;; function CREATE-FILE-BUFFER, which is called to create new file-visiting
;; buffers.  Note that the latter may still produce ...<n>, if truly
;; identical buffer names are requested.  This may happen if you call dired
;; on a filename and then visit the same file.  Use C-x C-v M-p instead.

;; Loading this file is sufficient to install the package.
;; Reloading has no effect.

;;; History:

;; 2003-10-27 Peter S Galbraith <psg@debian.org>
;;
;;  I tried to contact the author but his host is down.  I like the concept
;;  of prefixing certain buffer names with a project name, but not renaming
;;  all unrelated buffers with the full directory path.  This breaks MH-E
;;  mail folder names for example.  So I'm introducing the variable
;;  `project-rename-all-buffers' with a default of nil.  You may customize
;;  this to obtain the old behaviour.
;;
;;  In addition, I am renaming commands:
;;
;;    `add-project'    to `project-add'
;;    `remove-project' to `project-remove'.
;;    `list-projects'  to `project-list'.
;;    `update-buffer-names' to `project-update-buffer-names'
;;
;;  variables (also made into defcustoms):
;;
;;    `buffer-name-directory-limit' to `project-buffer-name-directory-limit'
;;    `buffer-name-directory-prefix' to `project-buffer-name-directory-prefix'

;;; Code:

(require 'cl)

(provide 'projects)

(defgroup projects nil
  "Project-based buffer name management."
  :group 'convenience)

(defcustom project-rename-all-buffers nil
  "*Whether to rename buffer not belonging to a project."
  :type 'boolean
  :group 'projects)

(defcustom project-buffer-name-directory-limit 20
  "*Directories in buffer names are attempted kept shorter than this."
  :type 'integer
  :group 'projects)

(defcustom project-buffer-name-directory-prefix "<"
  "*String to prepend to an abbreviated buffer name."
  :type 'string
  :group 'projects)

;; External symbols

(defvar project-root-alist nil
  "Alist of projects and their root directories.
The key should be a (short) project name.
The value should be the project's root directory.
Multiple projects in the same hierarchy is handled correctly.")

;;;###autoload
(defun project-add (name directory)
  "Add the project named NAME with root directory DIRECTORY."
  (interactive "sName of project: \nDDirectory of project %s: ")
  (push (cons name directory) project-root-alist)
  (message "Project `%s' maps to `%s'" name directory)
  (project-update-buffer-names))

(defun project-remove (name)
  "Remove the project named NAME."
  (interactive
   (list (completing-read "Name of project: " project-root-alist nil t)))
  (setf project-root-alist
    (remove* name project-root-alist :key #'car :test #'equal))
  (project-update-buffer-names))

(defun project-list (&optional sort-by-root)
  "List all projects sorted by project name.
If optional argument SORT-BY-ROOT is true, sort by project root, instead."
  (interactive "P")
  (let* ((project-list
	  (sort* (copy-list (project-root-alist))
		 #'string< :key (if sort-by-root #'cdr #'car)))
	 (longest
	  (loop for (name) in project-list maximize (length name))))
    (if project-list
      (with-output-to-temp-buffer "*Help*"
	(princ "Current projects and their root directories:\n\n")
	(loop for (name . dir) in project-list do
	      (princ name)
	      (princ ":")
	      (princ (make-string (- (max 6 longest) -2 (length name)) ?\ ))
	      (princ (file-truename dir))
	      (terpri)))
      (message "There are no projects."))))

(defun project-update-buffer-names (&rest buffers)
  "Update the name of the indicated BUFFERS.
Interactively, or if no buffers are given, the names of all file-visiting
buffers are updated according to the new value of PROJECT-ROOT-ALIST."
  (interactive)
  (dolist (buffer (or buffers (buffer-list)))
    (with-current-buffer buffer
      (when buffer-file-name
	(setf (buffer-name) (project-buffer-name buffer-file-name))))))

;; Internal symbols

(defun project-root-alist ()
  "Return possibly updated cache from PROJECT-ROOT-ALIST."
  (symbol-macrolet			;fake closures badly
      ((project-alist    (get 'project-root-alist 'project-alist))
       (project-internal (get 'project-root-alist 'project-internal)))
    (if (equal project-alist project-root-alist)
      project-internal
      (setq project-internal
	(sort* (loop for (name . dir)
		     in (setq project-alist project-root-alist)
		     collect (cons name (file-name-as-directory
					 (file-truename dir))))
	      (lambda (f1 f2)
		(or (> (length f1) (length f2))
		    (string< f1 f1)))
	      :key #'cdr)))))

(defun project-buffer-name (filename)
  "Return the name of a buffer based on FILENAME and current projects.
If the file is under a project hierarchy, as determined by the variable
PROJECT-ROOT-ALIST, prefix its project-relative name with the name of the
project.  Otherwise, name the buffer like the filename, but limit the
directory to PROJECT-BUFFER-NAME-DIRECTORY-LIMIT characters by chopping
off from the front and prepending PROJECT-BUFFER-NAME-DIRECTORY-PREFIX."
  (block name
    (let* ((truename (file-truename (if (file-directory-p filename)
                                        (file-name-as-directory filename)
				      filename))))
      (loop for (name . dir) in (project-root-alist)
	    when (and (>= (length truename) (length dir))
		      (string= dir (substring truename 0 (length dir))))
	    do (return-from name
		 (concat name ":" (substring truename (length dir)))))
      (cond
       ((not project-rename-all-buffers)
        (let ((lastname (file-name-nondirectory filename)))
          (if (string= lastname "")
              (setq lastname filename))
          lastname))
       (t
        ;; Old behaviour
        ;; may not need to abbreviate if directory is short enough
        (when (<= (position ?/ (abbreviate-file-name truename) :from-end t)
                  project-buffer-name-directory-limit)
          (return-from name (abbreviate-file-name truename)))
        ;; keep directories shorter than PROJECT-BUFFER-NAME-DIRECTORY-LIMIT.
        ;; prepend PROJECT-BUFFER-NAME-DIRECTORY-PREFIX to abbreviated names.
        (let* ((final (position ?/ truename :from-end t))
               (start (- final project-buffer-name-directory-limit))
               (first (or (position ?/ truename :start start :end final)
                          (position ?/ truename :end start :from-end t)
                          start)))
          (concat project-buffer-name-directory-prefix
                  (subseq truename first))))))))

;; This overrides a function in EMACS:lisp/files.el

(defun create-file-buffer (filename)
  "Create a suitably named buffer for visiting FILENAME, and return it.
See PROJECT-BUFFER-NAME for more information."
  (generate-new-buffer (project-buffer-name filename)))

;;; projects.el ends here
