;;; anything-make.el --- Add targets of nearest makefile to anything sources

;; Copyright (C) 2008 Philip Jackson

;; Author: Philip Jackson <phil@shellarchive.co.uk>

;; This file is not currently part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program ; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Evaling (require 'anything-make) will make the targets for the
;; nearest makefile available as anything sources. It searches up the
;; file system tree for the closest makefile (the name of which is
;; defined by `anything-make-makefile-name').

(defvar anything-make-makefile-name "Makefile"
  "Name of the Makefile file to look for.")

(defun anything-make-targets (makefile-path)
  "Grab the targets from the makefile at MAKEFILE-PATH"
  (let ((hits '())
        (buf (find-file-noselect makefile-path)))
    (with-current-buffer buf
      (goto-char (point-min))
      (while (re-search-forward "^\\([[:alnum:]].+?\\):\\(:?$\\| \\)" nil t)
        (mapc '(lambda (s)
                 (add-to-list 'hits s t))
              (split-string (match-string-no-properties 1) " " t))))
    hits))

(defun anything-make-find-makefile (filename &optional startdir)
  "Search up the file tree for the next instance of FILENAME. If
STARTDIR is non-nil then start from there, otherwise start from
cwd."
  (let ((startdir (expand-file-name (or startdir "."))))
    (catch 'filename
      (if (file-exists-p (expand-file-name filename startdir))
          (throw 'filename startdir)
        (if (string= startdir "/")
            (throw 'filename nil)
          (anything-make-find-makefile
           filename (expand-file-name ".." startdir)))))))

(defvar anything-c-source-make-targets
  '((name . "Make targets")
    (init . (lambda ()
              (let ((path
                     (anything-make-find-makefile anything-make-makefile-name)))
                (setq anything-default-directory path))))
    (candidates . (lambda ()
                    (anything-make-targets anything-make-makefile-name)))
    (action .
            (("Make target" . (lambda (c)
                                (compile (concat "cd " anything-default-directory
                                                 " && make " c))))))
    (volatile)))

(add-to-list 'anything-sources anything-c-source-make-targets)

(provide 'anything-make)
