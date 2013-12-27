;;; find-dired-lisp.el ---  "find-dired" by lisp
;; -*- Mode: Emacs-Lisp -*-

;; $Id: find-dired-lisp.el,v 2.1 2008/04/03 13:02:19 akihisa Exp $

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Install:
;; find-dired-lisp.el require findr.el.
;; Download from http://www.emacswiki.org/cgi-bin/wiki/findr.el, and
;; put findr.el into load-path'ed directory
;;
;; Put this file into load-path'ed directory, and byte compile it if
;; desired.  And put the following expression into your ~/.emacs.
;;
;;     (require 'find-dired-lisp)

;;; Functions
;; M-x find-dired-lisp
;; M-x find-grep-dired-lisp

(require 'dired)
(require 'findr)

(defun mmemo-setup-dired-buffer (dir)
  (widen)
  (kill-all-local-variables)
  (setq buffer-read-only nil)
  (erase-buffer)
  (dired-mode dir (cdr find-ls-option))
  ;; This really should rerun the find command, but I don't
  ;; have time for that.
  (use-local-map (append (make-sparse-keymap)
                         (current-local-map)))
  (define-key (current-local-map) "g" 'undefined)
  (setq buffer-read-only nil)
  (mmemo-find-dired-set-variable dir)
  (insert "" (file-name-as-directory
              (expand-file-name dir)) ":\n")
  )

(defun mmemo-find-dired-set-variable (dir)
  (setq default-directory dir)
  (setq dired-directory dir)
  ;; Set subdir-alist so that Tree Dired will work:
  (if (fboundp 'dired-simple-subdir-alist)
      (dired-simple-subdir-alist)
    (set (make-local-variable 'dired-subdir-alist)
         (list (cons default-directory (point-min-marker)))))
  )

(defun mmemo-insert-dir (file-list)
  (let ((switches (concat "--dired " dired-actual-switches)))
    (dolist (f file-list)
      (let ((beg (point)))
	(insert-directory f switches nil nil)
	;; Re-align fields, if necessary.
	(dired-align-file beg (point))))))

(fset 'find-name-dired-lisp 'find-dired-lisp)
(defun find-dired-lisp (direddir args)
  (interactive
   (list
    (read-file-name "Run find in directory: " nil "" t)
    (read-string "Run find (with regexp): ")))
  (or (file-directory-p direddir)
      (error "find-dired needs a directory: %s" direddir))
  (let* ((dirs
          (mapcar '(lambda (name)
                     (file-relative-name name direddir)
                     )
                  (findr args direddir)))
         (dir (abbreviate-file-name
               (file-name-as-directory
                (expand-file-name direddir))))
         ;;(dirlist (cons dir dirs))
         (dirlist dirs)
         )
  
    (let ((dired-buffers dired-buffers))
      (switch-to-buffer (get-buffer-create "*Find*"))
      (mmemo-setup-dired-buffer dir)
      ;;(insert "" args "\n")
      (insert
       (concat
        "find with " args "\n"))
      (mmemo-insert-dir dirlist)
      ;;(dired-insert-directory dir dired-actual-switches dirlist nil nil)
      ;;(dired-readin-insert dirlist)
      (indent-rigidly (point-min) (point-max) 2)
      (mmemo-find-dired-set-variable dir)
      ;;(setq mode-line-process '(":%s"))
      (goto-char (point-min))
      )))

(defun find-grep-dired-lisp (direddir regxp)
  (interactive
   (list
    (read-file-name "Run find in directory: " nil "" t)
    (read-string "Run Grep (with regexp): ")))
  (or (file-directory-p direddir)
      (error "find-dired needs a directory: %s" direddir))
  (let* ((greplist
          (mapcar '(lambda (name)
                     (file-relative-name name direddir)
                     )
                  (findr "." direddir)))
         (dirlist nil) ;;(cons direddir dirs))
         (dirs nil)
         (dir (abbreviate-file-name
               (file-name-as-directory
                (expand-file-name direddir))))
         (name nil)
         )

    (while greplist
      (setq name (expand-file-name
                  (car greplist) dir))
      (when (and
             name
             (not (file-directory-p name))
             (file-readable-p name))
        (message (format "Grep in %s ..."
                         (file-name-directory name)))
        (with-temp-buffer
          (condition-case err
              (insert-file-contents name)
            (error
             ()))
          (if (re-search-forward regxp nil t)
              (setq dirs (cons
                          (file-relative-name name dir)
                          dirs)))))
      (setq greplist (cdr greplist)))
    (message "Grep done!")
    ;;(setq dirlist (cons dir dirs))
    (setq dirlist dirs)
    (let ((dired-buffers dired-buffers))
      (switch-to-buffer
       (get-buffer-create "*Find-grep*"))
      (mmemo-setup-dired-buffer dir)
      ;;(insert " " regxp "\n")
      (insert
       (concat
        "find-grep: "
        regxp "\n"))
      (mmemo-insert-dir dirlist)
      ;;(dired-insert-directory dir dired-actual-switches dirlist nil nil)
      ;;(dired-readin-insert dirlist)
      (indent-rigidly (point-min) (point-max) 2)
      (mmemo-find-dired-set-variable dir)
      ;;(setq mode-line-process '(":%s"))
      (goto-char (point-min))
      )))

(provide 'find-dired-lisp)
