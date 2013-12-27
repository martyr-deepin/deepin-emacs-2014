;;; project-root.el --- Define a project root and take actions based upon it.

;; Copyright (C) 2008 Philip Jackson

;; Author: Philip Jackson <phil@shellarchive.co.uk>
;; Version: 0.6

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

;; project-root.el allows the user to create rules that will identify
;; the root path of a project and then run an action based on the
;; details of the project.
;;
;; Example usage might be might be that you want a certain indentation
;; level/type for a particular project.
;;
;; once project-root-fetch has been run `project-details' will either
;; be nil if nothing was found or the project name and path in a cons
;; pair.

;; An example configuration:

;; (setq project-roots
;;       '(("Generic Perl Project"
;;          :root-contains-files ("t" "lib")
;;          :on-hit (lambda (p) (message (car p))))))
;;
;; I bind the following:
;;
;; (global-set-key (kbd "C-c p f") 'project-root-find-file)
;; (global-set-key (kbd "C-c p g") 'project-root-grep)
;; (global-set-key (kbd "C-c p a") 'project-root-ack)
;; (global-set-key (kbd "C-c p d") 'project-root-goto-root)
;;
;; (global-set-key (kbd "C-c p M-x")
;;                 'project-root-execute-extended-command)
;;
;; (global-set-key
;;  (kbd "C-c p v")
;;  (lambda ()
;;    (interactive)
;;    (with-project-root
;;        (let ((root (cdr project-details)))
;;          (cond
;;            ((file-exists-p ".svn")
;;             (svn-status root))
;;            ((file-exists-p ".git")
;;             (git-status root))
;;            (t
;;             (vc-directory root nil)))))))
;;
;; This defines one project called "Generic Perl Projects" by running
;; the tests path-matches and root-contains-files. Once these tests
;; have been satisfied and a project found then (the optional) :on-hit
;; will be run.

;;; The tests:

;; :path-matches maps to `project-root-path-matches' and
;; :root-contains-files maps to `project-root-upward-find-files'. You
;; can use any amount of tests.

;;; Bookmarks:

;; If you fancy it you can add a :bookmarks property (with a list of
;; strings) and when you run `project-root-browse-seen-projects' you
;; will see the bookmarks listed under the project name, linking
;; relatively to the project root. Also, the bookmarks will present
;; themselves as anything candidates if you configure as instructed
;; below.

;;; installation:

;; Put this file into your `load-path' and evaulate (require
;; 'project-root).

;;; Using yourself:

;; If you wrap a call in `with-project-root' then everything in its
;; body will execute under project root:
;;
;; (with-project-root
;;  (shell-command-to-string "pwd"))

;;; anything.el intergration

;; If you want to add the bookmarks for the current project to the
;; anything source list then use:
;;
;; (add-to-list 'anything-sources
;;              project-root-anything-config-bookmarks)
;;
;; If you want to add the bookmarks for each of the files in the
;; current project to the anything source list then use:
;;
;; (add-to-list 'anything-sources
;;              project-root-anything-config-files)

(require 'find-cmd)

(defvar project-root-extra-find-args
  (find-to-string '(prune (name ".svn" ".git")))
  "Extra find args that will be AND'd to the defaults (which are
in `project-root-file-find-process')")

(defvar project-root-seen-projects nil
  "All of the projects that we have met so far in this session.")

(defvar project-root-file-cache nil
  "Cache for `completing-read'")

(make-variable-buffer-local
 (defvar project-details nil
   "The name and path of the current project root."))

(defvar project-root-test-dispatch
  '((:root-contains-files . project-root-upward-find-files)
    (:path-matches . project-root-path-matches))
  "Map a property name to root test function.")

(defvar project-roots nil
  "An alist describing the projects and how to find them.")

(defvar project-root-max-search-depth 20
  "Don't go any further than this many levels when searching down
a filesystem tree")

(defun project-root-path-matches (re)
  "Apply RE to the current buffer name returning the first
match."
  (let ((filename (cond
                   ((string= major-mode "dired-mode")
                    (dired-get-filename nil t))
                   (buffer-file-name
                    buffer-file-name))))
    (when (and filename (not (null (string-match re filename))))
      (match-string 1 filename))))

(defun project-root-get-root (project)
  "Fetch the root path of the project according to the tests
described in PROJECT."
  (let ((root (plist-get project :root))
        (new-root))
    (catch 'not-a-project
      (mapcar
       (lambda (test)
         (when (plist-get project (car test))
           ;; grab a potentially different root
           (setq new-root
                 (funcall (cdr test) (plist-get project (car test))))
           (cond
            ((null new-root)
             (throw 'not-a-project nil))
            ;; check root is so far consistent
            ((and (not (null root))
                  (not (string= root new-root)))
             (throw 'not-a-project nil))
            (t
             (setq root new-root)))))
       project-root-test-dispatch)
      (when root
        (file-name-as-directory root)))))

(defun project-root-bookmarks (&optional project)
  "Grab the bookmarks (if any) for PROJECT. If PROJECT is ommited
then attempt to get the bookmarks for the current project."
  (let ((project (or project project-details)))
    (plist-get (cdr (assoc (car project) project-roots)) :bookmarks)))

(defun project-root-gen-org-url (project)
  ;; The first link to the project root itself
  (concat "** [[file:" (cdr project)
          "][" (car project)
          "]] (" (cdr project)
          ")\n"
          ;; And now the bookmarks, should there be any
          (mapconcat
           (lambda (b)
             (let ((mark (concat (cdr project) b)))
               (concat "*** [[file:" mark "][" b "]] (" mark ")")))
           (project-root-bookmarks project)
           "\n")
          "\n"))

(defun project-root-browse-seen-projects ()
  "Browse the projects that have been seen so far this session."
  (interactive)
  (let ((current-project project-details)
        (point-to nil))
    (switch-to-buffer (get-buffer-create "*Seen Project List*"))
    (erase-buffer)
    (insert "* Seen projects\n")
    (mapc (lambda (p)
            (when (file-exists-p (cdr p))
              (when (equal p current-project)
                (setq point-to (point)))
              (insert (project-root-gen-org-url p))))
          project-root-seen-projects)
    (org-mode)
    ;; show everything at second level
    (goto-char (point-min))
    (show-children)
    ;; expand bookmarks for current project only
    (when point-to
      (goto-char (+ point-to 3))
      (show-children))
    (setq buffer-read-only t)))

;; TODO: refactor me
(defun project-root-fetch (&optional dont-run-on-hit)
  "Attempt to fetch the root project for the current file. Tests
will be used as defined in `project-roots'."
  (interactive)
  (let ((project
         (catch 'root-found
           (unless (mapc
                    (lambda (project)
                      (let ((name (car project))
                            (run (plist-get (cdr project) :on-hit))
                            (root (project-root-get-root (cdr project))))
                        (when root
                          (when (and root (not dont-run-on-hit) run)
                            (funcall run (cons name root)))
                          (throw 'root-found (cons name root)))))
                    project-roots)
             nil))))
    ;; set the actual var used by apps and add to the global project
    ;; list
    (when (setq project-details project)
      (add-to-list 'project-root-seen-projects project))))

(defun project-root-every (pred seq)
  "Return non-nil if pred of each element, of seq is non-nil."
  (catch 'got-nil
    (mapc (lambda (x)
            (unless (funcall pred x)
              (throw 'got-nil nil)))
          seq)))

(defun project-root-upward-find-files (filenames &optional startdir)
  "Return the first directory upwards from STARTDIR that contains
all elements of FILENAMES. If STATDIR is nil then use
current-directory."
  (let ((default-directory (expand-file-name (or startdir ".")))
        (depth 0))
    (catch 'pr-finish
      (while t
        ;; don't go too far down the tree
        (when (> (setq depth (1+ depth)) project-root-max-search-depth)
          (throw 'pr-finish nil))
        (cond
         ((project-root-every 'file-exists-p filenames)
          (throw 'pr-finish default-directory))
         ;; if we hit root
         ((string= (expand-file-name default-directory) "/")
          (throw 'pr-finish nil)))
        ;; try again up a directory
        (setq default-directory
              (expand-file-name ".." default-directory))))))

(defun project-root-p (&optional p)
  "Check to see if P or `project-details' is valid"
  (let ((p (or p project-details)))
    (and p (file-exists-p (cdr p)))))

(defmacro with-project-root (&rest body)
  "Run BODY with default-directory set to the project root. Error
if not found. If `project-root' isn't defined then try and find
one."
  (unless project-details (project-root-fetch))
  `(if (project-root-p)
       (let ((default-directory ,(cdr project-details)))
         ,@body)
     (error "No project root found")))

(defun project-root-goto-root ()
  "Open up the project root in dired."
  (interactive)
  (with-project-root (find-file (cdr project-details))))

(defun project-root-grep ()
  "Run the grep command from the current project root."
  (interactive)
  (with-project-root (call-interactively 'grep)))

(defun project-root-ack ()
  "Run the ack command from the current project root (if ack is
avalible)."
  (interactive)
  (with-project-root
   (if (fboundp 'ack)
       (call-interactively 'ack)
     (error "`ack' not bound"))))

(defun project-root-find-file ()
  "Find a file from a list of those that exist in the current
project."
  (interactive)
  (with-project-root (call-interactively 'find-file)))

(defun project-root-execute-extended-command ()
  "Run `execute-extended-command' after having set
`default-directory' to the root of the current project."
  (interactive)
  (with-project-root (execute-extended-command current-prefix-arg)))

;;; anything.el config

(defvar project-root-anything-config-files
  '((name . "Project Files")
    (init . (lambda ()
              (unless project-details
                (project-root-fetch))
              (setq anything-project-root project-details)))
    (candidates . (lambda ()
                    (project-root-file-find-process anything-pattern)))
    (type . file)
    (requires-pattern . 2)
    (volatile)
    (delayed)))

(defvar project-root-anything-config-bookmarks
  '((name . "Project Bookmarks")
    (init . (lambda ()
              (unless project-details
                (project-root-fetch))
              (setq anything-project-root project-details)))
    (candidates . (lambda ()
                    (project-root-bookmarks anything-project-root)))
    (type . file)))

(defun project-root-file-find-process (pattern)
  "Return a process which represents a find of all files matching
`project-root-extra-find-args' and the hard-coded arguments in
this function."
  (when anything-project-root
    (start-process-shell-command "project-root-find"
                                 nil
                                 "find"
                                 (cdr anything-project-root)
                                 (find-to-string
                                  `(and ,project-root-extra-find-args
                                        (name ,(concat "*" pattern "*"))
                                        (type "f"))))))

(provide 'project-root)
