;;; mk-project.el ---  Lightweight project handling

;; Copyright (C) 2008  Matt Keller <mattkeller at gmail dot com>
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
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Quickly switch between projects and perform operations on a
;; per-project basis. A 'project' in this sense is a directory of
;; related files, usually a directory of source files. Projects are
;; defined in pure elisp with the 'project-def' function. See the
;; documentation of the following project operation functions for
;; more details.
;;
;; project-load        - set project variables, run startup hook
;; project-unload      - run shutdown hook, unset project variables
;; project-status      - print values of project variables
;; project-close-files - close all files in this project
;; project-compile     - run the compile command
;; project-grep        - fun find-grep on the project's basedir
;; project-find-file   - quickly open a file in basedir by regex
;; project-index       - re-index the project files
;; project-home        - cd to the project's basedir
;; project-tags        - regenerate the project's TAGS file
;; project-dired       - open 'dired' on the project's basedir

;; Example use:
;;
;; (require 'mk-project)
;;
;; (project-def "my-proj"
;;       '((basedir "/home/me/my-proj/")
;;         (src-patterns ("*.lisp" "*.c"))
;;         (ignore-patterns ("*.elc" "*.o"))
;;         (tags-file "/home/me/my-proj/TAGS")
;;         (file-list-cache "/home/mk/.my-proj-files")
;;         (vcs git)
;;         (compile-cmd "make")
;;         (startup-hook myproj-startup-hook)
;;         (shutdown-hook nil)))
;;
;; (defun myproj-startup-hook ()
;;   (find-file "/home/me/my-proj/foo.el"))
;;
;; (global-set-key (kbd "C-c p c") 'project-compile)
;; (global-set-key (kbd "C-c p g") 'project-grep)
;; (global-set-key (kbd "C-c p l") 'project-load)
;; (global-set-key (kbd "C-c p u") 'project-unload)
;; (global-set-key (kbd "C-c p f") 'project-find-file)
;; (global-set-key (kbd "C-c p i") 'project-index)
;; (global-set-key (kbd "C-c p s") 'project-status)
;; (global-set-key (kbd "C-c p h") 'project-home)
;; (global-set-key (kbd "C-c p d") 'project-dired)
;; (global-set-key (kbd "C-c p t") 'project-tags)

;; See the "Project Variables" documentation below for an explanation of
;; each project setting.

;; The latest version of this file can be found at:
;; http://www.littleredbat.net/mk/cgi-bin/gitweb/gitweb.cgi?p=elisp.git;a=blob;f=mk-project.el;hb=HEAD

;;; Code:

(require 'grep)
(require 'thingatpt)
(require 'cl)

;; ---------------------------------------------------------------------
;; Project Variables
;;
;; These variables are set when a project is loaded and nil'd out when
;; unloaded. These symbols are the same as defined in the 2nd parameter
;; to project-def except for their "mk-proj-" prefix.
;; ---------------------------------------------------------------------

(defvar mk-proj-name nil
  "Name of the current project. Required. First argument to project-def.")

(defvar mk-proj-basedir nil
  "Base directory of the current project. Required. Value is expanded with
expand-file-name. Example: ~me/my-proj/.")

(defvar mk-proj-src-patterns nil
  "List of shell patterns to search with grep-find and include in the TAGS
file. Optional. Example: '(\"*.java\" \"*.jsp\").")

(defvar mk-proj-ignore-patterns nil
  "List of shell patterns to avoid searching for with project-find-file.
Optional. Example: '(\"*.class\").")

; TODO: generalize this to ignore-paths variable
(defvar mk-proj-vcs nil
  "When set to one of the VCS types in mk-proj-vcs-path, grep and index
commands will ignore the VCS's private files (e.g., .CVS/). Example: 'git")

(defvar mk-proj-tags-file nil
  "Path to the TAGS file for this project. Optional. Use an absolute path,
not one relative to basedir. Value is expanded with expand-file-name.")

(defvar mk-proj-compile-cmd nil
  "Shell command to build the entire project. Optional. Example: make -k.")

(defvar mk-proj-startup-hook nil
  "Hook function to run after the project is loaded. Optional. Project
variables (e.g. mk-proj-basedir) will be set and can be referenced from this
function.")

(defvar mk-proj-shutdown-hook nil
  "Hook function to run after the project is unloaded. Optional.  Project
variables (e.g. mk-proj-basedir) will still be set and can be referenced
from this function.")

(defvar mk-proj-file-list-cache nil
  "Cache *file-index* buffer to this file. Optional. If set, the *file-index*
buffer will take its initial value from this file and updates to the buffer
via 'project-index' will save to this file. Value is expanded with
expand-file-name.")

(defconst mk-proj-fib-name "*file-index*"
  "Buffer name of the file-list cache. This buffer contains a list of all
the files under the project's basedir - minus those matching ignore-patterns.
The list is used by 'project-find-file' to quickly locate project files.")

(defconst mk-proj-vcs-path '((git . "'*/.git/*'")
                              (cvs . "'*/.CVS/*'")
                              (svn . "'*/.svn/*'")
                              (bzr . "'*/.bzr/*'"))
  "When mk-proj-vcs is one of the VCS types listed here, ignore the associated
paths when greping or indexing the project.")

;; ---------------------------------------------------------------------
;; Utils
;; ---------------------------------------------------------------------

(defun mk-proj-replace-tail (str tail-str replacement)
  (if (string-match (concat tail-str "$")  str)
    (replace-match replacement t t str)
    str))

(defun mk-proj-assert-proj ()
  (unless mk-proj-name
    (error "No project is set!")))

(defun mk-proj-maybe-kill-buffer (bufname)
  (let ((b (get-buffer bufname)))
    (when b (kill-buffer b))))

(defun mk-proj-get-vcs-path ()
  (if mk-proj-vcs
      (cdr (assoc mk-proj-vcs mk-proj-vcs-path))
    nil))

;; ---------------------------------------------------------------------
;; Project Configuration
;; ---------------------------------------------------------------------

(defvar mk-proj-list (make-hash-table :test 'equal))

(defun mk-proj-find-config (proj-name)
  (gethash proj-name mk-proj-list))

(defun project-def (proj-name config-alist)
  "Associate the settings in <config-alist> with project <proj-name>"
  (puthash proj-name config-alist mk-proj-list))

(defun mk-proj-defaults ()
  "Set all default values for project variables"
  (dolist (v '(mk-proj-name mk-proj-basedir mk-proj-src-patterns
               mk-proj-ignore-patterns mk-proj-vcs mk-proj-tags-file
               mk-proj-compile-cmd mk-proj-startup-hook
               mk-proj-shutdown-hook mk-proj-file-list-cache))
    (setf (symbol-value v) nil)))

(defun mk-proj-load-vars (proj-name proj-alist)
  "Set project variables from proj-alist"
  (labels ((config-val (key)
            (if (assoc key proj-alist)
                (car (cdr (assoc key proj-alist)))
              nil))
           (maybe-set-var (var &optional fn)
             (let ((proj-var (intern (concat "mk-proj-" (symbol-name var))))
                   (val (config-val var)))
               (when val (setf (symbol-value proj-var) (if fn (funcall fn val) val))))))
    (mk-proj-defaults)
    ;; required vars
    (setq mk-proj-name proj-name)
    (setq mk-proj-basedir (expand-file-name (config-val 'basedir)))
    ;; optional vars
    (dolist (v '(src-patterns ignore-patterns vcs tags-file compile-cmd
                 startup-hook shutdown-hook))
      (maybe-set-var v))
    (maybe-set-var 'tags-file #'expand-file-name)
    (maybe-set-var 'file-list-cache #'expand-file-name)))

(defun project-load ()
  "Load a project's settings."
  (interactive)
  (catch 'project-load
    (let ((oldname mk-proj-name)
          (name (completing-read "Project Name: " mk-proj-list)))
      (unless (string= oldname name)
        (project-unload))
      (let ((proj-config (mk-proj-find-config name)))
        (if proj-config
            (mk-proj-load-vars name proj-config)
          (message "Project %s does not exist!" name)
          (throw 'project-load t)))
      (when (not (file-directory-p mk-proj-basedir))
        (message "Base directory %s does not exist!" mk-proj-basedir)
        (throw 'project-load t))
      (when (and mk-proj-vcs (not (mk-proj-get-vcs-path)))
        (message "Invalid VCS setting!")
        (throw 'project-load t))
      (message "Loading project %s" name)
      (cd mk-proj-basedir)
      (mk-proj-set-tags-file mk-proj-tags-file)
      (mk-proj-fib-init)
      (when mk-proj-startup-hook
        (run-hooks 'mk-proj-startup-hook)))))

(defun project-unload ()
  "Unload the current project's settings after runnin the shutdown hook."
  (interactive)
  (when mk-proj-name
    (message "Unloading project %s" mk-proj-name)
    (mk-proj-set-tags-file nil)
    (mk-proj-maybe-kill-buffer mk-proj-fib-name)
    (when (and (mk-proj-buffers)
               (y-or-n-p (concat "Close all " mk-proj-name " project files? "))
      (project-close-files)))
    (when mk-proj-shutdown-hook (run-hooks 'mk-proj-shutdown-hook)))
  (mk-proj-defaults)
  (message "Project settings have been cleared"))

(defun project-close-files ()
  "Close all unmodified files that reside in the project's basedir"
  (interactive)
  (mk-proj-assert-proj)
  (let ((closed nil)
        (dirty nil)
        (basedir-len (length mk-proj-basedir)))
    (dolist (b (mk-proj-buffers))
      (cond
       ((buffer-modified-p b)
        (push (buffer-name) dirty))
       (t
        (kill-buffer b)
        (push (buffer-name) closed))))
    (message "Closed %d buffers, %d modified buffers where left open"
             (length closed) (length dirty))))

(defun mk-proj-buffer-p (buf)
  "Is the given buffer in our project based on filename? Also detects dired buffers open to basedir/*"
  (let ((file-name (buffer-file-name buf)))
    (if (and file-name
             (string-match (concat "^" (regexp-quote mk-proj-basedir)) file-name))
        t
      nil)))

(defun mk-proj-buffers ()
  "Get a list of buffers that reside in this project's basedir"
  (let ((buffers nil))
    (dolist (b (buffer-list))
      (when (mk-proj-buffer-p b) (push b buffers)))
    buffers))

(defun project-status ()
  "View project's variables."
  (interactive)
  (mk-proj-assert-proj)
  (message "Name=%s; Basedir=%s; Src=%s; Ignore=%s; VCS=%s; Tags=%s; Compile=%s; File-Cache=%s; Startup=%s; Shutdown=%s"
           mk-proj-name mk-proj-basedir mk-proj-src-patterns mk-proj-ignore-patterns mk-proj-vcs
           mk-proj-tags-file mk-proj-compile-cmd mk-proj-file-list-cache mk-proj-startup-hook mk-proj-shutdown-hook))

;; ---------------------------------------------------------------------
;; Etags
;; ---------------------------------------------------------------------

(defun mk-proj-set-tags-file (tags-file)
  "Setup TAGS file when given a valid file name; otherwise clean the TAGS"
  (mk-proj-maybe-kill-buffer "TAGS")
  (setq tags-file-name tags-file
        tags-table-list nil)
  (when (and tags-file (file-readable-p tags-file))
    (visit-tags-table tags-file)))

(defun mk-proj-etags-cb (process event)
  "Visit tags table when the etags process finishes."
  (message "Etags process %s received event %s" process event)
  (kill-buffer (get-buffer "*etags*"))
  (cond
   ((string= event "finished\n")
    (mk-proj-set-tags-file mk-proj-tags-file)
    (message "Refreshing TAGS file %s...done" mk-proj-tags-file))
   (t (message "Refreshing TAGS file %s...failed" mk-proj-tags-file))))

(defun project-tags ()
  "Regenerate the project's TAG file. Runs in the background."
  (interactive)
  (mk-proj-assert-proj)
  (if mk-proj-tags-file
    (progn
      (cd mk-proj-basedir)
      (message "Refreshing TAGS file %s..." mk-proj-tags-file)
      (let ((etags-cmd (concat "find " mk-proj-basedir " -type f "
                               (mk-proj-find-cmd-src-args mk-proj-src-patterns)
                               " | etags -o " mk-proj-tags-file " - "))
            (proc-name "etags-process"))
        (start-process-shell-command proc-name "*etags*" etags-cmd)
        (set-process-sentinel (get-process proc-name) 'mk-proj-etags-cb)))
    (message "mk-proj-tags-file is not set")))

(defun mk-proj-find-cmd-src-args (src-patterns)
  "Generate the ( -name <pat1> -o -name <pat2> ...) pattern for find cmd"
  (let ((name-expr " \\("))
    (dolist (pat src-patterns)
      (setq name-expr (concat name-expr " -name \"" pat "\" -o ")))
    (concat (mk-proj-replace-tail name-expr "-o " "") "\\) ")))

(defun mk-proj-find-cmd-ignore-args (ignore-patterns)
  "Generate the -not ( -name <pat1> -o -name <pat2> ...) pattern for find cmd"
  (concat " -not " (mk-proj-find-cmd-src-args ignore-patterns)))

;; ---------------------------------------------------------------------
;; Grep
;; ---------------------------------------------------------------------

(defun project-grep ()
  "Run find-grep using this project's settings for basedir and src files."
  (interactive)
  (mk-proj-assert-proj)
  (let* ((wap (word-at-point))
         (regex (if wap (read-string (concat "Grep project for (default \"" wap "\"): ") nil nil wap)
                 (read-string "Grep project for: "))))
    (cd mk-proj-basedir)
    (let ((find-cmd (concat "find . -type f"))
          (grep-cmd (concat "grep -i -n \"" regex "\"")))
      (when mk-proj-src-patterns
        (setq find-cmd (concat find-cmd (mk-proj-find-cmd-src-args mk-proj-src-patterns))))
      (when mk-proj-tags-file
        (setq find-cmd (concat find-cmd " -not -name 'TAGS'")))
      (when (mk-proj-get-vcs-path)
        (setq find-cmd (concat find-cmd " -not -path " (mk-proj-get-vcs-path))))
      (grep-find (concat find-cmd " -print0 | xargs -0 -e " grep-cmd)))))

;; ---------------------------------------------------------------------
;; Compile
;; ---------------------------------------------------------------------

(defun project-compile (opts)
  "Run the compile command for this project."
  (interactive "sCompile options: ")
  (mk-proj-assert-proj)
  (project-home)
  (compile (concat mk-proj-compile-cmd " " opts)))

;; ---------------------------------------------------------------------
;; Home and Dired
;; ---------------------------------------------------------------------

(defun project-home ()
  "cd to the basedir of the current project"
  (interactive)
  (mk-proj-assert-proj)
  (cd mk-proj-basedir))

(defun project-dired ()
  "Open dired in the project's basedir (or jump to the existing dired buffer)"
  (interactive)
  (mk-proj-assert-proj)
  (dired mk-proj-basedir))

;; ---------------------------------------------------------------------
;; Find-file
;; ---------------------------------------------------------------------

(defun mk-proj-fib-init ()
  "Either load the *file-index* buffer from the file cache, or create it afresh."
  (if (and mk-proj-file-list-cache
           (file-readable-p mk-proj-file-list-cache))
      (with-current-buffer (find-file-noselect mk-proj-file-list-cache)
          (with-current-buffer (rename-buffer mk-proj-fib-name)
            (setq buffer-read-only t)
            (set-buffer-modified-p nil)
            (message "Loading *file-index* from %s" mk-proj-file-list-cache)))
    (project-index)))

(defun mk-proj-fib-clear ()
  "Clear the contents of the fib buffer"
  (let ((buf (get-buffer mk-proj-fib-name)))
    (when buf
      (with-current-buffer buf
        (setq buffer-read-only nil)
        (kill-region (point-min) (point-max))
        (setq buffer-read-only t)))))

(defun mk-proj-fib-cb (process event)
  "Handle failure to complete fib building"
  (cond
   ((string= event "finished\n")
    (with-current-buffer (get-buffer mk-proj-fib-name)
      (setq buffer-read-only t)
      (when mk-proj-file-list-cache
        (write-file mk-proj-file-list-cache)))
    (message "Refreshing %s buffer...done" mk-proj-fib-name))
   (t
    (mk-proj-fib-clear)
    (message "Failed to generate the %s buffer!" mk-proj-fib-name))))

(defun project-index ()
  "Regenerate the *file-index* buffer that is used for project-find-file"
  (interactive)
  (mk-proj-assert-proj)
  (message "Refreshing %s buffer..." mk-proj-fib-name)
  (mk-proj-fib-clear)
  (let ((find-cmd (concat "find " mk-proj-basedir " -type f "
                          (mk-proj-find-cmd-ignore-args mk-proj-ignore-patterns)))
        (proc-name "index-process"))
    (when (mk-proj-get-vcs-path)
      (setq find-cmd (concat find-cmd " -not -path " (mk-proj-get-vcs-path))))
    (with-current-buffer (get-buffer-create mk-proj-fib-name)
      (buffer-disable-undo) ;; this is a large change we don't need to undo
      (setq buffer-read-only nil))
    (start-process-shell-command proc-name mk-proj-fib-name find-cmd)
    (set-process-sentinel (get-process proc-name) 'mk-proj-fib-cb)))

(defun* project-find-file (regex)
  "Find file in the current project matching the given regex.

The file list in buffer *file-index* is scanned for regex matches. If only
one match is found, the file is opened automatically. If more than one match
is found, this prompts for completion. See also: project-index."
  (interactive "sFind file in project matching: ")
  (mk-proj-assert-proj)
  (unless (get-buffer mk-proj-fib-name)
    (when (yes-or-no-p "No file index exists for this project. Generate one? ")
      (project-index))
    (message "Cancelling project-find-file")
    (return-from "project-find-file" nil))
  (with-current-buffer mk-proj-fib-name
    (let ((matches nil))
      (goto-char (point-min))
      (dotimes (i (count-lines (point-min) (point-max)))
        (let ((bufline (buffer-substring (line-beginning-position) (line-end-position))))
          (when (string-match regex bufline)
            (push bufline matches))
          (forward-line)))
      (let ((match-cnt (length matches)))
        (cond
         ((= 0 match-cnt)
          (message "No matches for \"%s\" in this project" regex))
         ((= 1 match-cnt )
          (find-file (car matches)))
         (t
          (let ((file (completing-read "Multiple matches, pick one: " matches)))
            (when file
              (find-file file)))))))))

(provide 'mk-project)

;;; mk-project.el ends here
