;; -*- Mode: emacs-lisp; -*-
;; command-frequency.el -- track command frequencies
;; Copyright 2006 by Ryan Yeske
;; Copyright 2006 by Michal Nazarewicz (mina86/AT/mina86.com)
;;
;;{{{ License
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of the
;; License, or (at your option) any later version.
;;
;; GNU Emacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;
;;}}}
;;{{{ Introduction
;;
;; This package provides function which saves how many times each
;; command was executed. You can then review those statistics to see
;; which commands you use the most often and therefore adjust your key
;; bindings.
;;
;; It's probably pretty useles but I had nothing to do and found
;; previous version of this file by Ryan Yeske and felt need to code a
;; bit in elisp. :]
;;
;;}}}
;;{{{ Installation
;;
;; To install Command Frequency mode, put this file on your Emacs
;; `load-path' (or extend the load path to include the directory
;; containing this file), optionally byte compile it, require it and
;; run `command-frequency-mode' function, for example by putting the
;; following to your .emacs file:
;;
;; (require 'command-frequency)
;; (command-frequency-mode 1)
;;
;; From now on you can use `command-frequency' command to see command
;; frequencies. However, each time you close emacs statistics will be
;; lost so better way is:
;;
;; (require 'command-frequency)
;; (command-frequency-table-load)
;; (command-frequency-mode 1)
;; (command-frequency-autosave-mode 1)
;;
;;}}}
;;{{{ Customization
;;
;; All customizable variables are in command-frequency custom group so
;; you can edit them there if you want. What is worth mentioning at
;; this point is that by enabling `command-frequency-autosave-mode'
;; modifying `command-frequency-autosave-destinations' you can make
;; this mode publsh your statistics on a web site, eg:
;;
;; (defun my-command-frequency-html-row-format (num percent command)
;; (format "\t<tr><td>%d</td><td>%.2f%%</td><td>%s</td></tr>\n"
;; num percent command))
;;
;; (setq command-frequency-autosave-destinations
;; (list
;; nil ; save in sexp format in `command-frequency-table-file'
;; ("~/public_html/command-frequency.txt" nil nil t)
;; ; save in given file as plain text with pecentage
;; ("~/public_html/command-frequency" nil nil
;; 'my-command-frequency-html-row-format)
;; ; save in given file in custom format
;; )
;;
;;}}}
;;{{{ Define minor mode
(defgroup command-frequency nil
  "Customization group for Command Frequency mode. Command
Frequency mode stores number of times each command was called and
provides it as a statistical data."
  )
;; [tongue]package-version '(command-frequency . "0.2")
;; :group 'local
;; [tongue]refix "command-frequency")
(define-minor-mode command-frequency-mode
  "Command Frequency mode records number of times each command was
called making it possible to access usage statistics through
various command-frequency-* functions."
  :global t
  :init-value nil
  :lighter nil
  :keymap nil
  :group 'command-frequency
  (if command-frequency-mode
      (add-hook 'post-command-hook 'command-frequency-record)
    (remove-hook 'post-command-hook 'command-frequency-record)))
(defcustom command-frequency-buffer "*frequencies*"
  "Buffer where frequencies are displayed."
  :group 'command-frequency
  :type 'string)
(defcustom command-frequency-table-file "~/.emacs.frequencies"
  "File `command-frequency-table' is saved to/loaded from by
`command-frequency-save' and `command-frequency-load' functions
by default."
  :group 'command-frequency
  :type 'file)
;;}}}
;;{{{ Recording
(defvar command-frequency-table (make-hash-table :test 'equal :size 128)
  "Hash table storing number of times each command was called.")
(defun command-frequency-record ()
  "Records command execution in `command-frequency-table' hash."
  (let ((command this-command) count)
    (when command
      (setq count (gethash command command-frequency-table))
      (puthash command (if count (1+ count) 1)
               command-frequency-table))))
;;}}}
;;{{{ Internal functions returning statistics
(defun command-frequency-list (&optional reverse limit)
  "Returns a cons which car is sum of times any command was used
and cdr is a list of (command . count) pairs. If REVERSE is nil
sorts it starting from the most used command; if it is 'no-sort
the list is not sorted; if it is non-nil and not 'no-sort sorts
it from the least used commands. If LIMIT is positive number
only commands which were used more then LIMIT times will be
added. If it is negative number only commands which were used
less then -LIMIT times will be added."
  (let (l (sum 0))
    (maphash
     (cond
      ((or (not (numberp limit)) (= limit 0))
       (lambda (k v) (setq l (cons (cons k v) l) sum (+ sum v))))
      ((= limit -1) (lambda (k v) (setq sum (+ sum v))))
      ((< limit 0)
       (setq limit (- limit))
       (lambda (k v) (setq sum (+ sum v))
         (if (< v limit) (setq l (cons (cons k v) l)))))
      (t
       (lambda (k v) (setq sum (+ sum v))
         (if (> v limit) (setq l (cons (cons k v) l))))))
     command-frequency-table)
    (cons sum
          (cond
           ((equal reverse 'no-sort) l)
           (reverse (sort l (lambda (a b) (< (cdr a) (cdr b)))))
           (t (sort l (lambda (a b) (> (cdr a) (cdr b)))))))))
(defun command-frequency-string (&optional reverse limit func)
  "Returns formatted string with command usage statistics.
If FUNC is nil each line contains number of times command was
called and the command; if it is t percentage usage is added in
the middle; if it is 'raw each line will contain number an
command separated by single line (with no formatting) otherwise
FUNC must be a function returning a string which will be called
for each entry with three arguments: number of times command was
called, percentage usage and the command.
See `command-frequency-list' for description of REVERSE and LIMIT
arguments."
  (let* ((list (command-frequency-list reverse)) (sum (car list)))
    (mapconcat
     (cond
      ((not func) (lambda (e) (format "%7d %s\n" (cdr e) (car e))))
      ((equal func t)
       (lambda (e) (format "%7d %6.2f%% %s\n"
                           (cdr e) (/ (* 1e2 (cdr e)) sum) (car e))))
      ((equal func 'raw) (lambda (e) (format "%d %s\n" (cdr e) (car e))))
      (t (lambda (e) (funcall func (cdr e) (/ (* 1e2 (cdr e)) sum) (car e)))))
     (cdr list) "")))
;;}}}
;;{{{ Interactive commands
(defun command-frequency (&optional where reverse limit func)
  "Formats command usage statistics using
`command-frequency-string' function (see for description of
REVERSE, LIMIT and FUNC arguments) and:
- if WHERE is nil inserts it in th e
or displays it in echo area if possible; else
- if WHERE is t inserts it in the current buffer; else
- if WHERE is an empty string inserts it into
`command-frequency-buffer' buffer; else
- inserts it into buffer WHERE.
When called interactively behaves as if WHERE and LIMIT were nil,
FUNC was t and:
- with no prefix argument - REVERSE was nil;
- with universal or positive prefix arument - REVERSE was t;
- with negative prefix argument - REVERSE was 'no-sort."
  (interactive (list nil
                     (cond
                      ((not current-prefix-arg) nil)
                      ((> (prefix-numeric-value current-prefix-arg) 0))
                      (t 'no-sort))
                     nil t))
  (cond
   ((not where)
    (display-message-or-buffer (command-frequency-string reverse limit func)
                               command-frequency-buffer))
   ((equal where t)
    (insert (command-frequency-string reverse limit func)))
   (t
    (display-buffer
     (if (and (stringp where) (string= where ""))
         command-frequency-buffer where)
     (command-frequency-string reverse limit func)))))
(defun command-frequency-insert (&optional reverse limit func)
  "This command is identical to `command-frequency' command called with
first argument equal t."
  (interactive (list (cond
                      ((not current-prefix-arg) nil)
                      ((> (prefix-numeric-value current-prefix-arg) 0))
                      (t 'no-sort))
                     nil t))
  (command-frequency t reverse limit func))
(defun command-frequency-write-file (&otpional file reverse limit func)
  "Formats command usage statistics using
`command-frequency-string' function (see for description of
REVERSE, LIMIT and FUNC arguments) and saves it in FILE.
When called interactively askas for file name and behaves as if
LIMIT was nil, FUNC was t and:
- with no prefix argument - REVERSE was nil;
- with universal or positive prefix arument - REVERSE was t;
- with negative prefix argument - REVERSE was 'no-sort."
  (interactive
   (list
    (read-file-name "File to save frequencies to: " nil nil nil "" nil)
    (cond
     ((not current-prefix-arg) nil)
     ((> (prefix-numeric-value current-prefix-arg) 0))
     (t 'no-sort))
    nil t))
  (with-temp-message (format "Saving frequencies to %s" file)
    (with-temp-file file (command-frequency t reverse limit func))))
;;}}}
;;{{{ Save/Load
(defun command-frequency-table-save (&optional file-name)
  "Saves `command-frequency-table' into a file FILE-NAME as a
sexp of an alist. If FILE-NAME is nil uses
`command-frequency-table-file'."
  (interactive
   (list (let ((f (expand-file-name command-frequency-table-file)))
           (read-file-name
            "File to save `command-frequency-table' to: "
            (file-name-directory f) f nil (file-name-nondirectory f)))))
  (with-temp-file
      (expand-file-name (or file-name command-frequency-table-file))
    (prin1 (cdr (command-frequency-list 'no-sort)) (current-buffer))))
(defun command-frequency-table-load (&optional file-name merge)
  "Loads `command-frequency-table' from a file FILE-NAME.
FILE-NAME must contain a valid sexp of an alist. If MERGE is
non-nil (or when called interactively with a prefix argument) the
values from file will be added to the current table. If
FILE-NAME is nil uses `command-frequency-table-file'."
                                        ; Interactive call
  (interactive
   (list (let ((f (expand-file-name command-frequency-table-file)))
           (read-file-name
            "File to load `command-frequency-table' from: "
            (file-name-directory f) f t (file-name-nondirectory f)))
         current-prefix-arg))
                                        ; file-name is nil
  (setq file-name
        (expand-file-name (or file-name command-frequency-table-file)))
                                        ; Does file-name exist?
  (if (not (file-exists-p file-name))
      (progn
        (if (called-interactively-p)
            (message "File %s does not exist" file-name))
        nil)
                                        ; Load sexp
    (let ((l (with-temp-buffer
               (insert-file-contents file-name)
               (goto-char (point-min))
               (read (current-buffer)))))
      (message (prin1-to-string l))
                                        ; Was it valid sexp?
      (if (or (not (listp l)) (null l))
          (progn
            (if (called-interactively-p)
                (message "File %s does not contain any valid data" file-name))
            nil)
                                        ; Merge?
        (if merge
            (while l
              (let ((count (gethash (cdar l) command-frequency-table)))
                (when (listp (car l))
                  (puthash (caar l) (+ (or count 0) (cdar l))
                           command-frequency-table)))
              (setq l (cdr l)))
                                        ; Overwrite
          (clrhash command-frequency-table)
          (while l
            (when (listp (car l))
              (puthash (caar l) (cdar l) command-frequency-table))
            (setq l (cdr l))))
                                        ; Loaded
        (if (called-interactively-p)
            (message "Table %s from %s"
                     (if merge "merged" "loaded") file-name))
        t))))
;;}}}
;;{{{ Auto save mode
(define-minor-mode command-frequency-autosave-mode
  "Command Frequency Autosave mode automatically saves
`command-frequency-table' every
`command-frequency-autosave-timeout' seconds and when emacs is
killed. `command-frequency-autosave-destinations' variable
describes where and how the table should be saved."
  :global t
  :init-value nil
  :lighter nil
  :keymap nil
  :group 'command-frequency
  (when command-frequency-autosave--timer
    (cancel-timer command-frequency-autosave--timer)
    (setq command-frequency-autosave--timer nil))
  (if command-frequency-autosave-mode
      (progn
        (setq command-frequency-autosave--timer
              (run-at-time t command-frequency-autosave-timeout
                           'command-frequency-autosave--do))
        (add-hook 'kill-emacs-hook 'command-frequency-autosave--do))
    (remove-hook 'kill-emacs-hook 'command-frequency-autosave--do)))
(defcustom command-frequency-autosave-timeout 600
  "How often in seconds `command-frequency-table' should be saved
when `command-frequency-autosave-mode' is enabled. Setting this
value will take effect only after (re)enabling
`command-frequency-autosave-mode'."
  :group 'command-frequency
  :type 'number)
(defcustom command-frequency-autosave-destinations (list nil)
  "Specifies where frequencie table should be saved when
`command-frequency-autosave-mode' is enabled.
This is a list where each element is:
- nil which means save in raw sexp format (using
`command-frequency-table-save' function) to
`command-frequency-table-file' file;
- a string which means to save in raw sexp format to given file;
- a list whcih means to save in plain text format using
`command-frequency-write-file' function - each element of the
list is passed as an argument to that function."
  :group 'command-frequency
  :type '(repeat
          (choice (const :tag "Default autosave file" nil)
                  (file :tag "Sexp formatted file")
                  (list :tag "Plain text file"
                        (file :tag "File name")
                        (choice :tag "Order"
                                (const :tag "Most used first" nil)
                                (const :tag "No sorting" 'no-sortt)
                                (other :tag "Most used last" t))
                        (choice :tag "Limit"
                                integer
                                (other :tag "No limits" nil))
                        (choice :tag "Format"
                                (const :tag "Standard text format" nil)
                                (const :tag "Standard format with percentage"
                                       t)
                                (const :tag "Raw plain text format" 'raw)
                                function)))))
(defvar command-frequency-autosave--timer nil)
(defun command-frequency-autosave--do (&optional destinations)
  (dolist (e (or destinations command-frequency-autosave-destinations))
    (cond
     ((not e) (command-frequency-table-save command-frequency-table-file))
     ((stringp e) (command-frequency-table-save e))
     ((listp e)
      (eval (cons 'command-frequency-write-file e))))))
;;}}}
(provide 'command-frequency)
