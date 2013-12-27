;;; dar.el --- disk archiver (DAR) interface for emacs

;; Copyright (C) 2005-2006 by Stefan Reichoer

;; Author: Stefan Reichoer, <stefan@xsteve.at>

;; dar.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; dar.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; dar.el provides an Emacs interface for DAR from:
;; http://dar.linux.free.fr/

;; The latest revision of dar.el can be found at:
;; http://www.xsteve.at/prg/emacs/dar.html

;; dar can be used to create backups and store them on harddisk
;; dar.el allows to define backup rules to create full and incremental
;; backups from various file trees.
;; A dry-run option allows to test your backup rules easily.

;; To use this package, put the following in your .emacs:
;; (require 'dar)
;; (setq dar-backup-rules
;;       '((all
;;          (backup-dir "~/bak/dar")
;;          (log-file "~/bak/dar/dar-el.log")
;;          (backup-interval-differential daily)
;;          (backup-interval-full monthly)
;;          )
;;         (create
;;          (compress bzip2)
;;          )
;;         ("xsteve-planner"
;;          (root "~/Plans/")
;;          (backup-interval-full weekly)
;;          )
;;         ("xsteve-wiki"
;;          (root "~/data/wiki/")
;;          (exclude-directories (".hg"))
;;          )
;;         ("xsteve-config"
;;          (root "~/xsteve-config/")
;;          (backup-interval-differential weekly)
;;         )))

;; The dar-backup-rules provide the rules for your backups.
;; The entry 'all is considered for all dar operations:
;;  - backup-dir: Specify the path for your backup files
;;  - log-file:   Specify the log file for archiving operations
;;  - backup-interval-differential: can be daily, weekly or monthly
;;  - backup-interval-full: can be daily, weekly or monthly

;; The entry 'create holds the rules for archive creation
;;  - (compress bzip2) enables bzip2 compression for the archived files

;; The entries xsteve-planner, xsteve-wiki and xsteve-config specify rule-sets
;;  - root: the root directory for the backup files
;;  - exclude-directories: A list of directories that should not be archived

;; The entries for create and all can be overridden for a specific rule

;; When you have written your dar-backup-rules, start viewing your
;; (initially empty list) via M-x dar-backups

;; The following commands are useful for the first experiments:
;; e ... dar-toggle-dry-run
;; v ... dar-toggle-verbose-run
;; r ... dar-toggle-rule-debug

;; The backup rule description above works for my use cases. Please
;; report your needs and enhancement ideas, I would like to see dar.el
;; as the full featured backup frontend for emacs.

;; Here is a recipe to automate the backup creation.
;; Use the midnight package to schedule a backup for 3:00am

;; (defun dar-backup-schedule-backup ()
;;   (message "dar-backup-schedule-backup run at %s" (current-time-string))
;;   (run-at-time "3:00am" nil 'dar-backup-all-rule-sets))
;;
;; (require 'midnight)
;; (add-hook 'midnight-hook 'dar-backup-schedule-backup)



;;; History:
;;

;;; Code:

(defvar dar-executable "dar" "Full path for the dar executable.")

(defvar dar-timestring-postfix "-%Y-%m-%d_%H-%M" "The format string that should be used as postfix for the dar archive names.
See `format-time-string' for details.")

;; some dar options
;; -v              verbose output
;; -e              dry run, fake execution, nothing is produced

;; special rules, these are similar used than the one in the /etc/darrc or ~/.darrc file (see man dar)
;; 'create  for creation of archives
;; 'all     for all operations

;; Not yet implemented:
;; **EXTRACT**
;; **LIST**
;; **TEST**
;; **DIFF**
;; **ISOLATE**
;; **DEFAULT** if none of the operations above - not used in dar.el

;; This is another example for backup rules:

;; (setq dar-backup-rules
;;       '((all
;;          (backup-dir "~/bak/dar")
;;          (log-file "~/bak/dar/dar-el.log")
;;          (backup-interval-differential daily)
;;          (backup-interval-full monthly)
;;          )
;;         (create
;;          (compress bzip2)
;;          ;; (compress gzip)
;;          ;; (compress nil)
;;          ;; (compress (gzip 7))
;;          )
;;         ("xsteve-config"
;;          (root "~/xsteve-config/")
;;          (exclude-directories (".arch-ids" "ion3/.arch-ids" "app-defaults/.arch-ids" "{arch}"))
;;          ;;(extra-flags ("-e")) use a more usefull option than -e here...
;;          ;;(extra-create-flags ("-e")) use a more usefull option than -e here...
;;          )
;;         ("old-project"
;;          (root "~/projects/hello-world")
;;          (disable-auto-backup t)
;;          )
;;         ("xsteve-mail"
;;          (root "~/gnus/nnml-mail")
;;          )))

;; backup interval specification
;; (backup-interval-full daily)
;; (backup-interval-full (daily))
;; (backup-interval-full (daily 3)) ;; every day. Earliest at 3am
;; (backup-interval-full weekly)
;; (backup-interval-full (weekly 1)) ;; every week. Earliest at day one of the week
;; (backup-interval-full monthly)
;; (backup-interval-full (monthly 15)) ;; every month. Earliest at day 15 of the month
;; (backup-interval-differential daily) ;; every day
;; TODO: differential backup should not be run, if the full is run on that day

;; TODO: make dar-backup-rules customizable??
;; That variable must be set in your .emacs!
(defvar dar-backup-rules nil "Backup rules for dar. Look at dar.el for possible rules.")
(defvar dar-backup-rules-lisp-file "~/.emacs") ;; needed by M-x dar-backup-edit-backup-rules

(defvar dar-temp-dir
  (expand-file-name
   (or
    (when (boundp 'temporary-file-directory) temporary-file-directory)
    (when (fboundp 'temp-directory) (file-name-as-directory (temp-directory)))
    "/tmp/")) "The directory that is used to store temporary files for dar.")

;; internal variables
(defvar dar-rule-set nil)
(defvar dar-finish-message nil)
(defvar dar-write-to-log-file nil)
(defvar dar-running-command nil)
(defvar dar-marked-file-list nil)
(defvar dar-marked-ruleset-list nil)
(defvar dar-extracted-files nil)

(defvar dar-run-queue nil)

(defconst dar-backup-rule-start-regex "^\\[")

;; Some thoughts about a useful backup strategy
;; Do a full backup every week
;; do an differential backup every day

(defun dar-get-rule-elem-for-rule-set (rule-set elem)
  "Get the value of the setting ELEM for RULE-SET."
  (let ((rules (car (delete nil (mapcar
                                 '(lambda (entry)
                                    (if (equal rule-set (car entry)) (cdr entry)))
                                 dar-backup-rules)))))
    (cadr (assoc elem rules))))

;; (dar-get-rule-elem 'all 'log-file)


(defun dar-all-rule-set-names ()
  "Get a list of the available backup rule sets."
  (delete nil (mapcar '(lambda(arg) (unless (symbolp (car arg)) (car arg))) dar-backup-rules)))

;; (dar-all-rule-set-names)

(defun dar-get-rule-elem (rule-set elem &optional specific-default-rule-set)
  "Get elem for the given RULE-SET.
If SPECIFIC-DEFAULT-RULE-SET is given look there, if it is not defined in RULE-SET.
Otherweise look in the 'all rule-set instead."
  (or (dar-get-rule-elem-for-rule-set rule-set elem)
      (dar-get-rule-elem-for-rule-set specific-default-rule-set elem)
      (dar-get-rule-elem-for-rule-set 'all elem)))

;; (dar-get-rule-elem "xsteve-wiki" 'root)
;; (dar-get-rule-elem "xsteve-wiki" 'backup-dir)
;; (dar-get-rule-elem "xsteve-wiki" 'log-file)
;; (dar-get-rule-elem "xsteve-wiki" 'compress)
;; (dar-get-rule-elem "xsteve-wiki" 'compress 'create)

(defun dar-backup-dir (rule-set)
  "Get the backup directory for the given RULE-SET.
If it is not specified there, use it from the 'all rule-set instead."
  (expand-file-name (file-name-as-directory (dar-get-rule-elem rule-set 'backup-dir))))

;; (dar-backup-dir "xsteve-wiki")

(defun dar-log-file-name (rule-set)
  "Get the backup directory for the given RULE-SET.
If it is not specified there, use it from the 'all rule-set instead."
  (expand-file-name (dar-get-rule-elem rule-set 'log-file)))

;;(dar-log-file-name "xsteve-wiki")

(defun dar-disable-auto-backup (rule-set)
  "Ask, whether auto backups are disabled for the given RULE-SET.
If it is not specified there, use it from the 'all rule-set instead.
If it is nowhere specified, auto backups are enabled, that means
`dar-backup-all-rule-sets' will backup all rule-sets.
if you add (disable-auto-backup t) to a rule-set, it will be skipped."
  (dar-get-rule-elem rule-set 'disable-auto-backup))

;;(dar-disable-auto-backup "xsteve-wiki")

;;(format-time-string dar-timestring-postfix)
(defun dar-archive-base-name (file-name)
  "Remove the .<num>.dar suffix from a filename"
  (replace-regexp-in-string "\.[0-9]+\.dar$" "" file-name))

;; (dar-archive-base-name (dar-last-backup-file "xsteve-wiki" nil))

(defun dar-build-archive-name (rule-set &optional differential base-archive time)
  (let ((archive-type-string (if differential "--incr-" ""))
        (base-string (if base-archive (dar-build-base-name-string rule-set base-archive) "")))
    (concat rule-set (format-time-string (concat archive-type-string
                                                 dar-timestring-postfix) time) base-string)))

;; (dar-build-archive-name "xsteve-wiki" t)
;; (dar-build-archive-name "xsteve-wiki" t (dar-last-backup-file "xsteve-wiki" nil))
;; (dar-build-archive-name "xsteve-config" t (dar-last-backup-file "xsteve-config" nil))
;; (dar-build-archive-name "xsteve-config" t (dar-last-backup-file "xsteve-config" t))

(defun dar-build-base-name-string (rule-set archive-name)
  (let* ((without-rule-set (replace-regexp-in-string (concat rule-set "-") "" (file-name-nondirectory (dar-archive-base-name archive-name))))
         (without-base-ref (replace-regexp-in-string "--base.+$" "" without-rule-set)))
    (concat "--base-" without-base-ref)))

;; (dar-build-base-name-string "xsteve-wiki" (dar-last-backup-file "xsteve-wiki" nil))
;; (dar-build-base-name-string "xsteve-config" (dar-last-backup-file "xsteve-config" nil))

(defun dar-get-compress-command-line-flag (rule-set)
  (let ((compress-sy (dar-get-rule-elem rule-set 'compress 'create))
        (compress-level ""))
    (when (and compress-sy (listp compress-sy))
      (setq compress-level (number-to-string (cadr compress-sy)))
      (setq compress-sy (car compress-sy)))
    (cond ((eq compress-sy 'gzip)
           (concat "-z" compress-level))
          ((eq compress-sy 'bzip2)
           (concat "-y" compress-level))
          (t
           nil))))

;; (dar-get-compress-command-line-flag "xsteve-wiki")

(defun dar-get-exclude-directories-command-line-flag (rule-set)
  (mapcar '(lambda (path) (list "-P" path))
          (dar-get-rule-elem rule-set 'exclude-directories)))

;; (dar-get-exclude-directories-command-line-flag "xsteve-wiki")

(defun dar-get-extra-flags-command-line-flag (rule-set)
  (dar-get-rule-elem rule-set 'extra-flags))

;; (dar-get-extra-flags-command-line-flag "xsteve-config")

(defun dar-get-extra-create-flags-command-line-flag (rule-set)
  (dar-get-rule-elem rule-set 'extra-create-flags))

;; (dar-get-extra-create-flags-command-line-flag "xsteve-config")
;; (dar-get-extra-create-flags-command-line-flag "xsteve-wiki")

(defun dar-get-dry-run-flag ()
  (when dar-dry-run "-e"))

(defun dar-get-verbose-run-flag ()
  (when dar-verbose-run "-v"))

(defun dar-backup-file-list (rule-set &optional full-name type)
  "Get a list of available backup files for a RULE-SET.
If FULL-NAME is t, use the full path, otherwise only the file name.

TYPE can be one of 'differential or 'full. Any other value gives all names for that RULE-SET."
  (let ((file-list
         (directory-files (dar-backup-dir rule-set) full-name (concat rule-set ".+\\.dar")))
        (filter-func
         (cond ((eq type 'differential)
                '(lambda (arg) (when (string-match "--incr-" arg) arg)))
               ((eq type 'full)
                '(lambda (arg) (unless (string-match "--incr-" arg) arg)))
               (t
                'identity))))
    (delete nil (mapcar filter-func file-list))))
;; (dar-backup-file-list "xsteve-wiki" nil 'differential)
;; (dar-backup-file-list "xsteve-wiki" nil 'full)
;; (dar-backup-file-list "xsteve-wiki" nil)

(defun dar-sorted-backup-file-list (rule-set &optional full-name type)
  "Return a list sorted by the creation time of backup files for RULE-SET."
  (let ((sorted-list (sort (dar-backup-file-list rule-set t type)
                           '(lambda (a b) (< (dar-seconds-since-last-write a) (dar-seconds-since-last-write b))))))
    (if full-name
        sorted-list
      (mapcar 'file-name-nondirectory sorted-list))))

;; (dar-sorted-backup-file-list "xsteve-wiki" nil 'full)

(defun dar-last-backup-file (rule-set &optional full-name type)
  "Return the last generated backup file for RULE-SET."
  (car (dar-sorted-backup-file-list rule-set full-name type)))


;; (dar-last-backup-file "xsteve-wiki" nil)
;; (dar-last-backup-file "xsteve-wiki" t)
;; (dar-last-backup-file "xsteve-wiki" t 'differential)
;; (dar-last-backup-file "xsteve-wiki" t 'full)

;; the same as ls-lisp-time-to-seconds
(defun dar-time-to-seconds (time)
  "Convert TIME to a floating point number."
  (+ (* (car time) 65536.0)
     (cadr time)
     (/ (or (nth 2 time) 0) 1000000.0)))

(defun dar-seconds-since-last-write (file-name)
  (if file-name
      (- (dar-time-to-seconds (current-time))
         (dar-time-to-seconds (nth 5 (file-attributes file-name))))
    "n/a"))


(defun dar-days-since-last-write (file-name)
  (if file-name
      (/ (dar-seconds-since-last-write file-name) (* 60 60 24))
    "n/a"))

(defun dar-float-as-string (float)
  (if (numberp float) (format "%1.1f" float) float))

                                        ;(dar-seconds-since-last-write (dar-last-backup-file "xsteve-wiki" t))
                                        ;(dar-days-since-last-write (dar-last-backup-file "xsteve-wiki" t))

(defun dar-days-since-last-backup (rule-set &optional type)
  (dar-days-since-last-write (dar-last-backup-file rule-set t type)))

                                        ;(dar-days-since-last-backup "xsteve-wiki")
                                        ;(dar-days-since-last-backup "xsteve-wiki" 'differential)
                                        ;(dar-days-since-last-backup "xsteve-wiki" 'full)

;; inspired by ls-lisp-format-file-size
(defun dar-file-size (file-name human-readable)
  (let ((file-size (nth 7 (file-attributes file-name))))
    (if (or (not human-readable)
            (< file-size 1024))
        (format (if (floatp file-size) "%1.0f" "%d") file-size)
      (do ((file-size (/ file-size 1024.0) (/ file-size 1024.0))
           ;; kilo, mega, giga, tera, peta, exa
           (post-fixes (list "k" "M" "G" "T" "P" "E") (cdr post-fixes)))
          ((< file-size 1024) (format "%1.0f%s"  file-size (car post-fixes)))))))

(defvar dar-trigger-action-string nil) ;; side effect of dar-trigger-action
(defun dar-trigger-action (days-since-last-action trigger-rule)
  (let ((current-hour (string-to-number (format-time-string "%H" (current-time))))
        (current-day (string-to-number (format-time-string "%d" (current-time))))
        (action-hour 0)
        (action-day 1))
    (when (equal "n/a" days-since-last-action)
      (setq days-since-last-action 10000))
    (when (listp trigger-rule)
      (cond ((eq (car trigger-rule) 'daily)
             (setq action-hour (cadr trigger-rule)))
            ((eq (car trigger-rule) 'weekly)
             (setq action-day (cadr trigger-rule)))
            ((eq (car trigger-rule) 'monthly)
             (setq action-day (cadr trigger-rule))))
      (setq trigger-rule (car trigger-rule)))
    (cond ((and (eq trigger-rule 'monthly)
                (or (and (> days-since-last-action 27) (>= action-day current-day))
                    (> days-since-last-action 31))
                (setq dar-trigger-action-string (format "Monthly trigger: days-since-last-action=%s, %S" (dar-float-as-string days-since-last-action) trigger-rule))
                t))
          ((and (eq trigger-rule 'weekly)
                (or (and (> days-since-last-action 6.9) (>= action-day current-day)) ;; fixme...
                    (> days-since-last-action 7.1))
                (setq dar-trigger-action-string (format "Weekly trigger: days-since-last-action=%s, %S" (dar-float-as-string days-since-last-action) trigger-rule))
                t))
          ((and (eq trigger-rule 'daily)
                (message "current-hour: %S action-hour: %S" current-hour action-hour)
                (or (and (> days-since-last-action 0.9) (>= current-hour action-hour))
                    (> days-since-last-action 1.1))
                (setq dar-trigger-action-string (format "Daily trigger: days-since-last-action=%s, %S" (dar-float-as-string days-since-last-action) trigger-rule))
                t))
          (t
           (setq dar-trigger-action-string "No trigger")
           nil))))

;; (dar-trigger-action 1.11 'daily)
;; (dar-trigger-action 1.0 '(daily 11))



(defun dar-flatten-list (list)
  "Flatten any lists within ARGS, so that there are no sublists."
  (loop for item in list
        if (listp item) nconc (svn-status-flatten-list item)
        else collect item))

(defun dar-run (cmd-id parameter-list &optional startup-function startup-param sync-run-output-buffer)
  "Run dar with PARAMETER-LIST as parameter."
  (if sync-run-output-buffer
      (apply 'call-process "dar" nil sync-run-output-buffer nil (dar-flatten-list parameter-list))
    (if dar-running-command
        (progn
          (message "Entering %S %S in dar-run-queue, because %S is still running" cmd-id parameter-list dar-running-command)
          (add-to-list 'dar-run-queue (list cmd-id parameter-list startup-function startup-param) t)
          nil)
      (let ((dar-proc)
            (dar-parameter-list (dar-flatten-list parameter-list)))
        (with-current-buffer
            (get-buffer-create "*dar-output*")
          (toggle-read-only -1)
          (delete-region (point-min) (point-max))
          (insert (format "Running dar %s\n\n" (mapconcat 'identity dar-parameter-list " "))))
        (setq dar-proc (apply 'start-process "dar" "*dar-output*" dar-executable dar-parameter-list))
        (setq dar-running-command cmd-id)
        (set-process-sentinel dar-proc 'dar-process-sentinel)
        (when startup-function
          (apply startup-function startup-param))
        dar-proc))))

(defun dar-run-next-queued ()
  (interactive)
  (setq dar-running-command nil) ;; not sure if this is a good idea...
  (when dar-run-queue
    (apply 'dar-run (car dar-run-queue))
    (setq dar-run-queue (cdr dar-run-queue))))

(defun dar-process-sentinel (process event)
  ;;(princ (format "Process: %s had the event `%s'" process event))
  (save-excursion
    (set-buffer (process-buffer process))
    (dar-output-mode)
    (cond ((string= event "finished\n")
           (cond ((eq dar-running-command 'extract)
                  (when (file-readable-p (car dar-extracted-files))
                    (view-file-other-window (car dar-extracted-files))))
                 (t
                  (if (and dar-rule-set dar-finish-message)
                      (progn
                        (when (and dar-write-to-log-file (not dar-dry-run))
                          (dar-write-to-log-file dar-rule-set (format "%s completed successfully" dar-finish-message)))
                        (message (format "%s completed successfully" dar-finish-message))
                        (setq dar-rule-set nil))
                    (message "dar process finished"))))
           (setq dar-running-command nil))
          ((string= event "killed\n")
           (message "dar process killed")
           (setq dar-running-command nil))
          ((string-match "exited abnormally" event)
           (while (accept-process-output process 0 100))
           ;; find last error message and show it.
           (goto-char (point-max))
           (message "dar failed: %s" event)
           (setq dar-running-command nil))
          (t
           (message "dar process had unknown event: %s" event))))
  (when dar-run-queue
    (dar-run-next-queued)))


;; create an archive
;; dar -c ~/bak/dar/xsteve-wiki-2005-07-08 -R ~/data/wiki/

;; create an archive, compress the invidual files
;; dar -z -c ~/bak/dar/xsteve-wiki-2005-07-08 -R ~/data/wiki/

(defun dar-create-full-archive (rule-set)
  "Create an archive based on the RULE-SET."
  (interactive "sWhich backup should I create: ")
  (let* ((backup-dir (dar-backup-dir rule-set))
         (root (dar-get-rule-elem-for-rule-set rule-set 'root))
         (archive-name (concat backup-dir (dar-build-archive-name rule-set)))
         (msg (format "Creating full dar backup for %s as %s" rule-set archive-name)))
    (dar-run 'create-full
             (list (dar-get-compress-command-line-flag rule-set)
                   (dar-get-exclude-directories-command-line-flag rule-set)
                   (dar-get-dry-run-flag)     ;; "-e"
                   (dar-get-verbose-run-flag) ;; "-v"
                   (dar-get-extra-flags-command-line-flag rule-set)
                   (dar-get-extra-create-flags-command-line-flag rule-set)
                   "-c" archive-name
                   "-R" (expand-file-name root))
             '(lambda (rule-set msg dar-dry-run)
                (with-current-buffer
                    (process-buffer dar-proc)
                  (set (make-local-variable 'dar-rule-set) rule-set)
                  (set (make-local-variable 'dar-write-to-log-file) t)
                  (set (make-local-variable 'dar-finish-message) (format "Full dar backup for %s" rule-set)))
                (message msg)
                (unless dar-dry-run
                  (dar-write-to-log-file rule-set msg)))
             (list rule-set msg dar-dry-run))))

;; (dar-create-full-archive "xsteve-wiki")
;; (dar-create-full-archive "xsteve-mail")
;; (dar-create-full-archive "xsteve-planner")

;; create an differential archive based on a base version
;; dar -z -c ~/bak/dar/xsteve-wiki--incr--2005-12-14 -R ~/data/wiki/ -A ~/bak/dar/xsteve-wiki-2005-12-14

(defun dar-create-differential-archive (rule-set &optional base-type)
  "Create an archive based on the RULE-SET"
  (interactive "sWhich backup should I create: ")
  (let* ((backup-dir (dar-backup-dir rule-set))
         (root (dar-get-rule-elem-for-rule-set rule-set 'root))
         (base-archive (dar-archive-base-name (dar-last-backup-file rule-set t base-type)))
         (archive-name (concat backup-dir (dar-build-archive-name rule-set t base-archive)))
         (msg (format "Creating differential dar backup for %s as %s (based on %s)" rule-set archive-name base-archive)))
    (dar-run 'create-differential
             (list (dar-get-compress-command-line-flag rule-set)
                   (dar-get-exclude-directories-command-line-flag rule-set)
                   (dar-get-dry-run-flag)     ;; "-e"
                   (dar-get-verbose-run-flag) ;; "-v"
                   (dar-get-extra-flags-command-line-flag rule-set)
                   (dar-get-extra-create-flags-command-line-flag rule-set)
                   "-c" archive-name
                   "-R" (expand-file-name root)
                   "-A" base-archive)
             '(lambda (rule-set msg dar-dry-run)
                (with-current-buffer
                    (process-buffer dar-proc)
                  (set (make-local-variable 'dar-rule-set) rule-set)
                  (set (make-local-variable 'dar-write-to-log-file) t)
                  (set (make-local-variable 'dar-finish-message) (format "Differential dar backup for %s" rule-set)))
                (message msg)
                (unless dar-dry-run
                  (dar-write-to-log-file rule-set msg)))
             (list rule-set msg dar-dry-run))))

;;(dar-create-differential-archive "xsteve-wiki")
;;(dar-create-differential-archive "xsteve-mail")

(defun dar-test-archive (file-name rule-set)
  (let* ((archive-name (dar-archive-base-name file-name))
         (msg (format "Running dar backup test for %s" archive-name))
         (dar-proc))
    (if archive-name
        (setq dar-proc (dar-run 'test
                                (list (dar-get-verbose-run-flag) ;;  "-v"
                                      "-t"
                                      (dar-get-extra-flags-command-line-flag rule-set)
                                      archive-name)))
      (message "No dar file at point."))
    (with-current-buffer
        (process-buffer dar-proc)
      (set (make-local-variable 'dar-rule-set) rule-set)
      (set (make-local-variable 'dar-write-to-log-file) nil)
      (set (make-local-variable 'dar-finish-message) msg))
    (message msg)))

(defun dar-diff-archive (file-name rule-set)
  (let* ((archive-name (dar-archive-base-name file-name))
         (root (dar-get-rule-elem-for-rule-set rule-set 'root))
         (msg (format "Running dar backup diff for %s" archive-name))
         (dar-proc))
    (if archive-name
        (setq dar-proc (dar-run 'diff
                                (list (dar-get-verbose-run-flag) ;;  "-v"
                                      (dar-get-extra-flags-command-line-flag rule-set)
                                      "-d"
                                      archive-name
                                      "-R" (expand-file-name root)
                                      )))
      (message "No dar file at point."))
    (with-current-buffer
        (process-buffer dar-proc)
      (set (make-local-variable 'dar-rule-set) rule-set)
      (set (make-local-variable 'dar-write-to-log-file) nil)
      (set (make-local-variable 'dar-finish-message) msg))
    (message msg)))

;; extract a file
;; -f ... flat, don't create directories
;; -O ... don't preserve ownership if not run as root so don't warn
;; -x ... extract
;; -g ... File to extract
;; -w ... overwrite files without warning
;; -r ... don't overwrite newer files
;;dar -f -O -w -x /home/srei/bak/dar/xsteve-wiki--incr--2006-02-21 -g EmacsSemantic.muse

(defun dar-extract-files (archive dest-dir names overwrite-mode)
  (let* ((archive-name (dar-archive-base-name archive))
         (overwrite-switch (cond ((eq overwrite-mode 'overwrite) "-w")
                                 ((eq overwrite-mode 'if-newer) "-r")
                                 (t nil)))
         (preserve-owner-switch "-O")
         (flat-switch "-f")
         (dar-arguments))
    (setq dar-extracted-files (mapcar '(lambda (arg) (concat dar-temp-dir (file-name-nondirectory arg))) names))
    (setq dar-arguments
          (list "-v"
                ;;"-e"
                flat-switch
                preserve-owner-switch
                overwrite-switch
                "-R" (expand-file-name dest-dir)
                "-x" archive-name
                (mapcar '(lambda (name) (list "-g" name)) names)))
    ;; (message (format "dar-extract-files: %s %s %S\nargs: %S" archive dest-dir names dar-arguments))
    (dar-run 'extract dar-arguments)))

;; (dar-extract-files "/home/srei/bak/dar/xsteve-wiki--incr--2006-02-21" "~/tmp/tst" '("EmacsSemantic.muse") 'overwrite)

(defun dar-write-to-log-file (rule-set message)
  (with-current-buffer
      (find-file-noselect (dar-log-file-name rule-set))
    (let ((buffer-read-only nil))
      (goto-char (point-max))
      (insert (format "[%s]: %s\n" rule-set (format-time-string "%c" (current-time))))
      (dolist (line (split-string message "\n"))
        (insert "  ")
        (insert line)
        (newline))
      (newline)
      (save-buffer))))

;;(dar-write-to-log-file "xsteve-wiki" "hello world\nblah blah")

(defun dar-dired-jump ()
  "Jump to a dired buffer, containing the file at point."
  (interactive)
  (let ((file-full-path (dar-file-at-point)))
    (when file-full-path
      (let ((default-directory (file-name-directory file-full-path)))
        (dired-jump))
      (dired-goto-file file-full-path))))

;; taken from DVC.el
(defsubst dar-face-add (str face &optional keymap menu help)
  "Add to string STR the face FACE.
Optionally, also add the text properties KEYMAP, MENU and HELP.

If KEYMAP is a symbol, (symbol-value KEYMAP) is used
as a keymap; and `substitute-command-keys' result
against (format \"\\{%s}\" (symbol-name keymap)) is appended to HELP.

If HELP is nil and if MENU is non nil, the MENU title is used as HELP."
  (let* ((strcpy (copy-sequence str))
         (key-help (when (symbolp keymap)
                     (substitute-command-keys (format "\\{%s}" (symbol-name keymap)))))
         (prefix-help (if help help (when (and menu (stringp (cadr menu))) (cadr menu))))
         (long-help (if key-help
                        (if prefix-help (concat prefix-help "\n"
                                                "================" "\n"
                                                key-help) key-help)
                      help))
         (keymap (if (symbolp keymap) (symbol-value keymap) keymap)))
    (add-text-properties 0 (length strcpy)
                         `(face ,face
                                font-lock-face ,face
                                ,@(when keymap
                                    `(mouse-face highlight
                                                 keymap ,keymap
                                                 help-echo ,long-help))
                                ,@(when menu
                                    `(,dar-cmenu ,menu))
                                )
                         strcpy)
    strcpy))

(defun dar-face-add-with-condition (condition text face1 face2)
  "If CONDITION then add TEXT the face FACE1, else add FACE2."
  (if condition
      (dar-face-add text face1)
    (dar-face-add text face2)))

;; the dar-backup-mode
(defvar dar-backup-mode-map () "Keymap used in `dar-backup-mode' buffers.")
(defvar dar-backup-mode-mark-map () "Subkeymap used for mark/unmark in `dar-backup-mode' buffers.")

(cond ((not dar-backup-mode-map)
       (setq dar-backup-mode-map (make-sparse-keymap))
       (define-key dar-backup-mode-map "q" 'bury-buffer)
       (define-key dar-backup-mode-map "g" 'dar-backups)
       (define-key dar-backup-mode-map "I" 'dar-backup-create-differential-archive)
       (define-key dar-backup-mode-map "F" 'dar-backup-create-full-archive)
       (define-key dar-backup-mode-map "B" 'dar-backup-create-archive)
       (define-key dar-backup-mode-map "L" 'dar-backup-view-log-file)
       (define-key dar-backup-mode-map "E" 'dar-backup-edit-backup-rules)
       (define-key dar-backup-mode-map "T" 'dar-backup-test-archive)
       (define-key dar-backup-mode-map "D" 'dar-backup-diff-archive)
       (define-key dar-backup-mode-map "s" 'dar-view-output-buffer)
       (define-key dar-backup-mode-map "v" 'dar-toggle-verbose-run)
       (define-key dar-backup-mode-map "e" 'dar-toggle-dry-run)
       (define-key dar-backup-mode-map "r" 'dar-toggle-rule-debug)
       (define-key dar-backup-mode-map "n" 'dar-backup-next-rule)
       (define-key dar-backup-mode-map "p" 'dar-backup-previous-rule)
       (define-key dar-backup-mode-map "m" 'dar-backup-mark)
       (define-key dar-backup-mode-map "u" 'dar-backup-unmark)
       (define-key dar-backup-mode-map (kbd "RET") 'dar-backup-view-dar-file)
       (define-key dar-backup-mode-map "x" 'dar-backup-delete-marked-backup-files)
       (define-key dar-backup-mode-map (kbd "C-x C-j") 'dar-dired-jump)
       (when (not dar-backup-mode-mark-map)
         (setq dar-backup-mode-mark-map (make-sparse-keymap))
         (define-key dar-backup-mode-mark-map "!" 'dar-backup-unmark-all)
         (define-key dar-backup-mode-mark-map "*" 'dar-backup-mark-all-rule-sets)
         (define-key dar-backup-mode-mark-map "x" 'dar-backup-mark-obsolete-backup-files))
       (define-key dar-backup-mode-map "*" dar-backup-mode-mark-map)))

(easy-menu-define dar-backup-mode-menu dar-backup-mode-map
  "`dar-backup-mode' menu"
  '("Dar-Backup"
    ["Edit backup rules" dar-backup-edit-backup-rules t]
    ["Create differential archive" dar-backup-create-differential-archive t]
    ["Create full archive" dar-backup-create-full-archive t]
    ["Test archive" dar-backup-test-archive t]
    ["Diff archive against sources" dar-backup-diff-archive t]
    ("Toggle dar run switches"
     ["Toggle dry run" dar-toggle-dry-run t]
     ["Toggle verbose run" dar-toggle-verbose-run t]
     ["Toggle rule debugging" dar-toggle-rule-debug t]
     )
    ("Mark/Unmark"
     ["Mark all rulesets" dar-backup-mark-all-rule-sets t]
     ["Mark obsolete backup files" dar-backup-mark-obsolete-backup-files t]
     ["Unmark all" dar-backup-unmark-all t]
     )
    ["View log file" dar-backup-view-log-file t]
    ["View dar command output" dar-view-output-buffer t]
    ))

(defun dar-backups-insert-entry (file-name)
  (insert (dar-face-add-with-condition (member file-name dar-marked-file-list) (format "  %s " (file-name-nondirectory file-name)) 'compilation-info nil))
  (insert (dar-face-add (format "<%s>" (dar-file-size file-name t)) 'font-lock-variable-name-face))
  (newline)
  (setq overlay (make-overlay (line-beginning-position 0) (point)))
  (overlay-put overlay 'dar-info file-name))

(defun dar-backups-insert-ruleset (rule-set &optional nonewline)
  "Insert the dar backup RULE-SET in the current buffer."
  (insert (dar-face-add-with-condition (member rule-set dar-marked-ruleset-list) (format "[%s]" rule-set) 'compilation-info 'font-lock-function-name-face))
  (when (dar-disable-auto-backup rule-set)
    (insert (dar-face-add " no-auto-backup" 'font-lock-warning-face)))
  (unless nonewline
    (newline)))

(defun dar-backups ()
  (interactive)
  (switch-to-buffer (get-buffer-create "*dar-backups*"))
  (let ((pos (point))
        (overlay))
    (toggle-read-only -1)
    (delete-region (point-min) (point-max))
    (dolist (rule-set (dar-all-rule-set-names))
      (dar-backups-insert-ruleset rule-set)
      (dolist (a (dar-sorted-backup-file-list rule-set t))
        (dar-backups-insert-entry a))
      (newline))
    (when (< pos (point-max))
      (goto-char pos))
    (dar-backup-mode)))


(add-to-list 'file-name-handler-alist '("\\.dar$" . dar-file-name-handler))
(setq auto-mode-alist (append '(("\\.dar$"  . dar-view-darfile-mode)) auto-mode-alist))

;;insert-file-contents ("/mnt/projects/svn_trac-2006-08-31_10-22.1.dar" t nil nil nil)
(defun dar-file-name-handler (operation &rest args)
  ;;(message "dar-file-name-handler: %S %S" operation args)
  (cond ((eq operation 'insert-file-contents)
         (message "dar-file-name-handler: insert-file-contents")
         (let ((filename (car args)))
           (setq buffer-file-name filename)
           (dar-view-darfile-mode)
           (dar-view-dar-file filename)
           (list filename 47)))
        (t
         (dar-run-real-file-name-handler operation args))))

(defun dar-run-real-file-name-handler (operation args)
  (let ((inhibit-file-name-handlers
         (cons 'dar-file-name-handler
               (and (eq inhibit-file-name-operation operation)
                    inhibit-file-name-handlers)))
        (inhibit-file-name-operation operation))
    (apply operation args)))

(defun dar-backup-mode ()
  "Major mode to view the list of made dar backups.
It allows the following actions:
 * create new backups based on `dar-backup-rules'.
 * view the contents of backup files
 * delete old backup files

The following keys are defined:
\\{dar-backup-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (use-local-map dar-backup-mode-map)
  (setq major-mode 'dar-backup-mode)
  (dar-backup-mode-update-mode-line)
  (toggle-read-only 1))

(defun dar-file-at-point ()
  "Return the full file name for the dar file at point."
  (let ((file-info nil))
    (cond ((eq major-mode 'dired-mode)
           (setq file-info (dired-get-filename)))
          (t
           (dolist (overlay (overlays-at (point)))
             (setq file-info (or file-info
                                 (overlay-get overlay 'dar-info))))
           file-info))))

(defun dar-view-output-buffer ()
  (interactive)
  (let ((cur-buf (current-buffer)))
    (pop-to-buffer "*dar-output*")
    (setq tab-width 8) ;; output is formated for tabwidth 8
    (pop-to-buffer cur-buf)))

(defun dar-current-rule-set ()
  "Returns the dar backup rule set at point."
  (save-excursion
    (forward-line 1)
    (dar-backup-previous-rule)
    (if (looking-at "\\[\\(.+\\)\\]")
        (match-string-no-properties 1))))

(defun dar-current-rule-sets ()
  (or dar-marked-ruleset-list (list (dar-current-rule-set))))

(defvar dar-dry-run nil "Whether to run archive creation/extraction with the -e switch")
(defun dar-toggle-dry-run ()
  (interactive)
  (setq dar-dry-run (not dar-dry-run))
  (dar-backup-mode-update-mode-line))

(defvar dar-verbose-run nil "Whether to run archive creation/extraction with the -v switch")
(defun dar-toggle-verbose-run ()
  (interactive)
  (setq dar-verbose-run (not dar-verbose-run))
  (dar-backup-mode-update-mode-line))

(defvar dar-rule-debug nil "Whether to debug which rules would fire")
(defun dar-toggle-rule-debug ()
  (interactive)
  (setq dar-rule-debug (not dar-rule-debug))
  (dar-backup-mode-update-mode-line))

(defun dar-backup-mode-update-mode-line ()
  (let ((one-flag (or dar-dry-run dar-verbose-run dar-rule-debug))
        (flags (mapconcat 'identity (delete nil (list (if dar-dry-run "dry") (if dar-verbose-run "verbose") (if dar-rule-debug "rule-dbg"))) "/")))
    (setq mode-name (concat "dar-backup" (if one-flag (concat "[" flags "]") "")))
    (force-mode-line-update)))

(defun dar-backup-edit-backup-rules ()
  "Edit the backup rules for the current rule set.
That command opens the file specified via `dar-backup-rules-lisp-file'."
  (interactive)
  (let ((current-rule-set (dar-current-rule-set)))
    (find-file-other-window dar-backup-rules-lisp-file)
    (goto-char (point-min))
    (re-search-forward "(setq dar-backup-rules")
    (search-forward (concat "(\"" current-rule-set "\""))
    (beginning-of-line)))

(defun dar-backup-create-differential-archive ()
  (interactive)
  (let ((rule-sets (dar-current-rule-sets)))
    (when (> (length rule-sets) 1)
      (unless (y-or-n-p (format "Create differential archives for the rulesets %S? " rule-sets))
        (setq rule-sets nil)))
    (when rule-sets
      (dolist (rule-set rule-sets)
        (dar-create-differential-archive rule-set)))))

(defun dar-backup-create-full-archive ()
  (interactive)
  (let ((rule-sets (dar-current-rule-sets)))
    (when (> (length rule-sets) 1)
      (unless (y-or-n-p (format "Create full archives for the rulesets %S? " rule-sets))
        (setq rule-sets nil)))
    (when rule-sets
      (dolist (rule-set rule-sets)
        (dar-create-full-archive rule-set)))))

(defun dar-backup-create-archive ()
  (interactive)
  (let ((rule-sets (dar-current-rule-sets)))
    (when (and (> (length rule-sets) 1) (not dar-rule-debug))
      (unless (y-or-n-p (format "Create archives for the rulesets %S? " rule-sets))
        (setq rule-sets nil)))
    (when rule-sets
      (dar-backup-create-archive-for-rule-sets rule-sets))))

(defun dar-backup-create-archive-for-rule-sets (rule-sets)
  (dolist (rule-set rule-sets)
    (message "Checking backup trigger rules for %S" rule-set)
    (message "  Last full backup %s days ago" (dar-float-as-string (dar-days-since-last-backup rule-set 'full)))
    (message "  Last diff backup %s days ago" (dar-float-as-string (dar-days-since-last-backup rule-set 'differential)))
    (if (dar-trigger-action (dar-days-since-last-backup rule-set 'full)
                            (dar-get-rule-elem rule-set 'backup-interval-full))
        (progn
          (message "  %s: %s" rule-set dar-trigger-action-string)
          (message "    ==> Creating full backup for %s" rule-set)
          (unless dar-rule-debug
            (dar-create-full-archive rule-set)))
      (message "full test:  %s: %s" rule-set dar-trigger-action-string)
      (if (dar-trigger-action (dar-days-since-last-backup rule-set)
                              (dar-get-rule-elem rule-set 'backup-interval-differential))
          (progn
            (message "  %s: %s" rule-set dar-trigger-action-string)
            (message "    ==> Creating differential backup for %s" rule-set)
            (unless dar-rule-debug
              (dar-create-differential-archive rule-set)))
        (message "differential test:  %s: %s" rule-set dar-trigger-action-string)))))

(defun dar-backup-test-archive ()
  (interactive)
  (dar-test-archive (dar-file-at-point) (dar-current-rule-set)))

(defun dar-backup-diff-archive ()
  (interactive)
  (dar-diff-archive (dar-file-at-point) (dar-current-rule-set)))

(defun dar-backup-view-log-file ()
  (interactive)
  (find-file-other-window (dar-log-file-name (dar-current-rule-set)))
  (dar-log-file-mode))

(defun dar-backup-view-dar-file (arg)
  (interactive "P")
  (let ((file (dar-file-at-point))
        (only-saved (not arg)))
    (if file
        (dar-view-dar-file (dar-file-at-point) only-saved)
      (message "No dar file at point."))))

(defun dar-backup-next-rule ()
  (interactive)
  (let ((pos (point)))
    (end-of-line)
    (if (re-search-forward dar-backup-rule-start-regex nil t)
        (beginning-of-line)
      (goto-char pos))))

(defun dar-backup-previous-rule ()
  (interactive)
  (let ((pos (point)))
    (beginning-of-line)
    (unless (re-search-backward dar-backup-rule-start-regex nil t)
      (goto-char pos))))

(defun dar-backup-mark ()
  (interactive)
  (let ((file-at-point (dar-file-at-point))
        (rule-set-at-point (dar-current-rule-set))
        (buffer-read-only nil))
    (cond (file-at-point
           (add-to-list 'dar-marked-file-list file-at-point t)
           ;;(message "dar-backup-mark: %s %S" file-at-point dar-marked-file-list)
           (delete-region (line-beginning-position) (+ (line-end-position) 1))
           (dar-backups-insert-entry file-at-point))
          (rule-set-at-point
           (add-to-list 'dar-marked-ruleset-list rule-set-at-point t)
           (save-excursion
             (delete-region (line-beginning-position) (line-end-position))
             (dar-backups-insert-ruleset rule-set-at-point t))
           ;;(message "dar-backup-mark: [%s] %S" rule-set-at-point dar-marked-ruleset-list)
           (dar-backup-next-rule)))))

(defun dar-backup-unmark ()
  (interactive)
  (let ((file-at-point (dar-file-at-point))
        (rule-set-at-point (dar-current-rule-set))
        (buffer-read-only nil))
    (cond (file-at-point
           (setq dar-marked-file-list (delete file-at-point dar-marked-file-list))
           ;;(message "dar-backup-unmark: %s %S" file-at-point dar-marked-file-list)
           (delete-region (line-beginning-position) (+ (line-end-position) 1))
           (dar-backups-insert-entry file-at-point))
          (rule-set-at-point
           (setq dar-marked-ruleset-list (delete rule-set-at-point dar-marked-ruleset-list))
           (save-excursion
             (delete-region (line-beginning-position) (line-end-position))
             (dar-backups-insert-ruleset rule-set-at-point t))
           (message "dar-backup-unmark: [%s] %S" rule-set-at-point dar-marked-ruleset-list)
           (dar-backup-next-rule)))))

(defun dar-backup-unmark-all ()
  (interactive)
  (setq dar-marked-file-list nil)
  (setq dar-marked-ruleset-list nil)
  (dar-backups))

(defun dar-backup-mark-all-rule-sets ()
  (interactive)
  (setq dar-marked-ruleset-list (dar-all-rule-set-names))
  (dar-backups))

(defun dar-backup-mark-obsolete-backup-files ()
  (interactive)
  (let ((old-files))
    (dolist (rule-set (dar-current-rule-sets))
      (setq old-files (cdr (member (car (dar-sorted-backup-file-list rule-set t 'full)) (dar-sorted-backup-file-list rule-set t))))
      (setq dar-marked-file-list (append dar-marked-file-list old-files))))
  (dar-backups))

(defun dar-backup-delete-marked-backup-files ()
  "Delete the marked backup file. There is no way to recover this file."
  (interactive)
  (if dar-marked-file-list
      (when (yes-or-no-p (format "Delete %d marked backup files? " (length dar-marked-file-list)))
        (dolist (file dar-marked-file-list)
          (delete-file file)
          (setq dar-marked-file-list (delete file dar-marked-file-list))
          (message "Deleted %s" file))
        (dar-backups))
    (message "No backup files marked for deletion.")))

;; the dar-output-mode
(defvar dar-output-mode-map () "Keymap used in `dar-output-mode' buffers.")

(cond ((not dar-output-mode-map)
       (setq dar-output-mode-map (make-sparse-keymap))
       (define-key dar-output-mode-map "q" 'bury-buffer)
       ))

(defun dar-output-mode ()
  (interactive)
  (kill-all-local-variables)
  (use-local-map dar-view-darfile-mode-map)
  (setq major-mode 'dar-output-mode)
  (setq mode-name "dar-output")
  (setq tab-width 8)
  (toggle-read-only 1))


;; list archive contents
;; dar -l ~/bak/dar/xsteve-wiki-2005-07-08

;; dar -v -as -l xsteve-wiki--incr--2006-02-06 ... -v display statistics first, -as display only saved files in this dar archive

(defun dar-view-dar-file (file-name &optional only-saved)
  (interactive "fOpen dar file: ")
  (let ((buffer (if (eq major-mode 'dar-view-darfile-mode)
                    (current-buffer)
                  (with-current-buffer (get-buffer-create "*dar-view*")
                    (let ((buffer-read-only nil))
                      (delete-region (point-min) (point-max))
                      (dar-view-darfile-mode)
                      (setq buffer-file-name file-name)
                      (current-buffer))))))
    (dar-run 'view (list (when only-saved "-as") "-l" (dar-archive-base-name file-name)) nil nil buffer)
    (switch-to-buffer buffer)
    (goto-char (point-min))
    (delete-region (line-beginning-position) (+ (line-end-position) 1))
    (forward-line 2)
    (setq buffer-read-only t)           ;*** for read only
    ))

;; (dar-view-dar-file "/home/srei/bak/dar/xsteve-wiki-2005-07-08.1.dar")
;; (dar-view-dar-file "/home/srei/bak/dar/xsteve-wiki-2006-02-03")
;; (dar-view-dar-file "/home/srei/bak/dar/xsteve-wiki--incr--2005-12-14.1.dar")

;; the dar-view-darfile-mode
(defvar dar-view-darfile-mode-map () "Keymap used in `dar-view-darfile-mode' buffers.")

(cond ((not dar-view-darfile-mode-map)
       (setq dar-view-darfile-mode-map (make-sparse-keymap))
       (define-key dar-view-darfile-mode-map "q" 'bury-buffer)
       (define-key dar-view-darfile-mode-map "s" 'dar-view-output-buffer)
       (define-key dar-view-darfile-mode-map "t" 'dar-view-toggle-only-saved)
       (define-key dar-view-darfile-mode-map "v" 'dar-view-extract-and-view-file)
       (define-key dar-view-darfile-mode-map (kbd "RET") 'dar-view-extract-and-view-file)
       ))

(easy-menu-define dar-view-darfile-mode-menu dar-view-darfile-mode-map
  "`dar-view-darfile-mode' menu"
  '("Dar-File"
    ["Extract and view file" dar-view-extract-and-view-file t]
    ["Toggle unsaved visibility" dar-view-toggle-only-saved t]
    ["View dar command output" dar-view-output-buffer t]
    ))

(defun dar-view-darfile-mode ()
  "Major mode to view the content of a dar archive."
  (interactive)
  (kill-all-local-variables)
  (use-local-map dar-view-darfile-mode-map)
  (setq major-mode 'dar-view-darfile-mode)
  (setq mode-name "dar-view")
  (setq tab-width 8)
  (toggle-read-only 1)
  (set (make-local-variable 'dar-view-hide-unsaved) nil))

(defun dar-view-extract-and-view-file ()
  (interactive)
  (let* ((dar-archive-name buffer-file-name)
         (file-in-archive (save-excursion (goto-char (line-beginning-position)) (looking-at "\\[Saved\\]")))
         (file-name-start-pos (save-excursion (goto-char (line-end-position)) (search-backward "\t" (line-beginning-position) t)))
         (file-at-point (if file-name-start-pos (buffer-substring-no-properties (+ file-name-start-pos 1) (line-end-position)) nil)))
    ;;(message "dar-file: %s %s" dar-archive-name file-at-point)
    (if file-in-archive
        (dar-extract-files dar-archive-name dar-temp-dir (list file-at-point) 'overwrite)
      (if file-at-point
          (message (format "Error: file '%s' is not stored in %s" file-at-point dar-archive-name))
        (message "Error: No file at point")))))

(defun dar-view-toggle-only-saved ()
  (interactive)
  (let ((ip (list 'invisible 'dar-not-saved))
        (buffer-read-only nil))
    (if dar-view-hide-unsaved
        (set-text-properties (point-min) (point-max) nil)
      (save-excursion
        (goto-char (point-min))
        (forward-line 2)
        (while (< (point) (point-max))
          (unless (looking-at "\\[Saved\\]")
            (add-text-properties (line-beginning-position) (+ (line-end-position) 1) ip))
          (forward-line 1))))
    (setq dar-view-hide-unsaved (not dar-view-hide-unsaved))))


;; --------------------------------------------------------------------------------
;; dar batch commands
;; --------------------------------------------------------------------------------
(defun dar-backup-all-rule-sets ()
  "Backup all rule sets specified in `dar-backup-rules'."
  (interactive)
  (let ((rule-sets (dar-flatten-list
                    (mapcar '(lambda (rule-set)
                               (if (dar-disable-auto-backup rule-set)
                                   (progn
                                     (message "Note: dar-backup-all-rule-sets: disabled auto backup for %s - no backup created." rule-set)
                                     nil)
                                 rule-set))
                            (dar-all-rule-set-names)))))
    (dar-backup-create-archive-for-rule-sets rule-sets)))

;; --------------------------------------------------------------------------------
;; dar-log-file-mode
;; --------------------------------------------------------------------------------
(defvar dar-log-font-lock-keywords
  (list
   '("\\[\\(.+\\)\\]: \\(.+\\)" (1 font-lock-keyword-face) (2 font-lock-function-name-face nil t)))
  "Expressions to highlight in `dar-log-file-mode' mode.")

(defvar dar-log-file-mode-map () "Keymap used in `dar-log-file-mode' buffers.")

(cond ((not dar-log-file-mode-map)
       (setq dar-log-file-mode-map (make-sparse-keymap))
       (define-key dar-log-file-mode-map "n" 'dar-log-file-next)
       (define-key dar-log-file-mode-map "p" 'dar-log-file-prev)
       (define-key dar-log-file-mode-map "q" 'dar-log-file-quit)
       ))

(defun dar-log-file-mode ()
  (interactive)
  (kill-all-local-variables)
  (use-local-map dar-log-file-mode-map)
  (setq major-mode 'dar-log-file-mode)
  (setq mode-name "dar-log")
  (setq font-lock-defaults '(dar-log-font-lock-keywords nil t))
  (toggle-read-only 1))

(defun dar-log-file-next ()
  (interactive)
  (let ((pos (point)))
    (end-of-line)
    (if (re-search-forward "^\\[" nil t)
        (beginning-of-line)
      (goto-char pos))))

(defun dar-log-file-prev ()
  (interactive)
  (let ((pos (point)))
    (beginning-of-line)
    (unless (re-search-backward "^\\[" nil t)
      (goto-char pos))))

(defun dar-log-file-quit ()
  (interactive)
  (kill-buffer (current-buffer)))

(provide 'dar)

;;; arch-tag: fd8d8121-9d7f-45b6-aeba-1fda0aaf1a94
;;; dar.el ends here
