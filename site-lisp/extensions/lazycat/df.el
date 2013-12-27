;;; df.el --- display space left on partitions in the mode-line

;; Copyright (C) 1999 by Association April

;; Author: Benjamin Drieu <bdrieu@april.org>
;; Keywords: unix, tools

;; This file is NOT part of GNU Emacs.

;; GNU Emacs as this program are free software; you can redistribute
;; them and/or modify them under the terms of the GNU General Public
;; License as published by the Free Software Foundation; either
;; version 2, or (at your option) any later version.

;; They are both distributed in the hope that they will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.


;;; Commentary:

;;  This is a quick hack to display disk usage in the mode-line.
;;  Disk space remaining is updated every `df-refresh' seconds.

;;  If you work with a lot of users sharing the same partition, it
;;  sometimes happens that there is no space left to save your work, which
;;  may drive you to serious brain damage when you lose important work.
;;  This package allows you to have the available disk space and the buffer
;;  size displayed in the mode-line, so you know when you can save your
;;  file or when it's time to do some cleanup.

;;  This package may (must) not be very optimized or efficient, but
;;  this is a quick hack.  Comments and suggestions are welcome.

;;  df is simple to use :
;;  - Put this file in your load-path
;;  and then
;;  - Put the following in your .emacs : (autoload 'df "df" nil t)
;;  - Add something like (df "/home") in your .emacs if you want to
;;    scan /home
;;  or more simply by using the custom interface:
;;    M-x customize-group df
;;  where you can toggle on `df-run-on-startup'.

;;; History:
;; 

;; $Id: df.el,v 1.5 2003/06/17 23:47:31 psg Exp $

;; $Log: df.el,v $
;; Revision 1.5  2003/06/17 23:47:31  psg
;; Peter S Galbraith <psg@debian.org>
;; - Add autoload for cancel-function-timers (for XEmacs).
;;
;; Revision 1.4  2003/06/17 02:05:26  psg
;;   Peter S Galbraith <psg@debian.org>
;;   - Add customize support.  Users can now enables `df' by simply
;;     customizing variables `df-partition' and `df-run-on-startup'.
;;
;; Revision 1.3  2003/06/17 01:19:23  psg
;; Use mode-line with a hyphen, like elsewhere in Emacs.
;;
;; Revision 1.2  2003/06/17 01:02:20  psg
;; Make checkdoc clean
;;
;; Revision 1.1.1.1  2003/04/04 20:15:58  lolando
;; Initial import, based on version 19.2-1 currently in unstable.
;;
;; Revision 1.8  2001/12/07 13:08:16  benj
;; - fixed a misplaced (interactive)
;;
;; Revision 1.7  2000/06/05 11:19:22  benj
;;  - put some variables local so buffer size is buffer-local
;;  - add a hook to find-file-hook to display correct size
;;
;; Revision 1.6  1999/11/05 22:04:03  benj
;; - Now use a minor mode instead of that ugly dance with mode-line-format
;; - Really use variables instead of constants in the code
;; - Better structuration (df-enable and df-disable)
;; - Some more documentation
;; - Licence typos fixed
;;
;; Revision 1.5  1999/01/24 17:25:54  drieu
;; - Add Paal Steihaug remarks :
;;   + use magic df argument, which only scan a partition
;;   + add (require 'cl)
;;   + df-update is now much clean
;;   + df now use either 'df -m' or 'df -k' when it is needed
;;
;; Revision 1.4  1999/01/04 14:51:01  drieu
;; - Correct a bug so Megabytes are *REALLY* Megabytes
;;
;; Revision 1.3  1999/01/02 15:46:44  drieu
;; - Fix few bugs one more time
;; - Add variables instead of hard-coded strings
;; - Add argument for df
;; - Document the file a bit more
;;
;; Revision 1.2  1998/12/15 17:37:42  drieu
;; - Fix few bugs
;; - Add Buffer size in the mode line
;; - Mesure either in K or Mega bytes
;; - And so on...

;;; Code:

;; Variables that users will want to change
(defgroup df nil
  "Display space left on partitions in the mode-line."
  :group 'convenience)

(defun df-list-partitions ()
  "Return list of mounted partition directories."
  (with-temp-buffer
    (insert-file-contents "/etc/mtab")
    (let ((result))
      (while (re-search-forward "^/dev[^ ]+ \\([^ ]+\\)" nil t)
        (if result
            (add-to-list 'result (match-string 1))
          (setq result (list (match-string 1)))))
      result)))

(defcustom df-partition "/home"
  "*Partition to scan by df package."
  :group 'df
  :load 'df
  :type (append '(radio)
                (nreverse
                 (cons
                  '(string :tag "Other directory")
                  (mapcar (function (lambda (arg) `(const ,arg)))
                          (df-list-partitions))))))

(defcustom df-run-on-startup nil
  "*If non-nil, run `df' on Emacs startup."
  :group 'df
  :require 'df
  :type 'boolean
  :set (lambda (symbol value)
         (set-default symbol value)
         (if (and value df-partition)
             (df))))

;; Variables that users are unlikely to want to change
(defvar df-refresh 60
  "*Refresh rate (in seconds) of the mode-line by df.")
(defvar df-mb-threshold 10
  "*When free disk space reaches this amount (in Mb), show in Mb.")
(defvar df-megabytes-unit "M"
  "String used for displaying megabytes.")
(defvar df-kilobytes-unit "K"
  "String used for displaying kilobytes.")
(defvar df-command "df"
  "*Command used to get disk usage (usually df).")
(defvar df-in-kilobytes "-k"
  "*Argument to use when `df-command' works in kilobytes.")
(defvar df-in-megabytes "-m"
  "*Argument to use when `df-command' works in megabytes.")
(defvar df-command-arguments df-in-kilobytes
  "*Arguments for `df-command'.")

;; Seemless variables to the end user.
(defvar df-space-left ""
  "Space left on device.")
(defvar df-unit nil
  "Unit (either M or K) used for space left.")
(defvar df-mode nil)
(defvar df-string "")
(defvar df-buffer-weight "")

;; Needed because of the 'when' construct
(require 'cl)
(autoload 'cancel-function-timers "timer"
  "Cancel all timers scheduled by `run-at-time' which would run FUNCTION."
  t)

(defun df-update ()
  "Function to update disk usage.  It is used every `df-refresh' seconds."
  (interactive)
  (set-variable
   'df-buffer-weight (int-to-string (/ (length (buffer-string)) 1000)))
  (cond
   ((> (string-to-int df-space-left) (* df-mb-threshold 1000))
    (set-variable 'df-unit df-megabytes-unit)
    (setq df-command-arguments df-in-megabytes))
   ((and (< (string-to-int df-space-left) df-mb-threshold)
         (string-equal df-command-arguments df-in-megabytes))
    (set-variable 'df-unit df-kilobytes-unit)
    (setq df-command-arguments df-in-kilobytes))
   ((not df-unit)
    (set-variable 'df-unit df-kilobytes-unit)))
  (set-process-filter
   (start-process df-command nil df-command df-command-arguments df-partition)
   'df-filter))



(defun df-filter (proc string)
  "Filter for df output.
This function is responsible from updating the mode-line from the df process.
Argument PROC is the df process.
Argument STRING is the output string."
  (when (string-match (format "\\(-?[0-9]+\\) *[0-9%%]+ *%s" df-partition) string)
    (setq df-space-left (match-string 1 string))
    (if (> (string-to-int df-space-left) 1000)
	(set-variable 'df-unit df-megabytes-unit)
      (set-variable 'df-unit df-kilobytes-unit))
    (when (equal df-unit df-megabytes-unit)
      (setq df-space-left (substring df-space-left 0 (- (length df-space-left) 3)))))
  (setq df-string (format " %s%s/%s%s" df-buffer-weight df-kilobytes-unit df-space-left df-unit)))



(defun df-disable ()
  "Stop all command `df-mode' actions."
  (interactive)
  (setq df-mode nil)
  (cancel-function-timers 'df-update))



(defun df-enable ()
  "Function to display disk statistics in the mode-line."
  (interactive)
  (setq df-mode t)
  (make-variable-buffer-local 'df-buffer-weight)
  (make-variable-buffer-local 'df-string)
;;(set-default 'df-string " plop")
  (run-with-timer 0 df-refresh 'df-update)
  (if (not (assq 'df-mode minor-mode-alist))
      (setq minor-mode-alist
	    (cons '(df-mode df-string) minor-mode-alist)))
  (add-hook 'find-file-hooks 'df-update)
;;(add-hook 'write-file-hooks 'df-check)
  (df-update))



;;;(defun df-check ()
					; ca servira plus tard a
					; demander si on est sur de
					; sauvegarder le fichier quand
					; meme
;;; )


(defun df-mode (&optional arg)
  "Toggle display of space left on any filesystem in mode-lines.
This display updates automatically every `df-refresh' seconds.

With a numeric argument, enable this display if ARG is positive."
  (interactive)
  (if
      (if (null arg) (not df-mode)
	(> (prefix-numeric-value arg) 0))
      (df-enable)
    (df-disable)))



;;;###autoload
(defun df (&optional partition)
  "Enables display of space left on any PARTITION in mode-lines.
This display updates automatically every `df-refresh' seconds."
  (interactive)
  (when partition
    (set-variable 'df-partition partition))
  (df-mode 1))

(provide 'df)
;;; df.el ends here
