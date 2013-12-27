;;; mldonkey-vd.el --- Part of the Emacs Interface to MLDonkey

;; Copyright (c) 2003, 2004 David Hansen

;; Author: David Hansen <david.hansen@physik.fu-berlin.de>
;; Maintainer: David Hansen <david.hansen@physik.fu-berlin.de>
;; Contributors: Joakim Verona <joakim@verona.se>
;; Version: 0.0.4

;; mldonkey-vd.el is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; mldonkey-vd.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;; This file is part if the mldonkey.el package.
;;
;; MLDonkey is a multi-networks peer-to-peer client.
;; See <http://www.nongnu.org/mldonkey/> for details.
;;
;; mldonkey.el connects to the telnet interface of the MlDonkey core
;; and uses a lot of regexps to parse the output.  So it might not work
;; with newer versions of MlDonkey.

;;; Change log
;;
;; 0.0.4:
;;   * fixed regexps to work with latest mldonkey core
;;     (thanks to Joakim Verona)
;; 0.0.3a:
;;   * fixed regexp for the network

;;; Documentation:
;;
;; This is most important module.  It shows the list of all downloads in
;; the *MlDonkey* buffer.
;;
;; To specify which columns should be shown set (or customize) the
;; variables mldonkey-show-* (type M-x apropos mldonkey-show- RET to get
;; a full list or see the example configuration).
;;
;; To refresh the list use `mldonkey-vd' which is bound to "v" and "g"
;; in mldonkey-mode.
;;
;; You can modify the filenames by adding a function to the list
;; `mldonkey-vd-filname-filters'.  The functions are called with a
;; string containing the filename and should return the modified
;; filename.
;;
;; Example configuration:
;;
;; ;; with setting the following variables you can decide what to show
;; ;; these are the default values that should nicely fit in a 80 columns
;; ;; terminal.  you can as well customize them.
;; (setq mldonkey-show-network nil)
;; (setq mldonkey-show-number t)
;; (setq mldonkey-show-filename t)
;; (setq mldonkey-show-percent t)
;; (setq mldonkey-show-downloaded nil)
;; (setq mldonkey-show-size nil)
;; (setq mldonkey-show-avail nil)
;; (setq mldonkey-show-days t)
;; (setq mldonkey-show-last-seen t)
;; (setq mldonkey-show-active-sources nil)
;; (setq mldonkey-show-total-sources nil)
;; (setq mldonkey-show-rate t)
;; (setq mldonkey-show-priority nil)
;; (setq mldonkey-show-finished-network nil)
;; (setq mldonkey-show-finished-number t)
;; (setq mldonkey-show-finished-filename t)
;; (setq mldonkey-show-finished-size t)
;; (setq mldonkey-show-finished-md4 nil)
;;
;; ;; this will convert "%20" to spaces in filenames
;; (add-to-list 'mldonkey-vd-filename-filters 'mldonkey-vd-filename-remove-p20)
;;
;; ;; this may or may not help to fix problems with weird characters
;; (add-to-list 'mldonkey-vd-filename-filters 'mldonkey-vd-filename-encode)




;;; Code:

(require 'mldonkey)

;;;; custom variables

(defcustom mldonkey-show-network nil
  "Show the network of a download."
  :group 'mldonkey
  :type 'boolean)

(defcustom mldonkey-show-number t
  "Show the number of a download."
  :group 'mldonkey
  :type 'boolean)

(defcustom mldonkey-show-filename t
  "Show the filename of a download."
  :group 'mldonkey
  :type 'boolean)

(defcustom mldonkey-show-percent t
  "Show the percantage of a download."
  :group 'mldonkey
  :type 'boolean)

(defcustom mldonkey-show-downloaded nil
  "Show how much bytes of a file are already downloaded."
  :group 'mldonkey
  :type 'boolean)

(defcustom mldonkey-show-size nil
  "Show the file size of a download."
  :group 'mldonkey
  :type 'boolean)

(defcustom mldonkey-show-avail nil
  "Show how much of a download is available."
  :group 'mldonkey
  :type 'boolean)

(defcustom mldonkey-show-days t
  "Show how many days a file is already downloading."
  :group 'mldonkey
  :type 'boolean)

(defcustom mldonkey-show-last-seen t
  "Show how many days ago the file was last seen completely."
  :group 'mldonkey
  :type 'boolean)

(defcustom mldonkey-show-active-sources nil
  "Show the number of active sources of a download."
  :group 'mldonkey
  :type 'boolean)

(defcustom mldonkey-show-total-sources nil
  "Show the total number of sources of a download."
  :group 'mldonkey
  :type 'boolean)

(defcustom mldonkey-show-rate t
  "Show the download rate."
  :group 'mldonkey
  :type 'boolean)

(defcustom mldonkey-show-priority nil
  "Show the priority of a download."
  :group 'mldonkey
  :type 'boolean)

(defcustom mldonkey-show-finished-network nil
  "Show the network of a finished download."
  :group 'mldonkey
  :type 'boolean)

(defcustom mldonkey-show-finished-number t
  "Show the number of a finished download."
  :group 'mldonkey
  :type 'boolean)

(defcustom mldonkey-show-finished-filename t
  "Show the filename of a finished download."
  :group 'mldonkey
  :type 'boolean)

(defcustom mldonkey-show-finished-size t
  "Show the size of a finished download."
  :group 'mldonkey
  :type 'boolean)

(defcustom mldonkey-show-finished-md4 nil
  "Show the MD4 sum of a finished download."
  :group 'mldonkey
  :type 'boolean)

;;;; faces

(defface mldonkey-vd-title-face
  '((t (:weight bold :underline t)))
  "Face for the title line of the downloads tables."
  :group 'mldonkey-faces)

(defface mldonkey-vd-downloading-face
  '((((type tty) (class color))
     (:foreground "green"))
    (((class color) (background dark))
      (:foreground "chartreuse3"))
    (((class color) (background light))
      (:foreground "green3")))
  "Face for downloading files."
  :group 'mldonkey-faces)

(defface mldonkey-vd-downloading-never-seen-face
  '((((type tty) (class color))
     (:foreground "magenta"))
    (((class color))
      (:foreground "chocolate")))
  "Face for downloading but never completely seen files."
  :group 'mldonkey-faces)

(defface mldonkey-vd-never-seen-face
  '((((type tty) (class color))
     (:foreground "red"))
    (((class color))
      (:foreground "tomato")))
  "Face for never completely seen files."
  :group 'mldonkey-faces)

(defface mldonkey-vd-seen-face
  '((((type tty) (class color))
     (:foreground "yellow"))
    (((class color))
     (:foreground "goldenrod")))
  "Face for completely seen files."
  :group 'mldonkey-faces)

(defface mldonkey-vd-queued-face
  '((((class color))
     (:foreground "grey60")))
  "Face for localy queued downloads."
  :group 'mldonkey-faces)

(defface mldonkey-vd-paused-face
  '((((class color) (background dark))
     (:foreground "grey50"))
    (((class color) (background light))
     (:foreground "grey70")))
  "Face for paused downloads."
  :group 'mldonkey-faces)

(defface mldonkey-vd-finished-face
  '((((type tty) (class color))
     (:foreground "blue" :weight bold))
    (((type tty) (class mono))
     (:weight bold))
    (((class color) (background dark))
      (:foreground "cornflower blue"))
    (((class color) (background light))
      (:foreground "royal blue")))
  "Face for displaying finished downloads."
  :group 'mldonkey-faces)



;;;; regular expressions

(defconst mldonkey-vd-downloaded-regexp

  ;;  "downloaded[ \t]+\\([0-9]+\\)/\\([0-9]+\\)[ \t]+files"
  "Down: \\([0-9]+\\).\\([0-9]+\\)"

  "Regular expression which matches the first line of the vd output.

This regexp mathes the first line of the vd output which tells about
the total number of downloads.")


(defconst mldonkey-vd-download-regexp

  (concat
   ;; FIXME: does a network may contain spaces?
   "\\[\\(.*?\\)[ \t]*\\([0-9]+\\)\\]"       ; network and number
   "[ \t]+"
   "\\([^\n]+\\)"                            ; filename
   "[ \t]+"
   "\\([0-9\\.]+\\)"                         ; percent
   "[ \t]+"
   "\\([0-9\\.]+\\(?:gb\\|mb\\|kb\\|b\\)\\)" ; downloaded
   "[ \t]+"
   "\\([0-9\\.]+\\(?:gb\\|mb\\|kb\\|b\\)\\)" ; size
   "[ \t]+"
   ;; "\\([0-9\\.]+\\(?:gb\\|mb\\|kb\\|b\\|[ \t]*chunks\\)\\)" ; left
   "\\(-\\|[0-9]+%\\)"                           ; lSeen
   "[ \t]+"
   "\\([0-9]+\\):\\([0-9\\-]+\\)"            ; age and last seen
   "[ \t]+"
   "\\([0-9]+\\)/\\([0-9]+\\)"               ; active sources and total sources
   "[ \t]+"
   "\\([0-9\\.]+\\|\\-\\|queued\\|paused\\)" ; rate
   "[ \t]+"
   "\\(\\-?[0-9]+\\)"                        ; priority
   "[ \t]*\n")

  "Regular expression which matches one line of the running downloads.")


(defconst mldonkey-vd-finished-regexp

  (concat
   "\\[\\(.*?\\)[ \t]+\\([0-9]+\\)[ \t]+\\]"  ; network and number
   "[ \t]+"
   "\\([^\n]+\\)"                            ; filename
   "[ \t]+"
   "\\([0-9]+\\)"                            ; size
   "[ \t]+"
   "\\([0-9A-Z]+\\)"                         ; MD4
   "[ \t]*\n")

  "Regular expression which matches one line of the finished downloads.")


;;;; variables

(defvar mldonkey-vd-num-downloading nil

  "Number of files currently downloading.

Has the value nil before the list of downloads is requested the
first time.")


(defvar mldonkey-vd-num-finished nil

  "Number of files finished downloading.

Has the value nil before the list of downloads is requested the
first time.")


(defvar mldonkey-vd-downloading-list nil

  "List of files currently downloading.

The value is nil before the list of downloads is requested the
first time.")


(defvar mldonkey-vd-finished-list nil

  "List of files finished downloading.

The value is nil before the list of downloads is requested the
first time.
This is a list of vectors.  Each vector has the format
\[network, number, filename, percent, downloaded, size, avail, age,
  last seen, active sources, total sources, rate, priority\].")


(defvar mldonkey-vd-before-inserting-hook nil

  "*Hook called before inserting the downloads in the buffer.

This is mainly for the mldonkey-vd-sort.el module.")


(defvar mldonkey-vd-hook nil

  "*Hook called after everything is done to process the vd command output.")



;;;; key map

(define-key mldonkey-mode-map "g" 'mldonkey-vd)
(define-key mldonkey-mode-map "v" 'mldonkey-vd)




;;;; basic command interface

(defun mldonkey-vd-init ()

  "Initialize the mldonkey-vd module.

Sets back variables to the default values."

  (setq mldonkey-vd-num-downloading nil)
  (setq mldonkey-vd-num-finished nil)
  (setq mldonkey-vd-downloading-list nil)
  (setq mldonkey-vd-finished-list nil))


(add-hook 'mldonkey-open-connection-hook 'mldonkey-vd-init)


(defun mldonkey-vd ()

  "Update the list of downloads in the mldonkey buffer."

  (interactive)

  (mldonkey-send-command "vd" "vd"))


(defun mldonkey-process-vd ()

  "Use the outout of the vd command and present it in the mldonkey buffer."

  (with-current-buffer (get-buffer-create mldonkey-temp-buffer-name)
    (goto-char (point-min))

    ;; output parsing
    (mldonkey-vd-get-downloads)
    (mldonkey-vd-get-finished)
    (erase-buffer)

    ;; modify the filenames
    (dolist (download mldonkey-vd-downloading-list)
      (dolist (filter mldonkey-vd-filename-filters)
        (aset download 2 (funcall filter (aref download 2)))))

    (dolist (finished mldonkey-vd-finished-list)
      (dolist (filter mldonkey-vd-filename-filters)
        (aset finished 2 (funcall filter (aref finished 2)))))

    (run-hooks 'mldonkey-vd-before-inserting-hook)

    ;; insert the tables
    (mldonkey-vd-insert)
    (run-hooks 'mldonkey-vd-hook)))


(add-to-list 'mldonkey-commands '("vd" mldonkey-process-vd))





;;;; functions for parsing the output

(defun mldonkey-vd-get-match (number)

  "Return the match NUMBER form the last search in a buffer."

  (buffer-substring (match-beginning number) (match-end number)))


(defun mldonkey-vd-get-downloads ()

  "Get the list of running downloads.

Parses the output of the vd command and sets the variables
`mldonkey-vd-downloading-list' and `mldonkey-vd-num-downloading'."

  (setq mldonkey-vd-downloading-list nil)
  (setq mldonkey-vd-num-downloading 0)
  
  (while (re-search-forward mldonkey-vd-download-regexp nil t)
    (setq mldonkey-vd-num-downloading (1+ mldonkey-vd-num-downloading))
    (add-to-list
     'mldonkey-vd-downloading-list
     (vconcat (mapcar 'mldonkey-vd-get-match (number-sequence 1 13))))))


(defun mldonkey-vd-get-finished ()

  "Get the list of finished downloads.

Parses the output of the vd command and sets the variable
`mldonkey-vd-finished-list' and `mldonkey-vd-num-finished'."

  (setq mldonkey-vd-finished-list nil)
  (setq mldonkey-vd-num-finished 0)

  (while (re-search-forward mldonkey-vd-finished-regexp nil t)
    (setq mldonkey-vd-num-finished (1+ mldonkey-vd-num-finished))
    (add-to-list
     'mldonkey-vd-finished-list
     (vconcat (mapcar 'mldonkey-vd-get-match (number-sequence 1 5))))))





;;;; functions for modifying the filenames

(defvar mldonkey-vd-filename-filters
  (list 'mldonkey-vd-filename-remove-trailing-ws)

  "*List of functions that are used to modify filenames.

Each function is called with the filename as the argument.  Don't
remove any default functions.  Use `add-to-list' to add your own
filters.")


(defun mldonkey-vd-filename-remove-p20 (filename)

  "Return the string FILENAME with \"%20\" converted to space."

  (mldonkey-replace-all-matches "\\%20" " " filename))


(defun mldonkey-vd-filename-remove-trailing-ws (filename)

  "Return the string FILENAME with trailing whitespace removed."

  (mldonkey-replace-all-matches "[ \t]+$" "" filename))


(defun mldonkey-vd-filename-encode (filename)

  "Try to encode the string FILENAME properly."

  ;; FIXME: no clue if this is doing anything useful
  (let ((coding (detect-coding-string filename t)))
    (if (eq coding 'undecided)
        filename
      (encode-coding-string filename coding))))




;;;; inserting the lists into the buffer


(defconst mldonkey-vd-downloads-titles

  (vector "net "
          "# "
          "file "
          "% "
          "down "
          "size "
          "av "
          "a "
          "l "
          "as "
          "ts "
          "kb/s "
          "pri ")

  "Vector with the titles of the columns for the downloading files.")


(defconst mldonkey-vd-finished-titles

  (vector "net"
          "#"
          "file"
          "size"
          "md4")

  "Vector with the titles of the columns for the finished files.")


(defconst mldonkey-vd-downloads-justs

  (vector 'right
          'right
          'left
          'right
          'right
          'right
          'right
          'right
          'right
          'right
          'right
          'right
          'right)

  "Vector with the justifications of the fields of downloading files.")


(defconst mldonkey-vd-finished-justs

  (vector 'right 'right 'left 'right 'right)

  "Vector with the justifications of the fields of finished files.")


(defun mldonkey-vd-downloads-show-vector ()

  "Generate a vector that show which columns of a dowload are shown.

The vector has entries t or nil which indicate if the column of
download with the same index should be shown or not."

  (vector mldonkey-show-network
          mldonkey-show-number
          mldonkey-show-filename
          mldonkey-show-percent
          mldonkey-show-downloaded
          mldonkey-show-size
          mldonkey-show-avail
          mldonkey-show-days
          mldonkey-show-last-seen
          mldonkey-show-active-sources
          mldonkey-show-total-sources
          mldonkey-show-rate
          mldonkey-show-priority))


(defun mldonkey-vd-finished-show-vector ()

  "Generate a vector that shows which columns of a finished file are shown.

The vector has entries t or nil which indicate if the column of
a finished download with the same index should be shown or not."

  (vector mldonkey-show-finished-network
          mldonkey-show-finished-number
          mldonkey-show-finished-filename
          mldonkey-show-finished-size
          mldonkey-show-finished-md4))


(defun mldonkey-vd-get-widths (files-list titles-list)

  "Calculate the max length of each string in FILES-LIST.

Use TITLES-LIST to initialize the lengths and return a vector
with the maximal widths."

  ;; initialize the list with the widths of the titles
  (let ((widths (vconcat (mapcar 'length titles-list)))
        (num-fields (length titles-list)))
    ;; iterate over all downloads
    (dolist (download files-list)
      ;; iterate over each field
      (dolist (index (number-sequence 0 (1- num-fields)))
        (let ((new-width))
          (when (> (setq new-width (length (aref download index)))
                   (aref widths index))
            (aset widths index new-width)))))
    widths))


(defun mldonkey-vd-fill-string (str len justification)

  "Fill the string STR with spaces up to length LEN according to JUSTIFICATION.

Justification might be one of the symbols 'left or 'right.  Everything
else is interpreted the same way as 'left.  Do nothing if the length of
STR is greater or equal to LEN"

  (let ((n-spaces (- len (length str))))
    (if (<= n-spaces 0)
        str
      (if (eq justification 'right)
          (concat (make-string n-spaces ?\ ) str)
        (concat str (make-string n-spaces ?\ ))))))


(defun mldonkey-vd-fill-vec (vec lens justs)

  "Apply `mldonkey-vd-fill-string' to each element of the vector VEC.

JUSTS and LENS have to be vectors of the same length as
VEC with lengths and justification arguments."

  (let ((result (make-vector (length vec) "")))
    (dolist (index (number-sequence 0 (1- (length vec))))
      (aset result index (mldonkey-vd-fill-string (aref vec index)
                                                  (aref lens index)
                                                  (aref justs index))))
    result))


(defun mldonkey-vd-fill-table (table lens justs)

  "Apply `mldonkey-vd-fill-vec' to each element of the list TABLE.

JUSTS and LENS have to be vectors of the same length as one
element of TABLE lengths and justification arguments."

  (let ((result))
    (dolist (vec table)
      (add-to-list 'result (mldonkey-vd-fill-vec vec lens justs)))
    result))


(defun mldonkey-vd-insert-line (vec prop show)

  "Insert the vector VEC with properties PROP in the current buffer.

Use the vector SHOW to determine which fields are actually
printed and which not."

  (dolist (index (number-sequence 0 (1- (length vec))))
    (when (aref show index)
      (mldonkey-insert-propertized " " prop)
      (mldonkey-insert-propertized (aref vec index) prop))))


(defun mldonkey-vd-downloads-insert-titles ()

  "Insert the titles of the downloads table.

The titles has to be in a vector named `titles'.  So use it
within a (let (... block with this variable defined."

  (insert "\n")
  (mldonkey-vd-insert-line titles '(face mldonkey-vd-title-face)
                           (mldonkey-vd-downloads-show-vector))
  (insert "\n\n"))


(defun mldonkey-vd-finished-insert-titles ()

  "Insert the titles of the finished table.

The titles has to be in a vector named `titles'.  So use it
withing a (let (... block with this variable defined."

  (mldonkey-vd-insert-line titles '(face mldonkey-vd-title-face)
                           (mldonkey-vd-finished-show-vector))
  (insert "\n\n"))


(defun mldonkey-vd-downloads-insert-line (vec)

  "Insert the line VEC of the running downloads."

  (let ((prop))
    ;; is it a running download?
    (if (string-match "^[ \t]*[0-9]" (aref vec 11))
        ;; was it never completely seen?
        (if (string-match "-" (aref vec 8))
            (setq prop '(face mldonkey-vd-downloading-never-seen-face))
          (setq prop '(face mldonkey-vd-downloading-face)))
      ;; is it queued
      (if (string-match "queued" (aref vec 11))
          (setq prop '(face mldonkey-vd-queued-face))
        ;; is it paused
        (if (string-match "paused" (aref vec 11))
            (setq prop '(face mldonkey-vd-paused-face))
          ;; was it never completely seen?
          (if (string-match "-" (aref vec 8))
              (setq prop '(face mldonkey-vd-never-seen-face))
            (setq prop '(face mldonkey-vd-seen-face))))))
    (mldonkey-vd-insert-line vec prop (mldonkey-vd-downloads-show-vector))
    (insert "\n")))


(defun mldonkey-vd-downloads-insert ()

  "Insert the downloads in the current buffer.

The list of downloads has to be in a list of vectors named
`downloads'.  So use it inside a (let (.. block with this
variable defined."

  (mapc 'mldonkey-vd-downloads-insert-line (reverse downloads))
  (insert "\n\n"))


(defun mldonkey-vd-finished-insert ()

  "Insert the list of finished downloads in the current buffer.

The list of downloads has to be in a list of vectors named
`finished'.  So use it inside a (let (.. block with this
variable defined."

  (dolist (vec (reverse finished))
    (mldonkey-vd-insert-line vec '(face mldonkey-vd-finished-face)
                             (mldonkey-vd-finished-show-vector))
    (insert "\n")))


(defun mldonkey-vd-insert (&optional ignore-1 &optional ignore-2)

  "Insert the table with the downloads in the MlDonkey buffer."

  (with-current-buffer (mldonkey-get-mldonkey-buffer)
    (let ((widths) (titles) (downloads) (finished) (inhibit-read-only t)
          (old-pnt (point)) (buffer (mldonkey-get-mldonkey-buffer)))
      (goto-char (point-min))

      ;; calculate the length of each column
      (setq widths (mldonkey-vd-get-widths mldonkey-vd-downloading-list
                                           mldonkey-vd-downloads-titles))

      ;; fill fields with spaces
      (setq titles (mldonkey-vd-fill-vec
                    mldonkey-vd-downloads-titles widths
                    (make-vector (length mldonkey-vd-downloads-titles) 'left)))

      (setq downloads (mldonkey-vd-fill-table
                       mldonkey-vd-downloading-list widths
                       mldonkey-vd-downloads-justs))

      ;; insert the downloads
      (mldonkey-vd-downloads-insert-titles)
      (mldonkey-vd-downloads-insert)

      ;; same for the finished downloads
      (when mldonkey-vd-finished-list
        (setq widths (mldonkey-vd-get-widths mldonkey-vd-finished-list
                                             mldonkey-vd-finished-titles))
        (setq titles (mldonkey-vd-fill-vec
                      mldonkey-vd-finished-titles widths
                      mldonkey-vd-finished-justs))

        (setq finished (mldonkey-vd-fill-table
                        mldonkey-vd-finished-list widths
                        mldonkey-vd-finished-justs))

        (mldonkey-vd-finished-insert-titles)
        (mldonkey-vd-finished-insert))
      (delete-region (point) (point-max))
      ;; evil hack to restore the cursor position (at least approximately)
      (goto-char old-pnt)
      (dolist (buf (get-buffer-window-list buffer 42 t))
  	(set-window-point buf (point))))))


(provide 'mldonkey-vd)

;;; mldonkey-vd.el ends here