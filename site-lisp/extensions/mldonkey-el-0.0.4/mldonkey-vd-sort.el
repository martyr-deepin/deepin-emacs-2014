;;; mldonkey-vd-sort.el --- Part of the Emacs Interface to MLDonkey

;; Copyright (c) 2003, 2004 David Hansen

;; Author: David Hansen <david.hansen@physik.fu-berlin.de>
;; Maintainer: David Hansen <david.hansen@physik.fu-berlin.de>
;; Version: 0.0.4

;; mldonkey-vd-sort.el is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; mldonkey-vd-sort.el is distributed in the hope that it will be useful,
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
;; with future versions of MlDonkey.

;;; Change log
;;
;; 0.0.4:
;;   * removed *-left, added *-avail

;;; Documentation:
;;
;; This modules allows you to sort the lists of files.  Either automatically
;; or interactive.
;;
;; To sort automatically you have to set the variables
;; `mldonkey-vd-sort-functions' and `mldonkey-vd-sort-fin-functions'.
;;
;; These variables are list of functions used to sort the downloads.  To
;; revert a search use (not <sorting-function>) (see the example
;; configuration).
;;
;; To get a list of all available functions for the running downloads type
;; M-x apropos RET mldonkey-vd-sort-dl- RET, for the list of finished
;; downloads M-x apropos RET mldonkey-vd-sort-fin- RET.
;;
;; Interactive Search works with the function mldonkey-sort-*.  Get a complete
;; list with M-x apropos RET mldonkey-sort- RET.  These function are bound to
;; the keys "S#", "Sn", "Sf", "S%", "Sd"m "Ss", "SL", "Sa", "Sl", "Sr", "SA",
;; "ST", "Sp", "F#", "Fn", "Ff", "Fs", "Fm".
;;
;; Example configuration
;;
;; ;; the default is to sort by number of the download
;; (setq mldonkey-vd-sort-functions
;;       '((not mldonkey-vd-sort-dl-rate)
;;         (not mldonkey-vd-sort-dl-percent)))
;;
;; (setq mldonkey-vd-sort-fin-functions
;;       '(mldonkey-vd-sort-dl-number))






;;; Code:

(require 'mldonkey)
(require 'mldonkey-vd)


(defcustom mldonkey-vd-sort-functions
  '(mldonkey-vd-sort-dl-number)
  "A list of functions used to sort the list of downloads"
  :group 'mldonkey
  :type 'sexp)

(defcustom mldonkey-vd-sort-fin-functions
  '(mldonkey-vd-sort-fin-number)
  "A list of functions used to sort the list of finished downloads"
  :group 'mldonkey
  :type 'sexp)





;;;; predicates for sorting the list of running downloads

(defun mldonkey-vd-sort-string-less (a b &optional ignore-case)

  "Return t if the string A is less than B.

If IGNORE-CASE is non-nil ignore case of A and B."

  (when ignore-case
    (setq a (downcase a))
    (setq b (downcase b)))

  (string< a b))


(defun mldonkey-vd-sort-dl-network (dl-a dl-b)

  "Return t if the network of DL-A is \"less than\" the one of DL-B.

Compares the network of DL-A and DL-B alphabetically (case insensitive)
and returns t if the network of DL-A is less than the one of DL-B.
Returns nil otherwise.  DL-A and DL-B must be vectors like the ones
saved in `mldonkey-vd-downloading-list'."

  (mldonkey-vd-sort-string-less (aref dl-a 0) (aref dl-b 0)))


(defun mldonkey-vd-sort-dl-number (dl-a dl-b)

  "Return t if the number of DL-A is less than the one of DL-B.

Return nil otherwise.  DL-A and DL-B must be vectors like the ones
saved in `mldonkey-vd-downloading-list'."

  (< (string-to-number (aref dl-a 1)) (string-to-number (aref dl-b 1))))


(defun mldonkey-vd-sort-dl-filename (dl-a dl-b)

  "Return t if the filename of DL-A is \"less than\" the one of DL-B.

Compares the filename of DL-A and DL-B alphabetically (case insensitive)
and returns t if the filename of DL-A is less than the one of DL-B.
Returns nil otherwise. DL-A and DL-B must be vectors like the ones
saved in `mldonkey-vd-downloading-list'."

  (mldonkey-vd-sort-string-less (aref dl-a 2) (aref dl-b 2)))


(defun mldonkey-vd-sort-dl-percent (dl-a dl-b)

  "Return if the percentage of DL-A is less than the one of DL-B.

Return nil otherwise.  DL-A and DL-B must be vectors like the ones
saved in `mldonkey-vd-downloading-list'."

  (< (string-to-number (aref dl-a 3)) (string-to-number (aref dl-b 3))))


(defun mldonkey-vd-sort-filesize-unit-to-number (str)

  "Convert the unit in STRING to a number.

Unit can be \"gb\", \"mb\", \"kb\" or \"b\"."

  (if (equal (downcase str) "gb")
      1000000000
    (if (equal (downcase str) "mb")
        1000000
      (if (equal (downcase str) "kb")
          1000
        (if (equal (downcase str) "b")
            1
          10000000))))) ; "chunks", ~10mb


(defun mldonkey-vd-sort-filesize (dl-a dl-b index)

  "Return t if the filesize of DL-A is less than the filesize of DL-B.

Both filesize have to appear at the position INDEX in the arrays.
Returns nil otherwise.  DL-A and DL-B must be vectors like the ones
saved in `mldonkey-vd-downloading-list'.

This is a helper function used for the sort predicates for the size,
and downloaded fields."

  ;; we have to recognize the "(m|k|g)?b" part
  (let ((number-a) (number-b) (unit-a) (unit-b))

    ;; extract the unit and number
    (string-match "\\([0-9\\.]+\\)\\(gb\\|mb\\|kb\\|b\\|[ \t]*chunks\\)"
                  (aref dl-a index))
    (setq number-a (string-to-number (match-string 1 (aref dl-a index))))
    (setq unit-a (match-string 2 (aref dl-a index)))

    (string-match "\\([0-9\\.]+\\)\\(gb\\|mb\\|kb\\|b\\|[ \t]*chunks\\)"
                  (aref dl-b index))
    (setq number-b (string-to-number (match-string 1 (aref dl-b index))))
    (setq unit-b (match-string 2 (aref dl-b index)))

    (and (< (mldonkey-vd-sort-filesize-unit-to-number unit-a)
            (mldonkey-vd-sort-filesize-unit-to-number unit-b))
         (< number-a number-b))))


(defun mldonkey-vd-sort-dl-downloaded (dl-a dl-b)

  "Return t if the downloaded part of DL-A is less than the one of DL-B.

Return nil otherwise.  DL-A and DL-B must be vectors like the ones
saved in `mldonkey-vd-downloading-list'."

  (mldonkey-vd-sort-filesize dl-a dl-b 4))


(defun mldonkey-vd-sort-dl-size (dl-a dl-b)

  "Return t if the size of DL-A is less than the size of DL-B.

Return nil otherwise.  DL-A and DL-B must be vectors like the ones
saved in `mldonkey-vd-downloading-list'."

  (mldonkey-vd-sort-filesize dl-a dl-b 5))


(defun mldonkey-vd-sort-dl-avail (dl-a dl-b)

  "Return t if the avail part of DL-A is less than the one of DL-B.

Return nil otherwise.  DL-A and DL-B must be vectors like the ones
saved in `mldonkey-vd-downloading-list'"

  (< (string-to-number (aref dl-a 6)) (string-to-number (aref dl-b 6))))


(defun mldonkey-vd-sort-dl-age (dl-a dl-b)

  "Return t if the age of DL-A is less than the one of DL-B.

Return nil otherwise.  DL-A and DL-B must be vectors like the ones
saved in `mldonkey-vd-downloading-list'."

  (< (string-to-number (aref dl-a 7)) (string-to-number (aref dl-b 7))))


(defun mldonkey-vd-sort-dl-last (dl-a dl-b)

  "Return t if the last seen date of DL-A is less than the one of DL-B.

Return nil otherwise.  DL-A and DL-B must be vectors like the ones
saved in `mldonkey-vd-downloading-list'."

  ;; the string might be "-" wich means never
  (if (equal (aref dl-a 8) "-")
      (if (equal (aref dl-b 8) "-")
          nil
        t)
    (if (equal (aref dl-b 8) "-")
        t
      (< (string-to-number (aref dl-a 8)) (string-to-number (aref dl-b 8))))))


(defun mldonkey-vd-sort-dl-active-sources (dl-a dl-b)

  "Return t if the active sources of DL-A are less than the one of DL-B.

Return nil otherwise.  DL-A and DL-B must be vectors like the ones
saved in `mldonkey-vd-downloading-list'."

  (< (string-to-number (aref dl-a 9)) (string-to-number (aref dl-b 9))))


(defun mldonkey-vd-sort-dl-total-sources (dl-a dl-b)

  "Return t if the total sources of DL-A are less than the one of DL-B.

Return nil otherwise.  DL-A and DL-B must be vectors like the ones
saved in `mldonkey-vd-downloading-list'."

  (< (string-to-number (aref dl-a 10)) (string-to-number (aref dl-b 10))))


(defun mldonkey-vd-sort-dl-rate (dl-a dl-b)

  "Return t if the rate of DL-A is less than the one of DL-B.

Return nil otherwise.  \"Paused\" is less than \"Queued\" is less than 0.
DL-A and DL-B must be vectors like the ones saved in
`mldonkey-vd-downloading-list'."

  (if (equal (downcase (aref dl-a 11)) "paused")
      (if (equal (downcase (aref dl-b 11)) "paused")
          nil
        t)
    (if (equal (downcase (aref dl-a 11)) "queued")
        (if (or (equal (downcase (aref dl-b 11)) "paused")
                (equal (downcase (aref dl-b 11)) "queued"))
            nil
          t)
        (if (or (equal (downcase (aref dl-b 11)) "paused")
                (equal (downcase (aref dl-b 11)) "queued"))
            nil
          (< (string-to-number (aref dl-a 11))
             (string-to-number (aref dl-b 11)))))))


(defun mldonkey-vd-sort-dl-state (dl-a dl-b)

  "Return t if the state of DL-A is less than the one of DL-B.

The rule is paused < queued < running."

    (if (equal (downcase (aref dl-a 11)) "paused")
      (if (equal (downcase (aref dl-b 11)) "paused")
          nil
        t)
    (if (equal (downcase (aref dl-a 11)) "queued")
        (if (or (equal (downcase (aref dl-b 11)) "paused")
                (equal (downcase (aref dl-b 11)) "queued"))
            nil
          t)
      nil)))


(defun mldonkey-vd-sort-dl-priority (dl-a dl-b)

  "Return t if the priority of DL-A are less than the one of DL-B.

Return nil otherwise.  DL-A and DL-B must be vectors like the ones
saved in `mldonkey-vd-downloading-list'."

  (< (string-to-number (aref dl-a 12)) (string-to-number (aref dl-b 12))))





;;;; predicates for sorting the list of running downloads

(defun mldonkey-vd-sort-fin-network (dl-a dl-b)

  "Return t if the network of DL-A is \"less than\" the one of DL-B.

Compares the network of DL-A and DL-B alphabetically (case insensitive)
and returns t if the network of DL-A is less than the one of DL-B.
Returns nil otherwise.  DL-A and DL-B must be vectors like the ones
saved in `mldonkey-vd-finished-list'."

  (mldonkey-vd-sort-string-less (aref dl-a 0) (aref dl-b 0)))


(defun mldonkey-vd-sort-fin-number (dl-a dl-b)

  "Return t if the number of DL-A is less than the one of DL-B.

Return nil otherwise.  DL-A and DL-B must be vectors like the ones
saved in `mldonkey-vd-finished-list'."

  (< (string-to-number (aref dl-a 1)) (string-to-number (aref dl-b 1))))


(defun mldonkey-vd-sort-fin-filename (dl-a dl-b)

  "Return t if the filename of DL-A is \"less than\" the one of DL-B.

Compares the filename of DL-A and DL-B alphabetically (case insensitive)
and returns t if the filename of DL-A is less than the one of DL-B.
Returns nil otherwise. DL-A and DL-B must be vectors like the ones
saved in `mldonkey-vd-finished-list'."

  (mldonkey-vd-sort-string-less (aref dl-a 2) (aref dl-b 2)))


(defun mldonkey-vd-sort-fin-size (dl-a dl-b)

  "Return t if the size in bytes  of DL-A is less than the one of DL-B.

Return nil otherwise.  DL-A and DL-B must be vectors like the ones
saved in `mldonkey-vd-finished-list'."

  (< (string-to-number (aref dl-a 3)) (string-to-number (aref dl-b 3))))


(defun mldonkey-vd-sort-fin-md4 (dl-a dl-b)

  "Return t if the md4 of DL-A is \"less than\" the one of DL-B.

Compares the md4 of DL-A and DL-B alphabetically (hex representation)
and returns t if the md4 of DL-A is less than the one of DL-B.
Returns nil otherwise. DL-A and DL-B must be vectors like the ones
saved in `mldonkey-vd-finished-list'."

  (mldonkey-vd-sort-string-less (aref dl-a 4) (aref dl-b 4)))





;;;; The function that do the actual sorting

(defun mldonkey-vd-sort-dl (predicate &optional reverse)

  "Sort the list of downloads, stably, comparing elements using PREDICATE."

  (if (not reverse)

      (setq mldonkey-vd-downloading-list
            (sort mldonkey-vd-downloading-list predicate))

    (setq mldonkey-vd-downloading-list
          (sort mldonkey-vd-downloading-list
                '(lambda (dl-a dl-b)
                   (funcall predicate dl-b dl-a))))))


(defun mldonkey-vd-sort-fin (predicate &optional reverse)

  "Sort the list of finished downloads, comparing elements using PREDICATE."

  (if (not reverse)

      (setq mldonkey-vd-finished-list
            (sort mldonkey-vd-finished-list predicate))

    (setq mldonkey-vd-finished-list
          (sort mldonkey-vd-finished-list
                (lambda (dl-a dl-b)
                  (funcall predicate dl-b dl-a))))))


(defun mldonkey-vd-sort ()

  "Sort the list of finished and running downloads.

Use `mldonkey-vd-sort-functions' as the predicates."

  ;; the running downloads
  (dolist (f mldonkey-vd-sort-functions)
    (if (sequencep f)  ; the sort 'function' is a list like (not func)
        (mldonkey-vd-sort-dl (cadr f) t)
      (mldonkey-vd-sort-dl f)))
  ;; same for the finished downloads
  (dolist (f mldonkey-vd-sort-fin-functions)
    (if (sequencep f)  ; the sort 'function' is a list like (not func)
        (mldonkey-vd-sort-fin (cadr f) t)
      (mldonkey-vd-sort-fin f))))


(add-hook 'mldonkey-vd-before-inserting-hook 'mldonkey-vd-sort)





;;;; functions for interactive sorting

(defun mldonkey-sort-network (&optional reverse)

  "Sort the running downloads by their networks.

If REVERSE is non nil reverse the order."

  (interactive "P")

  (mldonkey-vd-sort-dl 'mldonkey-vd-sort-dl-network reverse)
  (mldonkey-vd-insert))


(defun mldonkey-sort-number (&optional reverse)

  "Sort the running downloads by their numbers.

If REVERSE is non nil reverse the order."

  (interactive "P")

  (mldonkey-vd-sort-dl 'mldonkey-vd-sort-dl-number reverse)
  (mldonkey-vd-insert))


(defun mldonkey-sort-filename (&optional reverse)

    "Sort the running downloads by their filename.

If REVERSE is non nil reverse the order."

  (interactive "P")

  (mldonkey-vd-sort-dl 'mldonkey-vd-sort-dl-filename reverse)
  (mldonkey-vd-insert))


(defun mldonkey-sort-percent (&optional reverse)

    "Sort the running downloads by their percentage.

If REVERSE is non nil reverse the order."

  (interactive "P")

  (mldonkey-vd-sort-dl 'mldonkey-vd-sort-dl-percent reverse)
  (mldonkey-vd-insert))


(defun mldonkey-sort-downloaded (&optional reverse)

    "Sort the running downloads by their sizes of the downloaded part.

If REVERSE is non nil reverse the order."

  (interactive "P")

  (mldonkey-vd-sort-dl 'mldonkey-vd-sort-dl-downloaded reverse)
  (mldonkey-vd-insert))


(defun mldonkey-sort-size (&optional reverse)

    "Sort the running downloads by their file sizes.

If REVERSE is non nil reverse the order."

  (interactive "P")

  (mldonkey-vd-sort-dl 'mldonkey-vd-sort-dl-size reverse)
  (mldonkey-vd-insert))


(defun mldonkey-sort-avail (&optional reverse)

  "Sort the running downloads by availability.

If REVERSE is non nil reverse the order."

  (interactive "P")

  (mldonkey-vd-sort-dl 'mldonkey-vd-sort-dl-avail reverse)
  (mldonkey-vd-insert))


(defun mldonkey-sort-age (&optional reverse)

    "Sort the running downloads by their ages.

If REVERSE is non nil reverse the order."

  (interactive "P")

  (mldonkey-vd-sort-dl 'mldonkey-vd-sort-dl-age reverse)
  (mldonkey-vd-insert))


(defun mldonkey-sort-last (&optional reverse)

    "Sort the running downloads by their last seen date.

If REVERSE is non nil reverse the order."

  (interactive "P")

  (mldonkey-vd-sort-dl 'mldonkey-vd-sort-dl-last reverse)
  (mldonkey-vd-insert))


(defun mldonkey-sort-active-sources (&optional reverse)

    "Sort the running downloads by their numbers of active sources.

If REVERSE is non nil reverse the order."

  (interactive "P")

  (mldonkey-vd-sort-dl 'mldonkey-vd-sort-dl-active-sources reverse)
  (mldonkey-vd-insert))


(defun mldonkey-sort-total-sources (&optional reverse)

    "Sort the running downloads by their total numbers of sources.

If REVERSE is non nil reverse the order."

  (interactive "P")

  (mldonkey-vd-sort-dl 'mldonkey-vd-sort-dl-total-sources reverse)
  (mldonkey-vd-insert))


(defun mldonkey-sort-rate (&optional reverse)

    "Sort the running downloads by their rates.

If REVERSE is non nil reverse the order."

  (interactive "P")

  (mldonkey-vd-sort-dl 'mldonkey-vd-sort-dl-rate reverse)
  (mldonkey-vd-insert))


(defun mldonkey-sort-state (&optional reverse)

  "Sort the running downloads by their state.

If REVERSE is non nil reverse the order."

  (interactive "P")

  (mldonkey-vd-sort-dl 'mldonkey-vd-sort-dl-state reverse)
  (mldonkey-vd-insert))


(defun mldonkey-sort-priority (&optional reverse)

    "Sort the running downloads by their priority.

If REVERSE is non nil reverse the order."

  (interactive "P")

  (mldonkey-vd-sort-dl 'mldonkey-vd-sort-dl-priority reverse)
  (mldonkey-vd-insert))


(defun mldonkey-sort-fin-network (&optional reverse)

    "Sort the finished downloads by their networks.

If REVERSE is non nil reverse the order."

  (interactive "P")

  (mldonkey-vd-sort-fin 'mldonkey-vd-sort-fin-network reverse)
  (mldonkey-vd-insert))


(defun mldonkey-sort-fin-number (&optional reverse)

    "Sort the finished downloads by their numbers.

If REVERSE is non nil reverse the order."

  (interactive "P")

  (mldonkey-vd-sort-fin 'mldonkey-vd-sort-fin-number reverse)
  (mldonkey-vd-insert))


(defun mldonkey-sort-fin-filename (&optional reverse)

    "Sort the finished downloads by their file names.

If REVERSE is non nil reverse the order."

  (interactive "P")

  (mldonkey-vd-sort-fin 'mldonkey-vd-sort-fin-filename reverse)
  (mldonkey-vd-insert))


(defun mldonkey-sort-fin-size (&optional reverse)

    "Sort the finished downloads by their file size.

If REVERSE is non nil reverse the order."

  (interactive "P")

  (mldonkey-vd-sort-fin 'mldonkey-vd-sort-fin-size reverse)
  (mldonkey-vd-insert))


(defun mldonkey-sort-fin-md4 (&optional reverse)

    "Sort the finished downloads by their md4.

If REVERSE is non nil reverse the order."

  (interactive "P")

  (mldonkey-vd-sort-fin 'mldonkey-vd-sort-fin-md4 reverse)
  (mldonkey-vd-insert))




;;;; key bindings

(define-key mldonkey-mode-map "S#" 'mldonkey-sort-number)
(define-key mldonkey-mode-map "Sn" 'mldonkey-sort-network)
(define-key mldonkey-mode-map "Sf" 'mldonkey-sort-filename)
(define-key mldonkey-mode-map "S%" 'mldonkey-sort-percent)
(define-key mldonkey-mode-map "Sd" 'mldonkey-sort-downloaded)
(define-key mldonkey-mode-map "Ss" 'mldonkey-sort-size)
(define-key mldonkey-mode-map "SA" 'mldonkey-sort-avail)
(define-key mldonkey-mode-map "Sa" 'mldonkey-sort-age)
(define-key mldonkey-mode-map "Sl" 'mldonkey-sort-last)
(define-key mldonkey-mode-map "Sr" 'mldonkey-sort-rate)
(define-key mldonkey-mode-map "SS" 'mldonkey-sort-state)
(define-key mldonkey-mode-map "SA" 'mldonkey-sort-active-sources)
(define-key mldonkey-mode-map "ST" 'mldonkey-sort-total-sources)
(define-key mldonkey-mode-map "Sp" 'mldonkey-sort-priority)

(define-key mldonkey-mode-map "F#" 'mldonkey-sort-fin-number)
(define-key mldonkey-mode-map "Fn" 'mldonkey-sort-fin-network)
(define-key mldonkey-mode-map "Ff" 'mldonkey-sort-fin-filename)
(define-key mldonkey-mode-map "Fs" 'mldonkey-sort-fin-size)
(define-key mldonkey-mode-map "Fm" 'mldonkey-sort-fin-md4)

(provide 'mldonkey-vd-sort)

;;; mldonkey-vd.el ends here