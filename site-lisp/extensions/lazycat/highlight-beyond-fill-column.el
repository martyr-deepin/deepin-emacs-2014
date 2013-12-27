;;; highlight-beyond-fill-column.el --- fontify beyond the fill-column.

;; Copyright (C) 1985, 1986, 1987, 1992 Free Software Foundation, Inc.
;; Copyright (C) 2003 Peter S Galbraith <psg@debian.org>

;; Author: Sandip Chitale (sandip.chitale@blazesoft.com)
;; Keywords: programming decipline convenience

;; Keywords:
;; Time-stamp: Aug 23 2001  8:56 PM Pacific Daylight Time
;; Version: 1.2

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;; 
;; This defines a function that can be used by `font-lock-add-keywords' to
;; find the columns that are beyond `fill-column'.  It does not currently
;; work in XEmacs because it lacks the funcyom `font-lock-add-keywords'.
;;
;; Installation:
;; Put the following in your .emacs
;;
;; (require 'highlight-beyond-fill-column)
;;
;; Example usage:
;;
;;   Enable it on a buffer using `M-x highlight-beyond-fill-column.
;;   You may use that command in a hook (e.g. text-mode-hook)
;;
;;   Customize the `highlight-beyond-fill-column-face' variable to
;;   to setup the face used for highlight-beyond-fill-column
;;
;; Acknowledgement:
;;
;; This is based on initial code provided by Jim Janney (jjanney@xmission.com)

;;; History:
;; 
;; V1.2 2003-09-12 by Peter S Galbraith <psg@debian.org>
;; - Made checkdoc clean and fixed indentation and parentheses placement.
;; - Added defgroup; used defface.
;; - Removed `highlight-beyond-fill-column-in-modes' since it didn't work
;;   anymore.
;; - Created `highlight-beyond-fill-column' to use on a single buffer or as
;;   a hook.

;;; Code:
(defgroup highlight-beyond-fill-column nil
  "Fontify beyond the fill-column."
  :group 'fill)

(defface highlight-beyond-fill-column-face
  '((t (:underline t)))
  "Face used to highlight beyond the fill-column."
  :group 'highlight-current-line)

(defun highlight-beyond-fill-column-lock (limit)
  "Function for font-lock to highlight beyond the `fill-column' until LIMIT."
  (let ((original-point (point)))       ;; remember the point
    ;; if already past the fill column start on next line
    (if (> (current-column) fill-column)
        (forward-line 1))
    (while (and (< (point) limit)       ; still within limit
                ;; the line has less than `fill-column' columns
                (or (< (move-to-column fill-column) fill-column)
                    (= (point) (line-end-position)))) ; end of line
      ;; goto next line
      (forward-line 1))

    (if (>= (point) limit)              ; beyond limit
        (progn
          (goto-char original-point)    ; restore point
          nil)                          ; return nil

      (set-match-data (list (point-marker) ; set match data
                            (progn
                              (end-of-line)
                              (point-marker))))
      ;; return t indicating that the match data was set
      t)))

;;;###autoload
(defun highlight-beyond-fill-column ()
  "Setup this buffer to highlight beyond the `fill-column'."
  (interactive)
  (font-lock-add-keywords
   nil
   '((highlight-beyond-fill-column-lock 0 'highlight-beyond-fill-column-face
                                        prepend))))

(provide 'highlight-beyond-fill-column)

;;; highlight-beyond-fill-column.el ends here
