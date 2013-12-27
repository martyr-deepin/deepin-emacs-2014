;;; ascii-table.el --- display ASCII table

;; Copyright (C) 2001, 2004 Noah S. Friedman

;; Author: Noah Friedman <friedman@splode.com>
;; Maintainer: friedman@splode.com
;; Created: 2001-09-21

;; $Id: ascii-table.el,v 1.3 2004/05/11 09:21:32 friedman Exp $

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 59 Temple Place, Suite 330; Boston, MA 02111-1307, USA.

;;; Commentary:
;;; Code:

(defgroup ascii-table nil
  "Display table of ASCII and 8-bit characters."
  :group 'tools
  :group 'extensions)

;;;###autoload
(defcustom ascii-table-max-columns 3
  "Maximum number of columns to use in rendering ascii table.
The table may actually render with fewer columns if the width of the
selected window is too narrow."
  :type 'integer
  :group 'ascii-table)

;;;###autoload
(defcustom ascii-table-intercolumn-space 8
  "Amount of space to insert between columns."
  :type 'integer
  :group 'ascii-table)

;;;###autoload
(defun ascii-table-display (&optional binary)
  "Display a table of ASCII characters and their numeric equivalents.

By default the character keycode; display character (if any); and numeric
ASCII value in octal, decimal, and hexadecimal are displayed.  With a
prefix argument, the binary representation is also displayed.

This function actually displays 8-bit characters too, which are not part of ASCII.
However, they are used in numerous unibyte character sets such as ISO-8859-1 and will
render using the display table in effect."
  (interactive "P")
  (let* ((map-data (ascii-table-make-map binary))
         (widths (car map-data))
         (fmtstr (ascii-table-make-fmtstr widths))
         (header (apply 'format fmtstr (nth 1 map-data)))
         (map (nthcdr 2 map-data))
         (col-sep (make-string ascii-table-intercolumn-space ?\x20))
         (colwidth (length header))
         (columns (min ascii-table-max-columns
                       (/ (+ (window-width) (length col-sep))
                          (+ colwidth (length col-sep)))))
         (rows-per-column (/ (length map) columns))
         (row 0)
         (column 0)
         (buf (get-buffer-create "*Ascii Table*")))
    (save-excursion
      (set-buffer buf)
      (setq buffer-read-only nil)
      (erase-buffer)
      (while map
        (end-of-line)
        (cond ((= row 0)
               (if (= column 0)
                   (insert header "\n")
                 (insert col-sep header)
                 (forward-char)
                 (end-of-line))))

        (if (= column 0)
            (insert (apply 'format fmtstr (car map)) "\n")
          (insert col-sep (apply 'format fmtstr (car map)))
          (forward-char))

        (setq row (1+ row))
        (cond ((> row rows-per-column)
               (goto-char (point-min))
               (setq row 0)
               (setq column (1+ column))))

        (setq map (cdr map)))
      (setq buffer-read-only t)
      (goto-char (point-min)))
    (display-buffer buf)))

(defun ascii-table-make-map (&optional binary disp-table)
  (setq disp-table (ascii-table-window-display-table disp-table))
  (let ((i 255)
        c kc dc
        (kc-len 4)
        (dc-len 4)
        (map nil)
        elt)
    (while (>= i 0)
      (setq c (ascii-table-int-to-char i))
      (setq kc     (key-description (char-to-string c))
            kc-len (max kc-len (length kc)))
      (setq dc     (ascii-table-char-display c disp-table)
            dc-len (max dc-len (length dc)))

      (setq elt (list kc dc
                      (format "%3o" i)
                      (format "%3d" i)
                      (format "%3x" i)))
      (if binary
          (setcdr (nthcdr 1 elt)
                  (cons (ascii-table-char-to-base2-string i)
                        (nthcdr 2 elt))))
      (setq map (cons elt map))
      (setq i (1- i)))
    (nconc (mapcar (lambda (elt)
                     (or binary
                         (setcdr (nthcdr 1 elt) (nthcdr 3 elt)))
                     elt)
                   (list (list kc-len dc-len 8 3 3 3)
                         (list "Char" "Disp" "Bin" "Oct" "Dec" "Hex")))
           map)))

(defun ascii-table-window-display-table (&optional disp-table)
  (or disp-table
      (and (fboundp 'window-display-table)
           (window-display-table (selected-window)))
      (and (boundp 'buffer-display-table) buffer-display-table)
      (and (boundp 'standard-display-table) standard-display-table)
      ;; xemacs
      (and (boundp 'current-display-table)
           (fboundp 'specifier-instance )
           (specifier-instance current-display-table (selected-window)))))

(defun ascii-table-char-display (c &optional disp-table)
  (let ((v (and disp-table (aref disp-table c))))
    (cond ((and v (stringp v)) v)
          (v
           ;; If chars in the vector are multibyte, it's a glyph that's
           ;; already displayed properly; in fact unless
           ;; enable-multibyte-characters is t here, char-to-string will
           ;; not render the characters correctly anyway.
           (let* ((unibyte t)
                  (result (mapconcat
                           (lambda (c)
                             (if (> c 255) (setq unibyte nil))
                             (char-to-string c))
                           v "")))
             (if unibyte result (char-to-string c))))
          ((< c 32)
           (format "^%c" (+ c 64)))
          ((= c 127)
           "^?")
          ((< c 127) (char-to-string c))
          (t ""))))

(defun ascii-table-char-to-base2-string (c)
  (let ((s (make-string 8 ?0))
        (i 7))
    (while (>= i 0)
      (aset s i (+ ?0 (logand c 1)))
      (setq i (1- i))
      (setq c (lsh c -1)))
    s))

(defun ascii-table-make-fmtstr (widths)
  (mapconcat (lambda (w) (format "%%%ds" w)) widths " "))

(defalias 'ascii-table-int-to-char
  (if (fboundp 'int-to-char)
      'int-to-char
    'identity))

(provide 'ascii-table)

;;; ascii-table.el ends here
