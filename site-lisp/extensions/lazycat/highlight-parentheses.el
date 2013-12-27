;;; highlight-parentheses.el --- highlight surrounding parentheses
;;
;; Copyright (C) 2007 Nikolaj Schumacher
;;
;; Author: Nikolaj Schumacher <bugs * nschum de>
;; Version: 1.0
;; Keywords: faces, matching
;; URL: http://nschum.de/src/emacs/highlight-parentheses/
;; Compatibility: GNU Emacs 22.x
;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
;;
;;; Commentary:
;;
;; Add the following to your .emacs file:
;; (require 'highlight-parentheses)
;;
;; Enable `highlight-symbol-mode'.
;;
;;; Changes Log:
;;
;; 2007-07-30 (1.0)
;;    Added background highlighting and faces.
;;
;; 2007-05-15 (0.9.1)
;;    Support for defcustom.  Changed from vector to list.
;;
;; 2007-04-26 (0.9)
;;    Initial Release.
;;
;;; Code:

(eval-when-compile (require 'cl))

(defgroup highlight-parentheses nil
  "Highlight surrounding parentheses"
  :group 'faces
  :group 'matching)

(defvar hl-paren-overlays nil
  "This buffers currently active overlays.")
(make-variable-buffer-local 'hl-paren-overlays)

(defcustom hl-paren-colors
  '("firebrick1" "IndianRed4" "IndianRed")
  "*List of colors for the highlighted parentheses.
The list starts with the the inside parentheses and moves outwards."
  :type '(repeat color)
  :group 'highlight-parentheses)

(defcustom hl-paren-background-colors nil
  "*List of colors for the background highlighted parentheses.
The list starts with the the inside parentheses and moves outwards."
  :type '(repeat color)
  :group 'highlight-parentheses)

(defface hl-paren-face nil
  "*Face used for highlighting parentheses.
Color attributes might be overriden by `hl-paren-colors' and
`hl-paren-background-colors'."
  :group 'highlight-parentheses)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar hl-paren-last-point 0
  "The last point for which parentheses were highlighted.
This is used to prevent analyzing the same context over and over.")
(make-variable-buffer-local 'hl-paren-last-point)

(defun hl-paren-highlight ()
  "Highlight the parentheses around point."
  (unless (= (point) hl-paren-last-point)
    (save-excursion
      (let ((pos (point))
            (match-pos (point))
            (level -1)
            (max (1- (length hl-paren-overlays))))
        (while (and match-pos (< level max))
          (setq match-pos
                (when (setq pos (cadr (syntax-ppss pos)))
                  (ignore-errors (scan-sexps pos 1))))
          (when match-pos
            (hl-paren-put-overlay (incf level) pos 'hl-paren-face)
            (hl-paren-put-overlay (incf level) (1- match-pos) 'hl-paren-face)))
        (while (< level max)
          (hl-paren-put-overlay (incf level) nil nil))))
    (setq hl-paren-last-point (point))))

(defun hl-paren-put-overlay (n pos face)
  "Move or create the N'th overlay so its shown at POS."
  (let ((ov (elt hl-paren-overlays n)) end)
    (if (null pos)
        (when ov
          (delete-overlay ov)
          (aset hl-paren-overlays n nil))
      (if (atom pos)
          (setq end (1+ pos))
        (setq end (cdr pos))
        (setq pos (car pos)))
      (if ov
          (move-overlay ov pos end)
        (let ((face-attributes (face-attr-construct face))
              (color-value (nth (/ n 2) hl-paren-colors))
              (background-value (nth (/ n 2) hl-paren-background-colors)))
          (when color-value
            (let ((attribute (memq :foreground face-attributes)))
              (if attribute
                  (setcar (cdr attribute) color-value)
                (push color-value face-attributes)
                (push :foreground face-attributes))))
          (when background-value
            (let ((attribute (memq :background face-attributes)))
              (if attribute
                  (setcar (cdr attribute) background-value)
                (push background-value face-attributes)
                (push :background face-attributes))))
          (setq ov (make-overlay pos end))
          (aset hl-paren-overlays n ov)
          (overlay-put ov 'face face-attributes))))))

;;;###autoload
(define-minor-mode highlight-parentheses-mode
  "Minor mode to highlight the surrounding parentheses."
  nil " hl-p" nil
  (if highlight-parentheses-mode
      (progn
        (setq hl-paren-overlays
              (make-vector (* 2 (max (length hl-paren-colors)
                                     (length hl-paren-background-colors))) nil))
        (add-hook 'post-command-hook 'hl-paren-highlight nil t))
    (let (ov)
      (dotimes (i (length hl-paren-overlays))
        (when (setq ov (elt hl-paren-overlays i))
          (delete-overlay ov))))
    (kill-local-variable 'hl-paren-overlays)
    (kill-local-variable 'hl-paren-point)
    (remove-hook 'post-command-hook 'hl-paren-highlight t)))

(provide 'highlight-parentheses)

;;; highlight-parentheses.el ends here
