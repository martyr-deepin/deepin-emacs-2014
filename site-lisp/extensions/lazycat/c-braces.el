;;; c-braces.el --- C-style brace handling

;; Copyright (C) 2006  Mark Triggs

;; Author: Mark Triggs <mst@dishevelled.net>

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;


;;; Code:


(defun c-braces-backward-list-maybe ()
  (let ((start (point))
        (line (current-line-number)))
    (condition-case err
        (progn (backward-list)
               (unless (and (looking-at "(")
                            (= line (current-line-number)))
                 (goto-char start)))
      (error nil))))

(defun c-braces-insert-indented (string)
  (let ((start (point)))
    (insert string)
    (indent-region start (point) nil)))

(defun region-beginning-no-whitespace ()
  (let ((beg (region-beginning)))
    (while (eql (char-after beg) (string-to-char "\n"))
      (incf beg))
    beg))

(defun region-end-no-whitespace ()
  (let ((beg (region-end)))
    (while (eql (char-before beg) (string-to-char "\n"))
      (decf beg))
    beg))


(defmacro define-c-braces-mode (prefix
                                top-level-predicate
                                block-level-predicate)

  `(progn
     (defun ,(intern (format "%s-insert-braces" prefix)) ()
       (interactive)
       (cond (mark-active
              (let ((beg (region-beginning))
                    (end (region-end)))
                (while (string-match "[\s\n]" (string (char-before end)))
                  (decf end))
                (let ((text (delete-and-extract-region beg end)))
                  (goto-char beg)
                  (,(intern (format "%s-do-insert-braces" prefix)))
                  (push-mark)
                  (setq text
                        (subseq text (position-if
                                      (lambda (char)
                                        (not (string= (string char)
                                                      "\n")))
                                      text)))
                  (insert text)
                  (save-excursion
                    (flet ((indent ()
                                   (beginning-of-line)
                                   (unless (looking-at "^ *$")
                                     (insert " ")
                                     (indent-according-to-mode))))
                      (let ((end (point-marker)))
                        (indent)
                        (goto-char beg)
                        (while (< (point) (marker-position end))
                          (next-line 1)
                          (indent))))))))
             (t (,(intern (format "%s-do-insert-braces" prefix))))))
     (defun ,(intern (format "%s-do-insert-braces" prefix)) ()
       (interactive)
       (cond ((funcall ',block-level-predicate)
              (just-one-space)
              (insert "{")
              (newline-and-indent)
              (save-excursion
                (newline)
                (c-braces-insert-indented "}")))
             ((funcall ',top-level-predicate)
              (end-of-line)
              (delete-horizontal-space)
              (unless (looking-back "\n")
                (newline))
              (c-braces-insert-indented " {")
              (newline-and-indent)
              (save-excursion
                (newline)
                (c-braces-insert-indented "}\n")))
             (t (insert "{}")
                (forward-char -1))))
     (defun ,(intern (format "%s-closing-brace" prefix)) ()
       (interactive)
       (condition-case err
           (progn (up-list)
                  (indent-according-to-mode)
                  (save-excursion
                    (beginning-of-line)
                    (when (looking-at "[\t ]*}$")
                      (forward-line -1)
                      (while (looking-at "^[\t ]*$")
                        (delete-blank-lines)))))
         (scan-error (save-excursion
                       (goto-char (nth 2 err))
                       (insert "\n}\n")
                       (forward-line -1)
                       (indent-according-to-mode)))))))
(provide 'c-braces)
;;; c-braces.el ends here
