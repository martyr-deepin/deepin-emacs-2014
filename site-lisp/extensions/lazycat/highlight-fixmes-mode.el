;;; highlight-fixmes-mode.el --- Highlight FIXME messages

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
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; Code:


(defvar fixme-words '("FIXME" "TODO" "XXX" "THINKME")
  "Words that should be highlighted in source code to indicate work needed.")

(defvar found-fixmes-hook '()
  "A hook run after a FIXME has been highlighted.
Each hook is run with the FIXME's overlay as its argument.")

(defface fixme-face
  '((t (:foreground "orange"
                    :weight bold :box (:line-width 2 :color "orange"))))
  "The faced used to show FIXME lines")

(defun highlight-fixmes-fontify (beg end)
  (highlight-fixmes-unfontify beg end)
  (let ((regexp (regexp-opt fixme-words))
        (case-fold-search nil))
    (save-excursion
      (goto-char beg)
      (while (search-forward-regexp regexp end t)
        (let ((fixme (make-overlay (match-beginning 0) (match-end 0))))
          (overlay-put fixme 'type 'fixme)
          (overlay-put fixme 'evaporate t)
          (overlay-put fixme 'face 'fixme-face)
          (run-hook-with-args 'found-fixmes-hook fixme))))))

(defun highlight-fixmes-unfontify (beg end)
  (mapc #'(lambda (o)
            (when (eq (overlay-get o 'type) 'fixme)
              (delete-overlay o)))
        (overlays-in beg end)))

(define-minor-mode highlight-fixmes-mode
  "Highlight words like FIXME, TODO, etc."
  nil " fixme" nil
  (cond ((not highlight-fixmes-mode)
         (jit-lock-unregister 'highlight-fixmes-fontify)
         (highlight-fixmes-unfontify (point-min) (point-max)))
        (t (highlight-fixmes-fontify (point-min) (point-max))
           (jit-lock-register 'highlight-fixmes-fontify))))


(provide 'highlight-fixmes-mode)
;;; highlight-fixmes-mode.el ends here
