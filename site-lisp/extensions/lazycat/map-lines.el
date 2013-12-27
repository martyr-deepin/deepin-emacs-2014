;;; map-lines.el --- Map a command over many lines

;; Copyright (C) 2002  Andreas Fuchs <asf@void.at>

;; Author: Andreas Fuchs <asf@void.at>
;; Keywords: matching, files

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

;; This module allows you to map a command over a set of lines
;; matching a regex. The trick: You can then go ahead and insert these
;; lines in one clean yank.
;; <flamebait>Emacs can now be called ED, THE STANDARD TEXT EDITOR
;; </flamebait>
;;
;; To use this module, put into your ~/.emacs:
;; (autoload 'map-lines "map-lines"
;;           "Map COMMAND over lines matching REGEX."
;;           t)
;; ;; And, if you're feeling like you never need faces anyway:
;; (global-set-key "\M-g" 'map-lines)
;;
;; This is Version 0.1 of map-lines.el.
;;
;; You can find the latest version of this module at:
;; http://asf.void.at/emacs/map-lines.el
;;
;;; Code:

(defvar mapl-command-alist
  '((?k . mapl-kill-line)
    (?c . mapl-copy-line)
    (?o . mapl-other-command))
  "An alist of command-char->command-name mappings.")


(defun mapl-lookup-command (command-char)
  "Return the matching command for COMMAND-CHAR."
  (let ((command (cdr (assq command-char mapl-command-alist))))
    (if (eq command 'mapl-other-command)
        (read-command "Other command (takes no args and returns a string): ")
      command)))

(defun map-lines (command-c regex)
  "Map COMMAND over lines matching REGEX."
  (interactive "cCommand (Kill, Copy, Other) [kco]:
sRegular Expression: ")
  (save-excursion
    (let ((command (mapl-lookup-command command-c))
          (live-buffer (current-buffer)))
      (with-temp-buffer
        (let ((temp-buffer (current-buffer)))
          (with-current-buffer live-buffer
            (goto-char (point-min))
            (while (re-search-forward regex nil t)
              (let ((the-line (funcall command)))
                (with-current-buffer temp-buffer
                  (insert the-line)))
              (end-of-line)))
          (kill-region (point-min) (point-max)))))))

(defun mapl-kill-line ()
  "Kill a line entirely and return it."
  (mapl-kill-universal 'kill-line))

(defun mapl-copy-line ()
  "Copy a line entirely and return it."
  (mapl-kill-universal (lambda ()
                         (copy-region-as-kill (progn (beginning-of-line)
                                                     (point))
                                              (progn (end-of-line)
                                                     (goto-char (+ 1 (point)))
                                                     (point))))))

(defun mapl-kill-universal (kill-fun)
  "Execute KILL-FUN on an entire line."
  (beginning-of-line)
  (funcall kill-fun)
  (prog1 (car kill-ring)
    (setq kill-ring (cdr kill-ring))))

(provide 'map-lines)
;;; map-lines.el ends here
