;; sys-apropos.el --- Interface for the *nix apropos command.

;; Copyright (C) 2002 Henrik Enberg <henrik@enberg.org>

;; Author: Henrik Enberg <henrik@enberg.org>
;; Keywords: help, external

;; This file is not part of GNU Emacs.

;; This program is free software ; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307 USA
;;; Commentary:

;;; Commentary:

;; To install, drop it in a directory on your `load-path', and add
;; the following to your .emacs:

;;  (autoload 'sys-apropos "sys-apropos" nil t)

;; Then do `M-x sys-apropos' and you're off.  In the *System Apropos*
;; buffer, `RET' shows the manual for the program on that line and `q'
;; or `C-c C-c' quits the whole shebang.

;;; Code:

(require 'woman)

(defvar sys-apropos-line-regexp
  "^\\([a-z0-9-_]+\\)[ \t]*(\\([0-9]\\))[ \t-]+\\(.*\\)"
  "Regexp matching a line of output from the apropos command.")

;;;###autoload
(defun sys-apropos (query)
  "Ask the system apropos command for man-pages matching QUERY."
  (interactive "sApropos query: ")
  (let ((command (concat "apropos " query))
        (longest-name 0)
        (output nil))
    (with-temp-buffer
      (insert (shell-command-to-string command))
      (goto-char (point-min))
      (while (re-search-forward sys-apropos-line-regexp nil t)
        (push (list (match-string 1)
                    (match-string 2)
                    (match-string 3))
              output)
        (when (> (length (match-string 1)) longest-name)
          (setq longest-name (length (match-string 1))))
        (forward-line 1)))
    (if (not output)
        (message "%s: nothing appropriate." query)
      (let ((buffer (get-buffer-create "*System Apropos*"))
            (inhibit-read-only t))
        (pop-to-buffer buffer)
        (erase-buffer)
        (setq output (nreverse output))
        (dolist (i output)
          (let ((name (format "%s (%s)" (nth 0 i) (nth 1 i)))
                (desciption (nth 2 i))
                (max-len (+ longest-name 4))
                (pad-char ? ))
            (insert (propertize
                     (if (< (length name) max-len)
                         (concat name (make-string
                                       (- max-len (length name))
                                       pad-char))
                       name) 'face 'bold)
                    " - " desciption "\n")))
        (goto-char (point-min))
        (sys-apropos-mode)))))

(defun sys-apropos-run-woman ()
  "Show the woman page on the current line."
  (interactive)
  (let ((beg (line-beginning-position))
        (end (line-end-position))
        (line nil))
    (setq line (buffer-substring-no-properties beg end))
    (with-temp-buffer
      (insert line)
      (goto-char (point-min))
      (when (re-search-forward sys-apropos-line-regexp nil t)
        (let ((man-arg (concat (match-string 2) " " (match-string 1))))
          (switch-to-buffer-other-window (current-buffer)) ;lazycat add
          (WoMan-getpage-in-background man-arg))))))       ;lazycat modified

(defun sys-apropos-quit ()
  "Exit from the `sys-apropos' buffer."
  (interactive)
  (when (eq major-mode 'sys-apropos-mode)
    (kill-buffer (current-buffer))
    (when (/= (count-windows) 1)
      (delete-window))))

(define-derived-mode sys-apropos-mode fundamental-mode "System Apropos"
  "Major mode used in `sys-apropos' buffers.

\\{sys-apropos-mode-map}"
  (define-key sys-apropos-mode-map (kbd "RET") 'sys-apropos-run-woman)
  (define-key sys-apropos-mode-map (kbd "C-c C-c") 'sys-apropos-quit)
  (define-key sys-apropos-mode-map (kbd "q") 'sys-apropos-quit)
  (setq truncate-lines t
        buffer-read-only t))

(provide 'sys-apropos)

;;; sys-apropos.el ends here
