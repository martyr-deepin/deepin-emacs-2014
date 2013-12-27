;;; dedicated.el --- A very simple minor mode for dedicated buffers

;; Copyright (C) 2000 Eric Crampton <eric@atdesk.com>

;; Author: Eric Crampton <eric@atdesk.com>
;; Maintainer: Eric Crampton <eric@atdesk.com>
;; Version: 1.1.0
;; Keywords: dedicated, buffer

;; This file is not part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.

;; This is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.

;;; Commentary:

;; This minor mode allows you to toggle a window's "dedicated" flag.
;; When a window is "dedicated", Emacs will not select files into that
;; window. This can be quite handy since many commands will use
;; another window to show results (e.g., compilation mode, starting
;; info, etc.) A dedicated window won't be used for such a purpose.
;;
;; Dedicated buffers will have "D" shown in the mode line.

;;; History:
;; 
;; 2003-11-12 Peter S Galbraith <psg@debian.org>
;;  V1.0.0 found on gnu.emacs.sources archives for 2000/04/12:
;;   http://groups.google.com/groups?selm=izn1mzrs60.fsf%40elmo.atdesk.com
;;  V1.1.0 made `dedicated-mode' a true toggle; added autoload tag and made
;;   minor checkdoc edits.

;;; Code:

(defvar dedicated-mode nil
  "Mode variable for dedicated minor mode.
Use the command `dedicated-mode' to toggle or set this variable.")
(make-variable-buffer-local 'dedicated-mode)

;;;###autoload
(defun dedicated-mode (&optional arg)
  "Toggle dedicated minor mode.
With ARG, turn minor mode on if ARG is positive, off otherwise."
  (interactive "P")
  (setq hs-headline nil
	dedicated-mode (if (null arg)
                           (not dedicated-mode)
                         (> (prefix-numeric-value arg) 0)))
  (set-window-dedicated-p (selected-window) dedicated-mode)
  (if (not (assq 'dedicated-mode minor-mode-alist))
      (setq minor-mode-alist
            (cons '(dedicated-mode " D")
                  minor-mode-alist))))

(provide 'dedicated)

;;; dedicated.el ends here
