;;; @(#) bar-cursor.el -- package used to switch block cursor to a bar
;;; @(#) $Id: bar-cursor.el,v 1.1 2003-04-04 20:15:55 lolando Exp $

;; This file is not part of Emacs

;; Copyright (C) 2001, 2003 by Joseph L. Casadonte Jr.
;; Author:          Joe Casadonte (emacs@northbound-train.com)
;; Maintainer:      Joe Casadonte (emacs@northbound-train.com)
;; Created:         July 1, 2001
;; Keywords:        bar cursor overwrite
;; Latest Version:  http://www.northbound-train.com/emacs.html

;; COPYRIGHT NOTICE

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;;  Simple package to convert the block cursor into a bar cursor.  In
;;  overwrite mode, the bar cursor changes back into a block cursor.
;;  This is a quasi-minor mode, meaning that it can be turned on & off
;;  easily though only globally (hence the quasi-)

;;; Installation:
;;
;;  Put this file on your Emacs-Lisp load path and add the following to
;;  your ~/.emacs startup file
;;
;;     (require 'bar-cursor)
;;     (bar-cursor-mode 1)
;;
;;  To add a directory to your load-path, use something like the following:
;;
;;     (add-to-list 'load-path (expand-file-name "/some/load/path"))

;;; Usage:
;;
;;  M-x `bar-cursor-mode'
;;      Toggles bar-cursor-mode on & off.  Optional arg turns
;;      bar-cursor-mode on if arg is a positive integer.
;;
;;  You may also use the custom interface to enable or disable it:
;;
;;  M-x customize-variable [RET] bar-cursor-mode [RET]

;;; To Do:
;;
;;  o Nothing, at the moment.

;;; Credits:
;;
;;  The basis for this code comes from Steve Kemp by way of the
;;  NTEmacs mailing list.
;;
;;  Peter S. Galbraith <psg@debian.org> contributed a patch making
;;  bar-cursor-mode customizable.

;;; Comments:
;;
;;  Any comments, suggestions, bug reports or upgrade requests are welcome.
;;  Please send them to Joe Casadonte (emacs@northbound-train.com).
;;
;;  This version of bar-cursor was developed and tested with NTEmacs
;;  20.7.1 under Windows 2000 & NT 4.0 and Emacs 20.7.1 under Linux
;;  (RH7).  Please, let me know if it works with other OS and versions
;;  of Emacs.

;;; Change Log:
;;
;;  see http://www.northbound-train.com/emacs/bar-cursor.log

;;; **************************************************************************
;;; **************************************************************************
;;; **************************************************************************
;;; **************************************************************************
;;; **************************************************************************
;;; Code:

(eval-when-compile
  ;; silence the old byte-compiler
  (defvar byte-compile-dynamic nil)
  (set (make-local-variable 'byte-compile-dynamic) t)
  (require 'advice)
  (defvar bar-cursor-mode))

;;; **************************************************************************
;;; ***** version related routines
;;; **************************************************************************
(defconst bar-cursor-version
  "$Revision: 1.1 $"
  "Version number for 'bar-cursor' package.")

;; ---------------------------------------------------------------------------
(defun bar-cursor-version-number ()
  "Return 'bar-cursor' version number."
  (string-match "[0123456789.]+" bar-cursor-version)
  (match-string 0 bar-cursor-version))

;; ---------------------------------------------------------------------------
(defun bar-cursor-display-version ()
  "Display 'bar-cursor' version."
  (interactive)
  (message "bar-cursor version <%s>." (bar-cursor-version-number)))

;;; **************************************************************************
;;; ***** real functions
;;; **************************************************************************

;;;###autoload
(defun bar-cursor-mode (&optional arg)
  "Toggle use of variable `bar-cursor-mode'.
This quasi-minor mode changes cursor to a bar cursor in insert mode,
and a block cursor in overwrite mode.  It may only be turned on and
off globally, not on a per-buffer basis (hence the quasi- designation).

Optional ARG turns mode on if ARG is a positive integer."
  (interactive "P")

  ;; toggle on and off
  (let ((old-mode bar-cursor-mode))
    (setq bar-cursor-mode
          (if arg (or (listp arg)
                      (> (prefix-numeric-value arg) 0))
            (not bar-cursor-mode)))
    
    (when (not (equal old-mode bar-cursor-mode))
      (bar-cursor-change))))

;;;###autoload
(defun bar-cursor-change ()
  "Enable or disable advice based on value of variable `bar-cursor-mode'."
  (if bar-cursor-mode
      (ad-enable-advice 'overwrite-mode 'after 'bar-cursor-overwrite-mode-ad)
    (ad-disable-advice 'overwrite-mode 'after 'bar-cursor-overwrite-mode-ad))

  (ad-activate 'overwrite-mode)

  ;; set the initial cursor type now
  (bar-cursor-set-cursor)

  ;; add or remove to frame hook
  (if bar-cursor-mode
      (add-hook 'after-make-frame-functions 'bar-cursor-set-cursor)
    (remove-hook 'after-make-frame-functions 'bar-cursor-set-cursor)))

;;;--------------------------------------------------------------------------
(defadvice overwrite-mode (after bar-cursor-overwrite-mode-ad disable)
  "Advice that controls what type of cursor is displayed."
  (bar-cursor-set-cursor))

;;;--------------------------------------------------------------------------
(defun bar-cursor-set-cursor-type (cursor &optional frame)
  "Set the `cursor-type' for the named frame.

CURSOR is the name of the cursor to use (bar or block -- any others?).
FRAME is optional frame to set the cursor for; current frame is used
if not passed in."
  (interactive)
  (if (not frame)
	  (setq frame (selected-frame)))

  ;; Do the modification.
  (modify-frame-parameters frame
	(list (cons 'cursor-type cursor))))

;;; --------------------------------------------------------------------------
(defun bar-cursor-set-cursor (&optional frame)
  "Set the `cursor-type' according to the insertion mode.

FRAME is optional frame to set the cursor for; current frame is used
if not passed in."
  (if (and bar-cursor-mode (not overwrite-mode))
	  (bar-cursor-set-cursor-type 'bar frame)
	(bar-cursor-set-cursor-type 'box frame)))

;;; --------------------------------------------------------------------------
(defgroup bar-cursor nil
  "switch block cursor to a bar."
  :group 'convenience)

(defcustom bar-cursor-mode nil
  "*Non-nil means to convert the block cursor into a bar cursor.
In overwrite mode, the bar cursor changes back into a block cursor.
This is a quasi-minor mode, meaning that it can be turned on & off easily
though only globally (hence the quasi-)"
  :type 'boolean
  :group 'bar-cursor
  :require 'bar-cursor
  :set (lambda (symbol value)
         (set-default symbol value)
         (bar-cursor-change)))

;;; **************************************************************************
;;; ***** we're done
;;; **************************************************************************
(provide 'bar-cursor)

;;; bar-cursor.el ends here
;;; **************************************************************************
;;;; *****  EOF  *****  EOF  *****  EOF  *****  EOF  *****  EOF  *************
