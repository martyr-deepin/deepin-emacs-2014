;;; @(#) joc-toggle-buffer.el --- flips back and forth between two buffers

;; Copyright (C) 2001 by Joseph L. Casadonte Jr.

;; Author:          Joe Casadonte (emacs@northbound-train.com)
;; Maintainer:      Joe Casadonte (emacs@northbound-train.com)
;; Created:         January 26, 2001
;; Keywords:        toggle buffer
;; Latest Version:  http://www.northbound-train.com/emacs.html

;; This file is not part of Emacs

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
;;  This package provides a way to toggle back and forth between the
;;  last two active buffers, without any extra keystrokes (like
;;  accepting the default argument to `switch-to-buffer').

;;; Nut & Bolts:
;;
;;  This package works by advising `switch-to-buffer', so if your
;;  favorite buffer switching command does not ultimately call
;;  `switch-to-buffer', this won't work.  Packages that alter the
;;  current buffer *before* `switch-to-buffer' is called will also not
;;  work properly.  Both of these situations may be salvagable with
;;  the addition of more advice.  In the first case, just write a bit
;;  of advice which essentially duplicates what I'm doing here with
;;  `switch-to-buffer'.
;;
;;  I've provided a hack (and a "hook") to help with the second
;;  situation.  The hack is to define a second variable (the "hook")
;;  before the list is altered.  Once `switch-to-buffer' is called,
;;  the advice provided in this package will first look for this
;;  hook/hack variable and use its value; if that's not found, it will
;;  use the value returned by `buffer-name'.
;;
;;  An example of this is the `swbuff' package, which changes the
;;  current buffer before switching to the next one (though I'm not
;;  sure why it does this).  Since I use swbuff, I've included its
;;  hack along with this package.  You can customize whether or not
;;  this hack is loaded (see Customization below).

;;; Installation:
;;
;;  Put this file on your Emacs-Lisp load path and add the following to your
;;  ~/.emacs startup file
;;
;;     (require 'joc-toggle-buffer)

;;; Usage:
;;
;;  M-x `joc-toggle-buffer'
;;     Switched to the previous active buffer (when `switch-to-buffer' was
;;     called).  If there is no previous buffer, or if the buffer no longer
;;     exists, a message will be displayed in the minibuffer.

;;; Customization:
;;
;;  M-x `joc-toggle-buffer-customize' to customize all package options.
;;
;;  The following variables can be customized:
;;
;;  o `joc-toggle-buffer-swbuff-advice'
;;     A hack to be compatable with the swbuff package.
;;
;;     Valid values are:
;;       o Never Advise - never advise the swbuff functions [nil]
;;       o Advise if Provided - only advise if swbuff already provided [P]
;;       o Always Advise - always define & activate the swbuff advise [A]
;;
;;     If you don't use the swbuff package, you can safely choose
;;     Never Advise or Advise if Provided.  If you do use swbuff, you
;;     may use Advise if Provided (in which case swbuff must be
;;     `provide'd already) or Always Advise."

;;; To Do:
;;
;;  o Nothing, at the moment.

;;; Comments:
;;
;;  Any comments, suggestions, bug reports or upgrade requests are welcome.
;;  Please send them to Joe Casadonte (emacs@northbound-train.com).
;;
;;  This version of joc-toggle-buffer was developed and tested with NTEmacs 20.5.1
;;  and 2.7 under Windows NT 4.0 SP6 and Emacs 20.7.1 under Linux (RH7).
;;  Please, let me know if it works with other OS and versions of Emacs.

;;; Change Log:
;;
;;  see http://www.northbound-train.com/emacs/toggle-buffer.log
;;
;;  2003-11-23 Peter S Galbraith <psg@debian.org>
;;   This version, distributed in the Debian package `emacs-goodies-el',
;;   was renamed from toggle-buffer.el to joc-toggle-buffer.el.  The prefix
;;   was also added to a few variables.

;;; **************************************************************************
;;; **************************************************************************
;;; **************************************************************************
;;; **************************************************************************
;;; **************************************************************************
;;; Code:

(eval-when-compile
  (defvar byte-compile-dynamic nil) ; silence the old byte-compiler
  (set (make-local-variable 'byte-compile-dynamic) t))

;;; **************************************************************************
;;; ***** customization routines
;;; **************************************************************************
(defgroup joc-toggle-buffer nil
  "toggle-buffer package customization"
  :group 'tools)

;; ---------------------------------------------------------------------------
(defun joc-toggle-buffer-customize ()
  "Customization of the group `joc-toggle-buffer'."
  (interactive)
  (customize-group "joc-toggle-buffer"))

;; ---------------------------------------------------------------------------
(defcustom joc-toggle-buffer-swbuff-advice "P"
  "A hack to be compatable with the swbuff package.

Valid values are:
  o Never Advise - never advise the swbuff functions [nil]
  o Advise if Provided - only advise if swbuff already provided [P]
  o Always Advise - always define & activate the swbuff advise [A]

If you don't use the swbuff package, you can safely choose
Never Advise or Advise if Provided.  If you do use swbuff, you
may use Advise if Provided (in which case swbuff must be
`provide'd already) or Always Advise."
  :type `(choice
	  (const :tag "Never Advise" nil)
	  (const :tag "Advise if Provided" "P")
	  (const :tag "Always Advise" "A"))
  :group 'joc-toggle-buffer)

;; ---------------------------------------------------------------------------
(defcustom joc-toggle-buffer-load-hook nil
  "Hook to run when package is loaded."
  :type 'hook
  :group 'joc-toggle-buffer)

;;; **************************************************************************
;;; ***** version related routines
;;; **************************************************************************
(defconst joc-toggle-buffer-version
  "$Revision: 1.2 $"
  "Version number for toggle-buffer package.")

;; ---------------------------------------------------------------------------
(defun joc-toggle-buffer-version-number ()
  "Return `toggle-buffer' version number."
  (string-match "[0123456789.]+" joc-toggle-buffer-version)
  (match-string 0 joc-toggle-buffer-version))

;; ---------------------------------------------------------------------------
(defun joc-toggle-buffer-display-version ()
  "Display `toggle-buffer' version."
  (interactive)
  (message "toggle-buffer version <%s>." (joc-toggle-buffer-version-number)))

;;; **************************************************************************
;;; ***** interactive functions
;;; **************************************************************************
(defvar joc-toggle-buffer-last-buffer nil
  "Contains the name of the previous buffer.")

;;;###autoload
(defun joc-toggle-buffer ()
  "Switch to previous active buffer."
  (interactive)
  (if (or (not (boundp 'joc-toggle-buffer-last-buffer))
          (not joc-toggle-buffer-last-buffer))
	  (error "No previous buffer to switch to (yet)"))
  (let ((buff (get-buffer joc-toggle-buffer-last-buffer)))
	(if (not buff)
		(error "Invalid buffer \"%s\"" joc-toggle-buffer-last-buffer)
	  (switch-to-buffer buff))))

;;; **************************************************************************
;;; ***** normal advice
;;; **************************************************************************
(defadvice switch-to-buffer
  (before joc-toggle-buffer-setup-advice act)
  "Records active buffer (for possible later recall) before it's switched."
  (if (boundp 'joc-toggle-buffer-hack)
	  (setq joc-toggle-buffer-last-buffer joc-toggle-buffer-hack)
	(setq joc-toggle-buffer-last-buffer (buffer-name))))

;;; **************************************************************************
;;; ***** swbuff-specific advice
;;; **************************************************************************
(let ((advise-swbuff-fns nil))
  (if joc-toggle-buffer-swbuff-advice
	  (if (eq joc-toggle-buffer-swbuff-advice "P")
		  (if (featurep 'swbuff)
			  (setq advise-swbuff-fns t))
		(setq advise-swbuff-fns t)))

  (if advise-swbuff-fns
	  (progn
		(defadvice swbuff-switch-to-next-buffer
		  (around joc-toggle-buffer-swbuf-next-advice act)
		  "hack for swbuff-users"
		  (setq joc-toggle-buffer-hack (buffer-name))
		  ad-do-it
		  (makunbound 'joc-toggle-buffer-hack))

		(defadvice swbuff-switch-to-previous-buffer
		  (around joc-toggle-buffer-swbuf-prev-advice act)
		  "hack for swbuff-users"
		  (setq joc-toggle-buffer-hack (buffer-name))
		  ad-do-it
		  (makunbound 'joc-toggle-buffer-hack))
		)))

;;; **************************************************************************
;;; ***** we're done
;;; **************************************************************************
(run-hooks 'joc-toggle-buffer-load-hook)

(provide 'joc-toggle-buffer)

;;; toggle-buffer.el ends here
