;;; tail.el --- Tail files within Emacs

;; Copyright (C) 1989, 1990, 1994, 1998 Free Software Foundation, Inc.
;; (For appt.el code)
;; Copyright (C) 2000 Benjamin Drieu

;; Author: Benjamin Drieu <bdrieu@april.org>
;; Keywords: tools

;; This file is NOT part of GNU Emacs.

;; This program as GNU Emacs are free software; you can redistribute
;; them and/or modify them under the terms of the GNU General Public
;; License as published by the Free Software Foundation; either
;; version 2, or (at your option) any later version.

;; They are distributed in the hope that they will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with them; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;;  $Id: tail.el,v 1.3 2003/10/10 01:46:49 psg Exp $

;;; Commentary:

;;  This program displays ``tailed'' contents of files inside transients
;;  windows of Emacs.  It is primarily meant to keep an eye on logs within
;;  Emacs instead of using additional terminals.

;;  Historical URL for tail.el is
;;    http://inferno.cs.univ-paris8.fr/~drieu/emacs/
;;  Active developement URL is
;;    http://cvs.alioth.debian.org/cgi-bin/cvsweb.cgi/emacs-goodies-el/elisp/emacs-goodies-el/?cvsroot=pkg-goodies-el

;;; History:
;;
;;  2003-10-09 Peter S Galbraith <psg@debian.org>
;;   - minor checkdoc-suggested changes.
;;   - tail-hide-window: Bug fix. Would kill all but one window when more than
;;      one window was visible prior to the tail window being displayed.
;;      copied code from appt.el appt-delete-window.
;;   - Fix boolean defcustoms.
;;   - Make it work on XEmacs (only briefly tested).

;;; Code:

;;  Custom variables (may be set by the user)

(defgroup tail nil
  "Tail files or commands into Emacs buffers."
  :prefix "tail-"
  :group 'environment)

(defcustom tail-volatile t
  "Whether to erase previous output."
  :type 'boolean
  :group 'tail)

(defcustom tail-audible nil
  "Whether to produce a bell when some output is displayed."
  :type 'boolean
  :group 'tail)

(defcustom tail-raise nil
  "Whether to raise current frame when displaying (could be *very* annoying)."
  :type 'boolean
  :group 'tail)

(defcustom tail-hide-delay 5
  "Time in seconds before a tail window is deleted."
  :type 'integer
  :group 'tail)

(defcustom tail-max-size 5
  "Maximum size of the window."
  :type 'integer
  :group 'tail)


;; Functions

;; Taken from calendar/appt.el
(defun tail-disp-window (tail-buffer tail-msg)
  "Display some content specified by TAIL-MSG inside buffer TAIL-BUFFER.
Create this buffer if necessary and put it inside a newly created window on
the lowest side of the frame."

  (require 'electric)

  ;; Make sure we're not in the minibuffer
  ;; before splitting the window.

  (if (equal (selected-window) (minibuffer-window))
      (if (other-window 1)
	  (select-window (other-window 1))
	(if window-system
	    (select-frame (other-frame 1)))))

  (let* ((this-buffer (current-buffer))
	 (this-window (selected-window))
	 (tail-disp-buf (set-buffer (get-buffer-create tail-buffer))))

    (if (cdr (assq 'unsplittable (frame-parameters)))
	;; In an unsplittable frame, use something somewhere else.
	(display-buffer tail-disp-buf)
      (unless (or (and (fboundp 'special-display-p)
                       (special-display-p (buffer-name tail-disp-buf)))
                  (and (fboundp 'same-window-p)
                       (same-window-p (buffer-name tail-disp-buf)))
		  (get-buffer-window tail-buffer))
	;; By default, split the bottom window and use the lower part.
	(tail-select-lowest-window)
	(split-window))
      (pop-to-buffer tail-disp-buf))

    (toggle-read-only 0)
    (if tail-volatile
	(erase-buffer))
    (insert-string tail-msg)
    (toggle-read-only 1)
    (shrink-window-if-larger-than-buffer (get-buffer-window tail-disp-buf t))
    (if (> (window-height (get-buffer-window tail-disp-buf t)) tail-max-size)
	(shrink-window (- (window-height (get-buffer-window tail-disp-buf t)) tail-max-size)))
    (set-buffer-modified-p nil)
    (if tail-raise
	(raise-frame (selected-frame)))
    (select-window this-window)
    (if tail-audible
	(beep 1))
    (if tail-hide-delay
	(run-with-timer tail-hide-delay nil 'tail-hide-window tail-buffer))))

(defun tail-hide-window (buffer)
  ;; TODO: cancel timer when some output comes during that time
  (let ((window (get-buffer-window buffer t)))
    (and window
	 (or (eq window (frame-root-window (window-frame window)))
	     (delete-window window)))))

(defun tail-select-lowest-window ()
  "Select the lowest window on the frame."
  (if (fboundp 'frame-lowest-window)
      (select-window (frame-lowest-window))
    (let* ((lowest-window (selected-window))
           (bottom-edge (car (cdr (cdr (cdr (window-edges))))))
           (last-window (previous-window))
           (window-search t))
      (while window-search
        (let* ((this-window (next-window))
               (next-bottom-edge (cadr (cddr (window-edges this-window)))))
          (when (< bottom-edge next-bottom-edge)
            (setq bottom-edge next-bottom-edge)
            (setq lowest-window this-window))
          (select-window this-window)
          (when (eq last-window this-window)
            (select-window lowest-window)
            (setq window-search nil)))))))

;;;###autoload
(defun tail-file (file)
  "Tails FILE specified with argument FILE inside a new buffer.
FILE *cannot* be a remote file specified with ange-ftp syntax because it is
passed to the Unix tail command."
  (interactive "Ftail file: ")
  ;; TODO: what if file is remote (i.e. via ange-ftp)
  (tail-command "tail" "-f" file))

;;;###autoload
(defun tail-command (command &rest args)
  "Tails COMMAND with arguments ARGS inside a new buffer.
It is also called by `tail-file'"
  (interactive "sTail command: \neToto: ")
  (let ((process
	 (apply 'start-process-shell-command
		command
		(concat "*Tail: "
			command
			(if args " " "")
			(mapconcat 'identity args " ")
			"*")
		command
		args)))
    (set-process-filter process 'tail-filter)))

(defun tail-filter (process line)
  "Tail filter called when some output comes."
  (tail-disp-window (process-buffer process) line))

(provide 'tail)

;;; tail.el ends here
