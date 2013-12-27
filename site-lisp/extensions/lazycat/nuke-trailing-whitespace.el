;;; nuke-trailing-whitespace.el --- strip trailing whitespace from buffers

;; Copyright (C) 1995, 1996, 1997, 2000 Noah S. Friedman
;; Copyright (C) 2003 Peter S. Galbraith

;; Author: Noah Friedman <friedman@splode.com>
;; Maintainer: friedman@splode.com
;; Keywords: extensions
;; Status: Works in Emacs 20 and 21 and XEmacs.

;; $Id: nuke-trailing-whitespace.el,v 1.1 2003/10/03 16:49:26 psg Exp $

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
;;
;; This package strips trailing whitespace from buffers.  It can be used
;; manually on a buffer by calling `M-x nuke-trailing-whitespace' when
;; editing a buffer, or automatically when you save a file.
;;
;; For automatic stripping of files as you save them (or email messages as
;; you send them), either add the following in your .emacs:
;;
;;     (autoload 'nuke-trailing-whitespace "nuke-trailing-whitespace" nil t)
;;     (add-hook 'mail-send-hook 'nuke-trailing-whitespace)
;;     (add-hook 'write-file-hooks 'nuke-trailing-whitespace)
;;
;; or better yet accomplish the same effect by customizing the variable
;; `nuke-trailing-whitespace-in-hooks' and saving the result for future
;; sessions.
;;
;; By default, buffers that have a major-mode listed in the customizable
;; list `nuke-trailing-whitespace-always-major-modes' are stripped, those
;; listed in the list `nuke-trailing-whitespace-never-major-modes' are not,
;; and others are prompted for.  You can change this default behaviour by
;; customizing the variable `nuke-trailing-whitespace-p'.

;; See also: whitespace.el packaged in Emacs-21

;;; History:
;;
;; 2003-05-01 Cristian Ionescu-Idbohrn <cristian.ionescu-idbohrn@axis.com>
;;  Emacs21 also provides a whitespace.el.  This file, and its functions,
;;  were renamed to nuke-trailing-whitespace to avoid the name-space
;;  collision.
;;
;; 2003-10-06 - Peter S Galbraith <psg@debian.org>
;;  - Support for custom interface.  Allows enabling of package via custom.
;;  - nuke-trailing-whitespace-in-hooks: New defcustom to select hooks
;;

;;; Code:

(require 'cl)

(defgroup nuke-trailing-whitespace nil
  "Strip trailing whitespace from buffers."
  :group 'editing
  :group 'convenience)

(defcustom nuke-trailing-whitespace-p 'nuke-trailing-whitespace-check-mode
  "*Specify when stripping whitespace should be done.
This variable affects how the function `nuke-trailing-whitespace' behaves.
If t, unreservedly strip trailing whitespace, including excess newlines.
If nil, do nothing.
If the symbol 'query, then query for each instance.

The default setting, the function `nuke-trailing-whitespace-check-mode',
says to strip buffers with major modes listed in
`nuke-trailing-whitespace-always-major-modes', skip those listed in the
list `nuke-trailing-whitespace-never-major-modes' and query the user for
others.

You may set another custom-made function which will be called instead.
This function should return t, nil, or the symbol `query' to decide what to
do.

This variable is made buffer-local when set in any fashion."
  :group 'nuke-trailing-whitespace
  :type '(radio (const :tag "Never" nil)
                (const :tag "Always" t)
                (const :tag "Query" :value query)
                (const :tag
                       "Default function (nuke-trailing-whitespace-check-mode)"
                       :value nuke-trailing-whitespace-check-mode)
                (function :tag "Other function")))
(make-variable-buffer-local 'nuke-trailing-whitespace-p)

(defcustom nuke-trailing-whitespace-in-hooks nil
  "List of hooks to install `nuke-trailing-whitespace' into.
Unsetting values does not remove hooks from the current session."
  :type 'hook                           ;Not really, but gives us :options
  :options '(write-file-hooks mail-send-hook)
  :group 'nuke-trailing-whitespace
  :set (lambda (symbol value)
         (set-default symbol value)
         (when value
           (loop for x in value
                 do (add-hook x 'nuke-trailing-whitespace))))
  :require 'nuke-trailing-whitespace)

;; The regexp "\\s-+$" is too general, since form feeds (\n), carriage
;; returns (\r), and form feeds/page breaks (C-l) count as whitespace in
;; some syntaxes even though they serve a functional purpose in the file.
(defconst nuke-trailing-whitespace-regexp "[ \t]+$"
  "Regular expression which matches trailing whitespace.")

;; Match two or more trailing newlines at the end of the buffer; all but
;; the first newline will be deleted.
(defconst nuke-trailing-whitespace-eob-newline-regexp "\n\n+\\'"
  "Regular expression which matches newlines at the end of the buffer.")

(defcustom nuke-trailing-whitespace-always-major-modes
  '(ada-mode
    c++-mode
    c-mode
    change-log-mode
    emacs-lisp-mode
    fortran-mode
    latex-mode
    lisp-interaction-mode
    lisp-mode
    makefile-mode
    nroff-mode
    perl-mode
    plain-tex-mode
    prolog-mode
    scheme-mode
    sgml-mode
    tcl-mode
    slitex-mode
    sml-mode
    texinfo-mode)
  "*Major modes for which `nuke-trailing-whitespace-check-mode' will return t.
These are major modes for which `nuke-trailing-whitespace' should
strip all trailing whitespace and excess newlines at the end of the buffer
without asking."
  :group 'nuke-trailing-whitespace
  :type '(repeat (function :tag "Mode")))

(defcustom nuke-trailing-whitespace-never-major-modes
  '(mail-mode
    rmail-mode
    vm-mode
    vm-summary-mode)
  "*Major modes for which `nuke-trailing-whitespace-check-mode' returns nil.
These are major modes for which `nuke-trailing-whitespace' should
never strip trailing whitespace automatically."
  :group 'nuke-trailing-whitespace
  :type '(repeat (function :tag "Mode")))


;;;###autoload
(defun nuke-trailing-whitespace ()
  "Nuke all trailing whitespace in the buffer.
Whitespace in this case is just spaces or tabs.
This is a useful function to put on `write-file-hooks'.

Unless called interactively, this function uses
`nuke-trailing-whitespace-p' to determine how to behave.
However, even if this variable is t, this function will query for
replacement if the buffer is read-only."
  (interactive)
  (cond ((interactive-p)
	 (call-interactively 'nuke-trailing-whitespace-do-nuke-whitespace))
        (t
         (let ((flag nuke-trailing-whitespace-p))
           (and nuke-trailing-whitespace-p
                (symbolp nuke-trailing-whitespace-p)
                (fboundp nuke-trailing-whitespace-p)
                (setq flag (funcall nuke-trailing-whitespace-p)))

           (and flag
		(nuke-trailing-whitespace-do-nuke-whitespace flag)))))
  ;; always return nil, in case this is on write-file-hooks.
  nil)

(defun nuke-trailing-whitespace-do-nuke-whitespace (&optional flag)
  "Remove trailing whitespace in buffer, not prompting first if FLAG is t."
  (interactive)
  (let ((buffer-orig-read-only buffer-read-only)
        (buffer-read-only nil))
    (save-excursion
      (save-restriction
        (save-match-data
          (widen)
          (goto-char (point-min))
          (cond
           ((or (and (eq flag t)
                     (not buffer-orig-read-only))
                (interactive-p))
	    (while (re-search-forward nuke-trailing-whitespace-regexp
                                      (point-max) t)
              (delete-region (match-beginning 0) (match-end 0)))
            (goto-char (point-min))
	    (and (re-search-forward nuke-trailing-whitespace-eob-newline-regexp
                                    nil t)
                 (delete-region (1+ (match-beginning 0)) (match-end 0))))
           (t
	    (query-replace-regexp nuke-trailing-whitespace-regexp "")
            (goto-char (point-min))
	    (and (re-search-forward nuke-trailing-whitespace-eob-newline-regexp
                                    nil t)
                 (y-or-n-p
                  "Delete excess trailing newlines at end of buffer? ")
                 (delete-region (1+ (match-beginning 0)) (match-end 0))))))))))

(defun nuke-trailing-whitespace-check-mode (&optional mode)
  "Default function to determine if whitespace should be trimmed from a buffer.
Returns t if MODE is listed in `nuke-trailing-whitespace-always-major-modes'.
Returns nil if listed in `nuke-trailing-whitespace-never-major-modes'.
Else returns symbol 'query if buffer is visible or nil otherwise."
  (or mode (setq mode major-mode))
  (cond ((memq mode nuke-trailing-whitespace-always-major-modes) t)
        ((memq mode nuke-trailing-whitespace-never-major-modes) nil)
        ;; Only query for visible buffers; invisible buffers are probably
        ;; managed by programs (e.g. w3 history list) and a query for them
        ;; is confusing.
        ((get-buffer-window (current-buffer) t) 'query)
        (t nil)))

(provide 'nuke-trailing-whitespace)

;;; nuke-trailing-whitespace.el ends here
