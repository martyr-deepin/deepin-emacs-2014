;;; compilation-recenter-end.el --- compilation-mode window recentre

;; Copyright 2006, 2007, 2008, 2009 Kevin Ryde

;; Author: Kevin Ryde <user42@zip.com.au>
;; Version: 4
;; Keywords: processes
;; URL: http://user42.tuxfamily.org/compilation-recenter-end/index.html
;; EmacsWiki: CompilationMode

;; compilation-recenter-end.el is free software; you can redistribute it
;; and/or modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at your
;; option) any later version.
;;
;; compilation-recenter-end.el is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General
;; Public License for more details.
;;
;; You can get a copy of the GNU General Public License online at
;; <http://www.gnu.org/licenses/>.


;;; Commentary:

;; This is a spot of code to recentre compilation windows when a compile
;; finishes, so that the end of output is at the bottom of the window, thus
;; maximizing the visible part.  This is good under Emacs' default scrolling
;; policy where you could be left with only half the window used.

;; Emacsen:
;;
;; Emacs 20 and XEmacs 21
;;     In old emacs with only a single `compilation-finish-function'
;;     variable, that variable is forcibly set by this code.  See notes in
;;     the `compilation-recenter-end-enable' docstring if you want more than
;;     one finish function.

;;; Install:

;; Put compilation-recenter-end.el in one of your `load-path' directories,
;; and in your .emacs add
;;
;;     (autoload 'compilation-recenter-end-enable "compilation-recenter-end")
;;     (add-hook 'compilation-mode-hook 'compilation-recenter-end-enable)
;;
;; There's also autoload cookies below if you use update-file-autoloads
;; (autoloading the func, and adding a custom option, but not actually
;; modifying the hook).

;;; History:

;; Version 1 - the first version
;; Version 2 - autoloads and library ids suggested by Tom Tromey
;; Version 3 - compilation-mode-hook option explicitly as well as cookie
;; Version 4 - new home page, add an eval-when-compile for the byte compiler


;;; Code:

(defmacro compilation-recenter-end--with-selected-window (window &rest body)
  "`with-selected-window' made available in past versions of Emacs.
The current window is saved per `save-selected-window', then
WINDOW is made current with `select-window', and the BODY forms
executed.

In Emacs 22 this uses the actual `with-selected-window', but in
past versions it uses some fallback code.  The fallback doesn't
preserve the `buffer-list' order the way Emacs 22 does."

  (if (eval-when-compile (fboundp 'with-selected-window))
      `(with-selected-window ,window ,@body)
    `(save-selected-window
       (select-window ,window)
       ,@body)))
(put 'compilation-recenter-end--with-selected-window 'lisp-indent-function 1)

(defun compilation-recenter-end-at-finish (buffer string)
  "Recenter compilation window(s) to show a full window of text.
This function is designed for use in `compilation-finish-functions'.
BUFFER is the compilation buffer which has just finished, and
STRING is the finish reason (which is not used).

Under the default Emacs scrolling policy, the end of the
compilation output might be only 1/2 or 3/4 the way down the
window.  This function moves it so the end of the text is at the
end of the window, thus maximizing the visible part.

The recenter is only done if point is already at the end of the
buffer, so if you've moved up to look at early output, it isn't
forced down to the end."

  (dolist (window (get-buffer-window-list buffer))
    (compilation-recenter-end--with-selected-window window

      ;; In emacs 22 point is at (point-max).  But in emacs 21 when multiple
      ;; windows are showing the compilation buffer the non-selected ones
      ;; have point only at the end of output, before the "compilation
      ;; finished" message, hence allowing up to two lines from the end.
      ;;
      (when (<= (count-lines (point) (point-max)) 2)
        (save-excursion
          (goto-char (point-max))
          (recenter -1))))))

;;;###autoload
(defun compilation-recenter-end-enable ()
  "Enable recentring of compilation windows at finish.
This function adds `compilation-recenter-end-at-finish' to
`compilation-finish-functions' (for Emacs 21 and up) or sets it
into `compilation-finish-function' (past Emacs).  This is a
global change, affecting all compilation-mode buffers.

If you want multiple finish functions and only have an old Emacs
with the single `compilation-finish-function', you might try your
own defvar of `compilation-finish-functions' and set the single
function to call those.  `compilation-recenter-end-enable' here
will notice any `compilation-finish-functions' and use that.

The compilation-recenter-end home page is
URL `http://user42.tuxfamily.org/compilation-recenter-end/index.html'"

  ;; note no eval-when-compile on this boundp, so you can create a defvar of
  ;; it if doesn't already exist
  (cond ((boundp 'compilation-finish-functions)
         ;; emacs 21 up has list `compilation-finish-functions'
         (add-to-list 'compilation-finish-functions
                      'compilation-recenter-end-at-finish))
        (t
         ;; xemacs 21 only has the old single `compilation-finish-function'
         (setq compilation-finish-function
               'compilation-recenter-end-at-finish))))

;; As of emacs pre-23, `compilation-finish-functions' is only a defvar, not a
;; defcustom.  If it was a defcustom then compilation-recenter-end-at-finish
;; might be added to it with something like
;;
;; (custom-add-option 'compilation-finish-functions
;;                    'compilation-recenter-end-at-finish)

;;;###autoload
(custom-add-option 'compilation-mode-hook 'compilation-recenter-end-enable)

(provide 'compilation-recenter-end)

;;; compilation-recenter-end.el ends here
