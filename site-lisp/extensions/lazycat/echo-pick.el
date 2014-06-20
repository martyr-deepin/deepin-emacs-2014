;;; echo-pick.el --- filter for echo area status messages
;;
;; Copyright (C) 2008 Nikolaj Schumacher
;;
;; Author: Nikolaj Schumacher <bugs * nschum de>
;; Version: 0.1
;; Keywords: convenience
;; URL: http://nschum.de/src/emacs/echo-pick/
;; Compatibility: GNU Emacs 21.x, GNU Emacs 22.x
;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; A bunch of modes (e.g. Eldoc) exist that display information about the cursor
;; position in the echo area.  Unfortunately, these modes will often fight for
;; the echo area and there is no way to specify a priority.  echo aims to
;; provide a way to prioritize these messages, or even to display multiple
;; messages at once.
;;
;; To specify the priorities edit `echo-pick-function-list', then enable
;; `echo-pick-mode' and don't forget to disable the other modes' echo messages.
;; If they can't be turned off, maybe you can set a very long timeout.
;;
;; `echo-pick-mode' respects a lot of Eldoc's settings, including
;; `eldoc-message-commands'.
;;
;;; Change Log:
;;
;; 2008-07-16 (0.1)
;;    Initial release.
;;
;;; Code:

(eval-when-compile (require 'cl))
(require 'eldoc)

(defgroup echo-pick nil
  "Filter for echo area status messages."
  :group 'convenience)

(defcustom echo-pick-function-list
  `((git-blame-mode . git-blame-identify)
    (lambda () (let ((this-command nil)
                     (last-command (aref eldoc-message-commands 0))
                     (eldoc-mode t)
                     (eldoc-documentation-function nil))
                 (eldoc-print-current-symbol-info)))
    eldoc-documentation-function
    semantic-idle-summary-idle-function)
  "*List of functions to call when determining what to print in the echo area.
The echo area will display result of the first function in the list not to
return nil or an empty string.
All functions are expected to take no arguments."
  :group 'echo-pick
  :type '(repeat (choice (function :tag "function")
                         (cons :tag "conditional function"
                               (choice :tag "condition"
                                       (function :tag "function")
                                       (variable :tag "variable"))
                               (function :tag "function")))))

(defcustom echo-pick-limit 1
  "*Number of messages to display simultaneously."
  :group 'echo-pick
  :type 'integer)

(defvar echo-pick-timer nil)

(defun echo-pick-update-timer (value)
  (when echo-pick-timer
    (cancel-timer echo-pick-timer))
  (setq echo-pick-timer
        (run-with-idle-timer value t 'echo-pick-idle-do)))

(defun echo-pick-set (symbol value)
  (when symbol (set symbol value))
  (when echo-pick-timer
    (echo-pick-update-timer value)))

(defcustom echo-pick-idle-delay 0.5
  "*Number of seconds of idle time before highlighting the current symbol."
  :type 'number
  :set 'echo-pick-set
  :group 'echo-pick)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(define-minor-mode echo-pick-mode
  "Minor mode that displays `echo-pick' after `echo-pick-idle-delay'."
  nil " echo" nil
  (when echo-pick-mode
    (echo-pick-update-timer echo-pick-idle-delay)))

(defun echo-pick-extract-function (func)
  (cond
   ((functionp func) func)
   ((consp func) (when (let ((condition (car func)))
                         (cond ((eq major-mode condition) t)
                               ((boundp condition) (symbol-value condition))
                               ((functionp condition) (funcall condition))))
                   (cdr func)))
   ((boundp func) (symbol-value func))))

(defun echo-pick-funcall (func)
  "Call function and return its message.
If the function returns a string, it will be returned.  If it doesn't the last
call to `message' will be returned."
  (let (result last-msg)
    (cl-flet ((message (format-string &rest args)
                    (setq last-msg (apply 'format format-string args))))
      (setq result (funcall func))
      (if (stringp result)
          result
        last-msg))))

(defun echo-pick-message (functions limit)
  "Pick less or equal than LIMIT non-nil results from calling FUNCTIONS."
  (and functions
       (> limit 0)
       (let* ((func (echo-pick-extract-function (car functions)))
              (msg (condition-case err
                       (when func
                         (echo-pick-funcall func))
                     (error nil))))
         (or (and (stringp msg)
                  (not (equal msg ""))
                  (cons msg (echo-pick-message (cdr functions) (1- limit))))
             (echo-pick-message (cdr functions) limit)))))

(defun echo-pick-idle-do ()
  "When in `echo-pick-mode', call `echo-pick'."
  (let ((eldoc-mode t))
    (and echo-pick-mode
         (eldoc-display-message-p)
         (echo-pick))))

;;;###autoload
(defun echo-pick ()
  "Show the best echo message for the current `point'."
  (interactive)
  (let ((msg (echo-pick-message echo-pick-function-list echo-pick-limit)))
    (when msg
      (eldoc-message (mapconcat 'identity msg "\n")))))

(provide 'echo-pick)
;;; echo-pick.el ends here
