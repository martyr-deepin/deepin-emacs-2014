;;; async-eval.el --- execute Emacs lisp in a separate process
;;
;; Copyright (C) 2009 Nikolaj Schumacher
;;
;; Author: Nikolaj Schumacher <bugs * nschum de>
;; Version: 0.1
;; Keywords: extensions, lisp, processes
;; URL: http://nschum.de/src/emacs/async-eval/
;; Compatibility: GNU Emacs 22.x, GNU Emacs 23.x
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
;; Emacs lisp is not multi-threaded.  This library explores possibilities to
;; work around this.  It allows forms to be evaluated in a separate Emacs
;; process.  This means that the executed code cannot access the current
;; environment, and the code's side effects cannot be used directly.
;; There is a small overhead from starting a new process.
;;
;; Say, you need to calculate this:
;;
;; (let ((sum 0)
;;       (i 10000000))
;;   (while (> i 0)
;;     (setq sum (+ sum i)
;;           i (1- i)))
;;   sum)
;;
;; Instead of calling it directly, and blocking the Emacs interface, you can
;; call it like this:
;;
;; (async-eval
;;     (lambda (result) (message "async result: <%s>" result))
;;   (let ((sum 0)
;;         (i 10000000))
;;     (while (> i 0)
;;       (setq sum (+ sum i)
;;             i (1- i)))
;;     sum))
;;
;; The first argument is a function that is called with the result once the
;; calculation is done.
;;
;; Since the form is evaluated in a separate process, local functions are not
;; available by default.  If you have the following function:
;;
;; (defun fib (n)
;;   "Intentionally slow fibonacci numbers."
;;   (if (< n 2)
;;       n
;;     (+ (fib (1- n)) (fib (- n 2)))))
;;
;; You cannot do this, as fib won't be defined.
;;
;; (async-eval
;;     (lambda (result) (message "async result: <%s>" result))
;;   (fib 35))
;;
;; But you can conveniently export a list functions and variables to the other
;; process:
;;
;; (async-eval-with-export '(fib)
;;     (lambda (result) (message "async result: <%s>" result))
;;   (fib 30))
;;
;;; Change Log:
;;
;; 2009-03-09 (0.1)
;;    Initial release.
;;
;;; Code:

(defgroup async-eval nil
  "Execute Emacs lisp in a separate process."
  :group 'extensions
  :group 'lisp
  :group 'processes)

(defcustom async-eval-emacs-program-name nil
  "*Name of the Emacs executable to use for evaluation.
If this is nil, the program from the current process is used."
  :group 'async-eval
  :type '(choice (const :tag "automatic (nil)" nil)
                 (file :tag "file")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun async-eval-sentinel (proc msg)
  "The internal sentinel for the asynchronous process."
  (when (eq (process-status proc) 'exit)
    (let ((buffer (process-get proc 'async-eval-buffer)))
      (unwind-protect
          (if (= 0 (process-exit-status proc))
              (eval-buffer buffer)
            (error "%s(async-eval execution failed)"
                   (with-current-buffer buffer
                     (buffer-string)))))
      (kill-buffer buffer))))

(defun async-eval-filter (proc output)
  "The internal filter for the asynchronous process."
  (with-current-buffer (process-get proc 'async-eval-buffer)
    (insert output)))

(defsubst async-eval-format (handler form)
  "Format FORM to be evaluateable by an Emacs process.
Have HANDLER be called with the result."
  ;; Make sure %s in sentinel or text isn't expanded twice!
  (format "(message \"(funcall '%%S '%%S)\" '%S %S)" handler (prin1 form)))

(defun async-eval-form (handler form)
  "Evaluate FORM in a separate process and call HANDLER with the result."
  (let* ((emacs (or async-eval-emacs-program-name
                    (car command-line-args)))
         (buffer (generate-new-buffer "*async-lisp*"))
         (proc (start-process "async-eval"
                              nil emacs "-Q" "--batch" "--eval"
                              (async-eval-format handler form))))
    (process-put proc 'async-eval-buffer buffer)
    (set-process-filter proc 'async-eval-filter)
    (set-process-sentinel proc 'async-eval-sentinel)
    nil))

(defun async-eval-do-export (symbols)
  "Prepare SYMBOLS for export into an Emacs process."
  (let ((form nil)
        symbol)
    (while symbols
      (setq symbol (pop symbols))
      (when (functionp symbol)
        (push `(defun ,symbol . ,(cdr (symbol-function symbol))) form))
      (when (boundp symbol)
        (push `(setq ,symbol ',(symbol-value symbol)) form)))
    form))

(defmacro async-eval (sentinel &rest body)
  "Evaluate BODY in a separate process and call HANDLER with the result.
HANDLER should be a function that accepts one argument."
  (declare (indent 1) (debug t))
  `(async-eval-form ,sentinel (quote (progn ,@body))))

(defmacro async-eval-with-export (symbols sentinel &rest body)
  "Evaluate BODY in a separate process and call HANDLER with the result.
SYMBOLS is a list of variables and functions that are exported to the process,
before body is evaluated.  HANDLER should be a function that accepts one
argument."
  (declare (indent 2) (debug t))
  `(async-eval-form ,sentinel
                    (quote (progn ,@(async-eval-do-export (eval symbols)) ,@body))))

(provide 'async-eval)
;;; async-eval.el ends here
