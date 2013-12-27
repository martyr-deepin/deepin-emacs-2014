;;; timerfunctions.el, 
;; Time-stamp: <02/10/04 11:02:52 deego>
;; GPL'ed under GNU'S public license..
;; Copyright (C) Deepak Goel 2000, 2001, 2002
;; Emacs Lisp Archive entry
;; Filename: timerfunctions.el
;; Author: Deepak Goel <deego@glue.umd.edu>
;; Version: 1.3.7


(defvar tf-home-page  "http://www.glue.umd.edu/~deego/lisp-mine/idledo")
(defvar timerfunctions-version "1.3.7")
;;; Author(s)
;;; ---Deepak Goel (deego@glue.umd.edu)  11/20/00
;;;========================================================
;;;========================================================
;;; Commentary:   The latest version can always be downloaded from
;;; http://www.glue.umd.edu/~deego/emacs.html

;;; What is timerfunctions.el?  
;;; Enhancements to timer.el.

;;;Borrowed from the documentation of
;;; tf-run-with-idle-timer:
;;; Suppose you want emacs to run an action every REDOSECS for as long as
;;; emacs remains idle.  Think you can do it with the emacs' 
;;; run-with-idle-timer? Think again.. :)   That function will perform the
;;; action exactly once every time emacs goes idle.  This function, 
;;; tf-run-with-idle-timer *will* allow you to keep performing an action
;;; as long as emacs remains idle.  
;;; (You say: aha,  i can have the timer call more timers.. but you
;;; don't wanna have to do that every time do you, esp. if u r a mere
;;; mortal like myself.. )
;;;========================================================
;;;========================================================

;;; QUICKSTART INSTALLATION: 
;;; Place this file somewhere in yr emacs-load-path, and add the
;;; foll. to your .emacs: (load "timerfunctions.el")

;;; See also: midnight.el (part of emacs), timer.el

;;; Code: 


;;;###autoload
(defun tf-time-difference (timeplus timesub)
  "Gives the time in seconds elaspsed from TIMESUB to TIMEPLUS.
Almost like \(- TIMEPLUS TIMESUB \)."
  (+ (* (expt 2 16) (- (car timeplus) (car timesub)))
     (- (cadr timeplus) (cadr timesub)))
)


;;;###autoload
(defun tf-run-with-idle-timer  (secs repeat redosecs redorepeat includeruntime function &rest args) 
  "Args are SECS, REPEAT, REDOSECS, REDOREPEAT, INCLUDERUNTIME,
FUNCTION and &rest ARGS.
Similar to run-with-idle-timer, except that provides more options.
Suppose you want emacs to run an action every REDOSECS for as long as
emacs remains idle.  Think you can do it with the emacs' 
run-with-idle-timer? Think again.. :)   That function will perform the
action exactly once every time emacs goes idle.  This funciton, 
tf-run-with-idle-timer *will* allow you to keep performing an action
as long as emacs remains idle.

SECS is the number of seconds to wait once emacs has first gone
idle. It can really be any expression whose at runtime yields a
number..  Note that the way run-with-idle-timer is defined, SECS will
unfortunately be evalled immediately after you call this function, but
redosecs will be *every* time emacs *remains* idle..yay..


If REDOREPEAT is non-nil, the action is repeated as long emacs remains
idle.  REDOSECS is the number of additional seconds (after the action
has been done) to wait if emacs remains idle before performing the
action again.  Again, redosecs does not have to be a number, it can be
any expression whose eval yields to a number...

If INCLUDERUNTIME is non-nil, REDOSECS is the number of
additional seconds to wait after the action has been invoked (not
finished).

If REPEAT is nonnil, the entire cycle is repeated every time emacs
next goes idle.. (as in the default run-with-idle-timer."
  (apply 'run-with-idle-timer 
	 (eval secs) repeat 'tf-run-while-idle 
	 redosecs redorepeat includeruntime
	 function args)
  )


(defun tf-run-while-idle (redosecs redorepeat includeruntime
function &rest args)
  "Runs FUNCTION with ARGS and optionally repeats if emacs idle.
Probably is of no use unless used in programs.
 If REDOREPEAT is non-nil, the function is repeated periodically every
REDOSECS as long as emacs remains idle. By default, emacs waits
REDOSECS *after* the function is done executing to repeat. If you want
the execution-time to count towards REDOSECS, make INCLUDERUNTIME
non-nil.
SECS and REDOSECS can be any expressions that eval at runtime to
numbers.. In particular, they can simply be numbers..

"
  (if (not includeruntime)
      (progn
	(apply function args)
	(if redorepeat
	    (while (sit-for (eval redosecs))
	      (apply function args))))
    (progn
      (let ((before-time (current-time)))
	(apply function args)
	(if redorepeat
	    (while (sit-for (- 
			     (eval redosecs)
			     (tf-time-difference (current-time)
						 before-time)))
	      (setq before-time (current-time))
	      (apply function args))))))
  )


;;;====================================================
;;;TESTS FOLLOW
(defun tf-test-display-time-internal
  ()
  (let ((thisbuffer (buffer-name)))
    (switch-to-buffer-other-window "*scratch*")
    (goto-char (point-max))
    (insert (concat "\n" (format "%S" (cadr (current-time)))))
    (recenter)
    (switch-to-buffer-other-window thisbuffer))
)


(defun tf-test ()
  "Run this and watch..Play around with the options.. If you run it,
you may have to exit your emacs session to restore normal emacs!
unless you are an expert, that is.."

  (interactive)
  (tf-run-with-idle-timer
  1 t 3 t nil 'tf-test-display-time-internal)
)


  


(defun tf-wait-until-idle (&optional secs)
  "DOES NOT WORK YET. Waits until idle. 
Will help run processes in background.  This function will NOT create
a timer.  Will simply use sit-for.  "
  (if (null secs)
      (setq secs 1))
  (while (not (sit-for secs))
    (sit-for 1))
  (message "tf-wait-until-idle DONE WAITING!")
)


;;;Tue Jan 23 17:38:44 2001
(defmacro tf-ignore-errors (&rest body)
 "Like ignore-errors, but tells the error.."
 (let ((err (gensym)))
   (list 'condition-case err (cons 'progn body)
	 (list 'error
	       (list 'message
		     (list 'concat
			   "IGNORED ERROR: "
			   (list 'error-message-string err)))))
   ))


(provide 'timerfunctions)

;;;timerfunctions.el ends here.

