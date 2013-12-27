;;; idledo.el --- do stuff when emacs is idle..
;; Time-stamp: <2008-07-28 15:45:14 deego>
;; Copyright (C) Deepak Goel 2001
;; Emacs Lisp Archive entry
;; Filename: idledo.el
;; Package: idledo
;; Author: Deepak Goel <deego@gnufans.org>
;; Keywords:  idle startup speed timer
;; Version: 0.3
;; Author's homepage: http://deego.gnufans.org/~deego
;; REQUIRES: timerfunctions.el 1.2.7 or later.
;; ALSO uses: emacs' ('cl during compile.. for all the backquoting..)
;; For latest version:

(defvar idledo-home-page
  "http://deego.gnufans.org/~deego/emacspub/lisp-mine/idledo")

;; Requires: timerfunctions.el
;; See also: Jari's tinyload.el, dope.el.


;; This file is NOT (yet) part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.




;; Quick start:
(defvar idledo-quick-start
  "Drop idledo.el and timerfunctions.el somewhere in your
load-path. In your .emacs, type (require 'idledo) and (require
'timerfunctions).  In there, also create idledo-list-- a list of
expresions, either by hand, or by using one of the many functions and
macros provided.  Then, write (idledo-start), and idledo will start
doing the tasks mentioned in the idledo-list whenever emacs is idle.

Here, for example, are some
possible lines of code from a .emacs--->


  (idledo-require 'bbdb 'bbdb-com 'bbdb-gnus)
  (idledo-add-action
   '(progn      (unless (file-locked-p \"~/emacs/.bbdb\")
    (bbdb-records))
      nil))
  (idledo-require-now 'mailabbrev)
  ;; as below, or simply (idledo-gc)
  (idledo-add-action
               '(garbage-collect))

  (idledo-add-action '(load \"aliases-my\"))

  (idledo-add-action '(progn
            (garbage-collect)
            nil))

  (idledo-load \"mode-hook-functions-my\")
  (add-to-list 'idledo-list '(progn (message \"Just a sample\")))
  (idledo-require 'disp-table)
  (idledo-require 'gnus-score 'gnus 'gnus-msg)

A simple long example is (idledo-example-setup) which can be called
from your .emacs.  Alternatively, a more complicated example of how to
set up idledo-list can be seen in the function idledo-example.  That
one tries to save even more time by: moving the task of setting up an
idledo-list itself into the first idledo, and on top of that, calls
idledo-start not from emacs, but from an idle-timer.

To maintain idledo-history, see idledo-after-action-hooks

This author currently uses 126 idledo's.
PS: timerfunctions.el can be obtained from:
http://deeego.gnufans.org/~deego/emacspub/lisp-mine/timerfunctions/"
  )

(defun idledo-quick-start ()
  "Provides electric help for function `idledo-quick-start'."
  (interactive)
  (with-electric-help
   '(lambda () (insert idledo-quick-start) nil) "*doc*"))

;;; Introduction:
;; Stuff that gets posted to gnu.emacs.sources
;; as introduction
(defvar idledo-introduction
  "Idledo does stuff for you when emacs is idle.


The actions can be simple one-time actions or repetitive.  You can
include as many actions as you want.  Thus, with apprpriate actions,
if you leave emacs running for sometime, take a break and come back,
your emacs should have (require)'d almost everything you will ever
need..you can now start your gnus or eshell or w3 instantly.. When you
are using gnus, you can check mail periodically.. Make
color-theme-random a periodic idledo and you can convert emacs into a
shapeshifting color-changing aquarium..

idledo will probably someday be interfaced with a prioritizer, which
will include all sorts of enhanced capabilites, like weighting of
repetitive actions etc.

See also M-x idledo-quick-start
"
  )

;;;###autoload
(defun idledo-introduction ()
  "Provides electric help for function `idledo-introduction'."
  (interactive)
  (with-electric-help
   '(lambda () (insert idledo-introduction) nil) "*doc*"))

;;; Commentary:
(defvar idledo-commentary
  "First type M-x idledo-introduction.
Also see M-x idledo-quick-start

You give idledo a list of commands, and it will eval them when emacs is
idle and you are away..  Thus, if you take a hike and come back, your
w3, gnus, eshell should all start instantly..  Your gnus-news should
be checked periodically for you.. and *Group* buffer updated.. of
course, you have to set this all up :/)

If emacs is idle *not* because you are away, but because you are
deeply absorbed using info, you probably don't want idledo springing into
action and loading eshell for you.. So, idledo tries to alert you before
loading anything, and gives you enough time to cancel any action
before it is taken..

As an example, see the function idledo-example.  I call that function
from my .emacs as follows..

/(idledo-example/)

where:

Note: If you specify many idle-loads and thus make your emacs very big
with (idle) time, your emacs will get slow and do frequent gc.  Some
remedies:

* First, turn garbage-collection messages on to see what i am sayin,
  for yourself: (setq garbage-collection-messages t) in .emacs

* Next, increase gc-cons-threshold to say, 10 times its value:
 (setq gc-cons-threshold 40000000) in .emacs.

* Finally, ask idledo to do garbage-collections for you when emacs is
  idle.  See an example in idledo-example-setup.  In that example, once
  all my other idledo's are taken care of, emacs then alternates
  between doing garbage-collection and color-theme-random when it is
  idle..  Thus, trying to ensure that when I get back to work, least gc
  takes place...


0.1 new features:
* Now called idledo, to avoid a name-conflict with another package.
  Sorry about that, and Thanks to all who pointed this out.
* Macros like ido-add-require now called idledo-require.
* Minor bug fixed in idledo-add-periodic-action-to-beginning-crud
"  )

(defun idledo-commentary ()
  "Provides electric help for function `idledo-commentary'."
  (interactive)
  (with-electric-help
   '(lambda () (insert idledo-commentary) nil) "*doc*"))

;;; History:

;;; New features:
(defvar idledo-new-features
  "New in 0.3:
Some Bugfixes. Made compatible with the current
timerfunctions.el--posted here.
Improved doc."
  )

(defun idledo-new-features ()
  "Provides electric help for function `idledo-new-features'."
  (interactive)
  (with-electric-help
   '(lambda () (insert idledo-new-features) nil) "*doc*"))

(defvar idledo-version "0.3")

(defvar idledo-todo
  "TODO:
* Ideally, one should be able to cancel the timer if idledo-list
   becomes nil.

* Write a prioritizer, and interface the same with idledo. The priotizer
  should. among other things like weights and \(arbitrarily specified\)
  repetitivity, try to support different idle times for different
  tasks.."
  )


;;==========================================
;;; Code:
(defgroup idledo nil
  "idledo.el --- do stuff when emacs is idle..  "
  :group 'applications)

(defcustom idledo-before-load-hooks nil "."
  :type 'hook
  :group 'idledo
  )
(defcustom idledo-after-load-hooks nil "."
  :type 'hook
  :group 'idledo
  )
(run-hooks 'idledo-before-load-hooks)
(eval-when-compile (require 'cl))

(defcustom idledo-before-action-hooks nil
  "

This hook is run even if idledo-once is callesd byhand.

"
  :type 'hook
  :group 'idledo
  )

(defcustom idledo-before-idle-action-hooks nil
  "."
  :type 'hook
  :group 'idledo
  )

(defcustom idledo-after-action-hooks nil
  "Hooks to run after performing idledo-actions.
You could insert the command idledo-history-update into this hook.
This hook is run even if idledo-once is called by hand.
"
  :type 'hook
  :group 'idledo)

(defcustom idledo-after-idle-action-hooks nil
  "Hooks to run after performing idledo-actions.
You could insert the command idledo-history-update into this hook.
"
  :type 'hook
  :group 'idledo)

(defcustom idledo-before-possible-action-hooks nil "."
  :type 'hook
  :group 'idledo)

(defcustom idledo-after-possible-action-hooks nil "."
  :type 'hook
  :group 'idledo)



(defcustom idledo-list nil
  "A list of actions to perform.."
  :type 'list
  :group 'idledo
  )


(defcustom idledo-verbosity 0
  "Suggested: Anywhere from -100 to 100.

The design is such that a value of 0 should be optimum.
viz.: Once you are experienced with this library, you might prefer a value
of 0 for this variable if this is > 0 right now."
  :type 'integer
  :group 'idledo
  )

(defvar idledo-active-p nil
  "If t, no more idledo's can be initiated..
The aim is to only have one idledo active at a time.

Why?  I don't know.  You can easily setq this to nil, and start yet
another `idledo-start' if you want.

Why do i want only one idledo at a time?  My experience is that \(GNU\)
Emacs bahaves unpredictably if the activation of 2 or more timers
collide... maybe i am wrong?  It seems to me that sometimes, both get
executed, someimtes one, and sometimes none..  Although the one or
none situations seem to be rare, each of thses situations can be
potentially bad..particularly if: Suppose the timer is a
self-reinforcing timer \(as can be done by calls to
`tf-run-with-idle-timer'\).  Then, the very first time it fails to get
executed, the process gets killed and you want get those cherished
repetitions as long as Emacs remains idle.."
  )

(defcustom idledo-interval 30
  "The interval to wait the first time Emacs goes idle..
An additional small interval will be allowed to enable the user to
cancel the action.

Note that you can assign to this this interval any expression that
will be eval'ed at run-time \(see timerfunctions.el for more details..\)"
  :type 'list
  :group 'idledo
  )

(defcustom idledo-interval-subsequent 1
  "When Emacs remains idle, time to wait before next action.

Time is in seconds.. floats might work too.
Note that you can assign to this this interval any expression that
will be eval'ed at run-time \(see timerfunctions.el for more details..\)"
  :type 'list
  :group 'idledo
  )

(defcustom idledo-interval-small  5
  "Time to warn for before performing the imminent idledo.

Before beginning any action, idledo will flash a warning, and will
wait for these many seconds.. if you do something in this time, the
action will be cancelled.

Note that you can assign to this this interval any expression that
will be eval'ed at run-time \(see timerfunctions.el for more details..\)"  :type 'hook
:group 'idledo
)

(defvar idledo-timer nil
  "The timer stored here.. so can be cancelled.. Internal..")

(defvar idledo-last-action nil
  "Will store the last action.
--if the user needs this for any purpose. ")
(defvar idledo-last-result nil
  "The result of the eval of the last idledo-action.
provided in case the user needs this. ")

(defvar idledo-history nil
  "Stores, optionally, the reverse-history of idledo-actions and their
results. ")

(defcustom idledo-history-max-length 100
  "Max length of history to maintain. Nil means no limit.
When length exceeded, oldest entries are discarded. "
  :group 'idledo
  )

(defvar idledo-counter 0
  "The number of idledos performed.  ")



(defun idledo-history ()
  (interactive)
  (message "idledo-counter: %S   idledo-history: %S"
           idledo-counter idledo-history))

(defun idledo-history-update ()
  (interactive)
  (push (list (copy-tree idledo-last-action)
              (copy-tree idledo-last-result))
        idledo-history)
  (setq idledo-counter (+ 1 idledo-counter))
  (while (and (integerp idledo-history-max-length)
              (> (length idledo-history) idledo-history-max-length))
    (setq idledo-history (reverse (cdr (reverse idledo-history))))))


(defun idledo-start-forced-risky ()
  "Internal.
USED ONLY FOR DEBUGGING.. USE AT YOUR OWN RISK.. STARTS A PARALLEL
version of idledo if there already exists one..."
  (interactive)
  (tf-run-with-idle-timer
   'idledo-interval t
   'idledo-interval-subsequent
   t nil
   'idledo-one-action))


;;;###autoload
(defun idledo-stop ()
  "Stop any idledo."
  (interactive)
  (when (timerp idledo-timer)
    (cancel-timer idledo-timer))
  (setq idledo-active-p nil))



;;;###autoload
(defun idledo-start ()
  "Start idledo.

See also `idledo-active-p'.  Also returns the timer."
  (interactive)
  (if (not idledo-active-p)
      (progn
        (idledo-stop)
        (setq idledo-active-p t)
        (setq idledo-timer
              (tf-run-with-idle-timer
               'idledo-interval t
               'idledo-interval-subsequent
               t nil
               'idledo-one-action)))
    (error "Idledo is already active")))

(defcustom idledo-interval-done 1
  "Time to wait before showing the 'done' message.
Idledo will wait for this much time before flashing a 'done-action'
message"
  :group 'idledo
  )


(defcustom idledo-action-imminent-string
  "idledo imminent unless keypress ---> "
  "The `idledo-action-imminent-string'."
  :type 'string
  :group 'idledo
  )

(defun idledo-one-action ()
  "Internal.
Does one instance of processing of action."
  (when (not (null idledo-list))
    (run-hooks 'idledo-before-possible-action-hooks)
    (idledo-message 25
                    (concat idledo-action-imminent-string
                            (idledo-shorten (format "%S" (car idledo-list)))))
    (if (sit-for idledo-interval-small)
        (progn
          (run-hooks 'idledo-before-idle-action-hooks)
          (idledo-once 1)
          (run-hooks 'idledo-after-idle-action-hooks)
          (sit-for idledo-interval-done)
          (idledo-message 60 "%S more idledo(s) remainig.. "
                          (length idledo-list)))


      (idledo-message 20
                      (concat "IDLEDO's action canceled.."
                              (idledo-shorten (format "%S" (car idledo-list)))))
      )
    (run-hooks 'idledo-after-possible-action-hooks)))

(defun idledo-all ()
  "Tell the amount of time saved through idledo's.
Start emacs and run M-x idledo-all.  That will run all your
idledo's at once and show you how much time all of that took.

More like, it will run as many idledo's as there are currently in
your idledo-list, which may not correspond to ALL idledo's since you
may have repetitive idledo's"
  (interactive)
  (let ((ta (current-time))
        (len (length idledo-list))
        tb tott)
    (idledo-once len)
    (setq tb (current-time))
    (setq tott (idledo-time-diff tb ta))
    (message "That took %S milliseconds. " tott)))


(defun idledo-time-diff (tb ta)
  "Get the difference bet times TB and TA, in milliseconds.  A float."
  (+
   (* 0.001 (- (caddr tb) (caddr ta)))
   (* 1000.0
      (+
       (- (second tb) (second ta))
       (* 65536.0
          (- (car tb) (car ta)))))))

;;;###autoload
(defun idledo-once (arg)
  "Call this if you wanna run something in yr `idledo-list' NOW...
Provide numerical prefix ARG for multiple arguments...
but note that doesn't run after-action hooks etc."
  (interactive "p")
  (while
      (>= arg 1)
    (setq arg (- arg 1))
    (run-hooks 'idledo-before-action-hooks)
    (progn
      (idledo-message 20
                      (concat "IDLEDO doing action.."
                              (idledo-shorten (format "%S" (car idledo-list)))))
      (let ((carval (car idledo-list)))
        (setq idledo-last-action carval)
        (setq idledo-list (cdr idledo-list))
        (setq idledo-last-result
              (idledo-ignore-errors (eval  carval)))))
    (run-hooks 'idledo-after-action-hooks)

    ))



(defun idledo-add-periodic-action-crude (action)
  "Add a action to `idledo-list' to be repeated endlessly.
Is a crude mechanism for adding action to the `idledo-list' and make it
repetitive.  ACTION is a (quoted) list which will be evaled to perform an
eval.

Note that the ACTION this way is added to the END of `idledo-list'.
And ACTION is added to list no matter what (even if there is a similar
action already waiting in the list)."
  (setq
   idledo-list
   (append
    idledo-list
    (list
     `(progn
        ,action
        (idledo-add-periodic-action-crude
         (quote ,action)))))))

(defun idledo-add-periodic-action-to-beginning-crude (action)
  "Add an action to `idledo-list' to be repeated endlessly.

Is a crude mechanism for adding action to the `idledo-list' and make it
periodic.  ACTION is a list which will be evaled to perform an
eval.
Note that the ACTION this way is added to the BEGINNING and subsequent
calls are also added to the beginning of the list.
And ACTION is added to list no matter what (even if there is a similar
action already waiting in the list)."
  (idledo-add-action-forced
   `(progn
      ,action
      (idledo-add-periodic-action-to-beginning-crude
       (quote ,action)))))




;;;###autoload
(defun idledo-add-to-end-of-list (list-var element)
  "Like `add-to-list', but add at the end, if at all.

Add to the end of the list LIST-VAR, the element ELEMENT"
  (if (member element (symbol-value list-var))
      (symbol-value list-var)
    (set list-var (append  (symbol-value list-var) (list element)))))

(defun idledo-add-action (action)
  "Add ACTION to ideldo-list.

ACTION is an expression to be evaled.  Action is added at the
beginning if at all.  See similar commands too."
  (add-to-list 'idledo-list action))

(defun idledo-add-action-forced (action)
  "Add action ACTION to `idledo-list' even if it already exists."
  (setq idledo-list (cons action idledo-list)))

(defun idledo-add-action-at-end (&rest actions)
  "Add actions ACTIONS to the end of `idledo-list'."
  (mapcar
   (lambda (action)
     (idledo-add-to-end-of-list 'idledo-list action))
   actions))

(defmacro idledo-load (&rest files)
  "Add, for each of FILES,  a (load file) action to `idledo-list'."
  (cons 'progn
        (mapcar
         (lambda (arg)
           `(idledo-add-action-at-end '(load ,arg)))
         files)))

;;; 2001-11-03 T13:42:01-0500 (Saturday)    Deepak Goel
(defmacro idledo-load-now (&rest files)
  "Add, for each of FILES, a (load-file) action to `idledo-list'.

The action is added to the beginning of `idledo-list'."
  (cons 'progn
        (mapcar
         (lambda (arg)
           `(idledo-add-action '(load ,arg)))
         files)))


(defmacro idledo-require (&rest features)
  "Add, for each of the FEATURES, a (require) action to `idledo-list'."
  (cons 'progn
        (mapcar
         (lambda (arg)
           `(idledo-add-action-at-end '(require ,arg)))
         features)))


(defmacro idledo-require-now (feature)
  "Add a (require FEATURE) action to `idledo-list'.

The addition is done to the beginning of `idledo-list'."
  `(idledo-add-action '(require ,feature)))

(defun idledo-add-action-at-end-forced (action)
  "Add ACTION to the end of `idledo-list'.

Action is added even if it exists in the list already."
  (setq idledo-list (append idledo-list (list action))))

(defun idledo-initialize (initial-list)
  "Initialize `idledo-list' to INITIAL-LIST."
  (setq idledo-list initial-list))

(defun idledo-remove-action (action)
  "Remove ACTION from `idledo-list'."
  (idledo-remove-from-list 'idledo-list action))

(defun idledo-remove-from-list (listname elt)
  "INTERNAL.

Remove, from list LISTNAME, element ELT."
  (set listname (idledo-list-without-element
                 (eval listname)
                 elt)))

(defun idledo-list-without-element (list elt)
  "INTERNAL.
Returns the value of the LIST without the element ELT."
  (if (null list)
      list
    (if (equal (car list) elt)
        (idledo-list-without-element (cdr list) elt)
      (cons
       (car list)
       (idledo-list-without-element
        (cdr list) elt)))))


;; Thanks to Kim F. Storm for the suggestion:
(defun idledo-gc ()
  (idledo-add-action '(garbage-collect)))

(defun idledo-shorten (string)
  "Internal, return a shortened version with no newlines.
Internal, returns a shortened version of STRING with no newlines."
  (let
      ((string-no-enter
        (with-temp-buffer
          (insert string)
          (goto-char (point-min))
          (while (search-forward "\n" nil t)
            (replace-match " " nil t))
          (buffer-substring (point-min) (point-max)))))
    (if (> (length string-no-enter) 55)
        (substring string-no-enter 0 55)
      string-no-enter)))


(defcustom idledo-ignore-errors-p t
  "This should always be t unless you know what you are doing.

For regular idledo's if this is not t and an error occurs, this means
that your entire idle-timer might get canceled due to the error.  The
only place where this = nil makes sense is when you are running M-x
idledo-once by hand and want to debug the idledo action which is
giving you an error.  See also idledo-toggle-ignore-errors.")

(defun idledo-toggle-ignore-errors-p (&optional arg)
  "See idledo-ignore-errors-p. "
  (interactive "P")
  (let ((num (prefix-numeric-value arg)))
    (cond
     ((or (not arg) (equal num 0))
      (setq idledo-ignore-errors-p (not idledo-ignore-errors-p)))
     ((> num 0) (set idledo-ignore-errors-p t))
     ((< num 0) (set idledo-ignore-errors-p nil)))
    (message "Symbol %S set to %S"
             'idledo-ignore-errors-p
             idledo-ignore-errors-p)
    idledo-ignore-errors-p))


(defmacro idledo-ignore-errors (&rest body)
  "Like `ignore-errors', but tell the error..

A wrapper around the BODY."

  (if idledo-ignore-errors-p
      (let ((err (gensym)))
        `(condition-case ,err (progn ,@body)
           (error
            (ding t)
            (ding t)
            (ding t)
            (idledo-message 90 "IGNORED ERROR: %s"
                            (error-message-string ,err))
            (sit-for 1)
            nil)))
    `(progn ,@body)))


;;;###autoload
(defun idledo-example ()
  "Sample of code to include in your .emacs..
See this and `idledo-example-setup'.
Define a similar function idledo-yourname for yourself in your .emacs,
and call it in yr .emacs by inserting (idledo-yourname) somewhere.

See \\[idledo-quick-start] for simple examples.

This function tries to go one step further to and defers the setting
up of the `idledo-list' itself to a time when Emacs goes idle, so as to
try to save more .emacs loading time."
  (interactive)
  (message "Setting up idledo and starting it..")
  ;; testing
  ;;(setq idledo-interval 300)

  ;;(setq idledo-list nil)
  (idledo-add-action-at-end '(idledo-example-setup))


  (setq idledo-action-imminent-string
        "Idledo imminent--> ")
  (idledo-start)
  (message "Setting up idledo and starting it..done")

  )



(defun idledo-message (points &rest args)
  "Signal message, depending on POINTS and `idledo-verbosity'.
ARGS are passed to `message'."
  (unless (minusp (+ points idledo-verbosity))
    (apply #'message args)))

(defcustom idledo-message-nice-sit 1 "" :group 'idledo)

(defun idledo-message-nice (points &rest args)
  (unless (minusp (+ points idledo-verbosity))
    (with-temp-message (apply 'format args)
      (sit-for 0.5))))

;;;###autoload
(defun idledo-length-list ()
  "For you to quickly find the length of idledo-list..
If you use idledo bigtime, you will frequently find yourself wanting
to find out the length.. and you don't want to eval that parenthesised
expression all the time.. perhaps.."
  (interactive)
  (idledo-message
   (if (interactive-p) 135 35)
   "%s"
   (format "Length=  %S     ... %S..." (length idledo-list)
           (first idledo-list)))
  (length idledo-list))














(defun idledo-example-setup ()
  "Called by `idledo-example'.
This extra step is taken so that setting
up idledo itself takes place only when Emacs has gone idle..
This function is actually used by this user's .emacs.
"
  ;; The preference in all of below should be to load stuff that takes
  ;; time asap.. small libraries can always be loaded later.. or even
  ;; if they are not loaded, they do not make the user wait anyways
  ;; when they finally get  loaded..

  ;; once bbdb is loaded.. let's get the frobnicating stuff over with..

  ;; made interactive only for test purposes..
  (interactive)

  ;; hm, i now prefer directly setting the idledo list...


  (idledo-gc)

  ;; remove all calls to gnus.. we don't want fsbot starting gnus on
  ;;his own..
  ;;(idledo-require 'bbdb 'bbdb-com 'bbdb-gnus)
  (idledo-require 'bbdb 'bbdb-com)
  (idledo-add-action
   '(progn
      (require 'bbdb)
      (when (boundp 'bbdb-file)
        (unless (file-locked-p  bbdb-file)
          (bbdb-records)))
      nil))
  (idledo-require-now 'mailabbrev)
  (idledo-add-action '(progn
                        (garbage-collect)
                        nil))
  ;;(idledo-load "gnus-functions-my")
  (idledo-load "macros-my")
  (idledo-add-action '(load "aliases-my"))
  (idledo-gc)

  (idledo-load "mode-hook-functions-my")
  (idledo-require 'disp-table)
  ;;(idledo-require 'gnus-score 'gnus 'gnus-msg)
  ;;(idledo-require 'gnus-cache)
  ;;(idledo-require 'gnus-ml 'gnus-cite)
  (idledo-require 'timerfunctions)

  ;;maybe emacs needs a GC now.. we need to make sure GC is done when
  ;;emacs is idle..
  (idledo-gc)

  (idledo-require 'esh-mode
                  'em-alias)

  (idledo-require 'em-banner 'em-basic 'em-cmpl 'em-dirs 'em-glob
                  'em-hist 'em-ls 'em-prompt 'em-script 'em-term
                  'em-xtra 'etags
                  'ange-ftp
                  ;; no longer needed since pcomplete is now bundled
                  ;; with emacs (21..)
                  ;;'pcmpl-auto
                  'pcomplete
                  ;; 2002-05-02 T11:57:07-0400 (Thursday)    D. Goel
                  'shellhist
                  ;; 2002-05-02 T11:57:25-0400 (Thursday)    D. Goel
                  'pcmpl-unix

                  ;; no longer needed since eshell is now bundled
                  ;; with emacs (21)
                  ;;'eshell-auto

                  'em-unix 'bytecomp 'eshell 'runshell )
  (idledo-add-action '(progn
                        (garbage-collect)
                        nil))
  (idledo-add-action '(progn
                        (recentf-mode 1)
                        nil))
  (idledo-load "cl-seq")

  (idledo-require 'autokey)
  (idledo-require 'thingatpt 'ispell 'info)
  (idledo-require 'elder)

  (idledo-require 'mail-extr )
  (idledo-require 'autorevert 'view)
  (idledo-require 'time-stamp )
  (idledo-require 'imenu)
  (idledo-load "kinsoku")
  (idledo-require 'edlib )
  (idledo-require 'phonemode)
  (idledo-add-action '(progn
                        (garbage-collect)
                        nil))

  ;; bytecomp should be required before this...
  (idledo-add-action-at-end '(load "byte-opt"))

  ;;(idledo-load 'tex-mode)
  (idledo-require 'boxquote)
  (idledo-require 'dired)
  (idledo-require 'dired-x)
  (idledo-require 'bytecomp)
  (idledo-require 'find-func)
  (idledo-require 'diff 'diff-mode)
  (idledo-require 'add-log)
  (idledo-require 'calendar)
  (idledo-require 'mule-util)
  (idledo-require 'cal-move)
  (idledo-require 'advice)
  (idledo-require 'browse-kill-ring)
  ;; add for fsbot
  (idledo-require 'browse-url)
  (idledo-add-action '(progn
                        (garbage-collect)
                        nil))

  (idledo-require 'debug)
  ;;(idledo-require 'ell)
  (idledo-require 'table)
  (idledo-require 'tabify)
  (idledo-require 'edebug)
  ;; 2002-04-25 T15:43:21-0400 (Thursday)    Deepak Goel
  ;; this will shorten the time it takes to find a tag..
  ;;   (idledo-add-action
  ;;    '(progn
  ;;       (visit-tags-table  "~/TAGS")
  ;;       nil))
  ;;(idledo-require 'gnus-cus)
  ;;(idledo-require 'gnus-async)
  ;;(idledo-require 'smiley)
  ;;(idledo-add-action
  ;;(progn
  ;; (require 'smiley "smiley-ems")
  ;;nil))
  (idledo-require 'cus-edit)
  (idledo-require 'newcomment)
  (idledo-require 'genauto)
  (idledo-require 'mkback)
  (idledo-add-action '(progn
                        (mkback-install-for-eshell)
                        nil))
  (idledo-require 'flow-fill)
  (idledo-require 'findutils)
  (idledo-require 'erc)
  (idledo-add-periodic-action-crude
   '(progn
      (garbage-collect) nil))

                                        ;  (idledo-add-action
                                        ;   '(progn
                                        ;      (numshift-install)
                                        ;      nil))
  (idledo-add-action
   '(progn
      (if (display-mouse-p)
          (mouse-avoidance-mode 'animate))
      nil))
  (idledo-add-action
   '(progn
      (iswitchb-mode 1)
      nil))
  (idledo-require 'spook)
  (idledo-require 'autoinsert)
  (idledo-require 'sregex)
  (idledo-require 'choose)
  (idledo-require 'erc-complete)
  (idledo-require 'buffer-stack)
  (idledo-require 'emacs-wiki)
  (idledo-require 'planner)
  ;;(idledo-add-action
  ;;'(progn
  ;;(require 'eldoc)
  ;;(utils-add-minor-mode 'lisp-mode 'eldoc-mode)
  ;;(utils-add-minor-mode 'emacs-lisp-mode 'eldoc-mode)))



  (idledo-add-action '(progn
                        (global-font-lock-mode t)
                        nil))


  (idledo-add-action
   '(progn
      (if
          (locate-library "bbdb" nil nil)
          (require 'bbdb)
        (message "NO BBDB found..."))
      nil))


  ;;   (idledo-add-action
  ;;    (progn
  ;;      ;; CVS's type break currently has an annoying "sabve file? "
  ;;      ;; question.
  ;;      (when (< emacs-minor-version 3)
  ;;        (type-break-mode 1))
  ;;      nil))

  (idledo-require 'emacs-wiki)


  ;; top priority stuff...
  (idledo-add-action
   '(progn
      ;; do we still need all of these for emacs21?
      (ignore-errors-my
       (add-to-list 'ispell-skip-region-alist
                    '("\\\\[a-z]?ref{". "}"))
       (add-to-list 'ispell-skip-region-alist
                    '("\\[\\[\\$". "\\$\\]\\]")) ; for latex..
       (add-to-list 'ispell-skip-region-alist
                    '("\\\\[a-z]?cite{". "}"))
       (add-to-list 'ispell-skip-region-alist
                    '("\\\\begin{al[a-z]*}" . "\\\\end{al[a-z]*}"))
       (add-to-list 'ispell-skip-region-alist
                    '("(\\[ebf\\]ll". "\\[eef\\])")) ; see the function
                                        ; regexp-quote..
       )))


  (idledo-add-action
   '(windmove-default-keybindings))
  ;;(idledo-add-action
  ;;`(progn
  ;;   (load "chess-auto")
  ;;   nil))
  ;;(idledo-require 'scroll-in-place)
  (idledo-require 'auto-recompile)
  (idledo-add-action
   '(progn
      (require 'elder-beautify)
      (elder-beautify-latex)
      nil))

  (idledo-add-action
   (progn
     (ignore-errors-my (elder-editing-modes))
     nil))


  ;; NB: that these are just autoload-definitions..  so their only use
  ;; is really for fsbot.
  (idledo-require 'calc)
  (idledo-require 'calc-ext)

  ;;(idledo-require 'elder-set-keys)

  ;; at the very end.. we want this!
  (idledo-add-action
   '(progn
      (icomplete-mode 1)
      nil))

  (idledo-require-now 'fetch)

  (idledo-require 'emacs-wiki)


  (idledo-require 'boxquote)
  (idledo-require 'assoc)
  (idledo-require 'spam-stat)
  ;; for fsbot
  (idledo-require 'cc-mode)
  (idledo-require 'custom)

  (idledo-require 'repeat)
  (idledo-require 'thinks)
  (idledo-add-action '(mwheel-install))
  (idledo-add-action
   '(progn
      (setq vel-verbosity 0)
      (setq vel-echo-status-p t)
      (require 'vel)
      (setq-default vel-mode t)))



  ;;   (idledo-add-action
  ;;    '(progn
  ;;       (show-paren-mode 1)
  ;;       nil))





  (idledo-add-action
   '(progn
      (tabbar-mode 1)))

  ;; top priority
  (idledo-add-action
   '(progn
      (require 'fetch)
      (miniedit-install)
      (fetch-install-for-eshell)
      (mkback-install-for-eshell)
      nil))


  ;; may not play well with enriched mode?
;;;   (idledo-add-action
;;;    '(when window-system
;;;       (require 'highlight-tail)
;;;       (call-interactively 'highlight-tail-mode 1)))



  ;; TOP TOP priority
  (idledo-add-action
   '(progn
      (auto-compression-mode 1)
      nil))

  (idledo-require 'windmove)
  (idledo-add-action
   '(windmove-default-keybindings))



  (idledo-require 'parse-time)
  ;;(idledo-add-action
  ;;'(progn
  ;;  (require 'color-theme)
  ;;(color-theme-parus)
  ;;(color-theme-fischmeister)
  ;;(color-theme-gray1)
  ;;(utils-color-theme-nice-random-contextual)
  ;;))


  ;;   (idledo-add-periodic-action-crude
  ;;    '(progn
  ;;       (setq idledo-verbosity -100)
  ;;       (utils-color-theme-random-contextual) nil))

  ;;   (idledo-add-action
  ;;    '(utils-color-theme-nice-random-contextual))

;;; (idledo-add-periodic-action-crude
;;;    '(progn
;;;       (require 'diary-lib)
;;;       (require 'appt)
;;;       (diary)
;;;       (message "%S" appt-time-msg-list)
;;;       (appt-check)

;;;       ))


  ;; initialize woman..
  (idledo-add-action-at-end
   '(when (sit-for 300)
      (require 'woman)
      (woman-file-name "")))




  ;;   (idledo-add-action
  ;;    '(progn
  ;;       (require 'remem)
  ;;       (remem-toggle)))





  ;;(idledo-add-action '(dabbrev-hover-install t t))



  (idledo-add-action '(require 'apt-utils))




  ;;   (idledo-add-action-at-end
  ;;    '(when (sit-for 4200)
  ;;       (when (or (not (fboundp 'gnus-alive-p))
  ;;        (not (gnus-alive-p)))
  ;;    (spam-stat-doit-my))))

  )




;;;###autoload
(defun idledo-nullify ()
  (interactive)
  (setq idledo-list nil)
  (message "Idledo-list set to nil"))


(provide 'idledo)
(run-hooks 'idledo-after-load-hooks)
;;; idledo.el ends here
