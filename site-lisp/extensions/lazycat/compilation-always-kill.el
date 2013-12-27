;;; compilation-always-kill.el --- kill compilation without prompting

;; Copyright 2008 Kevin Ryde

;; Author: Kevin Ryde <user42@zip.com.au>
;; Version: 1
;; Keywords: processes
;; EmacsWiki: CompilationMode
;; URL: http://www.geocities.com/user42_kevin/compilation-always-kill/

;; compilation-always-kill.el is free software; you can redistribute it
;; and/or modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at your
;; option) any later version.
;;
;; compilation-always-kill.el is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General
;; Public License for more details.
;;
;; You can get a copy of the GNU General Public License online at
;; <http://www.gnu.org/licenses>.


;;; Commentary:

;; `compilation-always-kill-mode' minor mode makes M-x compile kill any
;; existing compilation process without its normal `yes-or-no-p' query.
;; See the mode docstring below for more.

;;; Install:

;; Put compilation-always-kill.el somewhere in your `load-path', and in your
;; .emacs add
;;
;;    (autoload 'compilation-always-kill-mode "compilation-always-kill" nil t)
;;
;; and to enable it
;;
;;    (compilation-always-kill-mode 1)
;;
;; or deferred until you actually compile something,
;;
;;    (eval-after-load "compile" '(compilation-always-kill-mode 1))
;;
;; There's an autoload cookie for the mode, if you know how to use
;; `update-file-autoloads' and friends.  You can M-x customize-variable
;; compilation-always-kill-mode to enable too, though that depends on either
;; those autoloads or a whole (require 'compilation-always-kill) in your
;; .emacs.

;;; Emacsen:

;; Designed for Emacs 22, works in Emacs 21 and XEmacs 21.

;;; History:

;; Version 1 - the first version


;;; Code:

;;;###autoload
(define-minor-mode compilation-always-kill-mode
  "Always kill an existing compilation process in M-x compile.
This minor mode arranges for `compile' to kill any existing
compilation process when starting a new one, where normally it
asks with `yes-or-no-p'.

Interactively \\[compilation-always-kill-mode] toggles, likewise
with no argument from lisp code.  Otherwise t or non-zero
enables, or nil or 0 disables.

Whether you want `compile' to ask is a personal preference,
basically a choice between

  - convenience of restarting a \"make\" etc when you've edited
    something, without answering a question

  - risk of killing a long running or important job you forgot
    you had going

If you have automatic kill most of the time then you can always
turn it off with \\[compilation-always-kill-mode] while running
something important.  Turning it off like that is the main reason
for a whole minor mode for what's otherwise two lines of code.

The query when exiting Emacs about \"running processes\" is not
changed.  See also quick-yes.el for answering that or `compile'
with less keys.

----

Incidentally the `compile' query is particularly annoying when
the time it takes you to think about killing is long enough for
it to finish by itself anyway!  It could be cute for
`yes-or-no-p' to let you just press return, or abandon the
question and just continue, when that happens.  A special Ret
binding might be able to do that, though ideally you'd want
`compile' to somehow specify the conditions under which the query
is no longer applicable."

  :group  'compilation
  :global t
  :type   'boolean
  :link   '(url-link
            :tag "compilation-always-kill.el home page"
            "http://www.geocities.com/user42_kevin/compilation-always-kill/index.html"))

(defadvice yes-or-no-p (around compilation-always-kill-mode activate)
  "Minor mode for `compile' to always kill existing compilation."
  (if (and compilation-always-kill-mode
           (string-match "A compilation process is running; kill it\\?"
                         prompt))
      (setq ad-return-value t)
    ad-do-it))

(provide 'compilation-always-kill)

;;; compilation-always-kill.el ends here
