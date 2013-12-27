;;; toggle-option.el --- easily toggle frequently toggled options

;; Copyright (C) 2001 Cyprian Laskowski

;; Author: Cyprian Laskowski <swagbelly@yahoo.com>
;; Created: 8 May 2001
;; Version: 1.0
;; Keywords: convenience

;; This file is NOT currently part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.


;;; Commentary:

;; I find myself toggling the same Emacs features very often, and I
;; always set up key bindings for these features.  The problem is that
;; the list is getting rather big, and it's a nuisance to sacrifice
;; individual bindings to such a simple operation as the toggling of a
;; variable.  So the idea here is: set up a customizable list of
;; options and how they are to be toggled (whether the buffer-local or
;; global value is toggled, or whether a function is called), and
;; assign ONE command (`toggle-option') to ONE key, from which all
;; those options can be easily toggled (using completion).  For
;; individual variables, you can set values to toggle to override the
;; default of nil and t.

;; Get the most recent version at http://www.swagbelly.net/elisp/lib/.

;; To install, put this file in your Emacs load-path and the following line in
;; your .emacs file:

;; (autoload 'toggle-option "toggle-option" "Easily toggle frequently toggled options." t)

;; Then load this file or restart Emacs, and customize the variable
;; `toggle-option-list': (M-x customize-variable RET toggle-option-list RET).
;; See the documentation for `toggle-option-list' for details.

;; I also highly recommend that you bind `toggle-option' to a key, by putting
;; something like the following in your .emacs file as well:

;; (global-set-key "\M-o" 'toggle-option)

;; Now you can toggle options by typing M-o (remember that completion can be
;; used on your list) and supplying the first few characters of an option.


;;; Change Log:

;; Changes from 0.1 to 1.0

;;  * toggle-option: Allow possible values of the togglable variables
;;    to range over a specified list, rather than just t and nil.
;;  * toggle-option-list: Updated customization type to reflect change
;;    in `toggle-option'.
;;  * toggle-option-default-message-function: New variable.


;;; Code:

(require 'cl)


;;; Customization variables

(defgroup toggle-option nil
  "Convenience library for toggling commonly toggled variables/functions."
  :group 'convenience)

(defcustom toggle-option-default-message-function
  'toggle-option-message-generic
  "Default function which informs you about what's been changed by `toggle-option'."
  :type '(function :value toggle-option-message-generic)
  :group 'toggle-option)

(defcustom toggle-option-list nil
  "List of options commonly toggled and interpreted by function `toggle-option'.

Each element has the form (OPTION TYPE MESSAGE-FUNC VALUES).  OPTION
is an option to toggle; it should be either a function or a variable.
Which of these it is must be specified by TYPE, which must be
'function', 'buffer-var', or 'global-var'.  If 'function', then
`toggle-option' will simply invoke that function.  If 'buffer-var',
then `toggle-option' will toggle the buffer-local value of the
variable; if 'global-var', it will toggle the global value of the
variable.  There is one exception to this: see `toggle-option' for
details.  MESSAGE-FUNC is a function to use to show the user what has
happened.  It takes two arguments, an option and a type, and it should
return a string, which will be shown in the minibuffer after
`toggle-option' is called.  It can also be nil, in which case the
function `toggle-option-default-message-function' will be used.
Finally, VALUES, if non-nil, is a list of values that the \"toggling\"
should cycle through.  If nil, the values t and nil are toggled between."
  :type '(repeat (list (symbol :tag "Function or variable")
                       (choice :value function
                               (const :tag "Function" function)
                               (const :tag "Buffer-local variable" buffer-var)
                               (const :tag "Global variable" global-var))
		       (choice :value nil
                               (function-item :tag "Default" nil)
                               (function :tag "Message function"))
                       (choice :value nil
			       (const :tag "Nil and t" nil)
			       (repeat (sexp :tag "Value")))))
  :group 'toggle-option)


;;; Commands

(defun toggle-option (option &optional arg)
  "Toggle OPTION from `toggle-option-list'.
See that variable for an explanation of how the toggling occurs and what
confirmation message is shown.  Optional prefix argument ARG specifies that the
choice of buffer vs global setting to be toggled is the opposite of that set in
`toggle-option-list'; it has no effect if the type is set to 'function'."
  (interactive
   (list (intern (completing-read
                  "Toggle option: "
                  (mapcar '(lambda (x)
                             (cons (symbol-name (car x)) nil))
                          toggle-option-list)))
         current-prefix-arg))
  (let* ((elt (assoc option toggle-option-list))
         (default-type (nth 1 elt))
         (type
          (cond ((and arg (eq default-type 'buffer-var))
                 'global-var)
                ((and arg (eq default-type 'global-var))
                 'buffer-var)
                (t default-type)))
	 (vals (nth 3 elt)))
    (if (eq type 'function)
	(funcall option)
      (let* ((global (eq type 'global-var))
	     (val-get-func (if global 'default-value 'symbol-value))
	     (val-set-func (if global 'set-default 'set))
	     (val (funcall val-get-func option))
	     (tail (member val vals)))
	(funcall val-set-func option
		 (cond
		  ((null vals) (not val))
		  ((or (null tail) (equal (car (last vals)) val)) (car vals))
		  (t (cadr tail))))))
    (message "%s"
	     (funcall
	      (or (nth 2 elt) toggle-option-default-message-function)
	      option type))))


;;; Internal functions

(defun toggle-option-message-generic (option type)
  "A generic function for possible use in `toggle-option'.
OPTION is the option being toggled, and TYPE identifies how the toggling was
done.  See `toggle-option-list' for details on what these can be."
  (cond ((eq type 'function)
         (concat "Invoked function `" (symbol-name option) "'"))
        ((eq type 'buffer-var)
         (concat "Buffer-local setting of " (symbol-name option) " toggled to: "
		 (prin1-to-string (symbol-value option))))
        ((eq type 'global-var)
         (concat "Global setting of " (symbol-name option) " toggled to: "
		 (prin1-to-string (default-value option))))))


(provide 'toggle-option)

;;; toggle-option.el ends here
