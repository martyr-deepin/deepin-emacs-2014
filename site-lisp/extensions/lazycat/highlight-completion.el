;;; highlight-completion.el --- completion with highlighted provisional text
;; Copyright (c) 1991-1996 Mark Haiman, Nick Reingold, John Palmieri
;; Copyright (c) 1997-2001 John Palmieri
;;
;; Author: John Palmieri <palmieri@math.washington.edu>
;; URL: http://www.math.washington.edu/~palmieri/Emacs/hlc.html
;; Keywords: completion
;; Version:  0.06 of Thu Jun 21 20:32:40 PDT 2001
;;
;; This file is not part of GNU Emacs.
;;
;; This package is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This package is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This package is based on the lightning completion package,
;; written by Mark Haiman and Nick Reingold, then modified by me.  I
;; am the author of this package, so any problems are completely
;; my fault.  All the good parts probably came from Mark and Nick's
;; original code...
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Description: 
;;   
;; This package modified how Emacs performs completions.  Ordinarily,
;; if you are typing a file name into the minibuffer (after hitting
;; C-x C-f, say), if you type a few letters and hit the TAB key, then
;; Emacs completes as far as possible.  For example, suppose the
;; directory contains only these files:
;;    filbert   filibuster   frank   grunge.tex
;; If you type 'g' followed by TAB, then 'runge.tex' is inserted.  If
;; you hit 'fi' then TAB, an 'l' is inserted.  If you hit 'f' then TAB,
;; there is no unique continuation of the file name, so Emacs opens up
;; a new window displaying the list of possible completions.
;;
;; That's the old system.  This package provides a variant: if you
;; type 'g', then 'runge.tex' is automatically inserted as highlighted
;; text, to indicate that it's only provisional.  The point remains
;; immediately after the 'g'.  If you hit TAB, the point jumps to the
;; end, and the added text is no longer highlighted.  (So if you
;; weren't looking at the screen, you wouldn't know that anything
;; different had happened.)  If after hitting 'g', you typed 'a'
;; (because you wanted to find a new file 'gaptooth.el') the
;; highlighted text would disappear.  The effects of various keys:
;;    TAB: jump forward to the end of the highlighted text.  If no
;;       text is highlighted, open up a window showing possible
;;       completions.
;;    SPC: jump forward a word (so 'g' followed by SPC would yield
;;       'grunge.tex', with the point after the '.', and with 'tex'
;;       highlighted).  If no text is highlighted, open up a window
;;       showing possible completions.
;;    ?: open up a window showing possible completions.
;;    RET: open the named file (so 'g' followed by RET would open 
;;       'grunge.tex').
;;    C-g: delete the highlighted text and stop this modified
;;       completion process (and exit the minibuffer, if you're in the
;;       minibuffer).
;;    C-c: delete the highlighted text and stop this modified
;;       completion process.
;;    character: if consistent with completion, unhighlight it and
;;       move the point forward.  if inconsistent, insert the
;;       character and delete the highlighted text, stopping this
;;       completion process.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; How to use:
;;
;; 1. Put this file (i.e., "highlight-completion.el") in your load-path.
;; 2. Put 
;;      (require 'highlight-completion)
;;    in your .emacs file (or your .xemacs/init.el file)
;; 3. Turn on highlight completion by either: running
;;       M-x highlight-completion-mode
;;    or putting this in your .emacs file:
;;       (highlight-completion-mode 1)
;;    or customizing variables:
;;       M-x customize-group highlight-completion
;;    Then turn on "Highlight completion mode".
;;    You may want to modify some of the entries in "Highlight completion list".
;; 4. You can also run the functions
;;      hc-completing-insert-file-name       to complete file names
;;      hc-completing-insert-lisp-function               lisp functions
;;      hc-completing-insert-lisp-variable               lisp variables
;;      hc-completing-insert-kill                        contents of kill ring
;;      hc-completing-insert-buffer-contents             buffer contents
;;      hc-ispell-complete-word                          words, using ispell
;;    These functions can be used anywhere, not just in the
;;    minibuffer.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Lightning completion, on which this is based, works with a package
;; called Ultra-TeX to provide dynamic completion of TeX commands.  I
;; will work on adding this new completion as an option for
;; Ultra-TeX mode.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Version history
;;
;; 0.01 (30-May-2001) first version.
;; 0.02 (30-May-2001) tinkering.
;; 0.03 (31-May-2001) tinkering.
;; 0.04 (31-May-2001) use overlays instead of text-properties in GNU Emacs.
;; 0.05 (21-Jun-2001) add function hc-ispell-complete-word
;; 0.06 (21-Jun-2001) new customization procedure.  see above.  some
;;                    bug fixes, too.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst hc-version-string "0.06"
  "Version of highlighting completion package.")

(defconst hc-version hc-version-string
  "Version of highlighting completion package.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Customization
;;

(defgroup highlight-completion nil
  "Highlight completion mode: display completion as highlighted text."
  :tag "Highlight completion"
  :prefix "hc"
  :link '(url-link :tag "Home Page" "http://www.math.washington.edu/~palmieri/Emacs/hlc.html")
  :group 'abbrev)

(defconst hc-xemacs-p
  (string-match "XEmacs\\|Lucid" emacs-version)
  "Non-nil if using XEmacs.")

(defconst hc-emacs-20-p
  (and (boundp 'emacs-major-version)
       (= emacs-major-version 20))
  "Non-nil if using Emacs 20.")

(defconst hc-emacs-21-p
  (and (boundp 'emacs-major-version)
       (not hc-xemacs-p)
       (= emacs-major-version 21))
  "Non-nil if using GNU Emacs 21.")

(defcustom highlight-completion-mode nil
  "Toggle whether `highlighting' is on.
If on, you may want to customize highlight-completion-list to specify
contexts in which to use highlighting.  If off, you can still
run functions like hc-completing-insert-file-name or
hc-completing-insert-according-to-mode to use this completion."
  :type '(boolean)
  :set (lambda (symbol value)
	 (highlight-completion-mode (if value 1 -1)))
  :initialize 'custom-initialize-default
  :require 'highlight-completion
  :group 'highlight-completion)
  
(defun highlight-completion-mode (&optional prefix)
  "Activate highlight-completion.  Deactivates with negative universal
argument."
  (interactive "p")
  (or prefix (setq prefix 0))
  (cond ((>= prefix 0)
	 (setq highlight-completion-mode t)
	 (add-hook 'minibuffer-setup-hook 'highlight-completion-setup))
	(t (setq highlight-completion-mode nil))))
  
(defconst highlight-completion-list-default
  '((files . t)
    (functions . t)
    (commands . t)
    (variables . t)
    (user-variables . t) 
    (lisp-objects . t) 
    (info-menu-items . t) 
    (buffers . t)
    (query . nil)
    (misc . nil))
  "default value of highlight-completion-list")

(defun hc-convert-completion-list (list)
  "Convert LIST (which should be highlight-completion-list-external)
to a list of (symbol . boolean) pairs."
  (let ((hc-list highlight-completion-list-default)
	(temp list)
	answer)
    (if (< (length temp) (length hc-list))
	(setq temp (append temp (make-list
				 (- (length hc-list) (length temp))
				 nil))))
    (while hc-list
      (setq answer (cons (cons (caar hc-list) (car temp)) answer)
	    hc-list (cdr hc-list)
	    temp (cdr temp)))
    (reverse answer)))

(defun hc-unconvert-completion-list (list)
  "Convert LIST (which should be highlight-completion-list)
to a list of boolean values."
  (mapcar 'cdr list))

(defcustom highlight-completion-list-external
  (hc-unconvert-completion-list highlight-completion-list-default)
  "Enable highlighting completion in specific contexts.
If nil, turn off completion in that context.  If t, turn on
completion.  The contexts are reasonably self-explanatory: 
  `Files' means file name completion (e.g., after `C-x C-f').
  `Functions' means lisp function completion (e.g., after `C-h f').
  `Commands' means command completion (e.g., after `M-x').
  `Variables' means lisp variable completion (e.g., after `C-h v').
  `User variables' means completion on `user variables'--see the
     documentation for the function `user-variable-p', for instance,
     to see what this means.
  `Lisp objects' means both funtions and variables.
  `Info menu items' is what it says (e.g., after hitting `m' in info mode).
  `Buffer names' is what it says (e.g., after hitting `C-x C-b').
  `Query replace' means: complete on contents of the current buffer
     when asking for a string to replace when running query-replace (`M-%').
  `Miscellany' means: complete on whatever seems appropriate when
     Emacs knows how to complete (e.g., in gnus, if you hit `j' to run
     `gnus-jump-to-group', this will complete on group names)."
  :tag "Highlight completion list"
  :type '(list (boolean :tag "Files          ")
	       (boolean :tag "Functions      ")
	       (boolean :tag "Commands       ")
	       (boolean :tag "Variables      ")
	       (boolean :tag "User variables ")
	       (boolean :tag "Lisp objects   ")
	       (boolean :tag "Info menu items")
	       (boolean :tag "Buffer names   ")
	       (boolean :tag "Query replace  ")
	       (boolean :tag "Miscellany     "))
  :set (lambda (symbol value)
	 (setq highlight-completion-list
	       (hc-convert-completion-list value))
	 (set symbol value))
  :group 'highlight-completion)

(defvar highlight-completion-list
  (hc-convert-completion-list highlight-completion-list-external)
  "List of things on which to complete.
This is a list, each element of which looks like (SITUATION)
or (SITUATION . t).  In the former case, highlighting completion is off
in SITUATION, and in the latter case, highlighting completion is on in
SITUATION.  You can modify this list directly, but it is better
customize it.")

(defcustom hc-ignored-file-extensions-external
  completion-ignored-extensions
  "File extensions to ignore when doing highlight completion"
  :type '(repeat string)
  :tag "Hc Ignored File Extensions"
  :set (lambda (symbol value)
	 (setq hc-ignored-file-extensions
	       (concat "\\(" 
		       (mapconcat 'regexp-quote value "\\|")
		       "\\)$"))
	 (set symbol value))
  :group 'highlight-completion)

(defvar hc-ignored-file-extensions
  (concat "\\(" 
	  (mapconcat 'regexp-quote
		     hc-ignored-file-extensions-external
		     "\\|")
	  "\\)$")
  "Regular expression of file extensions to ignore when doing
highlight completion.")

(defcustom hc-word-connectors-external '("." "-" "/")
  "Characters which will be added automatically when completing a word."
  :type '(repeat string)
  :tag "Hc Word Connectors"
  :set (lambda (symbol value)
	 (setq hc-word-connectors
	       (concat "\\(" 
		       (mapconcat 'regexp-quote value "\\|")
		       "\\)"))
	 (set symbol value))
  :group 'highlight-completion)

(defvar hc-word-connectors
  (concat "\\(" 
	  (mapconcat 'regexp-quote
		     hc-word-connectors-external
		     "\\|")
	  "\\)$")
  "Regular expression of characters to be added to the end when completing a word.")

(defvar hc-completions-map (make-sparse-keymap)
  "Key map for highlight completion functions.")

(defcustom hc-ctrl-x-c-is-completion nil
  "Toggle whether `C-x c' is the prefix key for the various highlight
completion commands.  If on,

  C-x c f   runs   hc-completing-insert-lisp-function
  C-x c v   runs   hc-completing-insert-lisp-variable
  C-x c F   runs   hc-completing-insert-file-name
  C-x c k   runs   hc-completing-insert-kill
  C-x c y   runs   hc-completing-insert-buffer-contents
  C-x c i   runs hc-ispell-complete-word
  C-x c C-h   lists all of the key bindings starting with C-x c

These functions do completion on the appropriate thing in any buffer,
not just the minibuffer.  This is useful for typing file names or lisp
functions or whatever.
If turned off, `C-x c' does nothing."
  :type '(boolean)
  :set (lambda (symbol value)
	 (if value
	     (define-key ctl-x-map "c" hc-completions-map)
	   (define-key ctl-x-map "c" nil))
	 (set symbol value))
  :group 'highlight-completion)

(define-key hc-completions-map "f" 'hc-completing-insert-lisp-function)
(define-key hc-completions-map "v" 'hc-completing-insert-lisp-variable)
(define-key hc-completions-map "o" 'hc-completing-insert-lisp-object)
(define-key hc-completions-map "F" 'hc-completing-insert-file-name)
(define-key hc-completions-map "u" 'hc-completing-insert-according-to-mode)
(define-key hc-completions-map "b" 'hc-completing-insert-buffer-name)
(define-key hc-completions-map "k" 'hc-completing-insert-kill)
(define-key hc-completions-map "y" 'hc-completing-insert-buffer-contents)
(define-key hc-completions-map "i" 'hc-ispell-complete-word)

(defcustom hc-ctrl-backslash-completes-a-la-mode nil
  "Toggle whether `C-\\' runs the `hc-completing-insert-according-to-mode'.
If turned on, `C-\\' runs this function, which turns on highlighting
completion.  This is helpful in the minibuffer, for instance, if the
completion process has stopped and you want to start it up
again---just hit `C-\\'.
If turned off, `C-\\' does nothing."
  :type '(boolean)
  :set (lambda (symbol value)
	 (if value
	     (global-set-key "\C-\\" 'hc-completing-insert-according-to-mode)
	   (global-set-key "\C-\\" nil))
	 (set symbol value))
  :group 'highlight-completion)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set up hc-mode, hc-mode-map, etc. 
;;

(defvar hc-mode nil
  "Non-nil if using Highlight mode as a minor mode")
(make-variable-buffer-local 'hc-mode)
(or (assq 'hc-mode minor-mode-alist)
    (setq minor-mode-alist (cons '(hc-mode " Highlight")
				 minor-mode-alist)))

(defvar hc-mode-map nil
  "Minor mode map for highlighting completion.")
(if hc-mode-map
    nil
  (let ((i 31)
	(map (copy-keymap minibuffer-local-completion-map))
	(meta-map (make-keymap)))
    (set-keymap-parent map nil)
    (substitute-key-definition 'switch-to-completions
			       'hc-switch-to-completions
			       map)
    (substitute-key-definition 'switch-to-completions
			       'hc-switch-to-completions
			       map
			       minibuffer-local-map)
    (substitute-key-definition 'advertised-switch-to-completions
			       'hc-advertised-switch-to-completions
			       map)
    (substitute-key-definition 'advertised-switch-to-completions
			       'hc-advertised-switch-to-completions
			       map
			       minibuffer-local-map)
    (defalias 'hc-advertised-switch-to-completions
      'hc-switch-to-completions)
    (substitute-key-definition 'exit-minibuffer
			       'hc-exit-and-then
			       map)
    (substitute-key-definition 'exit-minibuffer
			       'hc-exit-and-then
			       map
			       minibuffer-local-map)
    (substitute-key-definition 'keyboard-quit
			       'hc-keyboard-quit
			       map)
    (substitute-key-definition 'keyboard-quit
			       'hc-keyboard-quit
			       map
			       minibuffer-local-map)
    (substitute-key-definition 'abort-recursive-edit
			       'hc-exit-and-then
			       map)
    (substitute-key-definition 'abort-recursive-edit
			       'hc-exit-and-then
			       map
			       minibuffer-local-map)
    (substitute-key-definition 'minibuffer-keyboard-quit
			       'hc-keyboard-quit
			       map)
    (substitute-key-definition 'minibuffer-keyboard-quit
			       'hc-keyboard-quit
			       map
			       minibuffer-local-map)
    (substitute-key-definition 'next-history-element
			       'hc-exit-and-then
			       map)
    (substitute-key-definition 'next-history-element
			       'hc-exit-and-then
			       map
			       minibuffer-local-map)
    (substitute-key-definition 'previous-history-element
			       'hc-exit-and-then
			       map)
    (substitute-key-definition 'previous-history-element
			       'hc-exit-and-then
			       map
			       minibuffer-local-map)
    (substitute-key-definition 'minibuffer-complete
			       'hc-try-to-complete
			       map)
    (substitute-key-definition 'minibuffer-completion-help
			       'hc-display-completions
			       map)
    (if (keymapp (lookup-key map [menu-bar minibuf]))
	(progn
	  (define-key map [menu-bar highlight]
	    (cons "Highlight" (make-sparse-keymap "Highlight")))
	  (define-key map [menu-bar highlight tab]
	    '("List Completions" . hc-display-completions))
	  (defalias 'hc-exit-and-then-alias 'hc-exit-and-then)
	  (define-key map [menu-bar highlight quit]
	    '("Quit" . hc-exit-and-then-alias))
	  (define-key map [menu-bar highlight return]
	    '("Enter" . hc-exit-and-then-alias))
	  (define-key map [menu-bar minibuf] 'undefined)))
    (define-key map [escape] meta-map)
    (while (<= (setq i (1+ i)) 126)
      (or (lookup-key map (vector (list 'control i)))
	  (define-key map (vector (list 'control i))
	    'hc-exit-and-then))
      (or (lookup-key map (vector (list 'meta i)))
	  (progn
	    (define-key meta-map (char-to-string i) 'hc-exit-and-then)
	    (define-key map (vector (list 'meta i))
	      'hc-exit-and-then)))
      (unless (string= (char-to-string i) "?")
	(define-key map (char-to-string i) 'hc-self-insert-char)))
    (define-key map [return] 'hc-exit-and-then)
    (define-key map [linefeed] 'hc-exit-and-then)
    (define-key map [(control j)] 'hc-exit-and-then)
    (define-key map [(control g)] 'hc-keyboard-quit)
    (define-key map [(control m)] 'hc-exit-and-then)
    (define-key map (char-to-string 127) 'hc-exit-and-then)
    (define-key map " " 'hc-keep-if-complete)
    (define-key map [space] 'hc-keep-if-complete)
    (define-key map [backspace] 'hc-delete)
    (substitute-key-definition 'delete-backward-char
			       'hc-delete
			       map)
    (substitute-key-definition 'delete-backward-char
			       'hc-delete
			       map
			       global-map)
    (define-key map [tab] 'hc-try-to-complete)
    (define-key map [(control c)] 'hc-quit)
    (setq hc-mode-map map)))

(defvar hc-completion-list-mode-map nil
  "Local map for completion list buffers (for use with highlighting completion).")
(or hc-completion-list-mode-map
    (let ((map (make-sparse-keymap)))
      (define-key map [mouse-2] 'hc-mouse-choose-completion)
      (define-key map [down-mouse-2] nil)
      (define-key map "\C-m" 'hc-choose-completion)
      (define-key map "\e\e\e" 'delete-completion-window)
      (define-key map [left] 'previous-completion)
      (define-key map [right] 'next-completion)
      (setq hc-completion-list-mode-map map)))

(and (boundp 'minor-mode-map-alist)
     (or (assq 'hc-mode minor-mode-map-alist)
	 (setq minor-mode-map-alist
	       (cons (cons 'hc-mode hc-mode-map)
		     minor-mode-map-alist))))
(make-variable-buffer-local 'hc-mode-map)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; miscellaneous variables
;;

(defvar hc-stack nil)
(make-variable-buffer-local 'hc-stack)
(defvar hc-original-text nil)
(make-variable-buffer-local 'hc-original-text)
(defvar hc-highlighted-text nil)
(make-variable-buffer-local 'hc-highlighted-text)
(defvar hc-table nil)
(make-variable-buffer-local 'hc-table)
(defvar hc-predicate nil)
(make-variable-buffer-local 'hc-predicate)
(defvar hc-hook nil)
(make-variable-buffer-local 'hc-hook)
(defvar hc-prev-windows nil)		; state before completions window
(defvar hc-display-filter nil)
(make-variable-buffer-local 'hc-display-filter)
(defvar hc-last-display-time nil)	; "time" measured by stack top eq-ness

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; main functions
;;

(defun hc-completing-insert (table pred init &optional hook message display)
  "Highlight-complete string before point in the buffer, relative to
completion TABLE; allowing only completions that satisfy PRED.  These
are used exactly as they are by `completing-read', which means this:
  TABLE may be an alist, an obarray, or a function-symbol.  For an
alist, PRED applies to the entries (conses).  For an obarray, PRED
applies to the symbols.  A function symbol will be called with a
STRING as first arg, PRED as second arg and third arg nil, t, or
`lambda'; according to third arg, the function is supposed to return
the common completion of STRING, all its completions, or the
truth-value of its completeness.  In particular the function can be
like 'read-file-name-internal, with PRED the name of a directory.
  Third arg INIT is the number of characters before point to complete
as the initial string.  Barf immediately if this is no match.  If
negative, we are resuming, so return nil unless situation at last quit
agrees with buffer before point; then restore that situation.
  Optional arg HOOK is run on successful completion; gets same kind of
argument as PRED, or the complete string if TABLE is a function symbol.
  On entering, message \"Completing <optional arg MESSAGE>...\" is
displayed.
  Optional arg DISPLAY is a function to call on each possible
completion before displaying.  If the DISPLAY function returns nil,
that string is NOT displayed."
  (condition-case nil
      (if (not
	   (or
	    (and (>= init 0)		; starting fresh
		 (prog1			; if so, reset things and be t
		     t
		   (setq hc-stack nil)
		   (let ((grab (buffer-substring-no-properties
				(- (point) init) (point)))
			 (n 0))
		     (if (eq table 'hc-read-file-name-internal)
			 (setq hc-original-text grab
			       grab (hc-expand-file-name grab)
			       init (length grab)))
		     (while (<= n init)
		       (setq hc-stack (cons (substring grab 0 n) hc-stack))
		       (setq n (1+ n)))) ; completions=part grabs
		   (setq hc-table table
			 hc-predicate pred
			 hc-hook hook
			 hc-display-filter display)))
	    ;; see if resuming state is consistent:
	    (and
	     hc-stack
	     (and
	      (>= (point)
		  (+ (point-min) (length (car hc-stack))))
	      (string= (car hc-stack)
		       (buffer-substring-no-properties
			(- (point) (length (car hc-stack)))
			(point))))
	     (eq table hc-table)
	     (equal pred hc-predicate)
	     (equal hook hc-hook)
	     (equal display hc-display-filter))))
	  nil				; trying to resume inconsistently
	(setq hc-mode t)
	(add-hook 'mouse-leave-buffer-hook
		  (function (lambda nil (hc-quit 'mouse))))
	(set-buffer-modified-p (buffer-modified-p)) ; update mode line
	(setq hc-prev-windows (current-window-configuration))
	(if (or (> 0 init)
		(string= (car hc-stack) "") ; don't try to complete ""
		(let ((stat (hc-complete-stack-top "")))
		  (or (stringp stat) (prog1 nil (hc-quit stat)))))
	    (progn
	      (while nil)))		; no-op
	t)				; return t except for bad resume
    (quit (setq unread-command-events
		(list (hc-character-to-event ?\C-g))))))

;; bound to [(control c)], and also called by other functions
(defun hc-quit (arg &optional quick)
  "Exit highlight completion mode.
ARG nil means because of error.  ARG t means because successful.  ARG
other means intentional quit without being complete.  Interactively,
you get the last."
  (interactive '(lambda))
  (remove-hook 'mouse-leave-buffer-hook
	       (function (lambda nil (hc-quit 'mouse))))
  (set-buffer-modified-p (buffer-modified-p)) ; update mode line
  (add-hook 'minibuffer-setup-hook 'highlight-completion-setup)
  (setq hc-mode nil)
  (or arg (ding))			; yell if an error
  (or (eq arg 'mouse)
      (and hc-prev-windows
	   (or (null hc-xemacs-p)
	       (null (minibuffer-window-active-p (minibuffer-window))))
	   (progn
	     (set-window-configuration hc-prev-windows)
	     (setq hc-prev-windows nil))))
  (and (eq arg 'choose)
       (looking-at (regexp-quote (car hc-stack)))
       (forward-char (length (car hc-stack))))
  (if (or (eq arg t) (eq arg 'choose))
      (let ((name (car hc-stack)))
	(setq hc-stack nil)		; no resume after success
	(if hc-hook			; on success, call possible hook
	    (funcall hc-hook
		     (cond ((vectorp hc-table) ; table is an obarray
			    (intern-soft name hc-table))
			   ((listp hc-table) ; table is an alist
			    (assoc name hc-table))
			   (t name))))	; table is a function
	(if (> (current-column) fill-column)  (run-hooks 'auto-fill-hook)))
    ;; unsuccessful quit:
    (setq hc-last-display-time nil))
  (unless (eq arg 'keep)
    (delete-char (length hc-highlighted-text)))
  (hc-unhighlight)
  (setq hc-stack nil)
  (setq hc-highlighted-text nil))

(defun hc-switch-stack-top (str &optional char)
  "Replace top of stack with STR, fixing buffer.  If optional arg CHAR
is 't, then modify highlighting etc as though a printable character
were hit: add just a single character to the stack and re-highlight.
If CHAR is a string, then add all of STR to the stack and highlight
CHAR--this is used by hc-complete-word."
  (let ((inhibit-quit t))
    (hc-unhighlight)
    (if hc-original-text
	(delete-backward-char (length hc-original-text))
      (delete-backward-char (length (car hc-stack))))
    (setq hc-original-text nil)
    (insert str)
    (if hc-highlighted-text
	(delete-char (length hc-highlighted-text)))
    (if char
	(progn
	  (if (stringp char)
	      (progn
		(setq hc-highlighted-text char)
		(save-excursion (insert hc-highlighted-text))
		(hc-highlight (point) (+ (point) (length
						  hc-highlighted-text)))
		(setcar hc-stack str))
	    (if (< (length (car hc-stack)) (length str))
		(progn
		  (forward-char 
		   (- (length (car hc-stack))
		      (length str)))
		  (if (< (point) (point-max))
		      (progn
			(setq hc-highlighted-text
			      (substring str (length (car hc-stack))))
			(hc-highlight (point) (+ (point) (length hc-highlighted-text))))
		    (setq hc-highlighted-text nil))
		  (setcar hc-stack (substring str 0
					      (min 
					       (length (car hc-stack))
					       (length str)))))
	      (setq hc-highlighted-text nil)
	      (setcar hc-stack str))))
      (setq hc-highlighted-text nil)
      (setcar hc-stack str))))

(defvar hc-highlight-face
  (if hc-xemacs-p     
      'zmacs-region
    'region))

(defvar hc-extent nil
  "In XEmacs, extent for the highlighted text.  In GNU Emacs,
overlay for the highlighted text.")

(defun hc-highlight (start end)
  "Highlight text from position START to END in the current buffer."
  (if hc-xemacs-p
      (progn
	(setq hc-extent (make-extent start end (current-buffer)))
	(set-extent-face hc-extent hc-highlight-face))
    (setq hc-extent (make-overlay start end))
    (overlay-put hc-extent 'face hc-highlight-face)))

(defun hc-unhighlight nil
  "Turn off highlighting, if it's on."
  (if hc-xemacs-p
      (progn
	(if (extent-live-p hc-extent)
	    (delete-extent hc-extent)))
    (if hc-extent
	(delete-overlay hc-extent))))

(defun hc-pop-stack nil
  "Pop the stack, fixing buffer."
  (let ((inhibit-quit t)
	(old-str (cadr hc-stack))
	(new-str (car hc-stack))
	str)
    (setq str (hc-complete-stack-top nil t))
    (cond ((eq str t)
	   (setq str hc-highlighted-text)
	   (hc-switch-stack-top old-str)
	   (if (eq (hc-complete-stack-top nil t) t)
	       (hc-switch-stack-top
		old-str 
		(concat (substring new-str (length old-str)) str))))
	  ((stringp str)
	   (hc-switch-stack-top old-str)
	   (if (and (hc-complete-stack-top "")
		    (null (string= old-str (hc-complete-stack-top nil t))))
	       (hc-switch-stack-top
		old-str
		(substring str (length old-str)))))
	  (t
	   (hc-switch-stack-top old-str)))
    (setcdr hc-stack (cddr hc-stack))))

(defun hc-complete-stack-top (more &optional no-modify char)
  "If possible, replace what's on top of stack, and before point, with
the common completion of that extended by MORE, returning that.  Return
nil if no match.  If result is complete and unique, return t.  If
optional arg NO-MODIFY is non-nil, don't modify the stack--just see if
it would be complete.  If optional arg CHAR is non-nil, this was
called after hitting a character (which may affect the placement of
the point when done)."
  (let* ((str (concat (car hc-stack) more))
	 ;; t:use real table. nil:truly no completions. alist:the completions
	 (all (or (symbolp hc-table)
		  (and (> (length str) 0) (= (aref str 0) ? ))
		  (mapcar 'list (all-completions str hc-table hc-predicate))))
	 (try (and all (try-completion
			str
			(if (eq all t) hc-table all)
			(if (eq all t) hc-predicate))))
	 (str (if (eq try t) str try)))
    (and try
	 (progn
	   (or no-modify
	       (hc-switch-stack-top str char))
	   (or (eq try t)
	       (try-completion str
			       (if (eq all t) hc-table all)
			       (if (eq all t) hc-predicate)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; functions bound to keys  (see also hc-quit above)
;;

;; bound to control characters
(defun hc-exit-and-then nil
  "Intentional unsuccessful quit, then put back char to be read again."
  (interactive)
  (setq unread-command-events (list last-command-event))
  (hc-quit 'keep))

(defun hc-keyboard-quit nil
  "Intentional unsuccessful quit, then put back char to be read again."
  (interactive)
  (setq unread-command-events (list last-command-event))
  (hc-quit 'quit))

;; bound to printing characters
(defun hc-self-insert-char nil
  "Update hc-stack, insert this char, and run hc-complete."
  (interactive)
  (setq hc-stack (cons (concat
			(car hc-stack)
			(char-to-string last-command-char))
		       hc-stack))
  (insert last-command-char)
  (hc-complete))

(defun hc-complete nil
  "Complete as far as possible.  If no valid completions, quit.
If no valid completions and the customizable variable hc-clean-up is
non-nil, then delete characters until a valid string remains."
  (interactive)
  (let ((top (hc-complete-stack-top "" nil t)))
    (cond ((eq top t)
	   (if (string= top (car hc-stack))
	       (hc-quit t)))
	  ((null top)
	   (hc-unhighlight)
	   (delete-char (length hc-highlighted-text))
	   (setq hc-highlighted-text nil)
	   (hc-quit 'quit)))))

;; bound to [space]
(defun hc-keep-if-complete nil
  "Quit with success if current stack top is complete.  Otherwise
insert a space."
  (interactive)
  (if hc-highlighted-text
      (hc-complete-word)
    (let (top)
      (if (setq top (hc-complete-stack-top " " nil t))
	  (hc-switch-stack-top (concat (car hc-stack) " ")
			       (substring hc-highlighted-text 1))
	(setq top (hc-complete-stack-top "" nil t))
	(if (eq top t)
	    (hc-quit t)
	  (hc-try-to-complete))))))

(defun hc-complete-word nil
  "Complete at most one word.  After one word is completed, a space or
hyphen is added, provided that matches some possible completion."
  (let ((old (car hc-stack))
	(top (hc-complete-stack-top "" t))
	(old-point (point))
	diff)
    (if (string= old top)
	(hc-try-to-complete)
      (save-excursion
	(goto-char old-point)
	(forward-word 1)
	(if (looking-at hc-word-connectors)
	    (forward-char 1))
	(setq diff (- (point) old-point)))
      (if (and (eq top t)
	       (<= (+ (length (concat old hc-highlighted-text))
		      (hc-minibuffer-prompt-width))
		   (+ diff old-point)))
	  (progn
	    (if hc-highlighted-text
		(forward-char (length hc-highlighted-text)))
	    (hc-quit 'keep))
	(setq top (concat old hc-highlighted-text))
	(if (< diff (length hc-highlighted-text))
	    (hc-switch-stack-top (substring top 0 (+ (length old) diff))
				 (substring top (+ (length old) diff)))
	  (hc-switch-stack-top top))))))

;; bound to [backspace]
(defun hc-delete nil
  "Go back one completion unit.  If there is no previous unit, quit quietly."
  (interactive)
  (if (null (cdr hc-stack)) (hc-quit 'keep)
    (hc-pop-stack)))

;; bound to [tab]
(defun hc-try-to-complete nil
  "Try to complete.  Complete as far as possible.
If there are choices, pop up buffer with list.  If there are no valid
completions, ding."
  (interactive)
  (let ((old (car hc-stack))
	(top (hc-complete-stack-top "" t)))
    (cond ((string= old top)
	   (hc-display-completions))
	  ((eq top t)
	   (hc-complete-stack-top "" nil nil)
	   (hc-quit t))
	  ((null top)
	   (ding))
	  (t
	   (hc-switch-stack-top top)
	   (if (eq t (hc-complete-stack-top "" t))
	       (progn
		 (hc-quit t)
		 (hc-complete-stack-top "" nil t))
	     (hc-complete-stack-top "" nil t))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; stuff for completions buffer.
;;

(defvar hc-completion-buffer-name " *Completions*"
  "Name of buffer in which to display list of completions")

(defun hc-display-completions (&optional jump)
  "Show possible completions, just like `minibuffer-completion-help'"
  (interactive)
  (if (and (not (equal jump 'jump))
	   (equal hc-last-display-time (car hc-stack))
	   (get-buffer-window hc-completion-buffer-name))
      (let ((ow (selected-window))
	    (w (get-buffer-window hc-completion-buffer-name)))
	(select-window w)
	(condition-case nil
	    (if (<= (point-max) (window-end))
		(goto-char (point-min))
	      (scroll-up))
	  (error (goto-char (point-min))))
	(select-window ow))
    (setq hc-last-display-time (car hc-stack))
    (let ((all (all-completions (car hc-stack) hc-table hc-predicate))
	  results ans)
      (if (not (fboundp hc-display-filter)) nil
	(while all
	  (setq ans (funcall hc-display-filter (car all)))
	  (and ans
	       (setq results (cons ans results)))
	  (setq all (cdr all)))
	(setq all (nreverse results)))
      (if all
	  (hc-display-completions-internal all)))))

(defun hc-switch-to-completions ()
  "Select the completion list window."
  (interactive)
  ;; Make sure we have a completions window.
  (hc-display-completions 'jump)
  (select-window (get-buffer-window hc-completion-buffer-name))
  (goto-char (point-min))
  (search-forward "\n\n")
  (forward-line 1))

(defun hc-choose-completion ()
  "Choose the completion that point is in or next to.
Just like choose-completion, except this calls
hc-choose-completion-string instead of choose-completion-string."
  (interactive)
  (let (beg end completion (buffer completion-reference-buffer)
	(base-size completion-base-size))
    (if (and (not (eobp)) (get-text-property (point) 'mouse-face))
	(setq end (point) beg (1+ (point))))
    (if (and (not (bobp)) (get-text-property (1- (point)) 'mouse-face))
	(setq end (1- (point)) beg (point)))
    (if (null beg)
	(error "No completion here"))
    (setq beg (previous-single-property-change beg 'mouse-face))
    (setq end (or (next-single-property-change end 'mouse-face)
		  (point-max)))
    (setq completion (buffer-substring-no-properties beg end))
    (let ((owindow (selected-window)))
      (if (and (one-window-p t 'selected-frame)
	       (window-dedicated-p (selected-window)))
	  ;; This is a special buffer's frame
	  (iconify-frame (selected-frame))
	(or (window-dedicated-p (selected-window))
	    (bury-buffer)))
      (select-window owindow))
    (hc-choose-completion-string completion buffer base-size)))

(defun hc-mouse-choose-completion (event)
  "Click on an alternative in the `*Completions*' buffer to choose it.
Just like mouse-choose-completion, except this calls
hc-choose-completion-string instead of choose-completion-string."
  (interactive "e")
  ;; Give temporary modes such as isearch a chance to turn off.
  (run-hooks 'mouse-leave-buffer-hook)
  (let ((buffer (window-buffer))
        choice
	base-size)
    (save-excursion
      (set-buffer (window-buffer (posn-window (event-start event))))
      (if completion-reference-buffer
	  (setq buffer completion-reference-buffer))
      (setq base-size completion-base-size)
      (save-excursion
	(goto-char (posn-point (event-start event)))
	(let (beg end)
	  (if (and (not (eobp)) (get-text-property (point) 'mouse-face))
	      (setq end (point) beg (1+ (point))))
	  (if (null beg)
	      (error "No completion here"))
	  (setq beg (previous-single-property-change beg 'mouse-face))
	  (setq end (or (next-single-property-change end 'mouse-face)
			(point-max)))
	  (setq choice (buffer-substring-no-properties beg end)))))
    (let ((owindow (selected-window)))
      (select-window (posn-window (event-start event)))
      (if (and (one-window-p t 'selected-frame)
	       (window-dedicated-p (selected-window)))
	  ;; This is a special buffer's frame
	  (iconify-frame (selected-frame))
	(or (window-dedicated-p (selected-window))
	    (bury-buffer)))
      (select-window owindow))
    (hc-choose-completion-string choice buffer base-size)))

(defun hc-choose-completion-string (choice &optional buffer base-size)
  "Like choose-completion-string (from simple.el), with some stuff to
make it work well (it says here) with highlighting completion."
  (let ((buffer (or buffer completion-reference-buffer)))
    ;; If BUFFER is a minibuffer, barf unless it's the currently
    ;; active minibuffer.
    (if (and (string-match "\\` \\*Minibuf-[0-9]+\\*\\'" (buffer-name buffer))
	     (or (not (active-minibuffer-window))
		 (not (equal buffer
			     (window-buffer (active-minibuffer-window))))))
	(error "Minibuffer is not active for completion")
      ;; Insert the completion into the buffer where completion was requested.
      (set-buffer buffer)
      (if base-size
	  (delete-region (+ base-size (point-min)) (point))
	(choose-completion-delete-max-match choice))
      (insert choice)
;       (remove-text-properties (- (point) (length choice)) (point)
; 			      '(mouse-face nil))
      (if (string-match (regexp-quote (car hc-stack)) choice)
	  (setq hc-stack (cons choice hc-stack))
	(setq hc-stack (cons (concat (car hc-stack) choice)
			     hc-stack)))
      ;; choice may be part of a multiline string (e.g. in ultra-tex),
      ;; so complete
      (if (hc-complete-stack-top "" t)
	  (hc-complete-stack-top ""))
      ;; Update point in the window that BUFFER is showing in.
      (let ((window (get-buffer-window buffer t)))
	(set-window-point window (point)))
      ;; If completing for the minibuffer, exit it with this choice.
      (if (and (equal buffer (window-buffer (minibuffer-window)))
	       minibuffer-completion-table)
	   ;; If this is reading a file name, and the file name chosen
	   ;; is a directory, don't exit the minibuffer.
	  (if (and (eq minibuffer-completion-table 'read-file-name-internal)
		   (file-directory-p (buffer-string)))
	      (select-window (active-minibuffer-window))
	    (exit-minibuffer))
	(and hc-prev-windows
	       (hc-quit 'choose))))))

(defvar hc-completion-fixup-function nil
  "A function to customize how completions are identified in completion lists.
`hc-completion-setup-function' calls this function with no arguments
each time it has found what it thinks is one completion.
Point is at the end of the completion in the completion list buffer.
If this function moves point, it can alter the end of that completion.")

(defvar hc-completion-message-function
  'hc-completion-default-message-function 
  "A function to give the text at the top of the *Completions*
buffer.  Called by `hc-completion-setup-function'.")

(defun hc-completion-default-message-function nil
  "Standard message function for hc-completion-setup-function."
  (if (hc-window-system)
      (insert (substitute-command-keys
	       "Click \\[hc-mouse-choose-completion] on a completion to select it.\n")))
  (insert (substitute-command-keys
	   "In this buffer, type \\[hc-choose-completion] to \
select the completion near point.\n\n"))
  (forward-line 1))

(defun hc-completion-setup-function ()
  "Like completion-setup-function (from simple.el), except with
slightly different messages."
  (save-excursion
    (let ((mainbuf (current-buffer)))
      (set-buffer standard-output)
      (completion-list-mode)
      (make-local-variable 'completion-reference-buffer)
      (setq completion-reference-buffer mainbuf)
      ;; The value 0 is right in most cases, but not for file name completion.
      ;; so this has to be turned off.
      ;;      (setq completion-base-size 0)
      (goto-char (point-min))
      (if hc-completion-message-function
	  (funcall hc-completion-message-function))
      (while (re-search-forward "[^ \t\n]+\\( [^ \t\n]+\\)*" nil t)
	(let ((beg (match-beginning 0))
	      (end (point)))
	  (if hc-completion-fixup-function
	      (funcall hc-completion-fixup-function))
	  (put-text-property beg (point) 'mouse-face 'highlight)
	  (goto-char end))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; utilities
;;

(defun word-grabber nil
  "Move point to just after the word point is in or after, and
return length of word."
  (skip-chars-forward "^ \n\t\f\"`'();{}")
  (- (point) (save-excursion (skip-chars-backward "^ \n\t\f\"`'();{}")
			     (point))))

(defun point-adjust-hook (arg)
  "Intended to be used when hc-table is an alist whose elements look
like `(<string> <number> . <hook>)'. Move point forward <number>
chars, and then run <hook> (if non-nil)."
  (forward-char (car (cdr arg)))
  (if (cdr (cdr arg)) (funcall (cdr (cdr arg)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; entry points for completion on various things.  see also
;; completing-insert-buffer-contents below.
;;

(defun hc-completing-insert-lisp-object nil
  "Complete lisp object in buffer at point."
  (interactive)
  (hc-completing-insert obarray nil (word-grabber) nil "lisp objects"))

(defun hc-completing-insert-lisp-function nil
  "Complete lisp object in buffer at point."
  (interactive)
  (hc-completing-insert obarray 'fboundp (word-grabber) nil "functions"))

(defun hc-completing-insert-lisp-variable nil
  "Complete lisp object in buffer at point."
  (interactive)
  (hc-completing-insert obarray 'boundp (word-grabber) nil "variables"))

(defun hc-completing-insert-buffer-name nil
  "Complete buffer name in buffer at point."
  (interactive)
  (hc-completing-insert (mapcar (function (lambda (x) (list (buffer-name x))))
			     (buffer-list))
		     nil (word-grabber) nil "buffer names"))

(defun hc-completing-insert-kill nil
  "Complete something from the kill ring in buffer at point."
  (interactive)
  (hc-completing-insert
   (mapcar 'list
	   (apply 'append
		  (mapcar
		   (function
		    (lambda (x)
		      (cons x (and (string-match "\\s-+" x)
				   (list (substring x (match-end 0)))))))
		   kill-ring)))
   nil 0 nil "recent kills"))

(defvar hc-completing-insert-function nil
  "Function to be called by M-x completing-insert-according-to-mode, 
if non-nil")
(make-variable-buffer-local 'completing-insert-function)

(defun hc-ispell-complete-word nil
  "Complete the current word using ispell."
  (interactive)
  (hc-completing-insert 'hc-lookup-words nil
			(word-grabber) nil
			"words"))

(defun hc-lookup-words (string pred flag)
  "Complete STRING a la ispell-complete-word.  PRED will always be 
nil--it's there for compatibility purposes.  If FLAG is non-nil, return 
all possible completions.  If FLAG is nil, complete as far as
possible.  If there is a unique completion, return it.  If STRING
equals the unique completion, return t."
  (require 'ispell)
  (let ((word-list (lookup-words string))
	(guess string))
    (if flag word-list
      (if (zerop (length word-list))
	  nil
	(if (= 1 (length word-list))
	    (or (string= string (car word-list))
		(car word-list))
	  (while (and (not (string= guess (car word-list)))
		      (not (member nil
				   (mapcar
			      (function
			       (lambda (word)
				 (string-match (regexp-quote
						(substring
						 (car word-list)
						 0 (1+ (length
							guess))))
					       word)))
			      word-list))))
	    (setq guess (substring (car word-list)
				 0 (1+ (length guess)))))
	  guess)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; completion a la mode
;;

(defun hc-completing-insert-according-to-mode nil
  "Start highlighting completion.  If possible, resumes stopped completion.  
Otherwise, in the minibuffer, uses its table and predicate (slightly
modified for file name reading).  Failing that, calls
`hc-completing-insert-function' if the mode has it set.  Final default
is lisp-object completion."
  (interactive)
  (cond ((hc-completing-insert hc-table hc-predicate -1 hc-hook) nil)
	((and (minibuffer-window-active-p (minibuffer-window))
	      minibuffer-completion-table)
	 (let* ((table (if (eq minibuffer-completion-table
			       'read-file-name-internal)
			   'hc-read-file-name-internal
			 minibuffer-completion-table))
		(message
		 (cond ((eq table 'hc-read-file-name-internal)
			"file names")
		       ((and (listp table) (bufferp (cdr (car table))))
			"buffers")
		       ((eq obarray table)
			(cond ((not
				(and (boundp
				      'minibuffer-completion-predicate)
				     minibuffer-completion-predicate))
			       "lisp objects")
			      ((eq 'fboundp minibuffer-completion-predicate)
			       "functions")
			      ((eq 'commandp minibuffer-completion-predicate)
			       "commands")
			      ((eq 'boundp minibuffer-completion-predicate)
			       "variables")
			      ((eq 'user-variable-p
				   minibuffer-completion-predicate)
			       "user variables")))
		       (t "something")))
		(display (and (eq table 'hc-read-file-name-internal)
			      'hc-file-display-filter)))
	   (or (hc-completing-insert table minibuffer-completion-predicate
				  -1)
	       (hc-completing-insert table minibuffer-completion-predicate
				  (progn (goto-char (point-max))
					 (- (point) (point-min)))
				  nil message display))))
	;; I moved this here to make existing minibuffer
	;; completion info take precedence over stopped completion.
	;; -- Nick Reingold 5/24/92
	((hc-completing-insert hc-table hc-predicate -1
			    hc-hook hc-display-filter) nil)
	(hc-completing-insert-function
	 (call-interactively hc-completing-insert-function))
	(t (hc-completing-insert-lisp-object))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; turning on highlighting
;;

;; Customize the variable highlight-completion-mode, to turn on
;; highlighting completion.
(defun highlight-completion-setup ()
  (interactive)
  (let ((pred minibuffer-completion-predicate)
	complete-p message table display)
    (cond ((eq minibuffer-history-variable 'file-name-history)
	   (setq complete-p (hc-complete-p 'files)
		 message "file names"
		 table 'hc-read-file-name-internal
		 pred (hc-expand-file-name pred)
		 display 'hc-file-display-filter))
	  ((eq 'fboundp minibuffer-completion-predicate)
	   (setq complete-p (hc-complete-p 'functions)
		 message "functions"
		 table obarray))
	  ((eq 'commandp minibuffer-completion-predicate)
	   (setq complete-p (hc-complete-p 'commands)
		 message "commands"
		 table obarray))
	  ((eq 'boundp minibuffer-completion-predicate)
	   (setq complete-p (hc-complete-p 'variables)
		 message "variables"
		 table obarray))
	  ((eq 'user-variable-p minibuffer-completion-predicate)
	   (setq complete-p (hc-complete-p 'user-variables)
		 message "user variables"
		 table obarray))
	  ((and (eq minibuffer-completion-table obarray)
		(not (and (boundp 'minibuffer-completion-predicate)
			  minibuffer-completion-predicate)))
	   (setq complete-p (hc-complete-p 'lisp-objects)
		 message "lisp objects"
		 table obarray))
	  ((eq 'Info-complete-menu-item minibuffer-completion-table)
	   (setq complete-p (hc-complete-p 'info-menu-items)
		 message "Info menu items"
		 table minibuffer-completion-table))
	  ((eq minibuffer-history-variable 'query-replace-history)
	   (setq complete-p (hc-complete-p 'query)
		 message "buffer contents"
		 table 'hc-buffer-completion-internal
		 pred (car (cdr (buffer-list)))))
	  ((and (listp minibuffer-completion-table)
		(listp (car minibuffer-completion-table))
		(bufferp (cdr (car minibuffer-completion-table))))
	   (setq complete-p (hc-complete-p 'buffers)
		 message "buffers"
		 table minibuffer-completion-table))
	  (minibuffer-completion-table
	   (setq complete-p (hc-complete-p 'misc)
		 message "something"
		 table minibuffer-completion-table)))
    (if (and highlight-completion-mode complete-p)
	(progn
	  (or (hc-completing-insert table pred -1)
	      (hc-completing-insert table pred
				 (progn (goto-char (point-max))
					(- (point)
					   (point-min)
					   (hc-minibuffer-prompt-width)))
				 nil message display))))))

(defun query-replace-read-args (string regexp-flag)
  (hc-query-replace-read-args string regexp-flag))

(defun hc-query-replace-read-args (string regexp-flag)
  (let (from to)
    (if query-replace-interactive
	(setq from (car (if regexp-flag regexp-search-ring search-ring)))
      (setq from (read-from-minibuffer (format "%s: " string)
				       nil nil nil
				       'query-replace-history)))
    (remove-hook 'minibuffer-setup-hook 'highlight-completion-setup)
    (condition-case ()
	(setq to (read-from-minibuffer (format "%s %s with: " string from)
				       nil nil nil
				       'query-replace-history))
	(quit
	 (add-hook 'minibuffer-setup-hook 'highlight-completion-setup)
	 (error "Quit")))
    (add-hook 'minibuffer-setup-hook 'highlight-completion-setup)
    (list from to current-prefix-arg)))

(defun hc-complete-p (arg)
  "Non-nil if one should do highlighting completion in environment ARG,
as determined by the value of the variable highlight-completion-list."
  (cdr (assoc arg highlight-completion-list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; file completion stuff
;;

(defun hc-completing-insert-file-name (&optional dir init)
  "Complete file name in buffer at point.  Non-interactively, use directory
DIR (nil for current default-directory); start with INIT chars before point."
  (interactive (list nil (word-grabber)))
  (hc-completing-insert 'hc-read-file-name-internal
		     (or dir default-directory) (or init 0)
		     nil "file names" 'hc-file-display-filter))

(defconst hc-literal-file-regexp
  "\\(\\(^\\|/\\)\\(~[^/]*\\|\\.\\.?\\)\\|\\${?[a-zA-Z0-9]*\\)$"
  "Regexp for file names which don't get completed, yet.")
(defconst hc-expand-this-file-regexp
  "\\(\\${[a-zA-Z0-9]*}\\|\\(^\\|/\\)\\.\\.?/\\)$"
  "Regexp for file names which get expanded before completion.")

(defun hc-read-file-name-internal (str dir action)
  "\"Internal\" subroutine for `completing-insert-file-name'. Do not
call this."
  (let (str-dir real-str)
    (cond ((and (null action) (string-match hc-literal-file-regexp str))
	   str)
	  ((progn (setq real-str (hc-expand-file-name
				  (substitute-in-file-name str) dir)
			str-dir (file-name-directory real-str))
		  (not (file-directory-p str-dir)))
	   nil)
	  ((eq action t)
	   (mapcar (function (lambda (x)
			       (expand-file-name x str-dir)))
		   (read-file-name-internal str dir action)))
	  ((file-directory-p real-str)
	   real-str)
	  (t
	   (let* ((exp (string-match hc-expand-this-file-regexp str))
		  (str (if exp real-str str))
		  (ans (read-file-name-internal str dir action)))
	     (if (null action)
		 (if (and exp (eq ans t)) str ans)
	       (and (not exp) ans)))))))

(defun hc-expand-file-name (name &optional dir)
  "Like expand-file-name, except that if first arg NAME is something
like `bozo/.' then return `bozo/'.  expand-file-name, in contrast,
would return `bozo'."
  (concat (expand-file-name name dir)
	  (if (or (and (< 1 (length name))
			  (string= "/." (substring name -2)))
		  (and (< 2 (length name))
			  (string= "/.." (substring name -3))))
	      "/")))

(defun hc-file-display-filter (fn)
  (cond ((string-match hc-ignored-file-extensions fn)
	 nil)
	((file-directory-p fn)
	 (let ((dir (if (file-directory-p (car hc-stack))
			(car hc-stack)
		      (directory-file-name (car hc-stack)))))
	   (if (string= fn (hc-expand-file-name "./" dir))
	       "./"
	     (if (string= fn (hc-expand-file-name "../" dir))
		 "../"
	       (concat (file-name-nondirectory (directory-file-name fn))
		       "/")))))
	(t (file-name-nondirectory fn))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; buffer completion stuff.
;;
;; This section (which used to be the file bufcomp.el) adapts
;; highlighting completion to complete on reasonably balanced substrings
;; of a buffer.  The main entry point is
;;   (hc-completing-insert-buffer-contents BUF)
;; where BUF is interactively the current buffer or, with arg, a buffer
;; specified by the user.

(defun hc-buffer-sub-hunk (start end)
  "Return substring of current buffer from START at least up to END, extended
sufficiently to be balanced if possible, but in any case not to include
more than one non-blank line past END."
  (save-excursion
    (goto-char end)
    (skip-chars-forward "\n")
    (skip-chars-forward "^\n")
    (save-restriction
      (narrow-to-region start (point))
      (goto-char start)
      (let (n)
	(while (< (point) end)
	  (condition-case what (goto-char (setq n (scan-sexps (point) 1)))
	    (error (if (or (null n) (= ?U (aref (car (cdr what)) 0)))
		       (goto-char (point-max))
		     (forward-char 1))))))
      (buffer-substring-no-properties start (point)))))

(defvar hc-buf-comp-internal-last nil)	; last return of a try-type call

(defun hc-buffer-completion-internal (str buf action)
  "Internal subroutine for `hc-completing-insert-buffer-contents'.  Do
not call this.
  Used like `read-file-name-internal' but for completing STR as a
substring of buffer BUF.  Completing with space as last char matches
anything, as long as the match is unique.  ACTION nil means common
part of proper extensions of STR, up to next sexp boundary, t means
list of some of these extensions.  Other means return nil (no
substring is ever considered complete)."
  (and
   (memq action '(nil t))		; never complete so keep is disabled
   (save-window-excursion
     (let* ((obuf (prog1 (current-buffer) (set-buffer buf)))
	    inhibit-quit case-fold-search find (l (length str)))
       (prog2
	   (if (eq buf obuf)		; hide completion in progress
	       (progn (setq inhibit-quit t)
		      (delete-backward-char (length (car hc-stack)))))
	   (if action
	       (let ((oball (make-vector 37 0)) (n 700))
		 (save-excursion
		   (goto-char (point-min))
		   (while (and (< 0 (setq n (1- n)))
			       (search-forward str nil t))
		     (intern (hc-buffer-sub-hunk (match-beginning 0)
					      (min (point-max) (1+ (point))))
			     oball))
		   (if (< 0 n) (all-completions "" oball)
		     '("Completions too numerous to mention!"))))
	     (setq			; this arranges that identical repeats
	      hc-buf-comp-internal-last	; of a try call do no work, speeding
	      (if (eq str hc-buf-comp-internal-last) str ; up hc-complete-stack-top.
		(save-excursion
		  (goto-char (point-min))
		  (or
		   (and
		    (search-forward str nil t)
		    (setq find (hc-buffer-sub-hunk (match-beginning 0) (point)))
		    (progn
		      (while (and (> (length find) l) (search-forward str nil t))
			(setq find (try-completion
				    ""
				    (list (list find)
					  (list
					   (buffer-substring-no-properties
					    (match-beginning 0)
					    (min (point-max)
						 (+ (match-beginning 0)
						    (length find)))))))))
		      find))
		   (and (string-match "\\s-" (substring str -1))
			(search-forward (setq str (substring str 0 -1)) nil t)
			(setq find (hc-buffer-sub-hunk (match-beginning 0)
						    (min (point-max)
							 (1+ (point)))))
			(progn
			  (setq l (1- l))
			  (while (and (> (length find) l)
				      (search-forward str nil t))
			    (setq find (try-completion
					""
					(list
					 (list find)
					 (list
					  (buffer-substring-no-properties
					   (match-beginning 0)
					   (min (point-max)
						(+ (match-beginning 0)
						   (length find)))))))))
			  (and (> (length find) l) find))))))))
	 ;; unhide:
	 (if (eq buf obuf) (insert (car hc-stack))))))))

(defun hc-completing-insert-buffer-contents  (&optional buf)
  "Complete on substrings of BUF extending to sexp boundaries.  String is
never complete, so exit with C-c.  Once unique, space means match more.
Interactively, with arg, ask for the buffer, else current buffer."
  (interactive "P")
  (if (and (interactive-p) buf)
      (setq buf (read-buffer "Complete from buffer: ")))
  (setq buf (or buf (current-buffer)))
  (hc-completing-insert 'hc-buffer-completion-internal buf 0 nil "buffer contents"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions that depend on the version of Emacs.
;;

(defun hc-character-to-event (char)
  "Convert a character CHAR into an event.  This just returns CHAR
in GNU Emacs 19 or 20.  In XEmacs, it calls character-to-event."
  (if (fboundp 'character-to-event)
      (character-to-event char)
    char))

(defun hc-window-system ()
  "Non-nil if using x windows"
  (if (fboundp 'console-type)
      (eq (console-type) 'x)
    (eq window-system 'x)))

(defun hc-minibuffer-prompt-width ()
  "0 unless using GNU Emacs 21, in which case minibuffer-prompt-width"
  (if hc-emacs-21-p
      (minibuffer-prompt-width)
    0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; More stuff dependent on the version of emacs.  This is all related
;; to displaying completions.
;;

(defvar hc-completion-default-help-string
  '(concat
    (if (device-on-window-system-p)
	(substitute-command-keys
	 "Click \\<hc-completion-list-mode-map>\\[hc-mouse-choose-completion] on a completion to select it.\n") "")
    (substitute-command-keys
     "Type \\<hc-mode-map>\\[hc-advertised-switch-to-completions] or \\<hc-mode-map>\\[hc-advertised-switch-to-completions] to move to this buffer, for keyboard selection.\n
In this buffer, type \\<hc-completion-list-mode-map>\\[hc-choose-completion] to
select the completion near point.\n\n"))
  "For use with XEmacs only.
Form the evaluate to get a help string for completion lists.
This string is inserted at the beginning of the buffer.
See `display-completion-list'.")

(defun hc-display-completions-internal (all)
  "Run display-completion-list with appropriate modifications,
depending on whether we're using XEmacs or not."
  (if hc-xemacs-p
      (with-output-to-temp-buffer hc-completion-buffer-name
	(display-completion-list
	 (sort all 'string<)
	 :help-string hc-completion-default-help-string))
    (let ((old-hook completion-setup-hook)
	  (old-map completion-list-mode-map))
      (setq completion-setup-hook
	    'hc-completion-setup-function
	    completion-list-mode-map
	    hc-completion-list-mode-map)
      (with-output-to-temp-buffer hc-completion-buffer-name
	(display-completion-list
	 (sort all 'string<)))
      (setq completion-setup-hook old-hook
	    completion-list-mode-map old-map))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'highlight-completion)

;;; highlight-completion.el ends here
