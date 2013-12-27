;;; framepop.el --- display temporary buffers in a dedicated frame

;; Copyright (C) 1993, 1995 Free Software Foundation, Inc.
;; Copyright (C) 2003 Peter S Galbraith

;; Author: David Smith <D.M.Smith@lancaster.ac.uk>
;; Maintainer: Peter S Galbraith <psg@debian.org>
;;  (I'll assign copyright to the FSF if requested. Send patches only if
;;  you are willing to do the same.  Contact me if you want to *actively*
;;  maintain this file.)
;; Created: 8 Oct 1993 by David Smith
;; Modified: $Date: 2003/10/15 14:16:54 $
;; Version: $Revision: 1.11 $
;; RCS-Id: $Id: framepop.el,v 1.11 2003/10/15 14:16:54 psg Exp $

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; 1. PURPOSE

;; Defines temp-buffer-show-function to display buffers in a dedicated
;; temporary frame (and so requires a display which can accomodate
;; separate frames). The frame is automatically shrink-wrapped to just
;; contain the buffer (restricted to a maximum and minimum
;; size). Buffers thus affected include *Help*, completion buffers and
;; buffer listings.
;;
;; Commands are provided for manipulating the FramePop frame:
;; scrolling, resizing, window manager functions, and also a facility
;; for copying the displayed buffer. You need never lose that handy
;; *Help* buffer again!
;;
;; Framepop is orthogonal to the Emacs' special-display-buffers feature;
;; you can use both at the same time if you so desire.  You can make
;; special-display buffers appear in the FramePop frame as well, if you
;; wish; see below.

;; 2. INSTALLATION

;; To make use of this package, place this file in your load-path,
;; byte-compile it *before* loading it (or do M-x ad-deactivate-all
;; first), and add the following line to your .emacs:
;;
;; (when window-system
;;   (require 'framepop)
;;   (framepop-enable))
;;
;;  or, alternatively, load the file once, customize the variable
;;  `framepop-enable' and save the setting.  The file will then be loaded
;;  automatically and enabled on startup.

;; Several user functions are defined and stored in framepop-map. You
;; will probably want to bind these to keys. See `M-x customize-variable RET
;; framepop-enable-keybinding RET'
;;
;; Type M-x framepop-display-help (bound to `?' in framepop-map) for
;; more information.

;; 3. CUSTOMIZATION

;; See `M-x customize-group RET framepop'.

;; The maximum and minimum height of the framepop buffer are
;; determined by the user options `framepop-max-frame-size' and
;; `framepop-min-frame-size'.

;; The variable `framepop-frame-parameters' holds the FramePop frame
;; parameters. You can define colours, fonts and positions for the
;; FramePop frame here. For example:
;;
;;   (setq framepop-frame-parameters
;;      '((name . nil)                     ; use buffer name
;;        (unsplittable . t)               ; always include this
;;        (menu-bar-lines . 0)             ; no menu bar
;;        (minibuffer . nil)               ;    or minubuffer
;;        (left . -1)                      ; top left corner of screen,
;;        (top . 30)                       ;    away from my main frame
;;        (width . 71)                     ; narrower, so it fits nicely
;;        (background-color . "orchid4")   ; I like purple. So sue me.
;;        (foreground-color . "cornsilk")
;;        (font . "-*-courier-bold-o-*-*-12-*-*-*-m-*-*-*")))
;;
;; But you may use the customize interface to edit `framepop-frame-parameters'.

;; By default, only temporary buffers (which call
;; `temp-buffer-show-function') are displayed in the FramePop frame.  To
;; make other buffers also appear in the selected frame, the easiest thing
;; to do is make buffers listed in special-display-buffer-names or
;; special-display-regexps appear in the framepop frame by adding the
;; following to your .emacs file:
;;
;; (setq special-display-function 'framepop-special-display)
;;
;; Here's a suggestion for some buffers to use this feature on:
;; 
;; (setq special-display-buffer-names '("*Shell Command Output*"
;;   "*grep*" "*compilation*"))

;; Alternatively (if you want to keep the special-display feature
;; separate from framepop) you can use the function `framepop-wrap'.

;; There are lots of nifty things that can be done with the advice
;; package to make FramePop work that much better. Many such things
;; will be done for you automatically if you
;;
;;   (require 'advice)
;;
;; before loading the framepop package (or customize the variable
;; `framepop-use-advice'. See the end of this file ("Customizations using
;; advice") for more details.

;; Buffer names listed in the variable framepop-do-not-display-list
;; will not be displayed in the framepop-frame by default.
;;
;; You may set the variable `framepop-auto-resize' to t to have the
;; FramePop frame automatically resize to accomodate buffers which
;; change size. If you do not, initially empty buffers (which are
;; likely to grow) get a FramePop frame of full size.

;; For elisp hackers:
;;
;; Alternatively, for greater control over the behaviour of the
;; framepop frame, you can redefine the variable `framepop-lines' to a
;; lambda expression which will return the desired height of a buffer
;; to be displayed in the framepop frame. It may also return nil,
;; meaning that the buffer should not be displayed in the FramePop
;; frame, but in an ordinary window instead. The default value of this
;; lambda expression is the number of lines in the buffer, except that
;; empty buffers and compilation buffers (both of which are likely to
;; grow) get full size. You may wish to disable this feature, or
;; perhaps make other constraints based on buffer mode, etc. For
;; example, placing the following in your .emacs will force the
;; framepop frame to have as many lines as the buffer being displayed
;; provided it is not the *Completions* buffer (which will not be
;; displayed in the FramePop frame at all):
;;
;;   (setq framepop-lines
;;     '(lambda (buf)
;;   	 (if (string= (buffer-name buf) "*Completions*") nil
;;   	   (save-excursion
;;   	     (set-buffer buf)
;;   	     (+ (count-lines (point-min) (point-max)) 1)))))
;;         
;; This will cause empty buffers to have the minimum height, because
;; the maximum and minimum frame sizes (as specified in
;; `framepop-max-frame-size' and `framepop-min-frame-size') are enforced
;; independently of `framepop-lines'. To get around this, define advice
;; around the function `framepop-frame-height'.
;;
;; The default value of `framepop-lines' is framepop-default-lines.

;; BUGS:
;;
;; 1. Completion in comint buffers doesn't work very well unless
;;    comint-dynamic-show-completions is given a lobotomy.  NB: this
;;    happens by default if advice is loaded.
;; 2. I'd like to redefine framepop-wrap so that it saves the window
;;    configuration, displays the requested buffer in the
;;    framepop-frame, and then restores the window configuration. But
;;    the job of framepop-wrap is better done by
;;    special-display-buffer-names, so I shan't bother.

;;; History:
;; 
;;  October 2003 - Peter S Galbraith
;;   Since David Smith no longer uses Emacs, I have decided to do a bit of
;;   work to update the file before packaging it for Debian.  This involved
;;   mostly switching to the customize interface.  It's still very much
;;   David's work.

;;; Code:

(defconst framepop-version (substring "$Revision: 1.11 $" 11 -2)
  "The revision number of the framepop package.

The complete RCS ID is:
$Id: framepop.el,v 1.11 2003/10/15 14:16:54 psg Exp $")

;;; Customizable variables at end

(defgroup framepop nil
  "Display temporary buffers in a dedicated frame."
  :group 'frames)

;;;###autoload
(defun framepop-disable nil
  "Disable automatic pop-up temporary windows."
  (interactive)
  (setq temp-buffer-show-function nil))

;;;###autoload
(defun framepop-enable nil
  "Enable automatic pop-up temporary windows."
  (interactive)
  (if (and temp-buffer-show-function
	   (not (eq temp-buffer-show-function 'framepop-display-buffer)))
      (message "Warning: framepop.el has redefined temp-buffer-show-function"))
  (setq temp-buffer-show-function 'framepop-display-buffer))

(defcustom framepop-enable nil
  "Whether to enable and use FramePop for temporary buffers."
  :group 'framepop
  :type 'boolean
  :require 'framepop
  :set (lambda (symbol value)
         (set-default symbol value)
         (if (and value window-system)
             (framepop-enable)
           (framepop-disable))))

(defvar framepop-map nil)
(if framepop-map nil
  (setq framepop-map (make-sparse-keymap))
  (define-key framepop-map "?" 'framepop-display-help)
  (define-key framepop-map "s" 'framepop-show-frame)
  (define-key framepop-map "k" 'framepop-kill-buffer)
  (define-key framepop-map "d" 'framepop-delete-frame)
  (define-key framepop-map "i" 'framepop-make-invisible-frame)
  (define-key framepop-map "w" 'framepop-resize-frame)
  (define-key framepop-map "g" 'framepop-grow)
  (define-key framepop-map "c" 'framepop-copy-frame)
  (define-key framepop-map "/" 'framepop-pull-down)
  (define-key framepop-map ">" 'framepop-eob)
  (define-key framepop-map "<" 'framepop-bob)
  (define-key framepop-map "v" 'framepop-scroll-frame)
  (define-key framepop-map "l" 'framepop-lower-frame)
  (define-key framepop-map "r" 'framepop-raise-frame)
  (define-key framepop-map [f2] 'framepop-iconify-frame)
  (define-key framepop-map "x" 'framepop-iconify-frame)
  (define-key framepop-map "z" 'framepop-toggle-frame)
  (define-key framepop-map "b" 'framepop-display-buffer))
(defcustom framepop-enable-keybinding nil
  "Global key binding for FramePop keymap.
The key F2 is suggested."
  :group 'framepop 
  :type '(choice (const :tag "Not enabled" nil)
                 (const "<f2>")
                 (string :tag "key sequence (string)"))
  :set (lambda (symbol value)
         (set-default symbol value)
         (if value
             (define-key global-map (read-kbd-macro value) framepop-map))))

(defcustom framepop-use-advice 'automatic
  "Whether to use `advice' to extend Framepop functionality"
  :group 'framepop
  :type '(radio (const :tag "Yes" t)
                (const :tag "No" nil)
                (const :tag "Automatically if `advice' is already loaded"
                       :value automatic)))

(defcustom framepop-max-frame-size 35
  "*Maximum height of the FramePop frame."
  :group 'framepop
  :type 'integer)

(defcustom framepop-min-frame-size 5
  "*Minimum height of the FramePop frame."
  :group 'framepop
  :type 'integer)

(defcustom framepop-auto-resize nil
  "Whether to dynamically resize for changing buffers."
  :group 'framepop
  :type 'boolean)

(defcustom framepop-resize-increment 4
  "*When auto-resizing, frame height is forced to a multiple of this value.
This prevents excessive frame recreations on slow displays."
  :group 'framepop
  :type 'integer)  

;; FIXME: Make this a list or regexp to build a single \\| expression from? 
(defcustom framepop-buffer-names-that-grow
  "^\\\\*grep\\\\*$\\|\\\\*[Cc]ompilation\\\\*$"
  "Regexp matching buffer names that are likely to grow from empty.
When `framepop-auto-resize' is nil, buffers with names matching this regexp
are given a framepop frame of maximal size, to accomodate the data which
is soon to appear."
  :group 'framepop
  :type 'regexp)

;; If you want the title of the FramePop frame to be *Help* or
;; *Completions* or whatever, remove the (name . "FRAMEPOP") parameter
;; in framepop-frame-parameters below.
;;
;; If you want the FramePop frame to autoraise when selected,
;; uncomment the approprate line below
;;
;; Colours and positions are also good things to set here. There
;; should be no "height" parameter.

(defcustom framepop-frame-parameters
  '((name . "FRAMEPOP")
    (unsplittable . t) ; always include this
    (width . 80) ; this parameter is needed
    (menu-bar-lines . 0)
    (tool-bar-lines . 0)
    (minibuffer . nil))
  "Default parameters used in with the FramePop frame."
  :type '(set (cons :tag "Constant frame name (optional)"
               (const name) string)
              (cons :tag "Cannot be split"
                    (const unsplittable)
                    (boolean :tag "Boolean (Should be on)" ))
              (cons :tag "Width of the Frame"
               (const width :tag "width (required parameter)") integer)
              (cons :tag "Auto-raise"
               (const auto-raise) boolean)
              (cons :tag "Frame Position"
                    (const left) number)
              (cons :tag "Frame Position"
                    (const top) number)
              (cons :tag "Frame Position"
                    (const user-position) boolean)
              (cons :tag "Background Color"
                    (const background-color) color)
              (cons :tag "Foreground Color"
                    (const foreground-color) color)
              (cons :tag "Disable menubar"
                    (const menu-bar-lines)
                    (integer :tag "Integer (Should be 0)"))
              (cons :tag "Disable Tool-bar"
                    (const tool-bar-lines)
                    (integer :tag "Integer (Should be 0)"))
              (cons :tag "Disable Minibuffer"
                    (const minibuffer)
                    (boolean :tag "Boolean (Should be off)")))
  :tag "FramePop Frame Parameters"
  :group 'framepop)

(defcustom framepop-do-not-display-list '("*Buffer List*")
  "List of buffer names which will not appear in the FramePop frame.
This behaviour is implemented by the function `framepop-lines-default
'"
  :group 'framepop
  :type '(repeat (string :tag "Buffer name")))

;;; Variables controlling gross hacks

(defcustom framepop-hack-help-buffer-title t
  "Try and produce sensible names for copied help buffers."
  :group 'framepop
  :type 'boolean)

;;; System variables

(defvar framepop-lines
  'framepop-lines-default
  "Lambda expression of one argument BUF.
It returns the number of lines the framepop frame should have to display
BUF.  If nil is returned, BUF is not displayed in the framepop frame.")

(defvar framepop-in-wrap nil
  "Flag set to t during the execution of commands wrapped with `framepop-wrap'.")

(defvar framepop-last-displayed-buffer ""
  "Name of last buffer displayed in temp frame.")

(defvar framepop-frame nil)

;; Is it XEmacs?
(defconst framepop-xemacs-p
  (string-match "\\(Lucid\\|XEmacs\\)" emacs-version))

;;; Shut up the byte compiler

(eval-when-compile
  (require 'compile)
  (require 'advice)
  (require 'reporter))

;;; System functions

(defun framepop-frame-height (buf)
  "Return the desired height of a FramePop frame showing buffer BUF.
Enforces the limits set by `framepop-max-frame-size' and
`framepop-min-frame-size'"
  (let ((lines (funcall framepop-lines buf)))
    (if lines
	(max (min framepop-max-frame-size
		  (funcall framepop-lines buf))
	     framepop-min-frame-size))))

(defun framepop-buffer nil
  "Return the buffer the framepop window is showing, or nil."
  (if (frame-live-p framepop-frame)
      (window-buffer (frame-root-window framepop-frame))))

(defun framepop-last-non-newline-char ()
  "Return the position of the last non-newline character in the current buffer."
  (save-excursion
    (goto-char (point-max))
    (save-match-data
      (re-search-backward "[^\n]" nil t))
    (point)))

(defun framepop-count-visual-lines (buf &optional max frame)
  "Return the number of visual lines in BUF as opposed to the actual lines.
If MAX is supplied, counting stops after MAX lines.  MAX defaults to
`framepop-max-frame-size'.  The maximum size of a visual line is determined
by the width of frame FRAME \(defaults to `framepop-frame')."
  (if framepop-xemacs-p
      ;; This is inaccurate by one line, if a line that has 79 characters
      ;; generates 2 visual lines.
      (let* ((count 0)
	     (max (or max framepop-max-frame-size))
	     (frame (or frame framepop-frame))
	     (width  (- (or (frame-width frame)
			    (cdr (assq 'width framepop-frame-parameters))
			    (frame-width (selected-frame))) 1))
	     col)
	(save-excursion
	  (set-buffer buf)
	  (if truncate-lines
	      (min max (count-lines (point-min) (point-max)))
	    (save-excursion
	      (goto-char (point-min))
	      (while (and (not (eobp)) (< count max))
		;; Add one for this logical line
		(setq count (1+ count))
		(while (not (eolp))
		  ;; Add on the extra screen lines it generates
		  (setq col (+ (current-column) width))
		  (move-to-column col)
		  (if (and (eq (current-column) col) (not (eolp)))
		      (setq count (1+ count))))
		;; move to the next line, if possible
		(if (not (eobp)) (forward-char 1)))
	      ;; Add one for a terminating newline
	      (if (and (eobp) (eq (preceding-char) ?\n))
		  (setq count (1+ count)))
	      count))))
    ;; FSF GNU Emacs has a nice function for doing this :)
    (save-excursion
      (set-buffer buf)
      (1+ (nth 2
	       (compute-motion
		1			; FROM
		'(0 . 0)		; FROMPOS
		(framepop-last-non-newline-char) ; TO
		(cons 0		; TOPOS horizontal
		      (1- (or max framepop-max-frame-size))) ; TOPOS vertical
		(1- (cdr (assq 'width framepop-frame-parameters))) ; WIDTH
		nil		; OFFSET
		(if (frame-live-p framepop-frame) ; WINDOW
		    (frame-root-window framepop-frame)
		  (selected-window))))))
    ))
		    
(defun framepop-lines-default (buf)
  "The default value for `framepop-lines'.
Ensures that the FramePop frame will be big enough to display all of BUF.
However, returns nil for buffers in `framepop-do-not-display-list'."
  (save-excursion
    (set-buffer buf)
    (if (member (buffer-name) framepop-do-not-display-list) nil
      (+
       (if (and (not framepop-auto-resize)
		(or
		 ;; likely to grow
		 (eq (buffer-size) 0)
		 (string-match framepop-buffer-names-that-grow (buffer-name))))
	   framepop-max-frame-size
	 (framepop-count-visual-lines buf))
	 (if (cdr (assq 'minibuffer (frame-parameters framepop-frame)))
	     1 0)
	 1 ;; for the mode line
	 ))))

;;; User commands

(defun framepop-resize-frame (&optional buf height)
  "Resize the framepop frame to accomodate buffer BUF.
BUF defaults to the buffer displayed in the framepop frame
If HEIGHT is non-nil, BUF is ignored and the frame is given height."
  (interactive)
  (let* ((win (frame-root-window framepop-frame))
	 (buf (or buf (window-buffer win))))
    (modify-frame-parameters framepop-frame
			     (list (cons 'height
					 (or height (framepop-frame-height buf)))))))

(defun framepop-pull-down nil
  "If last line of framepop buffer is visible, place it on last window line."
  (interactive)
  (let* ((win (frame-root-window framepop-frame))
	 (buf (window-buffer win))
	 (pmax (save-excursion
		 (set-buffer buf)
		 (point-max))))
    (if (= (window-end win) pmax)
	(let ((oldwin (selected-window)))
	  (select-window win)
	  (save-excursion
	    (goto-char pmax)
	    (recenter -1))
	  (select-window oldwin)))))

(defun framepop-grow (lines)
  "Increase the height of the framepop frame by LINES lines.
When called interactively, LINES is the numeric prefix argument"
  (interactive "p")
  (modify-frame-parameters
   framepop-frame
   (list (cons 'height
	       (max 2 (+ lines
			 (cdr (assoc 'height
				     (frame-parameters framepop-frame)))))))))
(defun framepop-display-help nil
  "Display help for the framepop commands."
  (interactive)
  (describe-function 'framepop-display-buffer)
  (save-excursion
    (set-buffer (framepop-buffer)) ; *Help*
    (save-excursion
      (goto-char (point-min))
      (let ((framepop-resize-increment 1))
	(delete-region (point)
		       (progn
			 ;; Delete the framepop-display-buffer-specific stuff
			 (forward-line 7)
			 (point)))
	(insert "Framepop help:\n\n"))))
  (if framepop-auto-resize nil
    (framepop-resize-frame)))

;;;###autoload
(defun framepop-display-buffer (buf)
  ;; Note: the fifth line of this docstring should begin general help:
  ;; see framepop-display-help
  "Display-buffer for FramePop.
Displays BUF in a separate frame -- the FramePop frame.
BUF bay be a buffer or a buffer name.

You can display a buffer in the FramePop frame with \\[framepop-display-buffer].

Several commands are available for manipulating the FramePop frame after
typing the keymap prefix (default F2).

\\{framepop-map}
"
  (interactive "bDisplay buffer: ")
  (and (stringp buf) (setq buf (get-buffer buf)))
  (let ((oframe (selected-frame))
      ;;(omouse (mouse-position))
	(lines (framepop-frame-height buf)))
    (if (not lines)
	;; framepop-lines should return nil for buffers which
	;; shouldn't be displayed in the framepop frame
	(display-buffer buf)
      (if (frame-live-p framepop-frame) nil
	;; No existing framepop frame
	(setq framepop-frame
	      (make-frame (cons (cons 'height lines)
				framepop-frame-parameters))))
      ;; For XEmacs, kill toolbars
      ;; Can't kill frame menubar in XEmacs til 19.13 comes out
      (if framepop-xemacs-p
	(progn
	  (set-specifier top-toolbar-height (list framepop-frame 0))
	  (set-specifier bottom-toolbar-height (list framepop-frame 0))
	  (set-specifier left-toolbar-width (list framepop-frame 0))
	  (set-specifier right-toolbar-width (list framepop-frame 0))))
      (delete-other-windows (frame-selected-window framepop-frame))
      (set-window-dedicated-p (frame-selected-window framepop-frame) nil)
      (set-window-buffer (frame-selected-window framepop-frame) buf)
      (set-window-dedicated-p (frame-selected-window framepop-frame) t)
      (framepop-resize-frame)
      ;; (framepop-bob)
      (setq framepop-last-displayed-buffer (buffer-name buf))
      (if framepop-auto-resize
	  (save-excursion
	    (set-buffer buf)
	    (make-local-variable 'after-change-functions)
	    ;; Some functions (e.g. grep) display the temporary buffer
	    ;; before setting its mode (which wipes buffer-local
	    ;; variables).  Prevent that happening.
	    (put 'after-change-functions 'permanent-local t)
	    (add-hook 'after-change-functions 'framepop-resizer t)))
      (raise-frame framepop-frame)
      (if (minibuffer-window-active-p (minibuffer-window)) nil
	;; Replace the default message with something more suitable
	(let ((message-log-max nil))	; don't log this message
	  (message (substitute-command-keys
		    "Type \\[framepop-scroll-frame] to scroll, \\[framepop-iconify-frame] to iconify"))))

      ;; PSG - Let minibuffer [TAB] scroll *completions* buffer
      ;; There is probably a better (less-generic) way of setting this up.
      (setq minibuffer-scroll-window (frame-root-window framepop-frame))

      ;; Set focus on the original frame
      (if framepop-xemacs-p
	  (let ((wind (frame-lowest-window oframe)))
	    ;; XEmacs doesn't have a function that redirects focus
	    (select-frame oframe)
	    (set-mouse-position
	     wind (1- (window-width wind)) (1- (window-height wind))))
	(redirect-frame-focus framepop-frame oframe)))))

(defun framepop-resizer (beg end pre-change-length)
  "Bound to after-change-function to automatically resize the framepop frame."
  ;; If a after-change-buffer has somehow escaped being reset, do it now
  (if (and framepop-auto-resize (eq (framepop-buffer) (current-buffer)))
      (let ((bufheight (framepop-frame-height (current-buffer))))
	(framepop-resize-frame nil
	      ;; round up to framepop-resize-increment
	      (if (zerop (mod bufheight framepop-resize-increment)) bufheight
		(* (1+ (/ bufheight framepop-resize-increment))
		   framepop-resize-increment))))
    ;; otherwise...
    (remove-hook 'after-change-functions 'framepop-resizer)))

(defun framepop-iconify-frame nil
  "Iconify the FramePop frame."
  (interactive)
  (if (frame-live-p framepop-frame)
      (iconify-frame framepop-frame)
    (message "FramePop frame deleted")))

(defun framepop-make-invisible-frame nil
  "Make the FramePop frame invisible."
  (interactive)
  (if (frame-live-p framepop-frame)
      (make-frame-invisible framepop-frame)
    (message "FramePop frame deleted")))

(defun framepop-show-frame nil
  "Force the FramePop frame to be visible."
  (interactive)
  (if (frame-live-p framepop-frame)
      (raise-frame framepop-frame)
    (let ((buf (or
		(get-buffer framepop-last-displayed-buffer)
		(get-buffer "*Help*"))))
      (if buf
	  (framepop-display-buffer buf)
	(message "Last displayed temporary buffer has been killed.")))))

(defun framepop-delete-frame nil
  "Delete (destroy) the FramePop frame."
  (interactive)
  (delete-frame framepop-frame))

(defun framepop-kill-buffer nil
  "Delete (destroy) the FramePop frame and its kill the buffer it was showing."
  (interactive)
  (kill-buffer (framepop-buffer))
  (delete-frame framepop-frame))

(defun framepop-toggle-frame nil
  "Iconify or deiconify the FramePop frame."
  (interactive)
  (if (frame-live-p framepop-frame)
      (let ((oframe (selected-frame)))
	(if framepop-xemacs-p
	    (progn
	      (select-frame framepop-frame) ; XEmacs takes only one arg
	      (if (frame-iconified-p framepop-frame)
		  (deiconify-frame framepop-frame)
		(iconify-frame framepop-frame)))
	  (select-frame framepop-frame t)
	  (iconify-or-deiconify-frame))
	;; (bury-buffer (framepop-buffer))
	(select-frame oframe))
    (message "No active FramePop frame")))
      
(defun framepop-scroll-frame (n)
  "Like `scroll-other-window', but scrolls the window in the FramePop frame.
Scroll-up N lines."
  (interactive "P")
  (framepop-show-frame)
  (save-window-excursion
    (select-window (frame-root-window framepop-frame))
    (scroll-up n)))

(defun framepop-bob nil
  "Go to the beginning of the framepop buffer."
  (interactive)
  (framepop-show-frame)
  (let* ((win (frame-root-window framepop-frame))
	 (buf (window-buffer win))
	 (min (save-excursion
		(set-buffer buf)
		(point-min))))
    (set-window-point win min)))

(defun framepop-eob nil
  "Go to the end of the framepop buffer, and resize the framepoop frame.
Useful for buffers (e.g. compilations) which grow"
  (interactive)
  (framepop-show-frame)
  (sit-for 0)
  (let* ((win (frame-root-window framepop-frame))
	 (buf (window-buffer win)))
    (set-window-point win (save-excursion
			    (set-buffer buf)
			    (point-max))))
  (sit-for 0)				; redisplay
  (framepop-pull-down))

(defun framepop-lower-frame nil
  "Lower the FramePop frame."
  (interactive)
  (if (frame-live-p framepop-frame)
      (lower-frame framepop-frame)
    (message "No active FramePop frame")))

(defun framepop-raise-frame nil
  "Raise the FramePop frame."
  (interactive)
  (if (frame-live-p framepop-frame)
      (raise-frame framepop-frame)
    (message "No active FramePop frame")))

(defun framepop-copy-frame (copy-buffer)
  "Duplicate the FramePop frame, and maybe the displayed buffer as well.
With a prefix arg (COPY-BUFFER), the buffer is also copied and given a
unique name.  This is useful for *Help*, *Completions* etc."
  (interactive "P")
  (let ((oframe (selected-frame))
	new-frame
	buf
	contents
	pos)
    (select-frame framepop-frame)
    (setq pos (point))
    (setq new-frame (make-frame (frame-parameters framepop-frame)))
    (modify-frame-parameters new-frame '((name . nil)))
    (if copy-buffer
	(progn
	  (let ((helpobj))
	    (setq buf (if (and framepop-hack-help-buffer-title
			       (string= (buffer-name) "*Help*")
			       (progn
				 (condition-case ()
				     (save-excursion
				       (goto-char (point-min))
				       (search-forward ":" (min
							    (save-excursion
							     (end-of-line)
							     (point))
							    (+ (point-min) 50)))
				       (setq helpobj (buffer-substring
						      (point-min)
						      (match-beginning 0))))
				   (error nil))
				 ;; (intern-soft helpobj)
				 ))
			  (generate-new-buffer (format "*Help* on %s" helpobj))
			(generate-new-buffer (buffer-name)))))
	  (select-frame new-frame)
	  (setq contents (buffer-string))
	  (save-excursion
	    (set-buffer buf)
	    (insert contents)
	    (goto-char pos)
	    (switch-to-buffer buf))))
    (select-frame oframe)))

(defun framepop-wrap (function buffer)
  "Define a wrapper on FUNCTION so that BUFFER will appear in a FramePop frame.
BUFFER may be a buffer, a buffer name, or a sexp evaluating to a buffer or
buffer name.  The function is advised with around advice named
framepop-display-buffer-in-framepop-frame.

WARNING: this will not work on autoloaded functions unless forward
advice has been enabled.  You must use `ad-activate' to activate the advice
after the package has been loaded.  See advice.el for details."
  (require 'advice)
  (require 'backquote)
  (ad-add-advice
   function
   (ad-make-advice
    'framepop-display-buffer-in-framepop-frame
    t
    t
    (` (advice lambda nil
	       ;; docstring:
	       (, (format "Displays %s buffer in a FramePop frame"
			  (if (stringp buffer) buffer "output")))
	       ;; body
	       (let ((framepop-in-wrap t))
		 ad-do-it
		 (let* ((arg (, buffer))
			(buf (if (stringp arg) (get-buffer arg) arg)))
		   (cond ((bufferp buf)
			  (delete-windows-on buf)
			  (framepop-display-buffer buf))))))))
		 
   'around
   'last)
  (ad-activate function))

(defun framepop-submit-feedback ()
  "Sumbit feedback on the FramePop package by electronic mail."
  (interactive)
  (require 'reporter)
  (reporter-submit-bug-report
   "Peter S Galbraith <psg@debian.org>"
   (concat "framepop.el; version " framepop-version)
   '(framepop-lines framepop-auto-resize framepop-frame-parameters)))

;;; Special display
;;; ---------------

(defun framepop-special-display (buffer &optional args)
  "Display BUF in the FramePop frame.
Pass it BUFFER as first arg, and (cdr ARGS) gives the rest of the args.
If ARGS is a list whose car is a symbol, use (car ARGS) as a function
to do the work.
Otherwise, ARGS is ignored."
  (if (and args (symbolp (car args)))
      (apply (car args) buffer (cdr args))
    (framepop-display-buffer buffer)
    (frame-selected-window framepop-frame)))

;;; Customizations using advice
;;; ---------------------------

;;; There are lots of useful things we can do with advice, but I
;;; really want to avoid forcing everyone to load the advice package
;;; just for framepop (advice is BIG). So here's a compromise: If
;;; advice has been loaded, the customizations below will be made.

(if (or (equal framepop-use-advice t)
        (and (equal framepop-use-advice 'automatic)
             (featurep 'advice)))
    (progn
      
      ;; (setq ad-activate-on-definition t) ; allow forward advice
      ;; (ad-start-advice)			 ; make forward advice work

;;; Without the following advice, any completions generated by comint
;;; will leave the focus in the framepop frame (ugh)

      (defadvice comint-dynamic-list-completions (around framepop-simple-complete activate)
	"Just display the completions buffer; no fancy tricks, OK?"
	(with-output-to-temp-buffer "*Completions*"
	  (display-completion-list (sort completions 'string-lessp))))

;;; You may use the function framepop-wrap to force buffers which do
;;; not normally appear in a FramePop frame to do so automatically. For
;;; example, the following commands force *Shell Command Output*
;;; buffers to appear in the FramePop frame:

;; This is better done with special-display-buffer-names, so I've disabled it
;;      (framepop-wrap 'shell-command "*Shell Command Output*")
;;      (framepop-wrap 'shell-command-on-region "*Shell Command Output*")
      
;;; You can also do this for the compile function, as shown below. In
;;; addition we also add the following advice to work around the
;;; strange way compile displays its windows:
      
;; This is better done with special-display-buffer-names, so I've disabled it
;;      (framepop-wrap 'compile "*compilation*")
;;      (framepop-wrap 'grep "*grep*")

;;; The above advised functions are part of compile.el, which is
;;; autoloaded. We need to activate these advices after compile.el has
;;; loaded.

;; 	 (eval-after-load "compile"
;; 			  '(progn
;; 			     (ad-activate 'compile)
;; 			     (ad-activate 'grep)
;; 			     (ad-activate 'compile-internal)
;; 			     (ad-activate 'compile-goto-error)))

;;; Under normal (i.e. without framepop) circumstances, completion
;;; buffers disappear after use. The following advice similarly
;;; arranges for a framepop frame displaying completions to get out of
;;; the way after use.  Other choices here include
;;; framepop-make-invisible-frame or framepop-lower-frame
      
      (defun framepop-completions-buffer-p nil
	;; Return non-nil if the framepop buffer is a completions buffer
	(let ((buf (framepop-buffer)))
	  (and buf
	       (string-match "[cC]ompletions" (buffer-name buf)))))

      (defvar framepop-go-away-function 'framepop-iconify-frame
	"*Function called to make the framepop frame go away")
      
      (defun framepop-maybe-go-away (&optional force)
	;; Get rid of the framepop frame if it shows completions buffer
	(if (or force (framepop-completions-buffer-p))
	    (funcall framepop-go-away-function)))

      (defadvice completing-read (after framepop-go-away protect activate)
	"Get rid of the FramePop frame showing the completions"
	(framepop-maybe-go-away))

      ;; It would seem we only need protected advice for
      ;; completing-read, but that subr is often called from other
      ;; subrs and advice won't work. The following advices get around
      ;; some of the cases where this happens, but unfortunately in
      ;; such cases C-g won't lower the framepop frame, as desired.

      (defadvice minibuffer-complete-and-exit (before framepop-go-away
						      activate)
	"Get rid of the FramePop frame showing the completions"
	(framepop-maybe-go-away))

      (defadvice exit-minibuffer (before framepop-go-away activate)
	"Get rid of the FramePop frame showing the completions"
	(framepop-maybe-go-away))

      (defadvice mouse-choose-completion (after framepop-go-away activate)
        "Get rid of the FramePop frame showing the completions"
        (framepop-maybe-go-away t))

      (defadvice keyboard-quit (before framepop-go-away activate)
	"Get rid of the FramePop frame showing the completions"
	(framepop-maybe-go-away))

      (defadvice abort-recursive-edit (before framepop-go-away activate)
	"Get rid of the FramePop frame showing the completions"
	(framepop-maybe-go-away))
      
;;; End of advice customizations
      ))

(provide 'framepop)

;;; framepop.el ends here
