;;; windresize.el --- resize windows interactively
;;
;; Copyright 2007 2008 Bastien Guerry
;;
;; Author: Bastien <bzg AT altern DOT org>
;; Version: 0.6d
;; Keywords: window
;; URL: http://www.cognition.ens.fr/~guerry/u/windresize.el
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;
;; This is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; This mode let's you edit the window configuration interactively just
;; by using the keyboard.  Quickstart: M-x windresize
;;
;;; History:
;;
;; This was largely inspired by Hirose Yuuji and Bob Wiener original
;; `resize-window' as posted on the emacs-devel mailing list by Juanma.
;; This was also inspired by Lennart Borgman's bw-interactive.el (now
;; winsize.el). See related discussions on the emacs-devel mailing
;; list. Special thanks to Drew Adams, Juri Linkov, Stefan Monnier and
;; JunJie Nan for useful suggestions.
;;
;; Also check http://www.emacswiki.org/cgi-bin/wiki/WindowResize for
;; general hints on window resizing.
;;
;; Put this file into your load-path and the following into your
;;   ~/.emacs: (require 'windresize)
;;
;;; Todo:
;;
;; - better help window
;; - register key sequences as macros
;; - maybe add numbers to window configurations in the ring
;;
;;; Code:

(require 'ring)				; for storing window configuration
(require 'windmove)			; for selecting adjacent window

;;; User variables:

(defconst windresize-version "0.6d"
  "The version number of the file windresize.el.")

(defcustom windresize-move-borders t
  "Default method for resizing windows.
\\<windresize-map>Non-nil means that windresize will move borders.
For example, \\[windresize-left] will move the first movable border to the
left, trying to move the right border then the left border.  \\[windresize-up]
will move the first movable border up, trying to move the bottom border then
the upper border.

Nil means that it will shrink or enlarge the window instead.
\\[windresize-down] and  \\[windresize-up] will shrink and enlarge the window
vertically.  \\[windresize-left] and \\[windresize-right] will shrink and
enlarge the window horizontally."
  :type 'boolean
  :group 'convenience)

(defcustom windresize-default-increment 1
  "The default number of lines for resizing windows."
  :type 'integer
  :group 'convenience)

(defcustom windresize-verbose 2
  "Integer that say how verbose Windresize should be.
The higher the number, the more feedback Windresize will give.
A value of 0 will prevent any message to be displayed.
A value of 1 will display errors only.
A value of 2 will display errors and messages."
  :type 'integer
  :group 'convenience)

(defcustom windresize-ring-size 10
  "The size of the ring for storing window configurations."
  :type 'integer
  :group 'convenience)

(defcustom windresize-windmove-relative-to-point 0
  "Nil means select adjacent window relatively to the point position.
Non-nil means select adjacent window relatively to the window
edges.  See the docstring of `windmove-up' for details."
  :group 'convenience
  :type 'integer)

(defcustom windresize-modifiers '((meta shift) meta
				  (control meta) control)
  "A list of modifiers for arrow keys commands.
Each element can be a modifier or a list of modifiers.

The first modifier is for selecting windows with `windmove'.
The second modifier is for moving the up/left border instead of
the bottom/right border when there are two movable borders.
The third modifier is to move borders and keep the width/height
size fixed.
The fourth modifier is to move boder or resize window while
temporarily negating the increment value.

Make sure the four elements of this list are distinct to avoid
conflicts between keybindings."
  :group 'convenience
  :type '(list
	  (choice :tag "Modifier for selecting the adjacent windows"
		  (symbol :tag "Single modifier")
		  (repeat :tag "Multiple modifiers"
			  (symbol :tag "Modifier")))
	  (choice :tag "Modifier for moving the left/up border instead of the right/bottom border"
		  (symbol :tag "Single modifier")
		  (repeat :tag "Multiple modifiers"
			  (symbol :tag "Modifier")))
	  (choice :tag "Modifier for moving borders with fixed width/height"
		  (symbol :tag "Single modifier")
		  (repeat :tag "Multiple modifiers"
			  (symbol :tag "Modifier")))
	  (choice :tag "Modifier for negating increment temporarily"
		  (symbol :tag "Single modifier")
		  (repeat :tag "Multiple modifiers"
			  (symbol :tag "Modifier")))))

;;; Variables and keymap:

(defvar windresize-msg '("" . 0))
(defvar windresize-buffer nil)
(defvar windresize-increment nil)
(defvar windresize-resizing nil)
(defvar windresize-configuration-ring nil)
(defvar windresize-window-configuration-0 nil)
(defvar windresize-overriding-terminal-local-map-0 nil)
(defvar windresize-overriding-menu-flag-0 nil)

(defvar windresize-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap self-insert-command] 'windresize-other-char)
    (define-key map (kbd "M-x") 'windresize-other-char)
    (define-key map (kbd "C-h") 'windresize-other-char)
    ;; move borders outwards or shrink/enlarge
    (define-key map [left] 'windresize-left)
    (define-key map [right] 'windresize-right)
    (define-key map [up] 'windresize-up)
    (define-key map [down] 'windresize-down)
    ;; Use windmove to select adjacent window. The default keybindings
    ;; in `windresize-modifiers' should match those of windmove
    (let ((mod (nth 0 windresize-modifiers)))
      (if (symbolp mod) (setq mod (list mod)))
      (define-key map (vector (append mod '(left))) 'windresize-select-left)
      (define-key map (vector (append mod '(right))) 'windresize-select-right)
      (define-key map (vector (append mod '(up))) 'windresize-select-up)
      (define-key map (vector (append mod '(down))) 'windresize-select-down))
    ;; Move the up/left border instead of bottom/right when there are
    ;; two movable borders
    (let ((mod (nth 1 windresize-modifiers)))
      (if (symbolp mod) (setq mod (list mod)))
      (define-key map (vector (append mod '(left))) 'windresize-left-force-left)
      (define-key map (vector (append mod '(right))) 'windresize-right-force-left)
      (define-key map (vector (append mod '(up))) 'windresize-up-force-up)
      (define-key map (vector (append mod '(down))) 'windresize-down-force-up))
    ;; Move borders with fixed width/height
    (let ((mod (nth 2 windresize-modifiers)))
      (if (symbolp mod) (setq mod (list mod)))
      (define-key map (vector (append mod '(left))) 'windresize-left-fixed)
      (define-key map (vector (append mod '(right))) 'windresize-right-fixed)
      (define-key map (vector (append mod '(up))) 'windresize-up-fixed)
      (define-key map (vector (append mod '(down))) 'windresize-down-fixed))
    ;; Negate increment temporarily
    (let ((mod (nth 3 windresize-modifiers)))
      (if (symbolp mod) (setq mod (list mod)))
      (define-key map (vector (append mod '(left))) 'windresize-left-minus)
      (define-key map (vector (append mod '(right))) 'windresize-right-minus)
      (define-key map (vector (append mod '(up))) 'windresize-up-minus)
      (define-key map (vector (append mod '(down))) 'windresize-down-minus))
    ;; Set the increment
    (define-key map "~" 'windresize-negate-increment)
    (define-key map "+" 'windresize-increase-increment)
    (define-key map "-" 'windresize-decrease-increment)
    ;; FIXME
    (define-key map "i" 'windresize-set-increment)
    ;; other keys
    (define-key map " " 'windresize-toggle-method)
    (define-key map "s" 'windresize-save-window-configuration)
    (define-key map "r" 'windresize-restore-window-configuration)
    ;; shorcut keys for manipulating windows
    (define-key map "0" 'delete-window)
    (define-key map "o" 'windresize-other-window)
    (define-key map "n" 'windresize-next-window)
    (define-key map "p" 'windresize-previous-window)
    (define-key map "/" 'windresize-bottom-right)
    (define-key map "\M-/" 'windresize-up-left)
    (define-key map (kbd "\\") 'windresize-up-right)
    (define-key map (kbd "M-\\") 'windresize-bottom-left)
    (define-key map "1" 'windresize-delete-other-windows)
    (define-key map "2" 'windresize-split-window-vertically)
    (define-key map "3" 'windresize-split-window-horizontally)
    (define-key map "=" 'windresize-balance-windows)
    (define-key map "\C-xo" 'windresize-other-window)
    (define-key map "\C-x0" 'windresize-delete-window)
    (define-key map "\C-x1" 'windresize-delete-other-windows)
    (define-key map "\C-x2" 'windresize-split-window-vertically)
    (define-key map "\C-x3" 'windresize-split-window-horizontally)
    (define-key map "\C-x+" 'windresize-balance-windows)
    (define-key map (kbd "C-a")
      (lambda() (interactive) (move-beginning-of-line 1)))
    (define-key map (kbd "C-e")
      (lambda() (interactive) (move-end-of-line 1)))
    (define-key map "=" 'windresize-balance-windows)
    (define-key map [mouse-1] 'mouse-set-point)
    ;; help, save and exit
    (define-key map (kbd "RET") 'windresize-exit)
    (define-key map "x" 'windresize-exit)
    (define-key map "\C-c\C-c" 'windresize-exit)
    (define-key map "?" 'windresize-help)
    (define-key map "q" 'windresize-cancel-and-quit)
    (define-key map "c" 'windresize-cancel-and-quit)
    (define-key map (kbd "C-g") 'windresize-cancel-and-quit)
    map)
  "Keymap for `windresize'.")

(defun windresize-other-char ()
  "Show a message instead of processing `self-insert-command'."
  (interactive)
  (let* ((key (if current-prefix-arg
		  (substring (this-command-keys)
			     universal-argument-num-events)
		(this-command-keys))))
    (cond ((vectorp key) (ding))
	  ((stringp key)
	   ;; send warning for only no warning for complex keys and
	   ;; mouse events
	   (setq windresize-msg
		 (cons (format "[`%s' not bound]" key) 1))))))

;;; Aliases:

(defun windresize-other-window ()
  "Select other window."
  (interactive)
  (if (one-window-p)
      (setq windresize-msg (cons "[No other window]" 1))
    (other-window 1)
    (setq windresize-msg (cons (format "Now in %s" (buffer-name)) 2))))

(defalias 'windresize-next-window 'windresize-other-window)

(defun windresize-previous-window ()
  "Select the previous window."
  (interactive)
  (if (one-window-p)
      (setq windresize-msg (cons "[No previous window]" 1))
    (other-window -1)
    (setq windresize-msg (cons (format "Now in %s" (buffer-name)) 2))))

(defun windresize-delete-window ()
  "Delete window."
  (interactive)
  (if (one-window-p)
      (setq windresize-msg (cons "[Can't delete sole window]" 1))
    (other-window 1)
    (setq windresize-msg (cons "Window deleted" 2))))

(defun windresize-delete-other-windows ()
  "Delete other windows."
  (interactive)
  (if (one-window-p)
      (setq windresize-msg (cons "[No other window]" 1))
    (delete-other-windows)
    (setq windresize-msg (cons "Windows deleted" 2))))

(defun windresize-split-window-horizontally ()
  "Split window horizontally."
  (interactive)
  (split-window-horizontally)
  (setq windresize-msg (cons "Window horizontally split" 2)))

(defun windresize-split-window-vertically ()
  "Split window vertically."
  (interactive)
  (split-window-vertically)
  (setq windresize-msg (cons "Window vertically split" 2)))

(defun windresize-balance-windows ()
  "Balance windows."
  (interactive)
  (balance-windows)
  (setq windresize-msg (cons "Windows balanced" 2)))

;;; Windresize:

;;;###autoload
(defun windresize (&optional increment)
  "Resize windows interactively.
INCREMENT is the number of lines by which borders should move.

By default, the method for resizing is by moving the borders.
The left/right key will move the only movable vertical border to
the left/right and the up/down key will move the only horizontal
movable border up/down.  If there are two movable borders, the
right and the bottom border will have priority over the left and
upper border.  You can reverse this priority by using \\[windresize-left-force-left],
\\[windresize-right-force-left], etc.

Resizing can also be done by increasing/decreasing the window
width and height.  The up and down arrow keys will enlarge or
shrink the window vertically and the right and left arrow keys
will enlarge or shrink the window horizontally.

You can toggle the method with \\[windresize-toggle-method].

You can set the number of line by which a border should move by
calling \\[windresize-set-increment] with a numeric prefix.
You can temporarily negate the number of lines by which the
windows are resized by using \\[windresize-left-minus], \\[windresize-right-minus], etc.
If you want to permanently negate this increment value,
use `\\[windresize-negate-increment]' instead.

You can also save window configurations with `\\[windresize-save-window-configuration]' in a ring,
and restore them with `\\[windresize-restore-window-configuration]'.

`\\[windresize-cancel-and-quit]' will quit `windresize' and cancel any change.  `\\[windresize-exit]'
will set the new window configuration and exit.

\\{windresize-map}"
  (interactive "P")
  (if windresize-resizing
      (windresize-exit)
    ;; FIXME shall we exit we calling again `windresize'?
    ;;       (progn (windresize-message '("[Already resizing]" . 0))
    ;; 	     (sit-for 2))
    (setq windresize-overriding-terminal-local-map-0
	  overriding-terminal-local-map)
    (setq windresize-overriding-menu-flag-0
	  overriding-local-map-menu-flag)
    (setq windresize-window-configuration-0
	  (current-window-configuration))
    ;; set increment, window configuration ring, initial buffer
    (setq windresize-increment windresize-default-increment)
    (setq windresize-configuration-ring
	  (make-ring windresize-ring-size))
    (ring-insert windresize-configuration-ring
		 (current-window-configuration))
    (setq windresize-buffer (current-buffer))
    ;; set overriding map and pre/post-command hooks
    (setq overriding-terminal-local-map windresize-map)
    (setq overriding-local-map-menu-flag t)
    (windresize-add-command-hooks)
    ;; set the initial message
    (setq windresize-msg
	  (if (one-window-p)
	      (setq windresize-msg (cons "Split window with [23]" 2))
	    (setq windresize-msg (cons "" 0))))
    (setq windresize-resizing t)
    (windresize-message)))

(defun windresize-message (&optional msg)
  "Display a message at the bottom of the screen.
If MSG is nil, use `windresize-msg' instead."
  (let* ((msg0 (or msg windresize-msg))
	 (msg-l (cdr msg0))
	 (msg-t (car msg0))
	 (method (if windresize-move-borders
		     "move borders " "resize window")))
    (cond ((< msg-l 2)			; information
	   (add-text-properties 0 (length msg-t) '(face bold) msg-t))
	  ((< msg-l 3)			; warnings
	   (add-text-properties 0 (length msg-t) '(face shadow) msg-t)))
    (add-text-properties 0 (length method) '(face bold) method)
    (message "Use arrow keys to %s by %d %s  RET:set  ?:help  %s"
	     method windresize-increment
	     (if (not (equal (abs windresize-increment) 1))
		 "lines" "line ")
	     (if (<= (cdr windresize-msg) windresize-verbose)
		 msg-t ""))))

(defun windresize-add-command-hooks ()
  "Add hooks to commands when entering `windresize'."
  (add-hook 'pre-command-hook 'windresize-pre-command)
  (add-hook 'post-command-hook 'windresize-post-command))

(defun windresize-remove-command-hooks ()
  "Remove hooks to commands when exiting `windresize'."
  (remove-hook 'pre-command-hook 'windresize-pre-command)
  (remove-hook 'post-command-hook 'windresize-post-command))

(defun windresize-pre-command ()
  "Pre-command in `windresize'."
  (setq windresize-msg (cons "" 0)))

(defun windresize-post-command ()
  "Post-command in `windresize'."
  (windresize-message))

(defun windresize-toggle-method ()
  "Toggle resizing method."
  (interactive)
  (setq windresize-move-borders
	(not windresize-move-borders))
  (setq windresize-msg
	(cons (format
	       "Method: %s"
	       (if (not windresize-move-borders)
		   "resize window" "move borders")) 2)))

;;; Use windmove to select the adjacent window:

(defun windresize-select-down (&optional arg)
  "Select the window below the current one.
If ARG is nil or zero, select the window relatively to the point
position.  If ARG is positive, select relatively to the left edge
and select relatively to the right edge otherwise."
  (interactive "P")
  (condition-case nil
      (windmove-down
       (or arg windresize-windmove-relative-to-point))
    (error (setq windresize-msg
		 (cons "[Can't select window below this one]" 1)))))

(defun windresize-select-up (&optional arg)
  "Select the window above the current one.
If ARG is nil or zero, select the window relatively to the point
position.  If ARG is positive, select relatively to the left edge
and select relatively to the right edge otherwise."
  (interactive "P")
  (condition-case nil
      (windmove-up
       (or arg windresize-windmove-relative-to-point))
    (error (setq windresize-msg
		 (cons "[Can't select window above this one]" 1)))))

(defun windresize-select-left (&optional arg)
  "Select the window to the left of the current one.
If ARG is nil or zero, select the window relatively to the point
position.  If ARG is positive, select relatively to the top edge
and select relatively to the bottom edge otherwise."
  (interactive "P")
  (condition-case nil
      (windmove-left
       (or arg windresize-windmove-relative-to-point))
    (error (setq windresize-msg
		 (cons "[Can't select window left this one]" 1)))))

(defun windresize-select-right (&optional arg)
  "Select the window to the right of the current one.
If ARG is nil or zero, select the window relatively to the point
position.  If ARG is positive, select relatively to the top edge
and select relatively to the bottom edge otherwise."
  (interactive "P")
  (condition-case nil
      (windmove-right
       (or arg windresize-windmove-relative-to-point))
    (error (setq windresize-msg
		 (cons "[Can't select window right this one]" 1)))))

;;; Increase/decrease/set the increment value:

(defun windresize-set-increment (&optional n)
  "Set the increment value to N."
  (interactive "p")
  (setq windresize-increment n)
  (setq windresize-msg (cons "Increment set" 2)))

(defun windresize-negate-increment (&optional silent)
  "Negate the increment value.
If SILENT, dont output a message."
  (interactive)
  (setq windresize-increment (- windresize-increment))
  (unless silent (setq windresize-msg (cons "Negated increment" 2))))

(defun windresize-increase-increment (&optional silent)
  "Increase the increment.
If SILENT is non-nil, don't output a message."
  (interactive)
  (let ((i windresize-increment))
    (if (eq i -1) (setq i (- i)) (setq i (1+ i)))
    (setq windresize-increment i))
  (unless silent (setq windresize-msg (cons "Increased increment" 2))))

(defun windresize-decrease-increment (&optional silent)
  "Decrease the increment.
If SILENT is non-nil, don't output a message."
  (interactive)
  (let ((i windresize-increment))
    (if (eq i 1) (setq i (- 1)) (setq i (1- i)))
    (setq windresize-increment i))
  (unless silent (setq windresize-msg (cons "Decreased increment" 2))))

;;; Window configuration ring:

(defun windresize-save-window-configuration ()
  "Save the current window configuration in the ring."
  (interactive)
  (if (equal (ring-ref windresize-configuration-ring 0)
	     (current-window-configuration))
      (setq windresize-msg
	    (cons "[Same window configuration: not saved]" 1))
    (ring-insert windresize-configuration-ring
		 (current-window-configuration))
    (setq windresize-msg
	  (cons "Configuration saved -- use `r' to restore" 2))))

(defun windresize-restore-window-configuration ()
  "Restore the previous window configuration in the ring."
  (interactive)
  (let ((wcf (ring-remove windresize-configuration-ring 0)))
    (set-window-configuration wcf)
    (ring-insert-at-beginning windresize-configuration-ring wcf))
  (setq windresize-msg (cons "Previous configuration restored" 2)))

;;; Commands for arrow keys:

(defun windresize-left (&optional n left-border fixed-width)
  "Main function for handling left commands.
N is the number of lines by which moving borders.
In the move-border method, move the right border to the left.
If LEFT-BORDER is non-nil, move the left border to the left.
In the resize-window method, shrink the window horizontally.

If FIXED-WIDTH is non-nil and both left and right borders are
movable, move the window to the left and preserve its width."
  (interactive "P")
  (let* ((left-w (windmove-find-other-window 'left))
	 (right-w (windmove-find-other-window 'right))
	 (i (if n (prefix-numeric-value n) windresize-increment))
	 (shrink-ok (> (- (window-width) i) window-min-width))
	 (w (selected-window)))
    (if (not windresize-move-borders)
	(if (not shrink-ok)
	    (setq windresize-msg
		  (cons "[Can't shrink window horizontally]" 1))
	  (condition-case nil
	      (if shrink-ok (shrink-window-horizontally i)
		(error t))
	    (error (setq windresize-msg
			 (cons "[Can't shrink window horizontally]" 1)))))
      (cond ((equal (frame-width) (window-width))
	     (setq windresize-msg (cons "No vertical split" 2)))
	    ((and left-w right-w)
	     (if left-border
		 (progn (windmove-left windresize-windmove-relative-to-point)
			(adjust-window-trailing-edge (selected-window) (- i) t)
			(select-window w)
			(if fixed-width (windresize-left)))
	       (condition-case nil
		   (progn (adjust-window-trailing-edge w (- i) t)
			  (if fixed-width (windresize-left nil t)))
		 (error (setq windresize-msg
			      (cons "[Can't move right border left]" 1))))))
	    (left-w
	     (condition-case nil
		 (adjust-window-trailing-edge left-w (- i) t)
	       (error (setq windresize-msg (cons "[Can't move left border left]" 1)))))
	    (right-w (windresize-left-inwards))
	    (t (setq windresize-msg (cons "[Can't move border]" 1)))))))

(defun windresize-right (&optional n left-border fixed-width)
  "Main function for handling right commands.
N is the number of lines by which moving borders.
In the move-border method, move the right border to the right.
If LEFT-BORDER is non-nil, move the left border to the right.
In the resize-window method, enlarge the window horizontally.

If FIXED-WIDTH is non-nil and both left and right borders are
movable, move the window to the right and preserve its width."
  (interactive "P")
  (let ((right-w (windmove-find-other-window 'right))
	(left-w (windmove-find-other-window 'left))
	(i (if n (prefix-numeric-value n) windresize-increment))
	(wcf (current-window-configuration))
	(w (selected-window)))
    (if (not windresize-move-borders)
	(progn (ignore-errors (enlarge-window-horizontally i))
	       (if (equal wcf (current-window-configuration))
		   (setq windresize-msg
			 (cons "[Can't enlarge window horizontally]" 1))))
      (cond ((equal (frame-width) (window-width))
	     (setq windresize-msg (cons "No vertical split" 2)))
	    ((and right-w left-w left-border)
	     (progn (windmove-left windresize-windmove-relative-to-point)
		    (adjust-window-trailing-edge left-w i t)
		    (select-window w)
		    (if fixed-width (windresize-right))))
	    (right-w
	     (condition-case nil
		 (adjust-window-trailing-edge w i t)
	       (error (setq windresize-msg
			    (cons "[Can't move right border right]" 1)))))
	    (left-w (windresize-right-inwards))
	    (t (setq windresize-msg (cons "[Can't move border]" 1)))))))

(defun windresize-up (&optional n upper-border fixed-height)
  "Main function for handling up commands.
N is the number of lines by which moving borders.
In the move-border method, move the bottom border upwards.
If UPPER-BORDER is non-nil, move the upper border upwards.
In the resize-window method, enlarge the window vertically.

If FIXED-HEIGHT is non-nil and both the upper and lower borders
are movable, move the window up and preserve its height."
  (interactive "P")
  (let ((up-w (windmove-find-other-window 'up))
	(down-w (windmove-find-other-window 'down))
	(i (if n (prefix-numeric-value n) windresize-increment))
	(wcf (current-window-configuration))
	(w (selected-window)))
    (if (not windresize-move-borders)
	(progn (ignore-errors (enlarge-window i))
	       (if (equal wcf (current-window-configuration))
		   (setq windresize-msg
			 (cons "[Can't enlarge window vertically]" 1))))
      (cond ((equal (frame-height) (1+ (window-height)))
	     (setq windresize-msg (cons "No horizontal split" 2)))
	    ((and up-w down-w (not (window-minibuffer-p down-w)))
	     (if upper-border
		 (progn (windmove-up windresize-windmove-relative-to-point)
			(adjust-window-trailing-edge (selected-window) (- i) nil)
			(select-window w)
			(if fixed-height (windresize-up)))
	       (condition-case nil
		   (adjust-window-trailing-edge w (- i) nil)
		 (error
		  (setq windresize-msg
			(cons "[Can't move bottom border up]" 1))))))
	    (up-w (condition-case nil
		      (adjust-window-trailing-edge up-w (- i) nil)
		    (error (setq windresize-msg
				 (cons "[Can't move upper border up]" 1)))))
	    ((and down-w (not (window-minibuffer-p down-w)))
	     (windresize-up-inwards))
	    (t (setq windresize-msg (cons "[Can't move border]" 1)))))))

(defun windresize-down (&optional n upper-border fixed-height)
  "Main function for handling down commands.
N is the number of lines by which moving borders.
In the move-border method, move the bottom border down.
If UPPER-BORDER is non-nil, move the upper border down.
In the resize-window method, shrink the window vertically.

If FIXED-HEIGHT is non-nil and both the upper and lower borders
are movable, move the window down and preserve its height."
  (interactive "P")
  (let* ((down-w (windmove-find-other-window 'down))
	 (up-w (windmove-find-other-window 'up))
	 (i (if n (prefix-numeric-value n) windresize-increment))
	 (shrink-ok (> (- (window-width) i) window-min-width))
	 (wcf (current-window-configuration))
	 (w (selected-window)))
    (if (not windresize-move-borders)
	(if (or (and (window-minibuffer-p down-w) (not up-w))
		(< (- (window-height) i) window-min-height))
	    (setq windresize-msg (cons "[Can't shrink window vertically]" 1))
	  (if shrink-ok (shrink-window i)
	    (setq windresize-msg (cons "[Can't shrink window vertically]" 1))))
      (cond ((equal (frame-height) (1+ (window-height)))
	     (setq windresize-msg (cons "No horizontal split" 2)))
	    ((and up-w down-w (not (window-minibuffer-p down-w))
		  upper-border)
	     (progn (windmove-up windresize-windmove-relative-to-point)
		    (adjust-window-trailing-edge (selected-window) i nil)
		    (select-window w)
		    (if fixed-height (windresize-down))))
	    ((and down-w (not (window-minibuffer-p down-w)))
	     (condition-case nil
		 (adjust-window-trailing-edge w i nil)
	       (error (setq windresize-msg (cons "[Can't move bottom border down]" 1)))))
	    (up-w (windresize-down-inwards))
	    (t (setq windresize-msg (cons "[Can't move border]" 1)))))))

;;; Moving the opposite border inwards:

(defun windresize-left-inwards (&optional n)
  "Move the right border left by N lines."
  (interactive "P")
  (let ((i (if n (prefix-numeric-value n) windresize-increment)))
    (condition-case nil
	(adjust-window-trailing-edge (selected-window) (- i) t)
      (error (setq windresize-msg
		   (cons "[Can't move right border to the left]" 1))))))

(defun windresize-right-inwards (&optional n)
  "Move the left border right by N lines."
  (interactive "P")
  (let ((i (if n (prefix-numeric-value n) windresize-increment))
	(left-w (windmove-find-other-window 'left)))
    (condition-case nil
	(if left-w (adjust-window-trailing-edge left-w i t) (error t))
      (error (setq windresize-msg
		   (cons "[Can't move left border right]" 1))))))

(defun windresize-up-inwards (&optional n)
  "Move the bottom border up by N lines."
  (interactive "P")
  (let ((i (if n (prefix-numeric-value n) windresize-increment))
	(down-w (windmove-find-other-window 'down)))
    (condition-case nil
	(progn (if (window-minibuffer-p down-w)
		   (setq windresize-msg
			 (cons "[Can't move bottom border up]" 1)))
	       (adjust-window-trailing-edge
		(selected-window) (- i) nil))
      (error (setq windresize-msg
		   (cons "[Can't move bottom border up]" 1))))))

(defun windresize-down-inwards (&optional n)
  "Move the upper border down by N lines."
  (interactive "P")
  (let ((i (if n (prefix-numeric-value n) windresize-increment))
	(wcf (current-window-configuration))
	(up-w (windmove-find-other-window 'up)))
    (condition-case nil
	(if up-w (adjust-window-trailing-edge up-w i nil)
	  (error t))
      (error (setq windresize-msg
		   (cons "[Can't move upper border down]" 1))))))

;;; Arrow keys temoporarily negating the increment value:

(defun windresize-down-minus ()
  "Same as `windresize-left' but negate `windresize-increment'."
  (interactive)
  (let ((i windresize-increment))
    (windresize-decrease-increment t)
    (windresize-down)
    (windresize-increase-increment t)))

(defun windresize-right-minus ()
  "Same as `windresize-left' but negate `windresize-increment'."
  (interactive)
  (let ((i windresize-increment))
    (windresize-decrease-increment t)
    (windresize-right)
    (windresize-increase-increment t)))

(defun windresize-up-minus ()
  "Same as `windresize-left' but negate `windresize-increment'."
  (interactive)
  (let ((i windresize-increment))
    (windresize-decrease-increment t)
    (windresize-up)
    (windresize-increase-increment t)))

(defun windresize-left-minus ()
  "Same as `windresize-left' but negate `windresize-increment'."
  (interactive)
  (let ((i windresize-increment))
    (windresize-decrease-increment t)
    (windresize-left)
    (windresize-increase-increment t)))

;;; Let's left/up borders have priority over right/bottom borders:

(defun windresize-left-force-left (&optional n)
  "If two movable borders, move the left border.
N is the number of lines by which moving borders."
  (interactive "P")
  (let ((i (if n (prefix-numeric-value n)
	     windresize-increment)))
    (windresize-left i t)))

(defun windresize-right-force-left (&optional n)
  "If two movable borders, move the left border.
N is the number of lines by which moving borders."
  (interactive "P")
  (let ((i (if n (prefix-numeric-value n)
	     windresize-increment)))
    (windresize-right i t)))

(defun windresize-up-force-up (n)
  "If two movable borders, move the upper border.
N is the number of lines by which moving borders."
  (interactive "P")
  (let ((i (if n (prefix-numeric-value n)
	     windresize-increment)))
    (windresize-up i t)))

(defun windresize-down-force-up (n)
  "If two movable borders, move the upper border.
N is the number of lines by which moving borders."
  (interactive)
  (let ((i (if n (prefix-numeric-value n)
	     windresize-increment)))
    (windresize-down i t)))

;;; Move the whole window, with fixed width/height:

(defun windresize-left-fixed ()
  "Move the window left, keeping its width constant."
  (interactive)
  (windresize-left nil t t))

(defun windresize-right-fixed ()
  "Move the window right, keeping its width constant."
  (interactive)
  (windresize-right nil t t))

(defun windresize-up-fixed ()
  "Move the window up, keeping its height constant."
  (interactive)
  (windresize-up nil t t))

(defun windresize-down-fixed ()
  "Move the window down, keeping its height constant."
  (interactive)
  (windresize-down nil t t))

;;; Move edges:

(defun windresize-bottom-right ()
  "Call `windresize-right' and `windresize-down' successively.
In move-borders method, move the bottom-right edge of the window
outwards.  In resize-window method, enlarge the window
horizontally and shrink it vertically."
  (interactive)
  (windresize-right)
  (windresize-down))

(defun windresize-up-left ()
  "Call `windresize-left' and `windresize-up' successively.
In move-borders method, move the upper-left edge of the window
outwards.  In resize-window method, shrink the window
horizontally and enlarge it vertically."
  (interactive)
  (windresize-left nil t)
  (windresize-up nil t))

(defun windresize-up-right ()
  "Call `windresize-right' and `windresize-up' successively.
In move-borders method, move the upper-right edge of the window
outwards.  In resize-window method, enlarge the window both
horizontally and horizontally."
  (interactive)
  (windresize-right)
  (windresize-up nil t))

(defun windresize-bottom-left ()
  "Call `windresize-left' and `windresize-up' successively.
In move-borders method, move the bottom-left edge of the window
outwards.  In resize-window method, shrink the window both
horizontally and vertically."
  (interactive)
  (windresize-left nil t)
  (windresize-down))

;;; Cancel, exit and help:

(defun windresize-cancel-and-quit ()
  "Cancel window resizing and quit `windresize'."
  (interactive)
  (if (eq major-mode 'help-mode)
      (progn (View-quit)
	     (setq windresize-msg '("Help quit" . 2)))
    (switch-to-buffer windresize-buffer)
    (set-window-configuration windresize-window-configuration-0)
    (setq overriding-local-map-menu-flag
	  windresize-overriding-terminal-local-map-0)
    (setq overriding-terminal-local-map
	  windresize-overriding-menu-flag-0)
    (message "Window resizing quit (not saved)")
    (windresize-remove-command-hooks)
    (setq windresize-resizing nil)))

(defun windresize-exit ()
  "Keep this window configuration and exit `windresize'."
  (interactive)
  (setq overriding-local-map-menu-flag
	windresize-overriding-terminal-local-map-0)
  (setq overriding-terminal-local-map
	windresize-overriding-menu-flag-0)
  (message "Window configuration set")
  (windresize-remove-command-hooks)
  (setq windresize-resizing nil))

(defun windresize-help ()
  "Display a help window for `windresize'."
  (interactive)
  (let ((pop-up-frames nil)		; otherwise we exit the loop
	(temp-buffer-show-hook
	 '(lambda ()
	    (fit-window-to-buffer)
	    (shrink-window-if-larger-than-buffer)
	    (goto-char (point-min))
	    (save-excursion
	      (while (re-search-forward
		      "^[ M][^\n:]+:\\|[0123~=oq]:\\|RET:" nil t)
		(add-text-properties (match-beginning 0)
				     (match-end 0) '(face bold))))))
	(help
	 "Use the arrow keys to move a border into the arrow direction.
Right and bottom borders have priority over left and up borders.
Press SPC to toggle between moving borders and resizing windows,
where arrow keys mean shrink/enlarge.

Here is a list of default keybindings:

    arrows:  move border or resize windows         =:  balance windows
M-S-arrows:  select adjacent window                o:  other-window
C-M-arrows:  move window with fixed width/height   0:  delete current window
  C-arrows:  temporarilly negate INCREMENT         ~:  negate INCREMENT
  M-arrows:  move with priority to left/up         1:  delete other windows
         i:  set INCREMENT (to numeric prefix)     2:  split window vertically
       +/-:  increase/decrease INCREMENT           3:  split window horizontally
         s:  save window configuration             q:  cancel and quit
         r:  restore window configuration          ?:  show this help window
       SPC:  toggle method: move border, resize  RET:  set and exit

         /:  move right-bottom edge outwards or left-upper edge inwards
       M-/:  move left-upper edge outwards or right-bottom edge inwards
         \\:  move right-upper edge outwards or left-bottom edge inwards
       M-\\:  move left-bottom edge outwards or right-upper edge inwards

See the docstring of `windresize' for detailed description."))
    (with-output-to-temp-buffer "*Help*"
      (princ help))))

(provide 'windresize)

;;; windresize.el ends here
