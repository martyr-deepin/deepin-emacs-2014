;;; hl-line+.el --- Extensions to hl-line.el.
;;
;; Filename: hl-line+.el
;; Description: Extensions to hl-line.el.
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 2006-2009, Drew Adams, all rights reserved.
;; Created: Sat Aug 26 18:17:18 2006
;; Version: 22.0
;; Last-Updated: Sun Feb 15 17:16:15 2009 (-0800)
;;           By: dradams
;;     Update #: 396
;; URL: http://www.emacswiki.org/cgi-bin/wiki/hl-line+.el
;; Keywords: highlight, cursor, accessibility
;; Compatibility: GNU Emacs 22.x
;;
;; Features that might be required by this library:
;;
;;   `hl-line'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;  This library extends standard library `hl-line.el' in these ways:
;;
;;  1. As an alternative to turning on `hl-line' highlighting at all
;;     times, you can turn it on only when Emacs is idle.  To do that,
;;     use command `toggle-hl-line-when-idle' and customize
;;     `global-hl-line-mode' to nil.
;;
;;  2. As another alternative, you can turn it on for only a few
;;     seconds.  To do that, use command `flash-line-highlight' and
;;     customize `global-hl-line-mode' to nil.
;;
;;  3. It provides a face, `hl-line', that you can customize, instead
;;     of using option `hl-line-face'.
;;
;;     I suggested #3 to the Emacs developers, and it has been added
;;     to Emacs 22, but with a different default value.  If you use
;;     library `crosshairs.el', you might want to customize this to a
;;     value similar to what is used there, so that the horizontal and
;;     vertical highlights will be the same.
;;
;;  4. It provides local and global modes to highlight several lines
;;     surrounding point using a different face, `hl-spotlight'.  You
;;     can enlarge or shrink this spotlight highlighting using command
;;     `hl-spotlight-enlarge'.  You can repeat, via `C-x z z z z...',
;;     to enlarge or shrink incrementally.
;;
;;     Spotlight highlighting can be used together with library
;;     `centered-cursor-mode.el', which keeps point (hence also the
;;     spotlight) centered in the window.  This can be helpful when
;;     reading text (as opposed to code).  This is controlled by user
;;     option `hl-spotlight-keep-centered-flag'. You can obtain
;;     library `centered-cursor-mode.el' here:
;;     http://www.emacswiki.org/emacs/centered-cursor-mode.el.
;;
;;  To use this library, put this in your Emacs init file (~/.emacs):
;;
;;    (require 'hl-line+) ; Load this file (it will load `hl-line.el')
;;
;;  To turn on `global-hl-line-mode' only when Emacs is idle, by
;;  default, add this line also to your init file:
;;
;;    (toggle-hl-line-when-idle 1) ; Highlight only when idle
;;
;;  You can use command `toggle-hl-line-when-idle' to turn idle
;;  highlighting on and off at any time.  You can use command
;;  `hl-line-when-idle-interval' to change the number of idle seconds
;;  to wait before highlighting.
;;
;;
;;  See also:
;;
;;  * Library `col-highlight.el', which highlights the current column.
;;
;;  * Library `crosshairs.el', which highlights the current line and
;;    the current column, at the same time.  It requires libraries
;;    `col-highlight.el' and `hl-line+.el'.
;;
;;  * Library `cursor-chg.el' or library `oneonone.el', to change the
;;    cursor type when Emacs is idle.
;;
;;
;;  Faces defined here:
;;
;;    `hl-line', `hl-spotlight'.
;;
;;  User options defined here:
;;
;;    `hl-line-flash-show-period', `hl-spotlight-height',
;;    `hl-spotlight-keep-centered-flag'.
;;
;;  Commands defined here:
;;
;;    `flash-line-highlight', `global-hl-spotlight-mode',
;;    `hl-line-flash', `hl-line-toggle-when-idle',
;;    `hl-line-when-idle-interval', `hl-spotlight-enlarge',
;;    `hl-spotlight-mode', `toggle-hl-line-when-idle'.
;;
;;  Non-interactive functions defined here:
;;
;;    `hl-line-highlight-now', `hl-line-unhighlight-now',
;;    `hl-spotlight-limits'.
;;
;;  Internal variables defined here:
;;
;;    `hl-line-idle-interval', `hl-line-idle-timer',
;;    `hl-line-when-idle-p', `hl-spotlight-old-state'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;; 2009/02/15 dadams
;;     Added: hl-spotlight(-height|-old-state|widen|limits|mode),
;;            global-hl-spotlight-mode.
;; 2008/01/28 dadams
;;     Fix from Yann Yodique: Moved adding/removing hl-line-unhighlight-now as
;;     pre-command-hook from hl-line-toggle-when-idle to hl-line-(un)highlight-now.
;; 2008/01/20 dadams
;;     Renamed: line-show-period to hl-line-flash-show-period.
;; 2007/10/11 dadams
;;     Commentary typo: toggle-cursor-type-when-idle -> toggle-hl-line-when-idle.
;; 2007/01/10 dadams
;;     Update commentary to indicate that the face is now provided by default.
;; 2006/09/08 dadams
;;     Added: flash-line-highlight, hl-line-flash.
;;     Renamed: hl-line-when-idle(-off) to hl-line-(un)highlight-now.
;; 2006/09/04 dadams
;;     Added: hl-line-when-idle-p, hl-line-idle-interval, hl-line-idle-timer,
;;            hl-line-toggle-when-idle, hl-line-when-idle-interval,
;;            hl-line-when-idle(-off).
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'hl-line)

(defvar hl-line-face)                   ; Quiet the byte-compiler.
(defvar global-hl-line-mode)            ; Quiet the byte-compiler.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This will be ignored, since this is now defined by default in Emacs 22.
;; I include it here as a different face definition that you might want to try.
(defface hl-line '((t (:background "SlateGray3"))) ; Try also (:underline "Yellow")
  "Face to use for `hl-line-face'." :group 'hl-line)
(setq hl-line-face 'hl-line)

(defcustom hl-line-flash-show-period 1
  "Number of seconds for `hl-line-flash' to highlight the line."
  :type 'integer :group 'cursor :group 'hl-line)

(defvar hl-line-idle-interval 5
  "Number of seconds to wait before turning on `global-hl-line-mode'.
Do NOT change this yourself to change the wait period; instead, use
`\\[hl-line-when-idle-interval]'.")

(defvar hl-line-idle-timer
  (progn                                ; Cancel to prevent duplication.
    (when (boundp 'hl-line-idle-timer) (cancel-timer hl-line-idle-timer))
    (run-with-idle-timer hl-line-idle-interval t 'hl-line-highlight-now))
  "Timer used to turn on `global-hl-line-mode' whenever Emacs is idle.")

;; Turn it off, by default.  You must use `toggle-hl-line-when-idle' to turn it on.
(cancel-timer hl-line-idle-timer)

(defvar hl-line-when-idle-p nil
  "Non-nil means to turn on `global-hl-line-mode' whenever Emacs is idle.
Do NOT change this yourself; instead, use `\\[toggle-hl-line-when-idle]'.")

(defalias 'toggle-hl-line-when-idle 'hl-line-toggle-when-idle)
(defun hl-line-toggle-when-idle (&optional arg)
  "Turn on or off using `global-hl-line-mode' when Emacs is idle.
When on, use `global-hl-line-mode' whenever Emacs is idle.
With prefix argument, turn on if ARG > 0; else turn off."
  (interactive "P")
  (setq hl-line-when-idle-p
        (if arg (> (prefix-numeric-value arg) 0) (not hl-line-when-idle-p)))
  (cond (hl-line-when-idle-p
         (timer-activate-when-idle hl-line-idle-timer)
         (message "Turned ON using `global-hl-line-mode' when Emacs is idle."))
        (t
         (cancel-timer hl-line-idle-timer)
         (message "Turned OFF using `global-hl-line-mode' when Emacs is idle."))))

(defun hl-line-when-idle-interval (secs)
  "Set wait until using `global-hl-line-mode' when Emacs is idle.
Whenever Emacs is idle for this many seconds, `global-hl-line-mode'
will be turned on.

To turn on or off using `global-hl-line-mode' when idle,
use `\\[toggle-hl-line-when-idle]."
  (interactive "nSeconds to idle, before using `global-hl-line-mode': ")
  (timer-set-idle-time hl-line-idle-timer (setq hl-line-idle-interval secs) t))

(defun hl-line-highlight-now ()
  "Turn on `global-hl-line-mode' and highlight current line now."
  (unless global-hl-line-mode
    (global-hl-line-mode 1)
    (global-hl-line-highlight)
    (add-hook 'pre-command-hook 'hl-line-unhighlight-now)))

(defun hl-line-unhighlight-now ()
  "Turn off `global-hl-line-mode' and unhighlight current line now."
  (global-hl-line-mode -1)
  (global-hl-line-unhighlight)
  (remove-hook 'pre-command-hook 'hl-line-unhighlight-now))

(defalias 'flash-line-highlight 'hl-line-flash)
(defun hl-line-flash (&optional arg)
  "Highlight the current line for `hl-line-flash-show-period' seconds.
With a prefix argument, highlight for that many seconds."
  (interactive)
  (hl-line-highlight-now)
  (let ((line-period hl-line-flash-show-period))
    (when current-prefix-arg
      (setq line-period (prefix-numeric-value current-prefix-arg)))
    (run-at-time line-period nil #'hl-line-unhighlight-now)))


;;; Spotlight modes

(defface hl-spotlight
  '((t :inherit highlight))
  "Face for the spotlight in Hl-Line-Window mode."
  :group 'hl-line)

(defcustom hl-spotlight-height 2
  "*Number of lines to highlight, before and after the current line."
  :type 'integer :group 'hl-line)

(defcustom hl-spotlight-keep-centered-flag t
  "*Non-nil means keep the cursor and spotlight centered in the window.
This has no effect unless library `centered-cursor-mode' is available."
  :type 'boolean :group 'hl-line)

(defvar hl-spotlight-old-state nil
  "Saved Hl-Line mode values, before `hl-spotlight-mode'.")

(defun hl-spotlight-enlarge (n)
  "Enlarge the hl-line spotlight by N lines.
N is the numeric prefix arg (one, by default).
A negative prefix arg shrinks the spotlight.
The spotlight is used by `hl-spotlight-mode' and
`global-hl-spotlight-mode'."
  (interactive "p")
  (set-variable 'hl-spotlight-height (+ n hl-spotlight-height))
  (when global-hl-spotlight-mode (global-hl-line-highlight))
  (when hl-spotlight-mode (hl-line-highlight)))

(defun hl-spotlight-limits ()
  "Return a cons of the limits to use for `hl-line-range-function'."
  (let ((start  (save-excursion (forward-line (- hl-spotlight-height)) (point)))
        (end    (save-excursion (forward-line (1+ hl-spotlight-height)) (point))))
    (cons start end)))

(define-minor-mode hl-spotlight-mode
  "Buffer-local minor mode to highlight lines surrounding point.
With ARG, turn Hl-Spotlight mode on if ARG is positive, off otherwise.

Hl-Spotlight mode uses Hl-Line mode.  Whenever Hl-Spotlight mode is on
in the current buffer, its overlay is used by Hl-Line mode, which
means that face `hl-spotlight' and option `hl-spotlight-height' are
used; face `hl-line' is not used.

Turn the spotlight on and off by using toggle command
`hl-spotlight-mode'.  After turning Hl-Spotlight mode on, command
`hl-line-mode' also toggles the spotlight on and off, but without
turning off Hl-Spotlight mode.  To return to the normal behavior of
`hl-line-mode', you must turn off Hl-Spotlight mode.  Turning off
Hl-Spotlight mode also turns off Hl-Line mode."
  :group 'hl-line
  (cond (hl-spotlight-mode
         (unless hl-spotlight-old-state
           (setq hl-spotlight-old-state  (list hl-line-face
                                               hl-line-overlay
                                               global-hl-line-overlay
                                               hl-line-range-function
                                               hl-line-sticky-flag)))
         (hl-line-unhighlight)
         (setq hl-line-overlay         nil
               hl-line-face            'hl-spotlight
               hl-line-range-function  'hl-spotlight-limits
               hl-line-sticky-flag     nil)
         (when (and (require 'centered-cursor-mode nil t)
                    hl-spotlight-keep-centered-flag)
           (centered-cursor-mode 1))
         ;; In case `kill-all-local-variables' is called.
         (add-hook 'change-major-mode-hook #'hl-line-unhighlight nil t)
         (if hl-line-sticky-flag
             (remove-hook 'pre-command-hook #'hl-line-unhighlight t)
           (add-hook 'pre-command-hook #'hl-line-unhighlight nil t))
         (add-hook 'post-command-hook #'hl-line-highlight nil t))
        (t
         (setq hl-line-face             (nth 0 hl-spotlight-old-state)
               hl-line-overlay          (nth 1 hl-spotlight-old-state)
               global-hl-line-overlay   (nth 2 hl-spotlight-old-state)
               hl-line-range-function   (nth 3 hl-spotlight-old-state)
               hl-line-sticky-flag      (nth 4 hl-spotlight-old-state))
         (when hl-spotlight-old-state (setq hl-spotlight-old-state  nil))
         (when (require 'centered-cursor-mode nil t) (centered-cursor-mode -1))
         (remove-hook 'post-command-hook #'hl-line-highlight t)
         (hl-line-unhighlight)
         (remove-hook 'change-major-mode-hook #'hl-line-unhighlight t)
         (remove-hook 'pre-command-hook #'hl-line-unhighlight t)))
  (hl-line-mode (if hl-spotlight-mode 1 -1)))

(define-minor-mode global-hl-spotlight-mode
  "Global minor mode to highlight lines around point in current window.
With ARG, turn Global-Hl-Spotlight mode on if ARG is positive, off
otherwise.

See `hl-spotlight-mode'.  The interaction between
`global-hl-spotlight-mode' and `global-hl-line-mode' is similar to
that between `hl-spotlight-mode' and `hl-line-mode'."
  :global t :group 'hl-line
  (cond (global-hl-spotlight-mode
         (unless hl-spotlight-old-state
           (setq hl-spotlight-old-state  (list hl-line-face
                                               hl-line-overlay
                                               global-hl-line-overlay
                                               hl-line-range-function
                                               hl-line-sticky-flag)))
         (global-hl-line-unhighlight)
         (setq global-hl-line-overlay  nil
               hl-line-face            'hl-spotlight
               hl-line-range-function  'hl-spotlight-limits
               hl-line-sticky-flag     nil)
         (when (and (require 'centered-cursor-mode nil t)
                    hl-spotlight-keep-centered-flag)
           (global-centered-cursor-mode 1))
         (add-hook 'pre-command-hook #'global-hl-line-unhighlight)
         (add-hook 'post-command-hook #'global-hl-line-highlight))
        (t
         (setq hl-line-face            (nth 0 hl-spotlight-old-state)
               hl-line-overlay         (nth 1 hl-spotlight-old-state)
               global-hl-line-overlay  (nth 2 hl-spotlight-old-state)
               hl-line-range-function  (nth 3 hl-spotlight-old-state)
               hl-line-sticky-flag     (nth 4 hl-spotlight-old-state))
         (when hl-spotlight-old-state (setq hl-spotlight-old-state  nil))
         (when (require 'centered-cursor-mode nil t)
           (global-centered-cursor-mode -1))
         (global-hl-line-unhighlight)
         (remove-hook 'pre-command-hook #'global-hl-line-unhighlight)
         (remove-hook 'post-command-hook #'global-hl-line-highlight)))
  (global-hl-line-mode (if global-hl-spotlight-mode 1 -1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'hl-line+)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; hl-line+.el ends here
