;;; modeline-posn.el --- Set up `mode-line-position'.
;; 
;; Filename: modeline-posn.el
;; Description: Set up `mode-line-position'.
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 2006-2007, Drew Adams, all rights reserved.
;; Created: Thu Sep 14 08:15:39 2006
;; Version: 22.0
;; Last-Updated: Mon Apr 02 13:39:36 2007 (-25200 Pacific Daylight Time)
;;           By: dradams
;;     Update #: 53
;; URL: http://www.emacswiki.org/cgi-bin/wiki/modeline-posn.el
;; Keywords: mode-line, region, column
;; Compatibility: GNU Emacs 22.x
;; 
;; Features that might be required by this library:
;;
;;   None
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; 
;;  Change variable `mode-line-position' so that the following changes
;;  are made to the mode line:
;;
;;  1. Highlight the column number when the current column is greater
;;     than limit `modelinepos-column-limit'.  Face
;;     `modelinepos-column-warning' is used for the highlighting.
;;
;;  2. Make `size-indication-mode' show the size of the region,
;;     instead of the buffer size, whenever the region is active.
;;
;;  Note: Loading this library changes the default definition of
;;        `mode-line-position'.
;;
;;  To use this library, put this in your Emacs init file (~/.emacs):
;;
;;    (require 'modeline-posn)
;;
;;  To show the column number highlighting, turn on Column Number
;;  mode.  You can do that in your Emacs init file this way:
;;
;;    (column-number-mode 1)
;;
;;  To show the buffer and region size indication in the mode line,
;;  turn on Size Indication.  You can do that in your Emacs init file
;;  this way:
;;
;;    (size-indication-mode 1) ; Turn on Size Indication mode.
;;
;;  You can customize `modelinepos-column-limit' or bind it to
;;  different values for different modes.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change log:
;;
;; 2007/04/02 dadams
;;     Added modelinepos-column-warning.  Thx to AmitPatel for the suggestion.
;; 2006/09/14 dadams
;;     Created.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defface modelinepos-column-warning '((t (:foreground "Red")))
  "*Face used to highlight the modeline column number.
This is used when the current column number is greater than
`modelinepos-column-limit'."
  :group 'Modeline :group 'Convenience :group 'Help :group 'faces)

(defcustom modelinepos-column-limit 70
  "Current column greater than this means highlight column in mode-line."
  :type 'integer :group 'Modeline :group 'Convenience :group 'Help)



;; REPLACES ORIGINAL defined in `simple.el'
;; Doc string updated to mention region size indication.
;; Added groups `Modeline', `Convenience', and `Help'.
;;
(define-minor-mode size-indication-mode
    "Toggle Size Indication mode.
With arg, turn Size Indication mode on iff arg is positive.
When Size Indication mode is enabled, the buffer or region size
appears in the mode line.  If Transient Mark mode is enabled, the
region size is shown; otherwise, the size of the accessible part
of the buffer is shown."
  :global t :group 'editing-basics :group 'Modeline
  :group 'Convenience :group 'Help)



;; REPLACES ORIGINAL defined in `bindings.el'.
;; 1. Use region size if region is active.
;; 2. Highlight line & column indicator if column > `modelinepos-column-limit'.
;;
(setq-default mode-line-position
              '(:eval
                (let ((help-echo "mouse-1: select (drag to resize), mouse-2: \
delete others, mouse-3: delete this"))
                  `((-3 ,(propertize "%p" 'help-echo help-echo))
                    (size-indication-mode
                     (8 ,(propertize
                          (if (and transient-mark-mode mark-active)
                              (format " %d chars" (abs (- (mark t) (point))))
                            " of %I")
                          'face (and transient-mark-mode mark-active 'region)
                          'help-echo help-echo)))
                    (line-number-mode
                     ((column-number-mode
                       (10 ,(propertize
                             " (%l,%c)"
                             'face (and (> (current-column)
                                           modelinepos-column-limit)
                                        'modelinepos-column-warning)
                             'help-echo help-echo))
                       (6 ,(propertize " L%l" 'help-echo help-echo))))
                     ((column-number-mode
                       (5 ,(propertize
                            " C%c"
                            'face (and (> (current-column)
                                          modelinepos-column-limit)
                                       'modelinepos-column-warning)
                            'help-echo help-echo)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'modeline-posn)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; modeline-posn.el ends here
