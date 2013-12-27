;;; ac-anything.el --- Auto Complete with Anything
;; $Id: ac-anything.el,v 1.2 2009/02/09 21:24:44 rubikitch Exp $

;; Copyright (C) 2009  rubikitch

;; Author: rubikitch <rubikitch@ruby-lang.org>
;; Keywords: convenience
;; URL: http://www.emacswiki.org/cgi-bin/wiki/download/ac-anything.el

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
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Auto Complete with Anything. It enables us to narrow candidates
;; with anything interface. If you have anything-match-plugin.el,
;; candidates can be narrowed many times.

;;; Requirements:

;; http://www.emacswiki.org/cgi-bin/wiki/download/anything.el
;; http://www.emacswiki.org/cgi-bin/wiki/download/auto-complete.el
;; http://www.emacswiki.org/cgi-bin/wiki/download/anything-match-plugin.el (optional)

;;; Installation:

;; Add below code in your ~/.emacs
;;
;; (require 'ac-anything)
;; (define-key ac-complete-mode-map (kbd "C-:") 'ac-complete-with-anything)

;; That's all.

;;; History:

;; $Log: ac-anything.el,v $
;; Revision 1.2  2009/02/09 21:24:44  rubikitch
;; *** empty log message ***
;;
;; Revision 1.1  2009/02/09 21:09:16  rubikitch
;; Initial revision
;;

;;; Code:

(defvar ac-anything-version "$Id: ac-anything.el,v 1.2 2009/02/09 21:24:44 rubikitch Exp $")
(require 'anything)
(require 'anything-match-plugin nil t)
(require 'auto-complete)

(defun ac-complete-with-anything ()
  "Select auto-complete candidates by `anything'.
It is useful to narrow candidates."
  (interactive)
  (when ac-completing
    (anything 'anything-c-source-auto-complete-candidates nil nil nil nil
              "*anything auto-complete*")))

(defun anything-c-auto-complete-init ()
  (anything-attrset 'ac-point ac-point)
  (anything-attrset 'ac-candidates ac-candidates)
  (anything-attrset 'menu-width (ac-menu-width ac-menu))
  (ac-abort))

(defun anything-c-auto-complete-action (string)
  (delete-region (anything-attr 'ac-point) (point))
  (insert string)
  (prog1 (let ((action (ac-get-candidate-property 'action string)))
           (if action (funcall action)))
    ;; for GC
    (anything-attrset 'ac-candidates nil)))
(defun anything-c-auto-complete-candidates ()
  (loop for x in (anything-attr 'ac-candidates) collect
        (cons
         (anything-aif (ac-get-candidate-property 'action x)
             (format "%s%s <%s>"
                     x
                     ;; padding
                     (make-string (- (anything-attr 'menu-width) (length x)) ? )
                     ;; action function name
                     it)
           x)
         x)))

(defvar anything-c-source-auto-complete-candidates
  '((name . "Auto Complete")
    (init . anything-c-auto-complete-init)
    (candidates . anything-c-auto-complete-candidates)
    (action . anything-c-auto-complete-action)
    (ac-point)
    (ac-candidates)
    (menu-width)))

(provide 'ac-anything)

;; How to save (DO NOT REMOVE!!)
;; (emacswiki-post "ac-anything.el")
;;; ac-anything.el ends here
