;;; ispell-extension.el --- Some extension for ispell

;; Filename: ispell-extension.el
;; Description: Some extension for ispell
;; Author: Andy Stewart lazycat.manatee@gmail.com
;; Maintainer: Andy Stewart lazycat.manatee@gmail.com
;; Copyright (C) 2009, Andy Stewart, all rights reserved.
;; Created: 2009-01-06 12:55:01
;; Version: 0.1
;; Last-Updated: 2009-01-06 12:55:01
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/ispell-extension.el
;; Keywords:
;; Compatibility: GNU Emacs 23.0.60.1
;;
;; Features that might be required by this library:
;;
;;
;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Some extension for ispell
;;

;;; Installation:
;;
;; Put ispell-extension.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'ispell-extension)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET ispell-extension RET
;;

;;; Change log:
;;
;; 2009/01/06
;;      * First released.
;;

;;; Acknowledgements:
;;
;;
;;

;;; TODO
;;
;;
;;

;;; Require


;;; Code:

(defadvice ispell-add-per-file-word-list (around modified-comment-style activate)
  "Modified ispell keyword comment style in special mode."
  (if (eq major-mode 'emacs-lisp-mode)
      (let ((temp-comment-start comment-start))
        (setq comment-start ";;;")
        ad-do-it
        (setq comment-start temp-comment-start))
    ad-do-it))

(provide 'ispell-extension)

;;; ispell-extension.el ends here

;;; LocalWords:  ispell el
