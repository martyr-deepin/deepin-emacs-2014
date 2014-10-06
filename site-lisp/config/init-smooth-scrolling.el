;;; init-smooth-scrolling.el --- Init for smooth-scrolling

;; Filename: init-smooth-scrolling.el
;; Description: Init for smooth-scrolling
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2014, Andy Stewart, all rights reserved.
;; Created: 2014-10-06 13:07:42
;; Version: 0.1
;; Last-Updated: 2014-10-06 13:07:42
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/init-smooth-scrolling.el
;; Keywords:
;; Compatibility: GNU Emacs 24.4.50.1
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
;; Init for smooth-scrolling
;;

;;; Installation:
;;
;; Put init-smooth-scrolling.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'init-smooth-scrolling)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET init-smooth-scrolling RET
;;

;;; Change log:
;;
;; 2014/10/06
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

(require 'smooth-scrolling)

;;; Code:

(setq smooth-scroll-margin 2)           ;触发滚动的行数

(provide 'init-smooth-scrolling)

;;; init-smooth-scrolling.el ends here
