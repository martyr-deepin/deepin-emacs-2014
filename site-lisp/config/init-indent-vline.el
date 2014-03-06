;;; init-indent-vline.el --- Init for indent-vline

;; Filename: init-indent-vline.el
;; Description: Init for indent-vline
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2014, Andy Stewart, all rights reserved.
;; Created: 2014-03-06 17:13:59
;; Version: 0.1
;; Last-Updated: 2014-03-06 17:13:59
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/init-indent-vline.el
;; Keywords:
;; Compatibility: GNU Emacs 24.3.50.1
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
;; Init for indent-vline
;;

;;; Installation:
;;
;; Put init-indent-vline.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'init-indent-vline)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET init-indent-vline RET
;;

;;; Change log:
;;
;; 2014/03/06
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

(dolist (hook (list
               'web-mode-hook
               ))
  (add-hook hook (lambda () (indent-hint-mode))))
(autoload 'indent-hint-mode "indent-vline")

(provide 'init-indent-vline)

;;; init-indent-vline.el ends here
