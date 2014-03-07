;;; init-auto-indent-mode.el --- Init for auto-indent-mode

;; Filename: init-auto-indent-mode.el
;; Description: Init for auto-indent-mode
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2014, Andy Stewart, all rights reserved.
;; Created: 2014-03-07 09:48:21
;; Version: 0.1
;; Last-Updated: 2014-03-07 09:48:21
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/init-auto-indent-mode.el
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
;; Init for auto-indent-mode
;;

;;; Installation:
;;
;; Put init-auto-indent-mode.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'init-auto-indent-mode)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET init-auto-indent-mode RET
;;

;;; Change log:
;;
;; 2014/03/07
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
               'qml-mode-hook
               'emacs-lisp-mode-hook
               ))
  (add-hook hook (lambda ()
                   (require 'auto-indent-mode)
                   (auto-indent-mode-on))))


(provide 'init-auto-indent-mode)

;;; init-auto-indent-mode.el ends here
