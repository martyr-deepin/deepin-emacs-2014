;;; init-iedit.el --- Init for iedit

;; Filename: init-iedit.el
;; Description: Init for iedit
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2014, Andy Stewart, all rights reserved.
;; Created: 2014-10-06 13:20:36
;; Version: 0.1
;; Last-Updated: 2014-10-06 13:20:36
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/init-iedit.el
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
;; Init for iedit
;;

;;; Installation:
;;
;; Put init-iedit.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'init-iedit)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET init-iedit RET
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

(require 'iedit)
(require 'iedit-lib)

;;; Code:

(setq iedit-toggle-key-default (kbd "s-m"))
(when iedit-toggle-key-default
  (define-key global-map iedit-toggle-key-default 'iedit-mode)
  (define-key isearch-mode-map iedit-toggle-key-default 'iedit-mode-from-isearch)
  (define-key esc-map iedit-toggle-key-default 'iedit-execute-last-modification)
  (define-key help-map iedit-toggle-key-default 'iedit-mode-toggle-on-function)
  )

(provide 'init-iedit)

;;; init-iedit.el ends here
