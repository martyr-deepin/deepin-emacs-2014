;;; init-benchmark.el --- Init for benchmark

;; Filename: init-benchmark.el
;; Description: Init for benchmark
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2014, Andy Stewart, all rights reserved.
;; Created: 2014-10-10 22:12:00
;; Version: 0.1
;; Last-Updated: 2014-10-10 22:12:00
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/init-benchmark.el
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
;; Init for benchmark
;;

;;; Installation:
;;
;; Put init-benchmark.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'init-benchmark)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET init-benchmark RET
;;

;;; Change log:
;;
;; 2014/10/10
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

(require 'benchmark-init-modes)
(require 'lazy-set-key)

;;; Code:

(lazy-set-key vi-move-key-alist benchmark-init/tabulated-mode-map) ;vi-mode的局部按键
(lazy-set-key vi-move-key-alist benchmark-init/tree-mode-map)      ;vi-mode的局部按键

(provide 'init-benchmark)

;;; init-benchmark.el ends here
