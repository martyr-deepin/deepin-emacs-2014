;;; flymake-config.el --- Config for flymake

;; Filename: flymake-config.el
;; Description: Config for flymake
;; Author: Andy Stewart <andy@freedom>
;; Maintainer: Andy Stewart <andy@freedom>
;; Copyright (C) 2013, Andy Stewart, all rights reserved.
;; Created: 2013-12-30 03:51:42
;; Version: 0.1
;; Last-Updated: 2013-12-30 03:51:42
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/flymake-config.el
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
;; Config for flymake
;; 

;;; Installation:
;;
;; Put flymake-config.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'flymake)
;;
;; No need more.

;;; Customize:
;;
;; 
;;
;; All of the above can customize by:
;;      M-x customize-group RET flymake RET
;;

;;; Change log:
;;	
;; 2013/12/30
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

(require 'flymake)          
(require 'flymake-extension)
(require 'flymake-shell)    
(require 'flymake-java)    
(require 'flymake-c++)    
(require 'flymake-c)    
(require 'flymake-haskell)    
(require 'flymake-python)   
(require 'flymake-jslint)   
(require 'flymake-go)       

;;; Code:

(provide 'flymake-config)

;;; flymake-config.el ends here
