;;; flymake-haskell.el --- Flymake for haskell

;; Filename: flymake-haskell.el
;; Description: Flymake for haskell
;; Author: Andy Stewart <andy@freedom>
;; Maintainer: Andy Stewart <andy@freedom>
;; Copyright (C) 2013, Andy Stewart, all rights reserved.
;; Created: 2013-12-30 03:43:17
;; Version: 0.1
;; Last-Updated: 2013-12-30 03:43:17
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/flymake-haskell.el
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
;; Flymake for haskell
;; 

;;; Installation:
;;
;; Put flymake-haskell.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'flymake-haskell)
;;
;; No need more.

;;; Customize:
;;
;; 
;;
;; All of the above can customize by:
;;      M-x customize-group RET flymake-haskell RET
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


;;; Code:


(defun flymake-Haskell-init ()
  (flymake-simple-make-init-impl
   'flymake-create-temp-with-folder-structure nil nil
   (file-name-nondirectory buffer-file-name)
   'flymake-get-Haskell-cmdline))

(defun flymake-get-Haskell-cmdline (source base-dir)
  (list "ghc"
        (list "--make" "-fbyte-code" "-XFlexibleContexts"
              (concat "-i" base-dir) ;; can be expanded for additional -i options as in the Perl script
              source)))

(add-hook
 'haskell-mode-hook
 '(lambda ()
    ;; use add-to-list rather than push to avoid growing the list for every Haskell file loaded
    (add-to-list 'flymake-allowed-file-name-masks
                 '("\\.l?hs$" flymake-Haskell-init flymake-simple-java-cleanup))
    (add-to-list 'flymake-err-line-patterns
                 '("^\\(.+\\.l?hs\\):\\([0-9]+\\):\\([0-9]+\\):\\(\\(?:.\\|\\W\\)+\\)"
                   1 2 3 4))
    (set (make-local-variable 'multiline-flymake-mode) t)))

(provide 'flymake-haskell)

;;; flymake-haskell.el ends here
