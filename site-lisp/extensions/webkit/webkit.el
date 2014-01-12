;;; webkit.el --- Running WebKit browser in Emacs

;; Filename: webkit.el
;; Description: Running WebKit browser in Emacs
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2014, Andy Stewart, all rights reserved.
;; Created: 2014-01-02 21:51:17
;; Version: 0.1
;; Last-Updated: 2014-01-02 21:51:17
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/webkit.el
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
;; Running WebKit browser in Emacs
;;

;;; Installation:
;;
;; Put webkit.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'webkit)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET webkit RET
;;

;;; Change log:
;;
;; 2014/01/02
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

(require 'epc)
(when noninteractive
  (load "subr")
  (load "byte-run"))
(eval-when-compile (require 'cl))

;;; Code:

(defcustom webkit-mode-hook '()
  "WebKit mode hook."
  :type 'hook
  :group 'webkit-mode)

(defvar webkit-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap used by `webkit-mode'.")

(define-derived-mode webkit-mode text-mode "WebKit"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'webkit-mode)
  (setq mode-name "WebKit")
  (set (make-local-variable 'buffer-id) (webkit-generate-id))
  (use-local-map webkit-mode-map)
  (run-hooks 'webkit-mode-hook))

(defun webkit-get-window-info ()
  (let* ((window-edges (window-inside-pixel-edges))
         (x (nth 0 window-edges))
         (y (nth 1 window-edges))
         (w (- (nth 2 window-edges) x))
         (h (- (nth 3 window-edges) y))
         )
    (list (frame-parameter nil 'window-id) x y w h)))

(defun webkit-create-buffer (url)
  (let ((webkit-buffer (get-buffer-create (concat "*" url "*"))))
    (with-current-buffer webkit-buffer
      (webkit-mode))
    webkit-buffer))

(defun webkit-generate-id ()
  (replace-regexp-in-string "\n" "" (shell-command-to-string "uuidgen")))

(defvar pyepc-file (expand-file-name "browser.py" (file-name-directory load-file-name)))

(defvar pyepc-browser
  (epc:start-epc (or (getenv "PYTHON") "python")
                 (list pyepc-file)))

(epc:define-method pyepc-browser
                   'message
                   (lambda (&rest args) (message "%S" args)))

(defun webkit-open-url (url)
  (interactive "sURL: ")
  (let ((buffer (webkit-create-buffer url))
        (window-info (webkit-get-window-info)))
    (switch-to-buffer buffer)
    (epc:call-deferred pyepc-browser 'create_buffer (list buffer-id url (nth 3 window-info) (nth 4 window-info)))
    (epc:call-deferred pyepc-browser 'create_view (append (list buffer-id) window-info))
    ))

(provide 'webkit)

;;; webkit.el ends here
