;;; emaf.el --- Emacs multimedia application framework

;; Filename: emaf.el
;; Description: Emacs multimedia application framework
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2014, Andy Stewart, all rights reserved.
;; Created: 2014-10-12 10:30:31
;; Version: 0.1
;; Last-Updated: 2014-10-12 10:30:31
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/emaf.el
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
;; Emacs multimedia application framework
;;

;;; Installation:
;;
;; Put emaf.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'emaf)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET emaf RET
;;

;;; Change log:
;;
;; 2014/10/12
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

(require 'dbus)

;;; Code:

(defvar emaf-python-file (expand-file-name "emaf.py" (file-name-directory load-file-name)))

(defvar emaf-process nil)

(defcustom emaf-name "*emaf*"
  "Name of emaf buffer."
  :type 'string
  :group 'emaf)

(defun emaf-call (method &rest args)
  (with-demoted-errors "emaf-call ERROR: %s"
    (apply 'dbus-call-method
           :session                     ; use the session (not system) bus
           "com.deepin.emaf"            ; service name
           "/com/deepin/emaf"           ; path name
           "com.deepin.emaf"            ; interface name
           method args)))

(defun emaf-get-emacs-xid ()
  (frame-parameter nil 'window-id))

(defun emaf-get-frame-height ()
  (- (frame-pixel-height) (window-pixel-height (minibuffer-window))))

(defun emaf-start-process ()
  (setq emaf-process
        (apply 'start-process
               emaf-name
               emaf-name
               "python" (list emaf-python-file (emaf-get-emacs-xid) (format "%s" (frame-pixel-width)) (format "%s" (emaf-get-frame-height)))))
  (set-process-query-on-exit-flag emaf-process nil)
  (set-process-sentinel
   emaf-process
   #'(lambda (process event)
       (message (format "%s %s" process event))
       ))
  )

(defun emaf-stop-process ()
  (delete-process emaf-process)
  )

(defun emaf-update-frame-size ()
  (emaf-call "update_frame_size" (frame-pixel-width) (emaf-get-frame-height))
  )

(defun emaf-show ()
  (emaf-call "show")
  )

(defun emaf-hide ()
  (emaf-call "hide")
  )

(defun emaf-enable ()
  (interactive)
  (emaf-start-process)
  (emaf-show)
  (add-hook 'window-configuration-change-hook #'emaf-update-frame-size)
  )

(defun emaf-disable ()
  (interactive)
  (emaf-hide)
  (remove-hook 'window-configuration-change-hook 'emaf-update-frame-size)
  (emaf-stop-process)
  )

(define-minor-mode emaf-mode
  :global t
  :group 'emaf-mode
  (if emaf-mode
      (emaf-enable)
    (emaf-disable))
  )

(provide 'emaf)

;;; emaf.el ends here
