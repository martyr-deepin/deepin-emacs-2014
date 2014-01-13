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

(defun webkit-get-window-allocation (&optional window)
  (let* ((window-edges (window-inside-pixel-edges window))
         (x (nth 0 window-edges))
         (y (nth 1 window-edges))
         (w (- (nth 2 window-edges) x))
         (h (- (nth 3 window-edges) y))
         )
    (list x y w h)))

(defun webkit-get-emacs-xid ()
  (frame-parameter nil 'window-id))

(defun webkit-generate-id ()
  (replace-regexp-in-string "\n" "" (shell-command-to-string "uuidgen")))

(defvar pyepc-file (expand-file-name "browser.py" (file-name-directory load-file-name)))

(defvar pyepc-browser
  (epc:start-epc (or (getenv "PYTHON") "python")
                 (list pyepc-file)))

(epc:define-method pyepc-browser
                   'message
                   (lambda (&rest args) (message "%S" args)))

(defun webkit-create-buffer (url)
  (let ((webkit-buffer (get-buffer-create (concat "*" url "*"))))
    (with-current-buffer webkit-buffer
      (webkit-mode))
    webkit-buffer))

(defun webkit-open-url (url)
  (interactive "sURL: ")
  (let* ((buffer (webkit-create-buffer url)))
    (with-current-buffer buffer
      (let* ((window-allocation (webkit-get-window-allocation))
             (x (nth 0 window-allocation))
             (y (nth 1 window-allocation))
             (w (nth 2 window-allocation))
             (h (nth 3 window-allocation))
             (view-id (webkit-generate-id)))
        (epc:call-deferred pyepc-browser 'create_buffer (list buffer-id url w h))
        ))
    (switch-to-buffer buffer)
    ))

(defun webkit-monitor-window-change (&rest _)
  (let ((view-infos)
        (selected-buffer (window-buffer (selected-window))))
    (dolist (window (window-list))
      (setq buffer (window-buffer window))
      (with-current-buffer buffer
        (message "%s" major-mode)
        (if (string= "webkit-mode" (format "%s" major-mode))
            (let* ((window-allocation (webkit-get-window-allocation window))
                   (x (nth 0 window-allocation))
                   (y (nth 1 window-allocation))
                   (w (nth 2 window-allocation))
                   (h (nth 3 window-allocation))
                   )
              (add-to-list 'view-infos (list buffer-id x y w h))
              (message "%s" (list buffer-id x y w h))
              ))))
    (epc:call-deferred pyepc-browser 'update_views (list (webkit-get-emacs-xid) view-infos))
    (message "%s" (length view-infos))
    (message "**************************")

    (with-current-buffer selected-buffer
      (if (string= "webkit-mode" (format "%s" major-mode))
          (let* ((window-allocation (webkit-get-window-allocation (selected-window)))
                 (x (nth 0 window-allocation))
                 (y (nth 1 window-allocation))
                 (w (nth 2 window-allocation))
                 (h (nth 3 window-allocation))
                 )
            (epc:call-deferred pyepc-browser 'adjust_view (list (webkit-get-emacs-xid) buffer-id (format "%s_%s" x y) x y w h))
            (message "%s" (list (webkit-get-emacs-xid) buffer-id (format "%s_%s" x y) x y w h))
            (message "----------------------")
            )))
    ))

(add-hook 'window-configuration-change-hook #'webkit-monitor-window-change)

(provide 'webkit)

;;; webkit.el ends here
