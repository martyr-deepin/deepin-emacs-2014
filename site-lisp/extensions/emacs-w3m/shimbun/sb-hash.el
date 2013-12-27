;;; sb-hash.el --- shimbun backend for contents hashing -*- coding: iso-2022-7bit -*-

;; Copyright (C) 2006 Tsuyoshi CHO <tsuyoshi_cho@ybb.ne.jp>

;; Author: Tsuyoshi CHO <tsuyoshi_cho@ybb.ne.jp>
;; Keywords: shimbun

;; This file is a part of shimbun.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; method memo
;; hash save/restore
;; shimbun-hash-set-item shimbun id item
;; shimbun-hash-get-item shimbun id
;;  # if item is existence, call update method
;; item update
;; shimbun-hash-update-items shimbun
;; shimbun-hash-update-items-impl (generic) shimbun
;;  # page fetched,require hash creation.
;; shimbun-hash-contents-url
;;  # normaly,using `shimbun-index-url'

;;; recommend
;; shimbun-get-headers
;;  # if runnable, shimbun-hash-update-items call(re-use loaded page)
;; shimbun-make-contents
;;  # hashed item from hash(if nil,call update) loaded to buffer.
;;  # work only mail body orthopedics.

;;; Code:

(require 'shimbun)

(eval-and-compile
  (luna-define-class shimbun-hash (shimbun) (content-hash))
  (luna-define-internal-accessors 'shimbun-hash))

(defvar shimbun-hash-content-hash-length 31)

(luna-define-method initialize-instance :after ((shimbun shimbun-hash)
						&rest init-args)
  (shimbun-hash-set-content-hash-internal
   shimbun
   (make-vector shimbun-hash-content-hash-length 0))
  shimbun)

(luna-define-generic shimbun-hash-get-item (shimbun id)
  "Return target ID related contents.")
(luna-define-method shimbun-hash-get-item ((shimbun shimbun-hash) id)
  (let ((sym (intern-soft
	      id (shimbun-hash-content-hash-internal shimbun))))
    (when sym
      (symbol-value sym))))

(luna-define-generic shimbun-hash-set-item (shimbun id item)
  "Save ID related contents ITEM to hash.")
(luna-define-method shimbun-hash-set-item ((shimbun shimbun-hash) id item)
  (set (intern id (shimbun-hash-content-hash-internal shimbun)) item))

(luna-define-generic shimbun-hash-contents-url (shimbun)
  "Return contents url.")
(luna-define-method shimbun-hash-contents-url ((shimbun shimbun-hash))
  (shimbun-index-url shimbun))

(luna-define-generic shimbun-hash-update-items (shimbun)
  "Update hash items.
Call timing for `shimbun-get-headers' and `shimbun-article'
(`shimbun-make-contents'). Need implements `shimbun-hash-update-items-impl'.")
(luna-define-method shimbun-hash-update-items ((shimbun shimbun-hash))
  (with-temp-buffer
    (erase-buffer)
    (shimbun-retrieve-url (shimbun-hash-contents-url shimbun)
			  'reload)
    (shimbun-hash-update-items-impl shimbun)))

(luna-define-generic shimbun-hash-update-items-impl (shimbun)
  "Update hash items main routine (need implements).")

(luna-define-method shimbun-article ((shimbun shimbun-hash) header
				     &optional outbuf)
  (when (shimbun-current-group-internal shimbun)
    (with-current-buffer (or outbuf (current-buffer))
      (w3m-insert-string
       (or (with-temp-buffer
	     (erase-buffer)
	     (let ((buf-string nil)
		   id)
	       (setq id (shimbun-header-id header))
	       (setq buf-string (shimbun-hash-get-item shimbun id))
	       (unless buf-string
		 (shimbun-hash-update-items shimbun)
		 (setq buf-string (shimbun-hash-get-item
				   shimbun (shimbun-header-id header))))
	       (when buf-string
		 (insert buf-string)
		 (shimbun-message shimbun "shimbun: Make contents...")
		 (goto-char (point-min))
		 (prog1 (shimbun-make-contents shimbun header)
		   (shimbun-message shimbun
				    "shimbun: Make contents...done")))))
	   "")))))

(provide 'sb-hash)

;;; sb-hash.el ends here
