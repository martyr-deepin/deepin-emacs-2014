;; bbdb-reportmail.el --- Hooks the Insidious Big Brother Database
;;                        into the Reportmail package

;; Copyright (C) 1997 Christopher Kline

;; Author: Christopher Kline <ckline@media.mit.edu>
;; Maintainer: Christopher Kline <ckline@media.mit.edu>
;; Version: 1.01
;; Created: 25 Jun 1997
;; Date: 26 Jun 1997

;; Bbdb-reportmail is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; Bbdb-reportmail is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Bbdb-reportmail advises the reportmail package function
;; display-time-get-field so that it attempts to replace the reported
;; "from" and "to" fields with the name field (or mail-name, if it
;; exists) of the corresponding BBDB record, if such a correspondence
;; can be made.

;; To use this, simply add the following lines AFTER you load in your
;; bbdb, set bbdb variables, etc.
;;
;;  (bbdb-insinuate-reportmail)
;;
;; (A require used to be necessary - it is no longer needed as long as
;; bbdb-insinuate-reportmail is called)

;;; History:

;; v1.01 26 June 1997
;;       Fixed the advice so that if we are the message recipient, do
;;       nothing so that display-time-process-new-mail will correctly
;;       trap this case.
;;
;; v1.00 26 June 1997
;;       Initial release.

;;
;; $Id: bbdb-reportmail.el,v 1.56 2005/07/23 15:04:43 waider Exp $
;;

;;-----------------------------------------------------------------------

(require 'bbdb)
(require 'reportmail)
(require 'advice)
(require 'mail-extr)

(defun bbdb/reportmail-alternate-full-name (address)
  (if address
      (let ((entry (bbdb-search-simple nil address)))
    (if entry
        (or (bbdb-record-getprop entry 'mail-name)
        (bbdb-record-name entry))))))

(defadvice display-time-get-field
  (around bbdb/reportmail-hack-display-time-get-field disable activate)
  "Advises the `display-time-get-field' function in the reportmail package.
If the field is \"from\" or \"to\", it tries to replace the value of the field
with the name field of the corresponding BBDB entry, if one can be found.

If no corresponding record can be found, the field value is left unaltered."
  (let (gf-field)
    ;; Get the original argument to display-time-get-field
    (setq gf-field (ad-get-arg 0))
    ;; Call the original display-time-get-field
    ad-do-it
    (if (or (string= gf-field "To") (string= gf-field "From"))
    (setq ad-return-value
          (or
           ;; If this message is to me, then do nothing so
           ;; reportmail can trap this case in
           ;; display-time-process-new-mail
           (if (display-time-member ad-return-value
                      display-time-my-addresses)
           ad-return-value
         nil)
           ;; Is the sender/recipient in our BBDB?
           (bbdb/reportmail-alternate-full-name
        (car (cdr (mail-extract-address-components ad-return-value))))
           ;; Can't find sender/recipient in BBDB; do nothing.
           ad-return-value)
          ))))

;;;###autoload
(defun bbdb-insinuate-reportmail ()
  "Call this function to hook BBDB into reportmail."
  (ad-enable-advice 'display-time-get-field 'around
            'bbdb/reportmail-hack-display-time-get-field)
  (ad-activate 'display-time-get-field)
  (message "Insinuated BBDB into Reportmail.")
)

(provide 'bbdb-reportmail)


