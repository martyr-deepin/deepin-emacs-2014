;;; -*- Mode:Emacs-Lisp -*-

;;; This file is an addition to the Insidious Big Brother Database
;;; (aka BBDB), copyright (c) 1991, 1992 Jamie Zawinski
;;; <jwz@netscape.com>.
;;;
;;; The Insidious Big Brother Database is free software; you can redistribute
;;; it and/or modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation; either version 1, or (at your
;;; option) any later version.
;;;
;;; BBDB is distributed in the hope that it will be useful, but WITHOUT ANY
;;; WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;;; details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.


;;; This file was written by Martin Sjolin <marsj@ida.liu.se>
;;; based the original code by Tom Tromey <tromey@busco.lanl.gov>.
;;;
;;; Thanks to Richard Stanton <stanton@haas.berkeley.edu> for ideas
;;; for improvements and to Michael D. Carney  <carney@ltx-tr.com>
;;; for testing and feedback.

;;; $Id: bbdb-sc.el,v 1.16 2005/07/23 15:06:46 waider Exp $

;;; This file adds the ability to define attributions for Supercite in
;;; a BBDB, enables you to retrieve your standard attribution from
;;; BBDB. If the from header in the mail to which you are replying
;;; only contains the e-mail address, the personal name is lookup in
;;; BBDB. You need Supercite to make this code work. The attribution
;;; os is stored under the key `attribution' (unless you've changed
;;; bbdb/sc-attribution-field).

;;; To use enable this code you will have to the "sc-consult" to your
;;; sc-preferred-attribution-list. This file sets variable if it is not
;;; set and isues an warning message if "sc-consult" is not included.
;;;
;;;   (setq sc-preferred-attribution-list
;;;     '("sc-lastchoice" "x-attribution" "sc-consult"
;;;       "initials" "firstname" "lastname"))
;;;
;;;
;;; We also set the sc-attrib-selection-list below if is not bound, if
;;; you have your own special sc-attrib-selection-list, please add
;;; an expression as below:
;;;
;;;   (setq sc-attrib-selection-list
;;;        '(("sc-from-address" ((".*" . (bbdb/sc-consult-attr
;;;                    (sc-mail-field "sc-from-address")))))))
;;;
;;; And finally we set the sc-mail-glom-frame to enable the
;;; fetching of the name of person when there is only an e-mail
;;; address in the original mail:
;;;
;;;  (setq sc-mail-glom-frame
;;;    '((begin                        (setq sc-mail-headers-start (point)))
;;;      ("^x-attribution:[ \t]+.*$"   (sc-mail-fetch-field t) nil t)
;;;      ("^\\S +:.*$"                 (sc-mail-fetch-field) nil t)
;;;      ("^$"                         (progn (bbdb/sc-default)
;;;                      (list 'abort '(step . 0))))
;;;      ("^[ \t]+"                    (sc-mail-append-field))
;;;      (sc-mail-warn-if-non-rfc822-p (sc-mail-error-in-mail-field))
;;;      (end                          (setq sc-mail-headers-end (point)))))
;;;
;;;

;;;
;;;

;;; packages
(require 'bbdb)
(require 'supercite)

;;; User variable(s)
(defcustom bbdb/sc-replace-attr-p t
 "t if you like to create a new BBDB entry when
entering a non-default attribution, 'ask if the user
should be asked before creation and NIL if we never create a new entry."
 :group 'bbdb-utilities-supercite
 :type '(choice (const "Create a new BBDB entry" t)
        (const "Confirm new record creation" ask)
        (const "Don't create a new entry" nil)))

(defcustom bbdb/sc-attribution-field 'attribution
  "The BBDB field used for Supercite attribution information."
  :group 'bbdb-utilities-supercite
  :type '(symbol :tag "Field name"))

;;; Code starts
(defcustom bbdb/sc-last-attribution ""
 "Default attribution return by the SuperCite citation engine,
used to compare against citation selected by the user."
 :group 'bbdb-utilities-supercite
 :type '(string :tag "Default citation" ""))

(defun bbdb/sc-consult-attr (from)
  "Extract citing information from BBDB using sc-consult where
FROM is user e-mail address to look for in BBDB."
  ;; if logged in user sent this, use recipients.
  (let ((from (if (or (null from)
                      (string-match (bbdb-user-mail-names) from))
                  (car (cdr (mail-extract-address-components
                             (or (sc-mail-field "to") from))))
                from)))
    (if from
        (let ((record (bbdb-search-simple nil from)))
          (and record
               (bbdb-record-getprop record bbdb/sc-attribution-field))))))

(defun bbdb/sc-set-attr ()
  "Add attribute to BBDB."
  (let ((from (sc-mail-field "from"))
    (address (sc-mail-field "sc-from-address"))
    (attr (sc-mail-field "sc-attribution")))
    (if (and from attr bbdb/sc-replace-attr-p
        (not (string-equal attr bbdb/sc-last-attribution))
        (not (string-match (bbdb-user-mail-names) address)))
    (let* ((bbdb-notice-hook nil)
           ;; avoid noticing any headers in the reply message
           (record (bbdb-annotate-message-sender
               from t
               (bbdb-invoke-hook-for-value
            bbdb/mail-auto-create-p) t)))
      (if record
          (let ((old (bbdb-record-getprop record 'attribution)))
        ;; ignore if we have an value and same value
        (if (and (not (and old (string-equal old attr)))
             (or (not (eq bbdb/sc-replace-attr-p 'ask))
                 (y-or-n-p (concat "Change attribution " attr))))
            (progn (bbdb-record-putprop record
                        bbdb/sc-attribution-field attr)
               (bbdb-change-record record nil)))))))))

;;; this is marked as autoload since someone managed to trip up Gnus
;;; with it. I'm not clear this needs fixing, as you should be calling
;;; bbdb-insinuate-sc if you're using supercite/BBDB. However.
;;;###autoload
(defun bbdb/sc-default ()
  "If the current \"from\" field in `sc-mail-info' alist
contains only an e-mail address, lookup e-mail address in
BBDB, and prepend a new \"from\" field to `sc-mail-info'."
  (let* ((from   (sc-mail-field "from"))
     (pair   (and from (mail-extract-address-components from))))
    (if (and pair (not (car pair)))
    (let* ((record (bbdb-search-simple nil (car (cdr pair))))
           (name   (and record (bbdb-record-name record))))
      (if name
          (setq sc-mail-info
            (cons (cons "from"
                (format "%s (%s)" (car (cdr pair)) name))
              sc-mail-info)))))))

;;; setup the default setting of the variables
(defun bbdb/sc-setup-variables ()
  "Set up the various Supercite variables for the BBDB.
`sc-preferred-attribution-list', `sc-attrib-selection-list', and
`sc-mail-glom-frame' are set, but only if they have not previously
been defined.  It is strongly suggested that you not call this
function directly, but that you use this function (specifically the
settings contained herein) as an example.  In other words, set these
variables yourself, either in your Emacs configuration file or using
Custom."

  ;; check for sc-consult in sc-preferred-attribution-list
  (if (boundp 'sc-preferred-attribution-list)
      (or (member '"sc-consult" sc-preferred-attribution-list)
      (bbdb-warn (concat "\"sc-consult\" not included in "
                 "sc-preferred-attribution-list.  Attributions cannot"
                 "be gathered from the BBDB without \"sc-consult\""
                 "in sc-preferred-attribution-list")))
    (defvar sc-preferred-attribution-list
      '("sc-lastchoice" "x-attribution" "sc-consult"
        "initials" "firstname" "lastname")))

  ;; check sc-attrib-selection-list
  (defvar sc-attrib-selection-list
    '(("sc-from-address"
       ((".*" . (bbdb/sc-consult-attr
         (sc-mail-field "sc-from-address")))))))

  ;; set sc-mail-glom-frame
  (defvar sc-mail-glom-frame
    '((begin                        (setq sc-mail-headers-start (point)))
      ("^x-attribution:[ \t]+.*$"   (sc-mail-fetch-field t) nil t)
      ("^\\S +:.*$"                 (sc-mail-fetch-field) nil t)
      ("^$"                         (progn (bbdb/sc-default)
                       (list 'abort '(step . 0))))
      ("^[ \t]+"                    (sc-mail-append-field))
      (sc-mail-warn-if-non-rfc822-p (sc-mail-error-in-mail-field))
      (end                          (setq sc-mail-headers-end (point))))))

;; insert our hooks - call me from your Emacs initialization file
(defvar attribution) ;; dammit, supercite!
;;;###autoload
(defun bbdb-insinuate-sc ()
  "Call this function to hook BBDB into Supercite."

  (add-hook 'sc-post-hook 'bbdb/sc-set-attr)
  (add-hook 'sc-attribs-postselect-hook
         (function (lambda()
                 (setq bbdb/sc-last-attribution
                   (if sc-downcase-p
                       (downcase attribution) attribution))))))

(provide 'bbdb-sc)
;;; end of bbdb-sc.el
