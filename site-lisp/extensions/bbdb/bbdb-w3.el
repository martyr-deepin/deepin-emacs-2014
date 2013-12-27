;;; This file is part of the Insidious Big Brother Database (aka BBDB),
;;; copyright (c) 1991, 1992, 1993 Jamie Zawinski <jwz@netscape.com>.
;;; WWW-related functions for the BBDB.  See bbdb.texinfo.

;;; The Insidious Big Brother Database is free software; you can redistribute
;;; it and/or modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation; either version 2 or (at your
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

;;
;; $Id: bbdb-w3.el,v 1.10 2006/05/14 13:36:59 waider Exp $
;;

(require 'bbdb-com)
(require 'browse-url)

(defvar w3-mode-map)
(eval-when-compile
  (condition-case() (require 'url) (error (fset 'url-view-url 'ignore))))

;;;###autoload
(defun bbdb-www (rec &optional which)
  "Visit URLs stored in the `www' field of the current record.
\\[bbdb-apply-next-command-to-all-records]\\[bbdb-www] \
means to try all records currently visible.
Non-interactively, do all records if arg is nonnil."
  (interactive (list (bbdb-get-record "Visit (WWW): ")
                     (or current-prefix-arg 0)))
  (browse-url (read-string "fetch: "
                           (or (bbdb-get-field rec 'www which)
                               (bbdb-get-field rec 'ftp which)))))

;;;###autoload
(defun bbdb-www-grab-homepage (record)
  "Grab the current URL and store it in the bbdb database"
  (interactive (list (bbdb-completing-read-one-record
                      "Add WWW homepage for: ")))
  ;; if there is no database record for this person, create one
  (unless record
    (setq record (bbdb-read-new-record))
    (bbdb-invoke-hook 'bbdb-create-hook record))
  (if (bbdb-record-getprop record 'www)
      (bbdb-record-putprop
       record 'www
       (concat (bbdb-record-getprop record 'www) "," (url-view-url t)))
    (bbdb-record-putprop record 'www (url-view-url t)))
  (bbdb-change-record record t)
  (bbdb-display-records (list record)))

;;;###autoload
(defun bbdb-insinuate-w3 ()
  "Call this function to hook BBDB into W3."
  (add-hook 'w3-mode-hook
        (lambda () (define-key w3-mode-map ":" 'bbdb-www-grab-homepage))))

(provide 'bbdb-w3)
