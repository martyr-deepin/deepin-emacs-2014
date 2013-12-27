;;; sb-sueddeutsche-de.el --- sueddeutsche.de shimbun backend

;; Copyright (C) 2008 David Engster

;; Author: David Engster <dengste@eml.cc>
;; Keywords: news

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

;;; Code:

(require 'shimbun)
(require 'sb-rss)

(luna-define-class shimbun-sueddeutsche-de (shimbun-rss) ())

(defvar shimbun-sueddeutsche-de-group-url
  '(("alles"
     "http://www.sueddeutsche.de/app/service/rss/alles/rss.xml")
    ("nachrichten"
     "http://www.sueddeutsche.de/app/service/rss/nachrichten/rss.xml")
    ("politik"
     "http://www.sueddeutsche.de/app/service/rss/ressort/politik/rss.xml")
    ("wirtschaft"
     "http://www.sueddeutsche.de/app/service/rss/ressort/wirtschaft/rss.xml")
    ("finanzen"
     "http://www.sueddeutsche.de/app/service/rss/ressort/finanzen/rss.xml")
    ("kultur"
     "http://www.sueddeutsche.de/app/service/rss/ressort/kultur/rss.xml")
    ("sport"
     "http://www.sueddeutsche.de/app/service/rss/ressort/sport/rss.xml")
    ("muenchen"
     "http://sueddeutsche.de/app/service/rss/ressort/muenchen/rss.xml")
    ("panorama"
     "http://sueddeutsche.de/app/service/rss/ressort/panorama/rss.xml")
    ("leben"
     "http://sueddeutsche.de/app/service/rss/ressort/leben/rss.xml")
    ("gesundheit"
     "http://sueddeutsche.de/app/service/rss/ressort/gesundheit/rss.xml")
    ("computer"
     "http://sueddeutsche.de/app/service/rss/ressort/computerwissen/rss.xml")))

(defvar shimbun-sueddeutsche-de-groups
  (mapcar 'car shimbun-sueddeutsche-de-group-url))
(defvar shimbun-sueddeutsche-de-from-address "invalid@sueddeutsche.de")
(defvar shimbun-sueddeutsche-de-content-start
  (concat "<!-- beginn content -->[^\0]*?alt=\"Trennlinie\">"
	  ;; kino
	  "\\|<div class=\"artikelImageBlockLeft\">"))
(defvar shimbun-sueddeutsche-de-content-end
  (concat "<!-- ende content -->"
	  ;; kino
	  "\\|<!--NP-DROPEND-->"))

(defvar shimbun-sueddeutsche-de-x-face-alist
  '(("default" . "\
Face: iVBORw0KGgoAAAANSUhEUgAAABAAAAAQAgMAAABinRfyAAAADFBMVEXLyspMSkr///9+fX1
 CK4DEAAAARUlEQVQI12NYBQQMCKL/jv13hk1rX01hkFqxSo5BRGtVEMPWohVVDAvDV3oxrGp9+4q
 ha+VTLQaR1earGNb/2W7PgGoAAO3JJfDNz7QzAAAAAElFTkSuQmCC")))

(luna-define-method shimbun-index-url ((shimbun shimbun-sueddeutsche-de))
  (let ((group (shimbun-current-group-internal shimbun)))
    (cadr (assoc group shimbun-sueddeutsche-de-group-url))))

(luna-define-method shimbun-get-headers :around ((shimbun
						  shimbun-sueddeutsche-de)
						 &optional range)
  (let ((headers (luna-call-next-method))
	url)
    (mapcar
     (lambda (header)
       (setq url (shimbun-header-xref header))
       (when (string-match "ns_url=\\(http://www.sueddeutsche.de/.*\\)/" url)
	 (setq url (concat (match-string 1 url) "/print.html"))
	 (shimbun-header-set-xref header url))
       header)
     headers)))

(luna-define-method shimbun-rss-build-message-id ((shimbun
						   shimbun-sueddeutsche-de)
						  url date)
  (let ((group (shimbun-current-group-internal shimbun))
	id)
    (cond ((string-match
	    "ns_url=.*sueddeutsche.de.*/\\([0-9]+\\)/\\([0-9]+\\)/" url)
	   (concat "<" (match-string 1 url) "." (match-string 2 url) "." group
		   "@sueddeutsche.de>"))
	  (t
	   (error "Cannot find message-id base")))))

(luna-define-method shimbun-clear-contents :before ((shimbun
						     shimbun-sueddeutsche-de)
						    header)
  (shimbun-remove-tags "<!-- Stoerer //-->" "<!-- END Stoerer //-->")
  (shimbun-remove-tags "<span class=\"hidePrint\">" "</span>")
  (shimbun-remove-tags "<table.*?class=\"stoerBS\".*?>" "</table>")
  (shimbun-remove-tags "<\\(?:a\\|span\\) .*?bildstrecke.*?>"
		       "</\\(?:a\\|span\\)>"))

(provide 'sb-sueddeutsche-de)

;;; sb-sueddeutsche-de.el ends here
