;;; google.el --- Emacs interface to the Google API

;; Copyright (C) 2002, 2008  Edward O'Connor <ted@oconnor.cx>

;; Author: Edward O'Connor <ted@oconnor.cx>
;; Keywords: comm, processes, tools

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; You should always be able to find the latest version here:

;;           <URL:http://oconnor.cx/elisp/google.el>

;; A really bare-bones first hack at Google API support for Emacs.
;; Note that you need a Google license key to use this; you can
;; get one by following the instructions here:

;;      <URL:http://code.google.com/apis/ajaxsearch/signup.html>

;; Usage:

;; (require 'google)
;; (setq google-license-key "my license key" ; optional
;;       google-referer "my url")            ; required!
;; (google-search-video "rickroll")

;;; History:
;; 2002 or thereabouts: Initial version, which used the SOAP API.
;; 2008-04-24: Use the AJAX Search API instead of the SOAP API.
;;             N.B., incompatible API changes galore!
;; 2008-05-01: Some convenience functions for parsing search result
;;             blobs. Passes checkdoc now.

;;; Code:

(require 'json)
(require 'url)

(defvar url-http-end-of-headers)

(defgroup google nil
  "Emacs interface to Google's AJAX Search API."
  :group 'tools)

(defcustom google-license-key nil
  "*Your Google license key.
This is optional. However, if you do specify it, it should correspond to
your `google-referer'."
  :type '(string)
  :group 'google)

(defcustom google-referer nil
  "*The referer to send when performing Google searches.
Note that this is required by Google's terms of service."
  :type '(string)
  :group 'google)

(defun google-response (buf)
  "Extract the JSON response from BUF."
  (with-current-buffer buf
    (goto-char url-http-end-of-headers)
    (prog1 (json-read)
      (kill-buffer buf))))

(defun google-search (terms &optional start search-domain)
  "Search for TERMS.
START, if non-null, is the search result number to start at.
SEARCH-DOMAIN can be one of \"web\", \"local\", \"video\",
\"blogs\", \"news\", \"books\", or \"images\"."
  (let ((url-package-name "google.el")
        (url-request-extra-headers
         `(("Accept" . "application/json")
           ("Referer" . ,google-referer)))
        (args `(("q" . ,terms)
                ("v" . "1.0"))))
    (unless search-domain
      (setq search-domain "web"))
    (when google-license-key
      (add-to-list 'args (cons "key" google-license-key)))
    (when start
      (add-to-list 'args (cons "start" start)))
    (google-response
     (url-retrieve-synchronously
      (format
       "http://ajax.googleapis.com/ajax/services/search/%s?%s"
       search-domain
       (mapconcat (lambda (cons)
                    (format "%s=%s"
                            (url-hexify-string (car cons))
                            (url-hexify-string (cdr cons))))
                  args
                  "&"))))))

(defmacro define-google-search-domain (domain)
  "Define a google search function for DOMAIN, a keyword."
  (setq domain (substring (symbol-name domain) 1))
  (let ((func (intern (concat "google-search-" domain))))
    `(defun ,func (terms &optional start)
       ,(format "Search %s with Google!

Results look like so:

\((responseStatus . N)
 (responseDetails)
 (responseData
  (cursor
   (moreResultsUrl . URL)
   (currentPageIndex . N)
   (estimatedResultCount . N)
   (pages .
          [((label . N)
            (start . N))
           ..]))
  (results .
           [((content . STR)
             (titleNoFormatting . STR)
             (title . STR)
             (cacheUrl . URL)
             (visibleUrl . URL)
             (url . URL)
             (unescapedUrl . URL)
             (GsearchResultClass . STR))
            ..])))

There are several utilities for extracting data from this structure; see
`google-result-field', `google-result-urls', and
`google-result-more-results-url'."
                (if (string= domain "web") "the web" domain))
       (google-search terms start ,domain))))

(define-google-search-domain :web)
(define-google-search-domain :local)
(define-google-search-domain :video)
(define-google-search-domain :blogs)
(define-google-search-domain :news)
(define-google-search-domain :books)
(define-google-search-domain :images)

;;; Parsing google search results

(defsubst google-result-field (key json)
  "Fetch KEY's value from JSON, a parsed JSON structure."
  (cdr (assoc key json)))

(defun google-result-urls (results)
  "Extract a list of search result URLs from RESULTS."
  (let* ((responseData (google-result-field 'responseData results))
         (records (google-result-field 'results responseData)))
    (mapcar (lambda (record)
              (google-result-field 'url record))
            records)))

(defun google-result-more-results-url (results)
  "Extract the URL for more search RESULTS."
  (let* ((responseData (google-result-field 'responseData results))
         (cursor (google-result-field 'cursor responseData)))
    (google-result-field 'moreResultsUrl cursor)))

(provide 'google)
;;; google.el ends here
