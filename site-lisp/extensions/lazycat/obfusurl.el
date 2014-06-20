;;; obfusurl.el --- Obfuscate URLs so they aren't spoilers
;; Copyright 2001,2002 by Dave Pearson <davep@davep.org>
;; $Revision: 1.2 $

;; obfusurl.el is free software distributed under the terms of the GNU
;; General Public Licence, version 2. For details see the file COPYING.

;;; Commentary:
;;
;; obfusurl.el provides `obfuscate-url', a command that will obfuscate an
;; URL under the cursor. This might be useful if you are writing out an URL
;; for someone but the URL itself might spoil the surprise.
;;
;; For example, this:
;;
;; <URL:http://www.davep.org/emacs/>
;;
;; is turned into this:
;;
;; <URL:http://www.davep.org/%65%6d%61%63%73/>
;;
;; The latest obfusurl.el is always available from:
;;
;;   <URL:http://www.davep.org/emacs/obfusurl.el>
;;   <URL:http://www.davep.org/%65%6d%61%63%73/%6f%62%66%75%73%75%72%6c%2e%65%6c>

;;; THANKS:
;;
;; Andy Sawyer <andys@morebhp.com> for initially pointing out that URLs with
;; percent escapes already in them would get broken.
;;
;; Kevin Rodgers <kevinr@ihs.com> for suggesting a method of fixing the
;; above.
;;
;; Toby Speight <streapadair@gmx.net> for pointing out that I needed to
;; cater for reserved characters.

;;; INSTALLATION:
;;
;; o Drop obfusurl.el somwehere into your `load-path'. Try your site-lisp
;;   directory for example (you might also want to byte-compile the file).
;;
;; o Add the following autoload statement to your ~/.emacs file:
;;
;;   (autoload 'obfuscate-url "obfusurl" "Obfuscate URL under point" t)

;;; Code:

;; Things we need:

(eval-when-compile
  (require 'cl))
(require 'thingatpt)

;; Constants.

(defconst obfuscate-url-reserved-chars '(?\; ?/ ?? ?: ?@ ?& ?= ?+ ?$ ?,)
  "Characters reserved by RFC 2396.")

;; Main code.

(defun obfuscate-url-hexify-string (string)
  "Return STRING as percent-escaped hex values.

Existing percent-escapes and reserved characters (as defined in RFC 2396) in
the text are preserved."
  (cl-flet ((hexify-string (string)
           (with-output-to-string
             (mapc (lambda (c)
                     (princ (format
                             (if (member c obfuscate-url-reserved-chars)
                                 "%c"
                               "%%%02x")
                             c))) string))))
    (let ((case-fold-search t))
      (with-output-to-string
        (loop for i = 0 then (match-end 0)
              while (string-match "%[0-9a-f][0-9a-f]" string i)
              do (princ
                  (concat (hexify-string (substring string i (match-beginning 0)))
                          (match-string 0 string)))
              finally (princ (hexify-string (substring string i))))))))

(defun obfuscate-url-hexify-url (url)
  "Return URL as a percent-escaped URL."
  (let ((trailing-slash (string-match "/$" url))
        (split          (split-string url "/")))
    (with-output-to-string
      (princ (format "%s//%s" (nth 0 split) (nth 2 split)))
      (loop for part in (nthcdr 3 split)
            unless (string= part "")    ; Because of XEmacs' `split-string'.
            do (princ (concat "/" (obfuscate-url-hexify-string part)))
            finally (when trailing-slash (princ "/"))))))

;;;###autoload
(defun obfuscate-url ()
  "Obfuscate an URL under `point'.

This might be useful if you're writing out an URL for someone but the URL
itself is a spoiler. The URL will still work but it won't be readable (by
most mortals anyway)."
  (interactive "*")
  (let ((url (thing-at-point 'url)))
    (if url
        (let ((bounds (bounds-of-thing-at-point 'url)))
          (setf (point) (car bounds))
          (delete-region (car bounds) (cdr bounds))
          (insert (obfuscate-url-hexify-url url)))
      (error "I can't see an URL here"))))

(provide 'obfusurl)

;;; obfusurl.el ends here
