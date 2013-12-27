;;; bbdb-snarf.el -- convert free-form text to BBDB records

;;;
;;; Copyright (C) 1997 by John Heidemann <johnh@isi.edu>.
;;; $Id: bbdb-snarf.el,v 1.44 2006/05/26 21:20:07 fenk Exp $
;;;
;;; This file is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published
;;; by the Free Software Foundation version 1.
;;;
;;; This file is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
;;;

;;;
;;; bbdb-snarf is code to pick addresses, phones, and such out of a
;;; free-form paragraphs.  Things are recognized by context (web pages
;;; start with http:// or www., for example).  I wrote it because I
;;; despise fill-in-the-blank forms (a la bbdb-create).  (if I wanted
;;; modes, I'd use vi :-).
;;;
;;; Eventually I'd like to be able to replace bbdb-mode with a free-form
;;; text mode where bbdb-snarf merges in any changes you make.
;;; I'm not there yet---merging is not good enough currently.
;;; Currently bbdb-snarf is good for pulling postal addresses
;;; from e-mail messages and converting other databases.
;;;

(require 'bbdb)
(require 'bbdb-com)
(require 'rfc822)
(require 'mail-extr)

(defconst bbdb-digit "[0-9]")
(defvar bbdb-snarf-phone-regexp
  (concat
   "\\(([2-9][0-9][0-9])[-. ]?\\|[2-9][0-9][0-9][-. ]\\)?"
   "[0-9][0-9][0-9][-. ][0-9][0-9][0-9][0-9]"
   "\\( *\\(x\\|ext\\.?\\) *[0-9]+\\)?"
   )
  "regexp to match phones.")
(defvar bbdb-snarf-zip-regexp
  (concat
   "\\<"
   bbdb-digit bbdb-digit bbdb-digit bbdb-digit bbdb-digit
   "\\(-" bbdb-digit bbdb-digit bbdb-digit bbdb-digit "\\)?"
   "\\>$")
  "regexp matching zip.")

(defcustom bbdb-snarf-web-prop 'www
  "What property bbdb should use for the web, or nil to not detect web URLs."
  :group 'bbdb
  :type 'symbol)

(defun bbdb-snarf-address-lines ()
  (let ((lines (bbdb-split (buffer-string) "\n")))
    (if (>= bbdb-file-format 5) nil
      (while (< (length lines) 3)
    (setq lines (append lines (list nil))))
      (if (> (length lines) 3)
      (error "bbdb-snarf-address-lines: too many lines in address.")))
    (delete-region (point-min) (point-max))
    lines))

(defun bbdb-snarf-make-address
  (label address-lines city state zip country)
  (if (>= bbdb-file-format 4)
      (vector label address-lines city state zip country)
    (if (>= bbdb-file-format 3)
    (vector label address-lines city state zip)
      (vector label
          (nth 0 address-lines)
          (nth 1 address-lines)
          (nth 2 address-lines)
          city state zip))))

(defun bbdb-snarf-prune-empty-lines ()
  (goto-char (point-min))
  (while (re-search-forward "^[ \t]*\n" (point-max) t)
    (replace-match "")))

(defun delete-and-return-region (begin end)
  (prog1
      (buffer-substring begin end)
    (delete-region begin end)))

(defun bbdb-snarf-extract-label (default consume-p)
  "Extract the label before the point, or return DEFAULT if no label.
If CONSUME-P is set, delete the text, if found."
  (interactive "sDefault label: ")
  (let ((end (point-marker)))
    (skip-chars-backward " \t")
    (if (not (= (point) (point-min)))
        (forward-char -1))
    (if (looking-at ":")
    (let* ((label-end (point))
           (label (delete-and-return-region
               (progn (skip-chars-backward "^\n,;") (point))
               label-end)))
      (delete-region (point) end)
      label)
      default)))

(defun bbdb-snarf-parse-phone-number (phone)
  "Fix the bogosity that is `bbdb-snarf-parse-phone-number'.
It doesn't always return a normalized phone number.
For (800) 555-1212 it returns a three element list."
  (let ((try (bbdb-parse-phone-number phone)))
    (if (= 3 (length try))
    (nconc try '(0)))
    try))

;;;###autoload
(defun bbdb-snarf (where)
  "snarf up a bbdb record WHERE the point is.
We assume things are line-broken and paragraph-bounded.
The name comes first and other fields (address,
phone, email, web pages) are recognized by context.

Required context:
    addresses end with \"City, State ZIP\" or \"City, State\"
    phones match bbdb-snarf-phone-regexp
        (currently US-style phones)
    e-mail addresses have @'s in them
    web sites are recognized by http:// or www.

Address and phone context are currently US-specific;
patches to internationalize these assumptions are welcome.

\\[bbdb-snarf] is similar to \\[bbdb-whois-sentinel], but less specialized."
  (interactive "d")
  (bbdb-snarf-region
   (progn (goto-char where) (forward-paragraph -1) (point))
   (progn (forward-paragraph 1) (point))))

;;;###autoload
(defun bbdb-snarf-region (begin end)
  "snarf up a bbdb record in the current region.  See `bbdb-snarf' for
more details."
  (interactive "r")

  (save-excursion
    (let
        ((buf (get-buffer-create " *BBDB snarf*"))
         (text (buffer-substring-no-properties begin end))
         phones nets web city state zip name address-lines
         address-vector notes)
      (set-buffer buf)
      (erase-buffer)
      (insert text)

      ;; toss beginning and trailing space
      (goto-char (point-min))
      (while (re-search-forward "^[ \t]+" (point-max) t)
        (replace-match ""))
      (goto-char (point-min))
      (while (re-search-forward "^\\s +$" (point-max) t)
        (replace-match ""))

      ;; first, pick out phone numbers
      (goto-char (point-min))
      (while (re-search-forward bbdb-snarf-phone-regexp (point-max) t)
        (let (phone
              (begin (match-beginning 0))
              (end (match-end 0)))
          (goto-char begin)
          (forward-char -1)
          (if (looking-at "[0-9A-Za-z]")
              (goto-char end);; not really phone
            (setq phone (bbdb-snarf-parse-phone-number
                         (delete-and-return-region begin end))
                  phones (append phones
                                 (list (vconcat
                                        (list (bbdb-snarf-extract-label
                                               (bbdb-label-completion-default
                                                'phone) t))
                                        phone)))))))

      ;; next, web pages
      (goto-char (point-min))
      (if (and bbdb-snarf-web-prop
               (re-search-forward "\\(http://\\|www\.\\)[^ \t\n]+"
                                  (point-max) t))
          (progn
            (setq web (match-string 0)
                  notes (append notes (list (cons bbdb-snarf-web-prop web))))
            (replace-match "")))

      ;; next e-mail
      (goto-char (point-min))
      (while (re-search-forward "[^ \t\n<]+@[^ \t\n>]+" (point-max) t)
        (setq nets (append nets (list (match-string 0))))
        (replace-match ""))

      (bbdb-snarf-prune-empty-lines)

      ;; name
      (goto-char (point-min))
      ;; This check is horribly english-centric (I think)
      (while (and (not (eobp)) (/= (char-syntax (char-after (point))) ?w))
        (forward-line 1))
      (if (re-search-forward "\\(\\sw\\|[ -\.,]\\)*\\sw" nil t)
          (progn 
            (setq name (match-string 0))
            (delete-region (match-beginning 0) (match-end 0))))

      ;; address
      (goto-char (point-min))
      (cond
       ;; city, state zip
       ((re-search-forward bbdb-snarf-zip-regexp (point-max) t)
        (save-excursion
          (save-restriction
            (let (mk)
              (narrow-to-region (point-min) (match-end 0))
              (goto-char (point-max))
              ;; zip
              (re-search-backward bbdb-snarf-zip-regexp (point-min) t)
              (setq zip (bbdb-parse-zip-string (match-string 0)))
              ;; state
              (skip-chars-backward " \t")
              (setq mk (point))
              (skip-chars-backward "^ \t,")
              (setq state (buffer-substring (point) mk))
              ;; city
              (skip-chars-backward " \t,")
              (setq mk (point))
              (beginning-of-line)
              (setq city (buffer-substring (point) mk))
              ;; toss it
              (forward-char -1)
              (delete-region (point) (point-max))
              ;; address lines
              (goto-char (point-min))
              (setq address-lines (bbdb-snarf-address-lines)
                    address-vector (list (bbdb-snarf-make-address
                                          (bbdb-label-completion-default
                                           'address)
                                          address-lines
                                           city
                                          state
                                          zip
                                          "";; FIXME: snarf country
                                          )))))))
       ;; try for just city, state
       ((re-search-forward "^\\(.*\\), \\([A-Z][A-Za-z]\\)$"
                           (point-max) t)
        (save-excursion
          (save-restriction
            (setq city (match-string 1)
                  state (match-string 2))
            (narrow-to-region (point-min) (match-end 0))
            (goto-char (point-min))
            (setq address-lines (bbdb-snarf-address-lines)
                  address-vector (list (bbdb-snarf-make-address
                                        "address"
                                        address-lines
                                        city
                                        state
                                        0
                                        "";; FIXME: snarf country
                                        ))))))
       (t
        (setq address-lines '(nil nil nil)
              address-vector nil)))

      ;; anything else -> notes
      (bbdb-snarf-prune-empty-lines)
      (if (/= (point-min) (point-max))
          (setq notes (append notes (list (cons 'notes (buffer-string))))))

      ;; debug
                                        ;      (goto-char (point-max))
                                        ;      (insert "\n\n"
                                        ;         "name: " name "\n"
                                        ;         "city: " city "\n"
                                        ;         "state: " state "\n"
                                        ;         "zip: " zip "\n")

      (setq name (or name
                     (and nets (car (car (bbdb-rfc822-addresses (car nets)))))
                     "?"))
      
      (bbdb-merge-interactively name
                                nil
                                nets
                                address-vector
                                phones
                                notes))))


; (setq bbdb-snarf-test-cases "
;
; another test person
; 1234 Gridley St.
; Los Angeles, CA 91342
; 555-1212
; test@person.net
; http://www.foo.bar/
; other stuff about this person
;
; test person
; 1234 Gridley St.
; St. Los Angeles, CA 91342-1234
; 555-1212
; test@person.net
;
; x test person
; 1234 Gridley St.
; Los Angeles, California 91342-1234
; 555-1212
; test@person.net
;
; y test person
; 1234 Gridley St.
; Los Angeles, CA
; 555-1212
; test@person.net
; "
;       "some test cases")



(defun bbdb-merge-interactively (name company nets addrs phones notes)
  "Interactively add a new record; arguments same as \\[bbdb-create-internal]."
  (let*
      ((f-l-name (bbdb-divide-name name))
       (firstname (car f-l-name))
       (lastname (nth 1 f-l-name))
       (aka nil)
       (new-record
        (vector firstname lastname aka company phones addrs
                (if (listp nets) nets (list nets)) notes
                (make-vector bbdb-cache-length nil)))
       (old-record (bbdb-search-simple name nets)))
    (if old-record
    (progn
      (setq new-record (bbdb-merge-internally old-record new-record))
      (bbdb-delete-record-internal old-record)))
    ;; create  new record
    (bbdb-invoke-hook 'bbdb-create-hook new-record)
    (bbdb-change-record new-record t)
    (bbdb-hash-record new-record)
    (bbdb-display-records (list new-record))))

(defun bbdb-merge-internally (old-record new-record)
  "Merge two records.  NEW-RECORDS wins over OLD in cases of ties."
  (if (and (null (bbdb-record-firstname new-record))
       (bbdb-record-firstname old-record))
      (bbdb-record-set-firstname new-record (bbdb-record-firstname old-record)))
  (if (and (null (bbdb-record-lastname new-record))
       (bbdb-record-lastname old-record))
      (bbdb-record-set-lastname new-record (bbdb-record-lastname old-record)))
  (if (and (null (bbdb-record-company new-record))
       (bbdb-record-company old-record))
      (bbdb-record-set-company new-record (bbdb-record-company old-record)))
  ;; nets
  (let ((old-nets (bbdb-record-net old-record))
    (new-nets (bbdb-record-net new-record)))
    (while old-nets
      (if (not (member (car old-nets) new-nets))
      (setq new-nets (append new-nets (list (car old-nets)))))
      (setq old-nets (cdr old-nets)))
    (bbdb-record-set-net new-record new-nets))
  ;; addrs
  (let ((old-addresses (bbdb-record-addresses old-record))
    (new-addresses (bbdb-record-addresses new-record)))
    (while old-addresses
      (if (not (member (car old-addresses) new-addresses))
      (setq new-addresses (append new-addresses (list (car old-addresses)))))
      (setq old-addresses (cdr old-addresses)))
    (bbdb-record-set-addresses new-record new-addresses))
  ;; phones
  (let ((old-phones (bbdb-record-phones old-record))
    (new-phones (bbdb-record-phones new-record)))
    (while old-phones
      (if (not (member (car old-phones) new-phones))
      (setq new-phones (append new-phones (list (car old-phones)))))
      (setq old-phones (cdr old-phones)))
    (bbdb-record-set-phones new-record new-phones))
  ;; notes
  (let ((old-notes (bbdb-record-raw-notes old-record))
    (new-notes (bbdb-record-raw-notes new-record)))
    (while old-notes
      (if (not (member (car old-notes) new-notes))
      (setq new-notes (append new-notes (list (car old-notes)))))
      (setq old-notes (cdr old-notes)))
    (bbdb-record-set-raw-notes new-record new-notes))
  ;; return
  new-record)

;;----------------------------------------------------------------------------
(eval-and-compile
  (if (fboundp 'replace-in-string)
      (fset 'bbdb-replace-in-string 'replace-in-string)
    (if (fboundp 'replace-regexp-in-string) ; defined in e21
        (fset 'bbdb-replace-regexp-in-string 'replace-regexp-in-string)
      ;; actually this is `dired-replace-in-string' slightly modified
      ;; We're not defining the whole thing, just enough for our purposes.
      (defun bbdb-replace-regexp-in-string (regexp newtext string &optional
                                                   fixedcase literal)
        ;; Replace REGEXP with NEWTEXT everywhere in STRING and return result.
        ;; NEWTEXT is taken literally---no \\DIGIT escapes will be recognized.
        (let ((result "") (start 0) mb me)
          (while (string-match regexp string start)
            (setq mb (match-beginning 0)
                  me (match-end 0)
                  result (concat result (substring string start mb) newtext)
                  start me))
          (concat result (substring string start)))))
    (defun bbdb-replace-in-string (string regexp newtext &optional literal)
      (bbdb-replace-regexp-in-string regexp newtext string nil literal))))

(defcustom bbdb-extract-address-component-regexps
    '(
      ;; "surname, firstname" <address>  from Outlookers
      ("\"\\([^\"]*\\)\"\\s-*<\\([^>]+\\)>"
       (bbdb-clean-username (match-string 1 adstring)) 2)

      ;; name <address>
      ("\\([^<>,\t][^<>,]+[^<>, \t]\\)\\s-*<\\([^>]+\\)>"
       1 2)
      ;; <address>
      ("<\\([^>,]+\\)>" nil 1)
      ;; address (name)
      ("\\(\\b[^<\",()]+\\b\\)\\s-*(\\([^)]+\\))"
       (car (mail-extract-address-components
             (concat "\"" (match-string 2 adstring) "\"")))
       1)
      ;; firstname.lastname@host
      ("\\b\\(\\([^@ \t\n.]+\\.[^@ \t\n.]+\\)@[^@ \t\n]+\\)\\b"
       (car (mail-extract-address-components
             (concat "\"" (match-string 2 adstring) "\"")))
       1)
      ;; user@host
      ("\\b\\(\\([^@ \t\n]+\\)@[^@ \t\n]+\\)\\b"
       nil 1)
      ;; localaddress
      ("\\b\\([^@ \t\n]+\\)\\b"
       nil 1)
      )
    "*List of regexps matching headers.
Each list element should have the form (REGEXP FULLNAME ADDRESS), where
REGEXP matches the address while the actual address components should
be a parenthesized expression.

FULLNAME is a default string for addresses without full name or a
number denoting parenthesized expression.
ADDRESS is a number denoting the parenthesized expression matching the
address.

If FULLNAME or ADDRESS is a list it will be evaluated to return a
string or nil.  If its a function it will be called with the remaining
address-string as argument."
    :group 'bbdb-noticing-records
    :type 'list)

(defcustom bbdb-extract-address-component-ignore-regexp
  "\\(\\(undisclosed\\|unlisted\\)[^,]*recipients\\)\\|no To-header on input"
  "*A regexp matching addresses which should be ignored."
  :group 'bbdb-noticing-records
  :type 'string)

(defcustom bbdb-extract-address-component-handler 'message
  "*Specifies how `bbdb-extract-address-components' reports errors.

A value of nil means ignore unparsable stuff and 'warn will report
a warning, 'message will report a message in the minibuffer and all
other value will fire a error.

When set to a function it will be called with the remaining string in
order to extract the address components and return the rest and the
components as list or to do what ever it wants, e.g. send a complain
to the author ...

To skip known unparseable stuff you rather should set the variable
`bbdb-extract-address-component-ignore-regexp' instead of disabling
this handler."
  :group 'bbdb-noticing-records
  :type '(choice (const :tag "Ignore problems."
                        nil)
                 (const :tag "Warn about parsing problems."
                        'warn)
                 (const :tag "Show a message about parsing problems."
                        'message)
                 (function :tag "A user defined handler")))

;;;###autoload
(defun bbdb-extract-address-components (adstring &optional ignore-errors)
  "Return a list of address components found in ADSTRING.
If extracting fails one probably has to adjust the variable
`bbdb-extract-address-component-regexps'."
  (let ((case-fold-search t)
    (fnadlist nil)
    adcom-regexp
    nomatch)

    ;; Do some string cleanup and trimming
    (setq adstring (bbdb-replace-in-string adstring "[\n\t]" " "))
    (setq adstring (bbdb-replace-in-string adstring "  " " "))
    (setq adstring (bbdb-replace-in-string adstring "^ +" ""))

    ;; scan the string
    (while (not (string= "" adstring))
      (setq adcom-regexp bbdb-extract-address-component-regexps
            nomatch t)
      (while adcom-regexp
        (let ((regexp (caar adcom-regexp))
              (fn (car (cdar adcom-regexp)))
              (ad (cadr (cdar adcom-regexp))))
          (cond ((string-match
                  (concat "^[^,]*\\("
                          bbdb-extract-address-component-ignore-regexp
                          "\\)[^,]*\\(,\\|$\\)")
                  adstring)
                 (setq adstring (substring adstring (match-end 0))
                       adcom-regexp nil
                       nomatch nil))
                ((string-match (concat "^\\s-*" regexp "\\s-*\\(,\\|$\\)")
                               adstring)
                 (add-to-list 'fnadlist
                              (list (let ((n
                                           (cond ((numberp fn)
                                                  (match-string fn adstring))
                                                 ((listp fn)
                                                  (save-match-data (eval fn)))
                                                 ((functionp fn)
                                                  (save-match-data
                                                    (funcall fn adstring)))
                                                 (t fn))))
                                      (if (string= n "")
                                          nil
                                        n))
                                    (let ((a
                                           (cond ((numberp ad)
                                                  (match-string ad adstring))
                                                 ((listp ad)
                                                  (save-match-data (eval ad)))
                                                 ((functionp ad)
                                                  (save-match-data
                                                    (funcall ad adstring)))
                                                 (t ad))))
                                      (if (string= a "")
                                          nil
                                        a))))
;                 (save-match-data
;                   (message "%S Match on %S to\n\t%S"
;                            regexp adstring fnadlist))
                 (setq adstring (substring adstring (match-end 0))
                       adcom-regexp nil
                       nomatch nil)))
          (setq adcom-regexp (cdr adcom-regexp))))

      ;; Now handle problems
      (if (and nomatch (not ignore-errors))
          (cond ((equal bbdb-extract-address-component-handler nil))
                ((equal bbdb-extract-address-component-handler 'warn)
                 (bbdb-warn "Cannot extract an address component at \"%s\".
See `bbdb-extract-address-component-handler' for more information."
                            adstring))
                ((equal bbdb-extract-address-component-handler 'message)
                 (message "Cannot extract an address component at \"%s\"."
                          adstring))
                ((functionp bbdb-extract-address-component-handler)
                 (let ((result
                        (funcall bbdb-extract-address-component-handler
                                 adstring)))
                   (if (and (listp result) (= 3 (length result)))
                       (progn (add-to-list 'fnadlist (cdr result))
                              (setq adstring (car result)
                                    nomatch nil)))))
                (t
                 (error "Cannot extract an address component at \"%30s\""
                        adstring))))

      ;; ignore the bad junk
      (if nomatch
          (if (string-match "^[^,]*," adstring)
              (setq adstring (substring adstring (match-end 0)))
            (setq adstring ""))))

    (delete '(nil nil) (nreverse fnadlist))))

;;; alternative name parser
;;;###autoload
(defun bbdb-rfc822-addresses ( addrline &optional ignore-errors)
  "Split ADDRLINE into a list of parsed addresses.

You can't do this with rfc822.el in any sort of useful way because it discards
the comments. You can't do this with mail-extr.el because the multiple address
parsing in GNU Emacs appears to be broken beyond belief, and the XEmacs
version doesn't support multiple addresses."
  (let (addrs (start 0))
    (setq addrline (concat addrline ",")) ;; kludge, to make parsing easier
    ;; Addresses are separated by commas. This is probably the worst
    ;; possible way to do this, but it does cut down on the amount of
    ;; coding effort I have to duplicate. Basically, we split on
    ;; commas, and then try and parse what we've found. Pathologically
    ;; bad address lines will break this.
    (while (string-match "\\([^,]+\\)," addrline start)
      (let* ((thisaddr (substring addrline 0 (match-end 1)))
             (comma (match-end 0)) ;; rfc822-addresses trashes match-data
             (parsed (rfc822-addresses thisaddr)))
        (if (string-match "(" (or (car parsed) "")) ;; rfc822 didn't like it.
            (setq start comma)
          (setq addrs
                (append addrs (list
                               (mail-extract-address-components
                                thisaddr)))
                ;; throw away what we just parsed
                addrline (substring addrline comma)
                start 0))))
    addrs))

(provide 'bbdb-snarf)
