;;; -*- Mode:Emacs-Lisp -*-

;;; This file is part of the Insidious Big Brother Database (aka BBDB),
;;; copyright (c) 1991, 1992, 1993 Jamie Zawinski <jwz@netscape.com>.
;;; It contains most of the user-level interactive commands for BBDB.
;;; See bbdb.texinfo.

;;; The Insidious Big Brother Database is free software; you can redistribute
;;; it and/or modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation; either version 2, or (at your
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
;; $Id: bbdb-com.el,v 1.180 2007/01/17 23:46:14 fenk Exp $
;;

(require 'cl)
(require 'bbdb)
;;(require 'bbdb-snarf) causes recursive compile, which I should fix.

;; ARGH. fmh, dammit.
(require
 (eval-and-compile
   (if (locate-library "mailabbrev")
       (quote mailabbrev)
     (quote mail-abbrevs))))

;; compiler placating.
;; not sure BBDB runs on anything old enough to use auto-fill-hook, mind.
(eval-and-compile
  (if (boundp 'auto-fill-function)
      (fset 'bbdb-auto-fill-function 'auto-fill-function)
    (fset 'bbdb-auto-fill-function 'auto-fill-hook))

  (autoload 'mh-send "mh-e")
  (autoload 'vm-session-initialization "vm-startup")
  (autoload 'vm-mail-internal "vm-reply")
  (autoload 'mew-send "mew")
  (autoload 'bbdb-header-start "bbdb-hooks")
  (autoload 'bbdb-extract-field-value "bbdb-hooks")
  (autoload 'bbdb-fontify-buffer "bbdb-gui")
  (autoload 'Info-goto-node "info")
  ;; this is very unpleasant, but saves me doing a lot of rewriting
  ;; for now. a big cleanup will happen for the next release, maybe.
  ;; NB if emacs 21 or older emacsen or even things you bolt on have
  ;; any of these functions, bad things will happen. Again, FITNR.
  (if (featurep 'xemacs)
      (progn
        (fset 'bbdb-extent-string 'extent-string)
        (fset 'bbdb-display-message 'display-message)
        (fset 'bbdb-event-to-character 'event-to-character))
    (fset 'bbdb-extent-string 'ignore)
    (fset 'bbdb-display-message 'ignore)
    (fset 'bbdb-event-to-character 'ignore)))

(defvar bbdb-define-all-aliases-needs-rebuilt nil)

(defcustom bbdb-extract-address-components-func
  'bbdb-rfc822-addresses
  "Function called to parse one or more email addresses.
See bbdb-extract-address-components for an example."
  :group 'bbdb-noticing-records
  :type 'function)

(defcustom bbdb-default-country
  '"Emacs";; what do you mean, it's not a country?
  "*Default country to use if none is specified."
  :group 'bbdb-record-creation
  :type 'string) ;; wonder if there's a smart place to get this? TZ, maybe?

(defmacro bbdb-grovel-elide-arg (arg)
  (list 'if arg
        (list 'not (list 'eq arg 0))
        'bbdb-display-layout))

(defvar bbdb-search-invert nil
  "Bind this variable to t in order to invert the result of `bbdb-search'.

\(let ((bbdb-search-invert t))
   \(bbdb-search records foo foo))")

(defun bbdb-search-invert-p ()
  "Return `bbdb-search-invert' and set it to nil.
To set it on again, use `bbdb-search-invert-set'."
  (let ((result bbdb-search-invert))
    (setq bbdb-search-invert nil)
    result))

;;;###autoload
(defun bbdb-search-invert-set ()
  "Typing \\<bbdb-mode-map>\\[bbdb-search-invert-set] inverts the meaning of the next search command.
Sets `bbdb-search-invert' to t.
You will have to call this function again, if you want to
do repeated inverted searches."
  (interactive)
  (setq bbdb-search-invert t)
  (message (substitute-command-keys
            "\\<bbdb-mode-map>\\[bbdb-search-invert-set] - ")))

(defmacro bbdb-search (records &optional name company net notes phone)
  "Search RECORDS for optional arguments NAME, COMPANY, NET, NOTES, PHONE.
This macro only emits code for those things being searched for;
literal nils at compile-time cause no code to be emitted.

If you want to reverse the search, bind `bbdb-search-invert' to t."
  (let (clauses)
    ;; I didn't protect these vars from multiple evaluation because that
    ;; actually generates *less efficient code* in elisp, because the extra
    ;; bindings can't easily be optimized away without lexical scope.  fmh.
    (or (stringp name) (symbolp name) (error "name must be atomic"))
    (or (stringp company) (symbolp company) (error "company must be atomic"))
    (or (stringp net) (symbolp net) (error "net must be atomic"))
    (or (stringp notes) (symbolp notes) (error "notes must be atomic"))
    (or (stringp phone) (symbolp phone) (error "phone must be atomic"))
    (if phone
        (setq clauses
              (cons
               (` (let ((rest-of-phones (bbdb-record-phones record))
                        (done nil))
                    (if rest-of-phones
                        (while (and rest-of-phones (not done))
                          (setq done (string-match (, phone)
                                                   ;; way way wasteful...
                                                   (bbdb-phone-string
                                                    (car rest-of-phones)))
                                rest-of-phones (cdr rest-of-phones)))
                      ;; so that "^$" can be used to find entries that
                      ;; have no phones
                      (setq done (string-match (, phone) "")))
                    done))
               clauses)))
    (if notes
        (setq clauses
              (cons
               (` (if (stringp (, notes))
                      (string-match (, notes)
                                    (or (bbdb-record-notes record) ""))
                    (if (eq (car (, notes)) '*)
                        (let ((fields all-fields) done tmp)
                          (if (bbdb-record-raw-notes record)
                              (while (and (not done) fields)
                                (setq tmp (bbdb-record-getprop
                                           record (car fields))
                                      done (and tmp (string-match
                                                     (cdr (, notes))
                                                     tmp))
                                      fields (cdr fields)))
                            ;; so that "^$" can be used to find entries that
                            ;; have no notes
                            (setq done (string-match (cdr (, notes)) "")))
                          done)
                      (string-match (cdr (, notes))
                                    (or (bbdb-record-getprop
                                         record (car (, notes))) "")))))
               clauses)))
    (if name
        (setq clauses
              (append
               (` ((string-match (, name) (or (bbdb-record-name record) ""))
                   (let ((rest-of-aka (bbdb-record-aka record))
                         (done nil))
                     (while (and rest-of-aka (not done))
                       (setq done (string-match (, name) (car rest-of-aka))
                             rest-of-aka (cdr rest-of-aka)))
                     done)))
               clauses)))
    (if net
        (setq clauses
              (cons
               (` (let ((rest-of-nets (bbdb-record-net record))
                        (done nil))
                    (if rest-of-nets
                        (while (and rest-of-nets (not done))
                          (setq done (string-match (, net) (car rest-of-nets))
                                rest-of-nets (cdr rest-of-nets)))
                      ;; so that "^$" can be used to find entries that
                      ;; have no net addresses.
                      (setq done (string-match (, net) "")))
                    done))
               clauses)))
    (if company
        (setq clauses
              (cons
               (` (string-match (, company)
                                (or (bbdb-record-company record) "")))
               clauses)))

    (` (let ((matches '())
             (,@ (if notes
                     '((all-fields (cons 'notes
                                         (mapcar (lambda (x) (intern (car x)))
                                                 (bbdb-propnames)))))
                   nil))
             (case-fold-search bbdb-case-fold-search)
             (records (, records))
         (invert (bbdb-search-invert-p))
             record)
         (while records
           (setq record (car records))
       (if (or (and invert
            (not (or (,@ clauses))))
           (and (not invert)
            (or (,@ clauses))))
           (setq matches (cons record matches)))
       (setq records (cdr records)))
         (nreverse matches)))))

(defun bbdb-search-prompt (prompt &rest rest)
  (if (string-match "%m" prompt)
      (setq prompt (replace-match (if bbdb-search-invert
                                      "not matching"
                                    "matching")
                                  nil nil prompt)))
  (read-string (apply 'format prompt rest)))

;;;###autoload
(defun bbdb (string elidep)
  "Display all entries in the BBDB matching the regexp STRING
in either the name(s), company, network address, or notes."
  (interactive
   (list (bbdb-search-prompt "Search records %m regexp: ")
         current-prefix-arg))
  (let* ((bbdb-display-layout (bbdb-grovel-elide-arg elidep))
         (notes (cons '* string))
         (records
          (bbdb-search (bbdb-records) string string string notes
                       nil)))
    (if records
        (bbdb-display-records records)
      ;; we could use error here, but it's not really an error.
      (message "No records matching '%s'" string))))

;;;###autoload
(defun bbdb-name (string elidep)
  "Display all entries in the BBDB matching the regexp STRING in the name
\(or ``alternate'' names\)."
  (interactive
   (list (bbdb-search-prompt "Search records with names %m regexp: ")
         current-prefix-arg))
   (let ((bbdb-display-layout (bbdb-grovel-elide-arg elidep)))
    (bbdb-display-records (bbdb-search (bbdb-records) string))))

;;;###autoload
(defun bbdb-company (string elidep)
  "Display all entries in BBDB matching STRING in the company field."
  (interactive
   (list (bbdb-search-prompt "Search records with company %m regexp: ")
         current-prefix-arg))
  (let ((bbdb-display-layout (bbdb-grovel-elide-arg elidep)))
    (bbdb-display-records (bbdb-search (bbdb-records) nil string))))

;;;###autoload
(defun bbdb-net (string elidep)
  "Display all entries in BBDB matching regexp STRING in the network address."
  (interactive
   (list (bbdb-search-prompt "Search records with net address %m regexp: ")
         current-prefix-arg))
  (let ((bbdb-display-layout (bbdb-grovel-elide-arg elidep)))
    (bbdb-display-records (bbdb-search (bbdb-records) nil nil string))))

;;;###autoload
(defun bbdb-notes (which string elidep)
  "Display all entries in BBDB matching STRING in the named notes field."
  (interactive
   (let (field)
     (list (setq field (completing-read "Notes field to search (RET for all): "
                                        (append '(("notes")) (bbdb-propnames))
                                        nil t))
           (if (featurep 'gmhist)
               (read-with-history-in 'bbdb-notes-field "Regular expression: ")
             (bbdb-search-prompt "Search records with %s %m regexp: "
                                 (if (string= field "")
                                     "one field"
                                   field)))
           current-prefix-arg)))
  (let ((bbdb-display-layout (bbdb-grovel-elide-arg elidep))
        (notes (if (string= which "")
                   (cons '* string)
                 (cons (intern which) string))))
    (bbdb-display-records (bbdb-search (bbdb-records) nil nil nil notes))))

(defun bbdb-phones (string elidep)
  "Display all entries in BBDB matching the regexp STRING in the phones field."
  (interactive
   (list (bbdb-search-prompt "Search records with phone %m regexp: ")
         current-prefix-arg))
  (let ((bbdb-display-layout (bbdb-grovel-elide-arg elidep)))
    (bbdb-display-records
     (bbdb-search (bbdb-records) nil nil nil nil string))))

;;;###autoload
(defun bbdb-changed (elidep)
  "Display all entries in the bbdb database which have been changed since
the database was last saved."
  (interactive "P")
  (let ((bbdb-display-layout (bbdb-grovel-elide-arg elidep))
        (changed-records  (bbdb-with-db-buffer bbdb-changed-records)))
    (if (bbdb-search-invert-p)
        (let ((recs (bbdb-records))
              unchanged-records
              r)
          (while recs
            (setq r (car recs)
                  recs (cdr recs))
            (when (not (member r changed-records))
              (setq changed-records (delete r changed-records)
                    unchanged-records (cons r unchanged-records))))
          (bbdb-display-records unchanged-records))
      (bbdb-display-records changed-records))))

(defun bbdb-display (records)
  "Prompts for and displays a single record (this is faster than searching.)"
  (interactive (list (bbdb-completing-read-record "Display record of: ")))
  (bbdb-display-records records))

(defun bbdb-display-some (function)
  "Display records according to FUNCTION.  FUNCTION is called with one
argument, the record, and should return nil if the record is not to be
displayed.  If the record is to be displayed, it (the record) should
be returned."
  (bbdb-display-records (delq nil (mapcar function (bbdb-records)))))

;;; fancy redisplay

;;;###autoload
(defun bbdb-redisplay-records ()
  "Regrinds the contents of the *BBDB* buffer, without scrolling.
If possible, you should call `bbdb-redisplay-one-record' instead."
  (let ((p (point))
        (m (condition-case nil (mark) (error nil))))
    (goto-char (window-start))
    (let ((p2 (point)))
      (bbdb-display-records-1 bbdb-records)
      (goto-char p2)
      (if m (set-mark m)))
    (recenter 0)
    (goto-char p)
    (save-excursion
      (run-hooks 'bbdb-list-hook))))

(defun bbdb-redisplay-one-record (record &optional record-cons next-record-cons
                                         delete-p)
  "Regrind one record. The *BBDB* buffer must be current when this is called."
  (bbdb-debug (if (not (eq (not (not delete-p))
                           (not (not (bbdb-record-deleted-p record)))))
                  (error "splorch.")))
  (if (null record-cons) (setq record-cons (assq record bbdb-records)))
  (if (null next-record-cons)
      (setq next-record-cons (car (cdr (memq record-cons bbdb-records)))))
  (if (null record-cons)
      (bbdb-display-records (list record) nil t)
    (let ((position (point))
          (marker (nth 2 record-cons))
          next-marker
          (buffer-read-only nil))
      (bbdb-debug
       (if (null record-cons) (error "doubleplus ungood: record unexists!"))
       (if (null marker) (error "doubleplus ungood: marker unexists!")))
      (beginning-of-line)
      (goto-char marker)
      (remove-text-properties marker (or (nth 2 next-record-cons) (point-max))
                              '(bbdb-field nil))
      (if delete-p nil
        (bbdb-format-record (car record-cons) (car (cdr record-cons))))
      (setq next-marker (or (nth 2 next-record-cons) (point-max)))
      (delete-region (point) next-marker)
      (if (< position next-marker)
          (goto-char position))

      (if (and bbdb-gui (not delete-p))
          (bbdb-fontify-buffer (list record-cons
                                     ;; the record ends here
                                     (list nil nil next-marker))))
      (save-excursion
        (run-hooks 'bbdb-list-hook)))))

;;; Parsing phone numbers
;;; XXX this needs expansion to handle international prefixes properly
;;; i.e. +353-number without discarding the +353 part. Problem being
;;; that this will necessitate yet another change in the database
;;; format for people who are using north american numbers.


(defconst bbdb-phone-area-regexp "(?[ \t]*\\+?1?[ \t]*[-\(]?[ \t]*[-\(]?[ \t]*\\([2-9][0-9][0-9]\\)[ \t]*)?[-./ \t]*")
(defconst bbdb-phone-main-regexp "\\([1-9][0-9][0-9]\\)[ \t]*[-.]?[ \t]*\\([0-9][0-9][0-9][0-9]\\)[ \t]*")

(defconst bbdb-phone-ext-regexp  "x?[ \t]*\\([0-9]+\\)[ \t]*")

(defconst bbdb-phone-regexp-1 (concat "^[ \t]*" bbdb-phone-area-regexp bbdb-phone-main-regexp bbdb-phone-ext-regexp "$"))
(defconst bbdb-phone-regexp-2 (concat "^[ \t]*" bbdb-phone-area-regexp bbdb-phone-main-regexp "$"))
(defconst bbdb-phone-regexp-3 (concat "^[ \t]*" bbdb-phone-main-regexp bbdb-phone-ext-regexp "$"))
(defconst bbdb-phone-regexp-4 (concat "^[ \t]*" bbdb-phone-main-regexp "$"))
(defconst bbdb-phone-regexp-5 (concat "^[ \t]*" bbdb-phone-ext-regexp "$"))

(defun bbdb-parse-phone-number (string &optional number-type)
  "Parse a phone number from STRING and return a list of integers the form
\(area-code exchange number) or (area-code exchange number extension).
This is both lenient and strict in what it will parse - whitespace may
appear (or not) between any of the groups of digits, parentheses around the
area code are optional, as is a dash between the exchange and number, and
a '1' preceeding the area code; but there must be three digits in the area
code and exchange, and four in the number (if they are present).  An error
will be signalled if unparsable.  All of these are unambigously parsable:

  ( 415 ) 555 - 1212 x123   -> (415 555 1212 123)
  (415)555-1212 123         -> (415 555 1212 123)
  (1-415) 555-1212 123      -> (415 555 1212 123)
  1 (415)-555-1212 123      -> (415 555 1212 123)
  555-1212 123              -> (0 555 1212 123)
  555 1212                  -> (0 555 1212)
  415 555 1212              -> (415 555 1212)
  1 415 555 1212            -> (415 555 1212)
  5551212                   -> (0 555 1212)
  4155551212                -> (415 555 1212)
  4155551212123             -> (415 555 1212 123)
  5551212x123               -> (0 555 1212 123)
  1234                      -> (0 0 0 1234)

Note that \"4151212123\" is ambiguous; it could be interpreted either as
\"(415) 121-2123\" or as \"415-1212 x123\".

\(And uh, oh yeah, this does little if `bbdb-north-american-phone-numbers-p'
is nil...\)"

  (cond ((if number-type
             (eq number-type 'euro)
           (not bbdb-north-american-phone-numbers-p))
         (list (bbdb-string-trim string)))
        ((string-match bbdb-phone-regexp-1 string)
         ;; (415) 555-1212 x123
         (list (bbdb-subint string 1) (bbdb-subint string 2)
               (bbdb-subint string 3) (bbdb-subint string 4)))
        ((string-match bbdb-phone-regexp-2 string)
         ;; (415) 555-1212
         (list (bbdb-subint string 1) (bbdb-subint string 2)
               (bbdb-subint string 3)))
        ((string-match bbdb-phone-regexp-3 string)
         ;; 555-1212 x123
         (list 0 (bbdb-subint string 1) (bbdb-subint string 2)
               (bbdb-subint string 3)))
        ((string-match bbdb-phone-regexp-4 string)
         ;; 555-1212
         (list 0 (bbdb-subint string 1) (bbdb-subint string 2)))
        ((string-match bbdb-phone-regexp-5 string)
         ;; x123
         (list 0 0 0 (bbdb-subint string 1)))
        (t (error "phone number unparsable."))))

;;; Parsing other things

(defcustom bbdb-expand-mail-aliases t
  "If non-nil, expand mail aliases in `bbdb-complete-name'."
  :group 'bbdb-record-use
  :type 'boolean)

(defcustom bbdb-check-zip-codes-p t
  "If non-nil, require legal zip codes when entering an address.
The format of legal zip codes is determined by the variable
`bbdb-legal-zip-codes'."
  :group 'bbdb-record-creation
  :type 'boolean)

(defcustom bbdb-legal-zip-codes
  '(;; empty string
    "^$"
    ;; Matches 1 to 6 digits.
    "^[ \t\n]*[0-9][0-9]?[0-9]?[0-9]?[0-9]?[0-9]?[ \t\n]*$"
    ;; Matches 5 digits and 3 or 4 digits.
    "^[ \t\n]*\\([0-9][0-9][0-9][0-9][0-9]\\)[ \t\n]*-?[ \t\n]*\\([0-9][0-9][0-9][0-9]?\\)[ \t\n]*$"
    ;; Match zip codes for Canada, UK, etc. (result is ("LL47" "U4B")).
    "^[ \t\n]*\\([A-Za-z0-9]+\\)[ \t\n]+\\([A-Za-z0-9]+\\)[ \t\n]*$"
    ;; Match zip codes for continental Europe.  Examples "CH-8057"
    ;; or "F - 83320" (result is ("CH" "8057") or ("F" "83320")).
    ;; Support for "NL-2300RA" added at request from Carsten Dominik
    ;; <dominik@astro.uva.nl>
    "^[ \t\n]*\\([A-Z]+\\)[ \t\n]*-?[ \t\n]*\\([0-9]+ ?[A-Z]*\\)[ \t\n]*$"
    ;; Match zip codes from Sweden where the five digits are grouped 3+2
    ;; at the request from Mats Lofdahl <MLofdahl@solar.stanford.edu>.
    ;; (result is ("SE" (133 36)))
    "^[ \t\n]*\\([A-Z]+\\)[ \t\n]*-?[ \t\n]*\\([0-9]+\\)[ \t\n]+\\([0-9]+\\)[ \t\n]*$")
  "List of regexps that match legal zip codes.
Whether this is used at all depends on the variable `bbdb-check-zip-codes-p'."
  :group 'bbdb-record-creation
  :type '(repeat regexp))

(defun bbdb-parse-zip-string (string)
  "Check whether STRING is a legal zip code.
Do this only if `bbdb-check-zip-codes-p' is non-nil."
  (if (and bbdb-check-zip-codes-p
           (not (memq t (mapcar (lambda (regexp)
                                  ;; if it matches, (not (not index-of-match)) returns t
                                  (not (not (string-match regexp string))))
                                bbdb-legal-zip-codes))))
      (error "not a valid zip code.")
    string))

(defun bbdb-read-new-record ()
  "Prompt for and return a completely new BBDB record.
Doesn't insert it in to the database or update the hashtables, but does
ensure that there will not be name collisions."
  (bbdb-records)                        ; make sure database is loaded
  (if bbdb-readonly-p
      (error "The Insidious Big Brother Database is read-only."))
  (let (firstname lastname)
    (bbdb-error-retry
     (progn
       (if current-prefix-arg
           (setq firstname (bbdb-read-string "First Name: ")
                 lastname (bbdb-read-string "Last Name: "))
         (let ((names (bbdb-divide-name (bbdb-read-string "Name: "))))
           (setq firstname (car names)
                 lastname (nth 1 names))))
       (if (string= firstname "") (setq firstname nil))
       (if (string= lastname "") (setq lastname nil))
       (if (and bbdb-no-duplicates-p
                (bbdb-gethash (bbdb-build-name firstname lastname)))
           (error "%s %s is already in the database"
                  (or firstname "") (or lastname "")))))
    (let ((company (bbdb-read-string "Company: "))
          (net (bbdb-split (bbdb-read-string "Network Address: ") ","))
          (addrs
           (let (L L-tail str addr)
             (while (not (string=
                          ""
                          (setq str
                                (bbdb-read-string
                                 "Address Description [RET when no more]: "
                                 ""
                                 (mapcar (function (lambda(x) (list x)))
                                         (bbdb-label-completion-list
                                          "addresses"))))))
               (setq addr (make-vector bbdb-address-length nil))
               (bbdb-record-edit-address addr str)
               (if L
                   (progn (setcdr L-tail (cons addr nil))
                          (setq L-tail (cdr L-tail)))
                 (setq L (cons addr nil)
                       L-tail L)))
             L))
          (phones
           (let (L L-tail str)
             (while (not (string=
                          ""
                          (setq str
                                (bbdb-read-string
                                 "Phone Location [RET when no more]: "
                                 ""
                                 (mapcar (function (lambda(x) (list x)))
                                         (bbdb-label-completion-list
                                          "phones"))))))
               (let* ((phonelist
                       (bbdb-error-retry
                        (bbdb-parse-phone-number
                         (read-string "Phone: "
                                      (and (integerp bbdb-default-area-code)
                                           (format "(%03d) "
                                                   bbdb-default-area-code))))))
                      (phone (apply 'vector str
                                    (if (= 3 (length phonelist))
                                        (nconc phonelist '(0))
                                      phonelist))))
                 (if L
                     (progn (setcdr L-tail (cons phone nil))
                            (setq L-tail (cdr L-tail)))
                          (setq L (cons phone nil)
                                L-tail L))))
                    L))
          (notes (bbdb-read-string "Additional Comments: ")))
      (if (string= company "") (setq company nil))
      (if (string= notes "") (setq notes nil))
      (let ((record
             (vector firstname lastname nil company phones addrs net notes
                     (make-vector bbdb-cache-length nil))))
        record))))

;;;###autoload
(defun bbdb-create (record)
  "Add a new entry to the bbdb database ; prompts for all relevant info
using the echo area, inserts the new record in the db, sorted alphabetically,
and offers to save the db file.  DO NOT call this from a program.  Call
bbdb-create-internal instead."
  (interactive (list (bbdb-read-new-record)))
  (bbdb-invoke-hook 'bbdb-create-hook record)
  (bbdb-change-record record t)
  (bbdb-display-records (list record)))


(defmacro bbdb-check-type (place predicate)
  (list 'while (list 'not (list predicate place))
        (nconc (cond ((eq (car-safe place) 'aref)
                      (list 'aset (nth 1 place) (nth 2 place)))
                     ((eq (car-safe place) 'car)
                      (list 'setcar (nth 1 place)))
                     ((eq (car-safe place) 'cdr)
                      (list 'setcdr (nth 1 place)))
                     (t (list 'setq place)))
               (list
                (list 'signal ''wrong-type-argument
                      (list 'list (list 'quote predicate) place))))))

(defun bbdb-create-internal (name company net addrs phones notes)
  "Adds a record to the database; this function does a fair amount of
error-checking on the passed in values, so it's safe to call this from
other programs.

NAME is a string, the name of the person to add.  An error is signalled
if that name is already in use and `bbdb-no-duplicates-p' is t.
COMPANY is a string or nil.
NET is a comma-separated list of email addresses, or a list of strings.
An error is signalled if that name is already in use.
ADDRS is a list of address objects.  An address is a vector of the form
  [\"location\" (\"line1\" \"line2\" ... ) \"City\" \"State\" \"Zip\" \"Country\"].
PHONES is a list of phone-number objects.  A phone-number is a vector of
the form
  [\"location\" areacode prefix suffix extension-or-nil]
or
  [\"location\" \"phone-number\"]
NOTES is a string, or an alist associating symbols with strings."
  (let (firstname lastname aka)
    (while (and (progn
                  (setq name      (and name (bbdb-divide-name name))
                        firstname (car name)
                        lastname  (nth 1 name))
                  (bbdb-gethash (bbdb-build-name firstname lastname)))
                bbdb-no-duplicates-p)
      (setq name (signal 'error
                         (list (format "%s %s is already in the database"
                                       (or firstname "") (or lastname ""))))))
    (and company (bbdb-check-type company stringp))
    (if (stringp net)
        (setq net (bbdb-split net ",")))
    (if bbdb-no-duplicates-p
        (let ((rest net))
          (while rest
            (while (bbdb-gethash (downcase (car rest)))
              (setcar rest
                      (signal 'error (list (format
                                            "%s is already in the database"
                                            (car rest))))))
            (setq rest (cdr rest)))))
    (setq addrs
          (mapcar
           (lambda (addr)
             (while (or (not (vectorp addr))
                        (/= (length addr) bbdb-address-length))
               (setq addr (signal 'wrong-type-argument (list 'vectorp addr))))
             (bbdb-check-type (aref addr 0) stringp) ;;; XXX use bbdb-addresses
             (bbdb-check-type (aref addr 1) listp)
             (bbdb-check-type (aref addr 2) stringp)
             (bbdb-check-type (aref addr 3) stringp)
             (bbdb-check-type (aref addr 4) stringp)
             (bbdb-check-type (aref addr 5) stringp)
             addr)
           addrs))
    (setq phones
          (mapcar
           (lambda (phone)
             (while (or (not (vectorp phone))
                        (and (/= (length phone) 2)
                             (/= (length phone) bbdb-phone-length)))
               (setq phone
                     (signal 'wrong-type-argument (list 'vectorp phone))))
             (bbdb-check-type (aref phone 0) stringp)
             (if (= 2 (length phone))
                 (bbdb-check-type (aref phone 1) stringp)
               (bbdb-check-type (aref phone 1) integerp)
               (bbdb-check-type (aref phone 2) integerp)
               (bbdb-check-type (aref phone 3) integerp)
               (and (aref phone 4) (bbdb-check-type (aref phone 4) integerp))
               (if (eq 0 (aref phone 4)) (aset phone 4 nil)))
             phone)
           phones))
    (or (stringp notes)
        (setq notes
              (mapcar (lambda (note)
                        (bbdb-check-type note consp)
                        (bbdb-check-type (car note) symbolp)
                        (if (consp (cdr note))
                            (setq note (cons (car note) (car (cdr note)))))
                        (bbdb-check-type (cdr note) stringp)
                        note)
                      notes)))
    (let ((record
           (vector firstname lastname aka company phones addrs net notes
                   (make-vector bbdb-cache-length nil))))
      (bbdb-invoke-hook 'bbdb-create-hook record)
      (bbdb-change-record record t)
      record)))


;;; bbdb-mode stuff

(defun bbdb-current-record (&optional planning-on-modifying)
  "Returns the record which the point is point at.  In linear time, man..."
  (if (and planning-on-modifying bbdb-readonly-p)
      (error "The Insidious Big Brother Database is read-only."))
  (if (not (equal bbdb-buffer-name (buffer-name (current-buffer))))
      (error "this command only works while in the \"%s\" buffer."
             bbdb-buffer-name))
  (let ((p (point))
        (rest bbdb-records)
        (rec nil))
    (while (and (cdr rest) (not rec))
      (if (> (nth 2 (car (cdr rest))) p)
          (setq rec (car (car rest))))
      (setq rest (cdr rest)))
    (or rec (car (car rest)))))


;; yow, are we object oriented yet?
(defun bbdb-record-get-field-internal (record field)
  (cond ((eq field 'name)   (bbdb-record-name record))
        ((eq field 'net)    (bbdb-record-net record))
        ((eq field 'aka)    (bbdb-record-aka record))
        ((eq field 'phone)  (bbdb-record-phones record))
        ((eq field 'address)    (bbdb-record-addresses record))
        ((eq field 'property)   (bbdb-record-raw-notes record))
        (t (error "doubleplus ungood: unknown field type %s" field))))

(defun bbdb-record-store-field-internal (record field value)
  (cond ((eq field 'name)   (error "doesn't work on names"))
        ((eq field 'net)    (bbdb-record-set-net record value))
        ((eq field 'aka)    (bbdb-record-set-aka record value))
        ((eq field 'phone)  (bbdb-record-set-phones record value))
        ((eq field 'address)    (bbdb-record-set-addresses record value))
        ((eq field 'property)   (bbdb-record-set-raw-notes record value))
        (t (error "doubleplus ungood: unknown field type %s" field))))

(defun bbdb-record-edit-field-internal (record field &optional which location)
  (cond ((eq field 'name)     (bbdb-record-edit-name record))
        ((eq field 'company)  (bbdb-record-edit-company record))
        ((eq field 'net)      (bbdb-record-edit-net record))
        ((eq field 'aka)      (bbdb-record-edit-aka record))
        ((eq field 'phone)    (bbdb-record-edit-phone which location))
        ((eq field 'address)  (bbdb-record-edit-address which location))
        ((eq field 'property) (bbdb-record-edit-property record (car which)))
        (t (error "doubleplus ungood: unknown field type %s" field))))


(defun bbdb-current-field (&optional planning-on-modifying)
  (or (bbdb-current-record planning-on-modifying)
      (error "unperson"))
  (delete 'field-name (get-text-property (point) 'bbdb-field)))

;;;###autoload
(defun bbdb-apply-next-command-to-all-records ()
  "Typing \\<bbdb-mode-map>\\[bbdb-apply-next-command-to-all-records] \
in the *BBDB* buffer makes the next command operate on all
of the records currently displayed.  \(Note that this only works for
certain commands.\)"
  (interactive)
  (message (substitute-command-keys
            "\\<bbdb-mode-map>\\[bbdb-apply-next-command-to-all-records] - "))
  (setq prefix-arg current-prefix-arg
        last-command this-command)
  nil)

(defmacro bbdb-do-all-records-p ()
  "Whether the last command was `bbdb-apply-next-command-to-all-records'."
  '(eq last-command 'bbdb-apply-next-command-to-all-records))


(defvar bbdb-append-records nil)

;;;###autoload
(defun bbdb-append-records-p ()
  (cond ((eq t bbdb-append-records))
        ((numberp bbdb-append-records)
         (setq bbdb-append-records
               (1- bbdb-append-records))
         (when (= 0 bbdb-append-records)
           (when (not bbdb-silent-running)
             (message "No further search results will be appended.")
             (sit-for 2))
           (setq bbdb-append-records nil))
         t)
        (bbdb-append-records
         (setq bbdb-append-records nil)
         t)
        (t nil)))

;;;###autoload
(defun bbdb-append-records (arg)
  "Typing \\<bbdb-mode-map>\\[bbdb-append-records] \
in the *BBDB* buffer makes the next search/display command to append
new records to those in the *BBDB* buffer.

With an prefix arg (C-u) toggle between always append and no append.
With an prefix arg that is a positive number append will be enabled for that
many times.
With any other argument append will be enabled once."
  (interactive "P")
  (message (substitute-command-keys
            "\\<bbdb-mode-map>\\[bbdb-append-records] - "))
  (setq bbdb-append-records
        (cond ((and arg (listp arg))
               (if (not bbdb-silent-running)
                   (if (not bbdb-append-records)
                       (message "Always append records.")
                     (message "Do not append records.")))
               (not bbdb-append-records))
              ((and (numberp arg) (< 1 arg))
               (if (not bbdb-silent-running)
                   (message "Append records for the next %d times." arg))
               arg)
              (t 'once))))

;;;###autoload
(defun bbdb-insert-new-field (record name contents)
  "Add a new field to the current record; the field type and contents
are prompted for if not supplied.

If you are inserting a new phone-number field, you can control whether
it is a north american or european phone number by providing a prefix
argument.  A prefix arg of ^U means it's to be a euronumber, and any
other prefix arg means it's to be a a structured north american number.
Otherwise, which style is used is controlled by the variable
`bbdb-north-american-phone-numbers-p'.

If you are inserting a new net address, you can have BBDB append a
default domain to any net address that does not contain one.  Set
`bbdb-default-domain' to a string such as \"mycompany.com\" (or,
depending on your environment, (getenv \"DOMAINNAME\")), and
\"@mycompany.com\" will be appended to an address that is entered as
just a username.  A prefix arg of ^U (or a `bbdb-default-domain'
value of \"\", the default) means do not alter the address."
  (interactive (let ((record (or (bbdb-current-record t)
                                 (error "current record unexists!")))
                     (name "")
                     (completion-ignore-case t))
                 (while (string= name "")
                   (setq name
                         (downcase
                          (completing-read "Insert Field: "
                                           (append '(("phone") ("address")
                                                     ("net") ("AKA") ("notes"))
                                                   (bbdb-propnames))
                                           nil
                                           nil ; used to be t
                                           nil))))
                 (setq name (intern name))
                 (list record name (bbdb-prompt-for-new-field-value name))))
  (if (null contents)
      (setq contents (bbdb-prompt-for-new-field-value name)))

  (cond ((eq name 'phone)
         (bbdb-record-set-phones record
                                 (nconc (bbdb-record-phones record)
                                        (list contents))))
        ((eq name 'address)
         (bbdb-record-set-addresses record
                                    (nconc (bbdb-record-addresses record)
                                           (list contents))))
        ((eq name 'net)
         (if (bbdb-record-net record)
             (error "There already are net addresses!"))
         (if (stringp contents)
             (setq contents (bbdb-split contents ",")))
         ;; first detect any conflicts....
         (if bbdb-no-duplicates-p
             (let ((nets contents))
               (while nets
                 (let ((old (bbdb-gethash (downcase (car nets)))))
                   (if (and old (not (eq old record)))
                       (error "net address \"%s\" is used by \"%s\""
                              (car nets)
                              (or (bbdb-record-name old)
                                  (car (bbdb-record-net old))))))
                 (setq nets (cdr nets)))))
         ;; then store.
         (let ((nets contents))
           (while nets
             (bbdb-puthash (downcase (car nets)) record)
             (setq nets (cdr nets))))
         (bbdb-record-set-net record contents))
        ((eq name 'aka)
        (if (bbdb-record-aka record)
            (error "there already are alternate names!"))
        (if (stringp contents)
            (setq contents (bbdb-split contents ";")))
        ;; first detect any conflicts....
        (if bbdb-no-duplicates-p
            (let ((aka contents))
              (while aka
                (let ((old (bbdb-gethash (downcase (car aka)))))
                  (if (and old (not (eq old record)))
                      (error "alternate name \"%s\" is used by \"%s\""
                             (car aka)
                             (or (bbdb-record-name old)
                                 (car (bbdb-record-net old))))))
                (setq aka (cdr aka)))))
        ;; then store.
        (let ((aka contents))
          (while aka
            (bbdb-puthash (downcase (car aka)) record)
            (setq aka (cdr aka))))
        (bbdb-record-set-aka record contents))
        ((eq name 'notes)
        (if (bbdb-record-notes record) (error "there already are notes!"))
        (bbdb-record-set-notes record contents))
        ((assoc (symbol-name name) (bbdb-propnames))
        (if (and (consp (bbdb-record-raw-notes record))
                 (assq name (bbdb-record-raw-notes record)))
            (error "there is already a \"%s\" note!" name))
        (bbdb-record-putprop record name contents))
        (t (error "doubleplus ungood: unknow how to set slot %s" name)))
  (bbdb-change-record record nil)
;    (bbdb-offer-save)
  (let ((bbdb-display-layout nil))
    (bbdb-redisplay-one-record record)))

(defun bbdb-prompt-for-new-field-value (name)
  (cond ((eq name 'net)
         (let
             ((n (bbdb-read-string "Net: ")))
           (if (string-match "^mailto:" n)
               (setq n (substring n (match-end 0))))
           (if (or (eq nil bbdb-default-domain)
                   current-prefix-arg (string-match "[@%!]" n))
               n
             (concat n "@" bbdb-default-domain))))
        ((eq name 'aka) (bbdb-read-string "Alternate Names: "))
        ((eq name 'phone)
         (let ((p (make-vector
                   (if (if current-prefix-arg
                           (numberp current-prefix-arg)
                         bbdb-north-american-phone-numbers-p)
                       bbdb-phone-length
                     2)
                   0)))
           (aset p 0 nil)
           (aset p 1
                 (if (= bbdb-phone-length (length p))
                     (if (integerp bbdb-default-area-code)
                         bbdb-default-area-code
                       0)
                   nil))
           (bbdb-record-edit-phone p)
           p))
        ((eq name 'address)
         (let ((a (make-vector bbdb-address-length nil)))
           (bbdb-record-edit-address a)
           a))
        ((eq name 'notes) (bbdb-read-string "Notes: "))
        ((assoc (symbol-name name) (bbdb-propnames))
         (bbdb-read-string (format "%s: " name)))
        (t
         (if (bbdb-y-or-n-p
              (format "\"%s\" is an unknown field name.  Define it? " name))
             (bbdb-set-propnames
              (append (bbdb-propnames) (list (list (symbol-name name)))))
           (error "unknown field \"%s\"" name))
         (bbdb-read-string (format "%s: " name)))))

(defun bbdb-add-new-field (name)
  "Programmatically add a new field called NAME. Returns the list of propnames."
  ;; check that we don't have one already; if we do, return quietly.
  (if (assoc (symbol-name name) (append '(("phone") ("address") ("net")
                                          ("AKA") ("notes"))
                                        (bbdb-propnames)))
      bbdb-propnames
    (bbdb-set-propnames (append (bbdb-propnames)
                                (list (list (symbol-name name)))))))

;;;###autoload
(defun bbdb-edit-current-field ()
  "Edit the contents of the Insidious Big Brother Database field displayed on
the current line (this is only meaningful in the \"*BBDB*\" buffer.)   If the
cursor is in the middle of a multi-line field, such as an address or comments
section, then the entire field is edited, not just the current line."
  (interactive)
  ;; when at the end of the line take care of it
  (if (and (eolp) (not (bobp)) (not (bbdb-current-field t)))
      (backward-char 1))

  (let* ((record (bbdb-current-record t))
         (field (bbdb-current-field t))
         need-to-sort)
    (or field (error "on an unfield"))
    (setq need-to-sort
          (apply 'bbdb-record-edit-field-internal record field))
    (bbdb-change-record record need-to-sort)
    (bbdb-redisplay-one-record record)
    ;; (bbdb-offer-save)
    (if (and (eq 'property (car field))
             (or (eq 'mail-alias (caadr field))
                 (eq 'net (caadr field))))
        (setq bbdb-define-all-aliases-needs-rebuilt 'edit))
    ))

(defun bbdb-record-edit-name (bbdb-record)
  (let (fn ln co need-to-sort new-name old-name)
    (bbdb-error-retry
     (progn
       (if current-prefix-arg
           (setq fn (bbdb-read-string "First Name: "
                                      (bbdb-record-firstname bbdb-record))
                 ln (bbdb-read-string "Last Name: "
                                      (bbdb-record-lastname bbdb-record)))
         (let ((names (bbdb-divide-name
                       (bbdb-read-string "Name: "
                                         (bbdb-record-name bbdb-record)))))
           (setq fn (car names)
                 ln (nth 1 names))))
       (setq need-to-sort
             (or (not (string= fn
                               (or (bbdb-record-firstname bbdb-record) "")))
                 (not (string= ln
                               (or (bbdb-record-lastname bbdb-record) "")))))
       (if (string= "" fn) (setq fn nil))
       (if (string= "" ln) (setq ln nil))
       ;; check for collisions
       (setq new-name (if (and fn ln) (concat fn " " ln)
                        (or fn ln))
             old-name (bbdb-record-name bbdb-record))
       (if (and bbdb-no-duplicates-p
                new-name
                (not (and old-name (string= (downcase new-name)
                                            (downcase old-name))))
                (bbdb-gethash (downcase new-name)))
           (error "%s is already in the database!" new-name))))
    (setq co (bbdb-read-string "Company: "
                               (bbdb-record-company bbdb-record)))
    (if (string= "" co) (setq co nil))
    (setq need-to-sort
          (or need-to-sort
              (not (equal (if co (downcase co) "")
                          (downcase (or (bbdb-record-company bbdb-record)
                                        ""))))))
    ;;
    ;; delete the old hash entry
    (let ((name    (bbdb-record-name    bbdb-record))
          (company (bbdb-record-company bbdb-record)))
      (if (> (length name) 0)
          (bbdb-remhash (downcase name) bbdb-record))
      (if (> (length company) 0)
          (bbdb-remhash (downcase company) bbdb-record)))
    (bbdb-record-set-namecache bbdb-record nil)
    (bbdb-record-set-firstname bbdb-record fn)
    (bbdb-record-set-lastname bbdb-record ln)
    (bbdb-record-set-company bbdb-record co)
    ;; add a new hash entry
    (and (or fn ln)
         (bbdb-puthash (downcase (bbdb-record-name bbdb-record))
                       bbdb-record))
    need-to-sort))

(defun bbdb-record-edit-company (bbdb-record)
  (let ((co (bbdb-read-string "Company: " (bbdb-record-company bbdb-record)))
        need-to-sort)

    (if (string= "" co) (setq co nil))
    (setq need-to-sort
          (or need-to-sort
              (not (equal (if co (downcase co) "")
                          (downcase (or (bbdb-record-company bbdb-record)
                                        ""))))))

    ;; delete the old hash entry
    (let ((name    (bbdb-record-name    bbdb-record))
          (company (bbdb-record-company bbdb-record)))
      (if (> (length name) 0)
          (bbdb-remhash (downcase name) bbdb-record))
      (if (> (length company) 0)
          (bbdb-remhash (downcase company) bbdb-record)))

    (bbdb-record-set-company bbdb-record co)
    ;; add a new hash entry
    (bbdb-puthash (downcase (bbdb-record-name bbdb-record))
                  bbdb-record)

    need-to-sort))

(defun bbdb-address-edit-default (addr)
  "Function to use for address editing.
The sub-fields are queried using the default order and using the
default names.  Set `bbdb-address-editing-function' to an alternate
address editing function if you don't like this function.  It is
mostly used for US style addresses.

The sub-fields and the prompts used are:
Street, line n:  (nth n street)
City:            city
State:           state
Zip Code:        zip
Country:         country"
  (let* ((str (let ((l) (s) (n 0))
                (while (not (string= "" (setq s (bbdb-read-string
                                                 (format "Street, line %d: " (+ 1 n))
                                                 (nth n (bbdb-address-streets addr))))))
                  (setq l (append l (list s)))
                  (setq n (1+ n)))
                l))
         (cty (bbdb-read-string "City: " (bbdb-address-city addr)))
         (ste (bbdb-read-string "State: " (bbdb-address-state addr)))
         (zip (bbdb-error-retry
               (bbdb-parse-zip-string
                (bbdb-read-string "Zip Code: " (bbdb-address-zip-string addr)))))
         (country (bbdb-read-string "Country: " (or (bbdb-address-country addr)
                                                    bbdb-default-country))))
    (bbdb-address-set-streets addr str)
    (bbdb-address-set-city addr cty)
    (bbdb-address-set-state addr ste)
    (bbdb-address-set-zip addr zip)
    (if (string= "" (concat cty ste zip country (mapconcat 'identity str "")))
        ;; user didn't enter anything. this causes a display bug. this
        ;; is a temporary fix. Ideally, we'd simply discard the entire
        ;; address entry, but that's going to require bigger hacking.
        (bbdb-address-set-country addr "Emacs")
      (bbdb-address-set-country addr country))
    nil))

(defun bbdb-address-edit-continental (addr)
  "Function to use for address editing.
The sub-fields are queried using the default order and using the
default names.  Set `bbdb-address-editing-function' to an alternate
address editing function if you don't like this function.  It is
mostly used for US style addresses.

The sub-fields and the prompts used are:
Street, line n:  (nth n street)
City:            city
State:           state
Zip Code:        zip
Country:         country"
  (let* ((str (let ((l) (s) (n 0))
                (while (not (string= "" (setq s (bbdb-read-string
                                                 (format "Street, line %d: " (+ 1 n))
                                                 (nth n (bbdb-address-streets addr))))))
                  (setq l (append l (list s)))
                  (setq n (1+ n)))
                l))
         (zip (bbdb-error-retry
               (bbdb-parse-zip-string
                (bbdb-read-string "Zip Code: " (bbdb-address-zip-string addr)))))
         (cty (bbdb-read-string "City: " (bbdb-address-city addr)))
         (ste "")
         (country (bbdb-read-string "Country: " (or (bbdb-address-country addr)
                                                    bbdb-default-country))))
    (bbdb-address-set-streets addr str)
    (bbdb-address-set-city addr cty)
    (bbdb-address-set-state addr ste)
    (bbdb-address-set-zip addr zip)
    (if (string= "" (concat cty ste zip country (mapconcat 'identity str "")))
        ;; user didn't enter anything. this causes a display bug. this
        ;; is a temporary fix. Ideally, we'd simply discard the entire
        ;; address entry, but that's going to require bigger hacking.
        (bbdb-address-set-country addr "Emacs")
      (bbdb-address-set-country addr country))
    nil))

(defcustom bbdb-address-editing-function 'bbdb-address-edit-default
  "Function to use for address editing.
The function must accept a BBDB address as parameter and allow the
user to edit it.  This variable is called from `bbdb-record-edit-address'.
The default value is the symbol `bbdb-address-edit-default'."
  :group 'bbdb-record-creation
  :type 'function)

(defun bbdb-record-edit-address (addr &optional location)
  "Edit an address ADDR.
If optional parameter LOCATION is nil, edit the location sub-field
of the address as well.  The address itself is edited using the editing
function in `bbdb-address-editing-function'."
  (let ((loc
         (or location (bbdb-read-string "Location: "
                                        (or (bbdb-address-location addr)
                                            (bbdb-label-completion-default
                                             "addresses"))
                                        (mapcar (function (lambda(x) (list x)))
                                                (bbdb-label-completion-list
                                                 "addresses"))))))
    (bbdb-address-set-location addr loc))
  (if current-prefix-arg
      (bbdb-address-edit-default addr)
    (funcall bbdb-address-editing-function addr)))

(defun bbdb-record-edit-phone (phone-number &optional location)
  (let ((newl (or location
                  (bbdb-read-string "Location: "
                                    (or (bbdb-phone-location phone-number)
                                        (bbdb-label-completion-default
                                         "phones"))
                                    (mapcar (function (lambda(x) (list x)))
                                            (bbdb-label-completion-list
                                             "phones")))))
        (newp (let ((bbdb-north-american-phone-numbers-p
                     (= (length phone-number) bbdb-phone-length)))
                (bbdb-error-retry
                 (bbdb-parse-phone-number
                  (read-string "Phone: " (bbdb-phone-string phone-number)))))))
    (bbdb-phone-set-location phone-number newl)
    (bbdb-phone-set-area phone-number (nth 0 newp)) ; euronumbers too.
    (if (= (length phone-number) 2)
        nil
      (bbdb-phone-set-exchange phone-number (nth 1 newp))
      (bbdb-phone-set-suffix phone-number (nth 2 newp))
      (bbdb-phone-set-extension phone-number (or (nth 3 newp) 0))))
  nil)

(defun bbdb-record-edit-net (bbdb-record)
  (let ((str (bbdb-read-string "Net: "
                               (mapconcat (function identity)
                                          (bbdb-record-net bbdb-record)
                                          ", "))))
    (let ((oldnets (bbdb-record-net bbdb-record))
          (newnets (bbdb-split str ",")))
      ;; first check for any conflicts...
      (if bbdb-no-duplicates-p
          (let ((rest newnets))
            (while rest
              (let ((old (delete bbdb-record (bbdb-gethash (downcase (car rest))))))
                (if old
                    (error "net address \"%s\" is used by \"%s\""
                           (car rest) (mapconcat (lambda (r) (bbdb-record-name r))
                                                 old ", "))))
              (setq rest (cdr rest)))))
      ;; then update.
      (let ((rest oldnets))
        (while rest
          (bbdb-remhash (downcase (car rest)) bbdb-record)
          (setq rest (cdr rest))))
      (let ((nets newnets))
        (while nets
          (bbdb-puthash (downcase (car nets)) bbdb-record)
          (setq nets (cdr nets))))
      (bbdb-record-set-net bbdb-record newnets)))
  nil)

(defun bbdb-record-edit-aka (bbdb-record)
  (let ((str (bbdb-read-string "AKA: "
                               (mapconcat (function identity)
                                          (bbdb-record-aka bbdb-record)
                                          "; "))))
    (let ((oldaka (bbdb-record-aka bbdb-record))
          (newaka (bbdb-split str ";")))
      ;; first check for any conflicts...
      (if bbdb-no-duplicates-p
          (let ((rest newaka))
            (while rest
              (let ((old (bbdb-gethash (downcase (car rest)))))
                (if (and old (not (eq old bbdb-record)))
                    (error "alternate name address \"%s\" is used by \"%s\""
                           (car rest) (bbdb-record-name old))))
              (setq rest (cdr rest)))))
      ;; then update.
      (let ((rest oldaka))
        (while rest
          (bbdb-remhash (downcase (car rest)) bbdb-record)
          (setq rest (cdr rest))))
      (let ((aka newaka))
        (while aka
          (bbdb-puthash (downcase (car aka)) bbdb-record)
          (setq aka (cdr aka))))
      (bbdb-record-set-aka bbdb-record newaka)))
  nil)

;;;###autoload
(defun bbdb-record-edit-notes (bbdb-record &optional regrind)
  (interactive (list (bbdb-current-record t) t))
  (let ((notes (bbdb-read-string "Notes: " (bbdb-record-notes bbdb-record))))
    (bbdb-record-set-notes bbdb-record (if (string= "" notes) nil notes)))
  (if regrind
      (save-excursion
        (set-buffer bbdb-buffer-name)
        (bbdb-redisplay-one-record bbdb-record)))
  nil)

;;;###autoload
(defun bbdb-record-edit-property (bbdb-record &optional prop regrind)
  (interactive (list (bbdb-current-record t) nil t))
  (let* ((propnames (bbdb-propnames))
         (propname (if prop (symbol-name prop)
                     (completing-read
                      (format "Edit property of %s: "
                              (bbdb-record-name bbdb-record))
                      (cons '("notes") propnames))))
         (propsym (or prop (if (equal "" propname) 'notes (intern propname))))
         (string (bbdb-read-string (format "%s: " propname)
                                   (bbdb-record-getprop bbdb-record propsym))))
    (bbdb-record-putprop bbdb-record propsym
                         (if (string= "" string) nil string)))
  (if regrind
      (save-excursion
        (set-buffer bbdb-buffer-name)
        (bbdb-redisplay-one-record bbdb-record)))
  nil)


(defsubst bbdb-field-equal (x y)
  (if (and (consp x) (consp y))
      (and (eq (car x) (car y))
           (eq (car (cdr x)) (car (cdr y)))
           (eq (car (cdr (cdr x))) (car (cdr (cdr y)))))
    (eq x y)))

(defun bbdb-next-field (&optional count planning-on-modifying)
  (or count (setq count 1))
  (beginning-of-line)
  (let* ((record (bbdb-current-record planning-on-modifying))
         (field (bbdb-current-field planning-on-modifying))
         (next-record record)
         (next-field field)
         (signum (if (< count 0) -1 1))
         (i 0))
    (if (< count 0) (setq count (- count)))
    (if field
        (while (and next-field (< i count))
          (while (bbdb-field-equal next-field field)
            (forward-line signum)
            (setq next-record (bbdb-current-record planning-on-modifying)
                  next-field (bbdb-current-field planning-on-modifying))
            (or (eq next-record record)
                (setq next-field nil)))
          (setq i (1+ i))
          (setq field next-field)))
    next-field))

;;;###autoload
(defun bbdb-transpose-fields (&optional arg)
  "This is like the `transpose-lines' command, but it is for BBDB fields.
If the cursor is on a field of a BBDB record, that field and the previous
field will be transposed.

With argument ARG, takes previous line and moves it past ARG fields.
With argument 0, interchanges field point is in with field mark is in.

Both fields must be in the same record, and must be of the same basic type
\(that is, you can use this command to change the order in which phone-number
fields are listed, but you can't use it to make an address appear before a
phone number; the order of field types is fixed.\)"
  (interactive "p")
  (let ((record (bbdb-current-record t))
        moving-field position-after position-before
        swap-p type list)
    (if (/= arg 0)
        (setq moving-field (or (bbdb-next-field -1 t)
                               (error "no previous field"))
              position-after (bbdb-next-field arg t)
              position-before (bbdb-next-field (if (< arg 0) -1 1) t))
      ;; if arg is 0, swap fields at point and mark
      (setq swap-p t)
      (setq position-after (bbdb-current-field))
      (save-excursion
        (goto-char (mark))
        (setq moving-field (bbdb-current-field))
        (or (eq record (bbdb-current-record)) (error "not in the same record"))))
    (if (< arg 0)
        (let ((x position-after))
          (setq position-after position-before
                position-before x)
          (forward-line 2)))
    (setq type (car moving-field))
    (or position-after position-before
        (error "that would be out of the record!"))
    (or (eq type (car position-after))
        (eq type (car position-before))
        (error "can't transpose fields of different types (%s and %s)"
               type (if (eq type (car position-after))
                        (car position-before) (car position-after))))
    (or (eq type (car position-after)) (setq position-after nil))
    (or (eq type (car position-before)) (setq position-before nil))
    (setq moving-field (nth 1 moving-field)
          position-after (nth 1 position-after)
          position-before (nth 1 position-before))
    (cond ((memq type '(name aka net))
           (error "there is only one %s field, so you can't transpose it"
                  type))
          ((memq type '(phone address property))
           (setq list (bbdb-record-get-field-internal record type)))
          (t (error "doubleplus ungood: unknown field %s" type)))
    (if swap-p
        (let ((rest list))
          (while rest
            (cond ((eq (car rest) moving-field) (setcar rest position-after))
                  ((eq (car rest) position-after) (setcar rest moving-field)))
            (setq rest (cdr rest))))
      (if (eq position-before (car list))
          (setq list (cons moving-field (delq moving-field list)))
        (let ((rest list))
          (while (and rest (not (eq position-after (car rest))))
            (setq rest (cdr rest)))
          (or rest (error "doubleplus ungood: couldn't reorder list"))
          (let ((inhibit-quit t))
            (setq list (delq moving-field list))
            (setcdr rest (cons moving-field (cdr rest)))))))
    (bbdb-record-store-field-internal record type list)
    (bbdb-change-record record nil)
    (bbdb-redisplay-one-record record)))


;;;###autoload
(defun bbdb-delete-current-field-or-record (&optional records noprompt)
  "Delete the line which the cursor is on; actually, delete the field which
that line represents from the database.  If the cursor is on the first line
of a database entry (the name/company line) then the entire entry will be
deleted."
  (interactive (list (if (bbdb-do-all-records-p)
                         (mapcar 'car bbdb-records)
                       (list (bbdb-current-record)))
                     current-prefix-arg))
  (let* ((field (bbdb-current-field t))
         (type (car field))
         record
         (name (cond ((null field) (error "on an unfield"))
                     ((eq type 'property) (symbol-name (car (nth 1 field))))
                     (t (symbol-name type)))))
    (while records
      (setq record (car records))
      (if (eq type 'name)
          (bbdb-delete-current-record record noprompt)
        (if (not (or noprompt
                     (bbdb-y-or-n-p (format "delete this %s field (of %s)? "
                                            name
                                            (bbdb-record-name record)))))
            nil
          (cond ((memq type '(phone address))
                 (bbdb-record-store-field-internal
                  record type
                  (delq (nth 1 field)
                        (bbdb-record-get-field-internal record type))))
                ((memq type '(net aka))
                 (let ((rest (bbdb-record-get-field-internal record type)))
                   (while rest
                     (bbdb-remhash (downcase (car rest)) record)
                     (setq rest (cdr rest))))
                 (bbdb-record-store-field-internal record type nil))
                ((eq type 'property)
                 (bbdb-record-putprop record (car (nth 1 field)) nil))
                (t (error "doubleplus ungood: unknown field type")))
          (bbdb-change-record record nil)
          (bbdb-redisplay-one-record record)))
      (setq records (cdr records)))))

;;;###autoload
(defun bbdb-delete-current-record (recs &optional noprompt)
  "Delete the entire bbdb database entry which the cursor is within.
Pressing \\<bbdb-mode-map>\\[bbdb-apply-next-command-to-all-records] will
delete all records listed in the BBDB buffer."
  (interactive (list (if (bbdb-do-all-records-p)
                         (mapcar 'car bbdb-records)
                       (list (bbdb-current-record t)))
                     current-prefix-arg))
  (if (not (listp recs))
      (setq recs (list recs)))
  (while recs
    (let ((r (car recs)))
      (setq recs (cdr recs))
      (bbdb-debug (if (bbdb-record-deleted-p r)
                      (error "deleting deleted record")))
      (if (or noprompt
              (bbdb-y-or-n-p (format "delete the entire db entry of %s? "
                                     (or (bbdb-record-name r)
                                         (bbdb-record-company r)
                                         (car (bbdb-record-net r))))))
          (let* ((record-cons (assq r bbdb-records))
                 (next-record-cons (car (cdr (memq record-cons
                                                   bbdb-records)))))
            (bbdb-debug (if (bbdb-record-deleted-p r)
                            (error "deleting deleted record")))
            (bbdb-record-set-deleted-p r t)
            (bbdb-delete-record-internal r)
            (if (eq record-cons (car bbdb-records))
                (setq bbdb-records (cdr bbdb-records))
              (let ((rest bbdb-records))
                (while (cdr rest)
                  (if (eq record-cons (car (cdr rest)))
                      (progn
                        (setcdr rest (cdr (cdr rest)))
                        (setq rest nil)))
                  (setq rest (cdr rest)))))
            (bbdb-redisplay-one-record r record-cons next-record-cons t)
            (bbdb-with-db-buffer
             (setq bbdb-changed-records (delq r bbdb-changed-records)))
            ;; (bbdb-offer-save)
            )))))

(defun bbdb-change-records-state-and-redisplay (desired-state records)
  (let (rec)
    (while records
      (setq rec (car records))
      (unless (eq desired-state (nth 1 rec))
        (setcar (cdr rec) desired-state)
        (bbdb-redisplay-one-record (car rec) rec))
      (setq records (cdr records)))))

;;;###autoload
(defun bbdb-toggle-all-records-display-layout (arg &optional records)
  "Show all the fields of all visible records.
Like `bbdb-toggle-records-display-layout' but for all visible records."
  (interactive "P")
  (if (null records)
      (setq records bbdb-records))
  (let* ((record (bbdb-current-record))
         (cons (assq record bbdb-records))
         (current-state (nth 1 cons))
         (layout-alist
          (or (delete nil (mapcar (lambda (l)
                                    (if (and (assoc 'toggle l)
                                             (cdr (assoc 'toggle l)))
                                        l))
                                  bbdb-display-layout-alist))
              bbdb-display-layout-alist))
         (desired-state (assoc current-state layout-alist)))
    (setq desired-state
          (cond ((eq arg 0)
                 'one-line)
                ((null current-state)
                 'multi-line)
                ((null (cdr (memq desired-state layout-alist)))
                 (caar layout-alist))
                (t
                 (caadr (memq desired-state layout-alist)))))
    (message "Using %S layout" desired-state)
    (bbdb-change-records-state-and-redisplay desired-state records)))

;;;###autoload
(defun bbdb-toggle-records-display-layout (arg)
  "Toggle whether the current record is displayed expanded or elided
\(multi-line or one-line display.\)  With a numeric argument of 0, the
current record will unconditionally be made elided; with any other argument,
the current record will unconditionally be shown expanded.
\\<bbdb-mode-map>
If \"\\[bbdb-apply-next-command-to-all-records]\\[bbdb-toggle-records-display-layout]\" is \
used instead of simply \"\\[bbdb-toggle-records-display-layout]\", then the state of all \
records will
be changed instead of just the one at point.  In this case, an argument
of 0 means that all records will unconditionally be made elided; any other
numeric argument means that all of the records will unconditionally be shown
expanded; and no numeric argument means that the records are made to be in
the opposite state of the record under point."
  (interactive "P")
  (bbdb-toggle-all-records-display-layout
   arg
   (if (not (bbdb-do-all-records-p))
       (list (assq (bbdb-current-record) bbdb-records)))))

;;;###autoload
(defun bbdb-display-all-records-completely
  (arg &optional records)
  "Show all the fields of all currently displayed records.
The display layout `full-multi-line' is used for this."
  (interactive "P")
  (if (null records)
      (setq records bbdb-records))
  (let* ((record (bbdb-current-record))
         (cons (assq record bbdb-records))
         (current-state (nth 1 cons))
         (desired-state
          (cond ((not (eq current-state 'full-multi-line))
                 'full-multi-line)
                (t
                 'multi-line))))
    (bbdb-change-records-state-and-redisplay desired-state records)))

;;;###autoload
(defun bbdb-display-record-completely (arg)
  "Show all the fields of the current record.
The display layout `full-multi-line' is used for this."
  (interactive "P")
  (bbdb-display-all-records-completely
   arg
   (if (not (bbdb-do-all-records-p))
       (list (assq (bbdb-current-record) bbdb-records)))))

;;;###autoload
(defun bbdb-display-record-with-layout (layout &optional records)
  "Show all the fields of the current record using LAYOUT."
  (interactive (list (completing-read "Layout: "
                      (mapcar (lambda (i)
                        (list (symbol-name (car i))))
                          bbdb-display-layout-alist))))
  (when (stringp layout)
    (setq layout (intern layout)))
  (when (null records)
    (setq records bbdb-records))
  (bbdb-change-records-state-and-redisplay layout records))

;;;###autoload
(defun bbdb-omit-record (n)
  "Remove the current record from the display without deleting it from the
database.  With a prefix argument, omit the next N records.  If negative,
omit backwards."
  (interactive "p")
  (while (not (= n 0))
    (if (< n 0) (bbdb-prev-record 1))
    (let* ((record (or (bbdb-current-record) (error "no records")))
           (rest bbdb-records)
           cons next prev-tail)
      (while rest
        (if (eq (car (car rest)) record)
            (setq cons (car rest)
                  next (car (cdr rest))
                  rest nil)
          (setq prev-tail rest
                rest (cdr rest))))
      (or record (error "can't find current record"))
      (let ((buffer-read-only nil))
        (delete-region (nth 2 cons) (if next (nth 2 next) (point-max))))
      (if prev-tail
          (setcdr prev-tail (cdr (cdr prev-tail)))
        (setq bbdb-records (cdr bbdb-records)))
      (setq n (if (> n 0) (1- n) (1+ n)))))
  (bbdb-frob-mode-line (length bbdb-records)))

;;; Fixing up bogus entries

(defcustom bbdb-refile-notes-generate-alist '((creation-date . bbdb-refile-notes-string-least) (timestamp . bbdb-refile-notes-string-most))
  "*An alist defining specific merging function, based on notes field."
  :group 'bbdb-noticing-records
  :type '(repeat (cons
                  (symbol :tag "Notes filed")
                  (hook :tag "Generating function"))))

(defcustom bbdb-refile-notes-default-merge-function 'bbdb-refile-notes-default-merge-function
  "*Default function to use for merging BBDB notes records.

If the note field has an entry in `bbdb-refile-notes-generate-alist',
that function will be used instead."
  :group 'bbdb-noticing-records
  :type 'function)


(defun bbdb-refile-notes-default-merge-function (string1 string2)
  "Returns the concatenation of STRING1 and STRING2"
  (concat string1 "\n" string2))

(defun bbdb-refile-notes-remove-duplicates (string1 string2)
  "Concatenate STRING1 and STRING2, but remove duplicate lines."
  (let ((note1 (split-string string1 "\n"))
        (note2 (split-string string2 "\n")))
    (while note2
      (if (not (member (car note2) note1))
          (setq note1 (cons (car note2) note1)))
      (setq note2 (cdr note2)))
    (mapconcat 'identity note1 "\n")))

(defun bbdb-refile-notes-string-least (string1 string2)
  "Returns the string that is lessp."
  (if (string-lessp string1 string2)
      string1
    string2))

(defun bbdb-refile-notes-string-most (string1 string2)
  "Returns the string that is not lessp."
  (if (string-lessp string1 string2)
      string2
    string1))

(defun bbdb-merge-lists! (l1 l2 cmp &optional mod)
  "Merge two lists l1 l2 (modifies l1) only adds elements from l2
if cmp returns false for all elements of l1.  If optional mod
is provided it is applied to each element of l1 and l2 prior to cmp"
  (if (null l1)
      l2
    (let ((end (last l1))
          (src2 l2)
          (chk (if mod (mapcar mod l1) (append l1 '()))))
      (while src2
        (let ((fail '())
              (src1 chk)
              (val  (if mod (apply mod (car src2) '()) (car src2))))
          (while src1
            (if (apply cmp (car src1) val '())
                (setq src1 '()
                      fail 't)
              (setq src1 (cdr src1))))
          (if fail '()
            (setcdr end (cons (car src2) '()))
            (setq end (cdr end)))
          (setq src2 (cdr src2))))
      l1)))

(defun bbdb-merge-records (old-record new-record)
  "Merge the contents of old-record into new-record, old-record
remains unchanged.  For name and company it queries about which to use
if they differ.  All other fields are concatenated.  Idealy this would
be better about checking for duplicate entries in other fields, as
well as possibly querying about differing values.

This function does nothing to ensure the integrity of the rest of the
database, that is somebody elses problem (something like
`bbdb-refile-record')."
  (if (or (null new-record) (eq old-record new-record))
      (error "those are the same"))
  (let ((new-name (bbdb-record-name    new-record))
        (new-co   (bbdb-record-company new-record))
        (old-name (bbdb-record-name    old-record))
        (old-co   (bbdb-record-company old-record))
        (old-nets (bbdb-record-net     old-record))
        (old-aka  (bbdb-record-aka     old-record))
        extra-name)
    (let ((name
           (cond ((= 0 (length old-name))
                  (cons (bbdb-record-firstname new-record)
                        (bbdb-record-lastname new-record)))
                 ((= 0 (length new-name))
                  (cons (bbdb-record-firstname old-record)
                        (bbdb-record-lastname old-record)))
                 ((string-equal (downcase old-name) (downcase new-name))
                  (cons (bbdb-record-firstname new-record)
                        (bbdb-record-lastname new-record)))
                 (t (prog1
                        (if (bbdb-y-or-n-p
                             (format "Use name \"%s\" instead of \"%s\"? "
                                     old-name  new-name))
                            (progn
                              (setq extra-name new-record)
                              (cons (bbdb-record-firstname old-record)
                                    (bbdb-record-lastname old-record)))
                          (setq extra-name old-record)
                          (cons (bbdb-record-firstname new-record)
                                (bbdb-record-lastname new-record)))
                      (or (and bbdb-use-alternate-names
                               (bbdb-y-or-n-p
                                (format "Keep \"%s\" as an alternate name? "
                                        (bbdb-record-name extra-name))))
                          (setq extra-name nil))))))
          (comp (cond ((= 0 (length old-co)) new-co)
                      ((= 0 (length new-co)) old-co)
                      ((string-equal old-co new-co) new-co)
                      (t (if (bbdb-y-or-n-p
                              (format "Use company \"%s\" instead of \"%s\"? "
                                      old-co new-co))
                             old-co new-co)))))

      (if extra-name
          (setq old-aka (cons (bbdb-record-name extra-name) old-aka)))

      (bbdb-record-set-phones new-record
                              (bbdb-merge-lists!
                               (bbdb-record-phones new-record)
                               (bbdb-record-phones old-record)
                               'equal))
      (bbdb-record-set-addresses new-record
                                 (bbdb-merge-lists!
                                  (bbdb-record-addresses new-record)
                                  (bbdb-record-addresses old-record)
                                  'equal))
      (bbdb-record-set-company new-record comp)

      (let ((n1 (bbdb-record-raw-notes new-record))
            (n2 (bbdb-record-raw-notes old-record))
            tmp)
        (or (equal n1 n2)
            (progn
              (or (listp n1) (setq n1 (list (cons 'notes n1))))
              (or (listp n2) (setq n2 (list (cons 'notes n2))))
              (while n2
                (if (setq tmp (assq (car (car n2)) n1))
                    (setcdr tmp
                            (funcall
                             (or (cdr (assq (car (car n2))
                                            bbdb-refile-notes-generate-alist))
                                 bbdb-refile-notes-default-merge-function)
                             (cdr tmp) (cdr (car n2))))
                  (setq n1 (nconc n1 (list (car n2)))))
                (setq n2 (cdr n2)))
              (bbdb-record-set-raw-notes new-record n1))))

      (bbdb-record-set-firstname new-record (car name))
      (bbdb-record-set-lastname  new-record (cdr name))
      (bbdb-record-set-namecache new-record nil)

      (bbdb-record-set-net new-record
                           (bbdb-merge-lists!
                            (bbdb-record-net new-record) old-nets
                            'string= 'downcase))
      (bbdb-record-set-aka new-record
                           (bbdb-merge-lists!
                            (bbdb-record-aka new-record) old-aka
                            'string= 'downcase))
      new-record)))

;;;###autoload
(defun bbdb-refile-record (old-record new-record)
  "Merge the current record into some other record; that is, delete the
record under point after copying all of the data within it into some other
record.  this is useful if you realize that somehow a redundant record has
gotten into the database, and you want to merge it with another.

If both records have names and/or companies, you are asked which to use.
Phone numbers, addresses, and network addresses are simply concatenated.
The first record is the record under the point; the second is prompted for.
Completion behaviour is as dictated by the variable `bbdb-completion-type'."
  (interactive
   (let ((r (bbdb-current-record))
         name)
     (setq name (bbdb-record-name r))
     (list r
           (if current-prefix-arg
               (car (delq r (bbdb-search (bbdb-records) name nil)))
             (bbdb-completing-read-one-record
              (format "merge record \"%s\" into: "
                      (or (bbdb-record-name r) (car (bbdb-record-net r))
                          "???")) (list r))))))

  (if (or (null new-record) (eq old-record new-record))
      (error "those are the same"))
  (setq new-record (bbdb-merge-records old-record new-record))

  (bbdb-delete-current-record old-record 'noprompt)
  (bbdb-change-record new-record t)     ; don't always need-to-sort...
  (let ((bbdb-display-layout nil))
    (if (assq new-record bbdb-records)
        (bbdb-redisplay-one-record new-record))
    (bbdb-with-db-buffer
     (if (not (memq new-record bbdb-changed-records))
         (setq bbdb-changed-records
               (cons new-record bbdb-changed-records))))
    (if (null bbdb-records)             ; nothing displayed, display something.
        (bbdb-display-records (list new-record))))
  (message "records merged."))

;; sort the notes
(defcustom bbdb-notes-sort-order
  '((notes . 0) (www . 1) (ftp . 2) (gopher . 3) (telnet . 4) (mail-alias . 5)
    (mail-folder . 6) (lpr . 7) (creation-date . 1000) (timestamp . 1001))
  "*The order for sorting the notes.
If a note is not in the alist, it is assigned weight 100, so all notes
with weights less then 100 will be in the beginning, and all notes with
weights more than 100 will be in the end."
  :group 'bbdb-noticing-records
  :type 'list)

;;;###autoload
(defun bbdb-sort-notes (rec)
  "Sort the notes in the record according to `bbdb-notes-sort-order'.
Can be used in `bbdb-change-hook'."
  (flet ((kk (nt) (or (cdr (assq (car nt) bbdb-notes-sort-order)) 100)))
    (bbdb-record-set-raw-notes
     rec (sort (bbdb-record-raw-notes rec)
               (lambda (aa bb) (< (kk aa) (kk bb)))))))

;;;###autoload
(defun bbdb-sort-phones (rec)
  "Sort the phones in the record according to the location.
Can be used in `bbdb-change-hook'."
  (bbdb-record-set-phones
   rec (sort (bbdb-record-phones rec)
             (lambda (xx yy) (string< (aref xx 0) (aref yy 0))))))

;;;###autoload
(defun bbdb-sort-addresses (rec)
  "Sort the addresses in the record according to the location.
Can be used in `bbdb-change-hook'."
  (bbdb-record-set-addresses
   rec (sort (bbdb-record-addresses rec)
             (lambda (xx yy) (string< (aref xx 0) (aref yy 0))))))


;;; Send-Mail interface

(defcustom bbdb-dwim-net-address-allow-redundancy nil
  "*Non-nil means always use full name when sending mail, even if same as net."
  :group 'bbdb
  :type '(choice (const :tag "Disallow redundancy" nil)
                 (const :tag "Return only the net" 'netonly)
                 (const :tag "Allow redundancy" t)))

;;;###autoload
(defun bbdb-dwim-net-address (record &optional net)
  "Returns a string to use as the email address of the given record.  The
given address is the address the mail is destined to; this is formatted like
\"Firstname Lastname <addr>\" unless both the first name and last name are
constituents of the address, as in John.Doe@SomeHost, or the address is
already in the form \"Name <foo>\" or \"foo (Name)\", in which case the
address is used as-is. If `bbdb-dwim-net-address-allow-redundancy' is non-nil,
the name is always included.  If `bbdb-dwim-net-address-allow-redundancy' is
'netonly the name is never included!"
  (or net (setq net (car (bbdb-record-net record))))
  (or net (error "record unhas network addresses"))
  (let* ((override (bbdb-record-getprop record 'mail-name))
         (name (or override (bbdb-record-name record)))
         fn ln (i 0))
    (if override
        (let ((both (bbdb-divide-name override)))
          (setq fn (car both)
                ln (car (cdr both)))
          (if (equal fn "") (setq fn nil))
          (if (equal ln "") (setq ln nil)))
      (setq fn (bbdb-record-firstname record)
            ln (bbdb-record-lastname record)))
    ;; if the name contains backslashes or double-quotes, backslash them.
    (if name
        (while (setq i (string-match "[\\\"]" name i))
          (setq name (concat (substring name 0 i) "\\" (substring name i))
                i (+ i 2))))
    (cond ((eq 'netonly bbdb-dwim-net-address-allow-redundancy)
           net)
          ((or (null name)
               (if (not bbdb-dwim-net-address-allow-redundancy)
                   (cond ((and fn ln)
                          (or (string-match
                               (concat "\\`[^!@%]*\\b" (regexp-quote fn)
                                       "\\b[^!%@]+\\b" (regexp-quote ln) "\\b")
                               net)
                              (string-match
                               (concat "\\`[^!@%]*\\b" (regexp-quote ln)
                                       "\\b[^!%@]+\\b" (regexp-quote fn) "\\b")
                               net)))
                         ((or fn ln)
                          (string-match
                           (concat "\\`[^!@%]*\\b" (regexp-quote (or fn ln)) "\\b")
                           net))))
               ;; already in "foo <bar>" or "bar <foo>" format.
               (string-match "\\`[ \t]*[^<]+[ \t]*<" net)
               (string-match "\\`[ \t]*[^(]+[ \t]*(" net))
           net)
          ;; if the name contains control chars or RFC822 specials, it needs
          ;; to be enclosed in quotes.  Double-quotes and backslashes have
          ;; already been escaped.  This quotes a few extra characters as
          ;; well (!,%, and $) just for common sense.
          ((string-match "[][\000-\037\177()<>@,;:.!$%]" name)
           (format "\"%s\" <%s>" name net))
          (t
           (format "%s <%s>" name net)))))


(defun bbdb-send-mail-internal (&optional to subj records)
  (let ((type (or bbdb-send-mail-style
          ;; In Emacs, `compose-mail' gets whatever you've
          ;; customized as your preferred `mail-user-agent'.
                  (cond ((fboundp 'compose-mail) 'compose-mail)
                        ((featurep 'mh-e) 'mh)
                        ((featurep 'vm) 'vm)
                        ((featurep 'message) 'message)
                        ((featurep 'mew) 'mew)
                        ((featurep 'gnus) 'gnus)
                        (t 'mail)))))
    (cond
     ((eq type 'mh)
      (or (fboundp 'mh-send) (autoload 'mh-send "mh-e"))
      (mh-send to "" (or subj "")))
     ((eq type 'vm)
      (cond ((not (fboundp 'vm-mail-internal))
             (load-library "vm")        ; 5.32 or later
             (or (fboundp 'vm-mail-internal)
                 (load-library "vm-reply")))) ; 5.31 or earlier
      (vm-session-initialization)
      (if (not subj)
          (vm-mail to)
        (vm-mail-internal nil to subj)
        (run-hooks 'vm-mail-hook)
        (run-hooks 'vm-mail-mode-hook)))
     ((eq type 'message)
      (or (fboundp 'message-mail) (autoload 'message-mail "message"))
      (message-mail to subj))
     ((or (eq type 'mail) (eq type 'rmail))
      (mail nil to subj))
     ((eq type 'mew)
      (or (fboundp 'mew-send) (load-library "mew"))
      (mew-send to nil subj))
     ((eq type 'compose-mail)
      (compose-mail to subj))
     ((eq type 'gnus)
      (gnus-msg-mail to subj))
     (t
      (error "bbdb-send-mail-style must be vm, mh, message, compose-mail, or rmail")))))

;;;###autoload
(defun bbdb-send-mail (bbdb-record &optional subject)
  "Compose a mail message to the person indicated by the current bbdb record.
The first (most-recently-added) address is used if there are more than one.
\\<bbdb-mode-map>
If \"\\[bbdb-apply-next-command-to-all-records]\\[bbdb-send-mail]\" is \
used instead of simply \"\\[bbdb-send-mail]\", then mail will be sent to \
all of the
folks listed in the *BBDB* buffer instead of just the person at point."
  (interactive (list (if (bbdb-do-all-records-p)
                         (mapcar 'car bbdb-records)
                       (bbdb-current-record))))
  (if (consp bbdb-record)
      (bbdb-send-mail-many bbdb-record subject)
    (bbdb-send-mail-1 bbdb-record subject)))


(defun bbdb-send-mail-1 (bbdb-record &optional subject)
  (if bbdb-inside-electric-display
      (bbdb-electric-throw-to-execute
       (list 'bbdb-send-mail bbdb-record subject)))
  ;; else...

  (cond ((null bbdb-record) (error "record unexists"))
        ((null (bbdb-record-net bbdb-record))
         (error "Current record unhas a network addresses."))
        (t (bbdb-send-mail-internal (bbdb-dwim-net-address bbdb-record)
                                    subject (list bbdb-record))
           (if (re-search-backward "^Subject: $" nil t) (end-of-line)))))


(defun bbdb-send-mail-many (records &optional subject)
  (if bbdb-inside-electric-display
      (bbdb-electric-throw-to-execute
       (list 'bbdb-send-mail (list 'quote records) subject)))
  ;; else...

  (let ((good '()) (bad '())
        (orec records))
    (while records
      (if (bbdb-record-net (car records))
          (setq good (cons (car records) good))
        (setq bad (cons (car records) bad)))
      (setq records (cdr records)))
    (bbdb-send-mail-internal
     (mapconcat (lambda (x) (bbdb-dwim-net-address x))
                (nreverse good) ",\n    ")
     subject orec)
    (if (not bad) nil
      (goto-char (point-max))
      (let ((p (point))
            (fill-prefix "    ")
            (fill-column 70))
        (insert "*** Warning: No net addresses for "
                (mapconcat (lambda (x) (bbdb-record-name x))
                           (nreverse bad) ", ") ".")
        (fill-region-as-paragraph p (point))
        (goto-char p))))
  (if (re-search-backward "^Subject: $" nil t) (end-of-line)))


(defun bbdb-yank-addresses ()
  "CC the people displayed in the *BBDB* buffer on this message.
The primary net-address of each of the records currently listed in the
*BBDB* buffer (whether it is visible or not) will be appended to the
CC: field of the current buffer (assuming the current buffer is a mail
composition buffer.)"
  (interactive)
  (let ((addrs (save-excursion
                 (set-buffer bbdb-buffer-name)
                 (delq nil
                       (mapcar (lambda (x)
                                 (if (bbdb-record-net (car x))
                                     (bbdb-dwim-net-address (car x))
                                   nil))
                               bbdb-records)))))
    (goto-char (point-min))
    ;; If there's a CC field, move to the end of it, inserting a comma if
    ;;  there are already addresses present.
    ;; Otherwise, if there's an empty To: field, move to the end of it.
    ;; Otherwise, insert an empty CC: field.
    (if (re-search-forward "^CC:[ \t]*" nil t)
        (if (eolp)
            nil
          (end-of-line)
          (while (looking-at "\n[ \t]")
            (forward-char) (end-of-line))
          (insert ",\n")
          (indent-relative))
      (re-search-forward "^To:[ \t]*")
      (if (eolp)
          nil
        (end-of-line)
        (while (looking-at "\n[ \t]")
          (forward-char) (end-of-line))
        (insert ",\n")
        (indent-relative))
      (if (eolp)
          nil
        (end-of-line)
        (while (looking-at "\n[ \t]")
          (forward-char) (end-of-line))
        (insert "\nCC:")
        (indent-relative)))
    ;; Now insert each of the addresses on its own line.
    (while addrs
      (insert (car addrs))
      (if (cdr addrs) (progn (insert ",\n") (indent-relative)))
      (setq addrs (cdr addrs)))))

;;;###autoload
(defun bbdb-show-all-recipients ()
  "*Display BBDB records for all recipients of the message in this buffer."
  (interactive)
  (let ((marker (bbdb-header-start))
        (fields '("from" "sender" "to" "cc" "bcc"
                  "resent-from" "resent-to" "resent-cc" "resent-bcc"))
        addrs)
    (message "Searching...")
    (save-excursion
      (set-buffer (marker-buffer marker))
      (while fields
        (goto-char marker)
        (setq addrs (append (bbdb-split (or (bbdb-extract-field-value
                                             (car fields))
                                            "")
                                        ",")
                            addrs)
              fields (cdr fields))))
    (let ((rest addrs)
          (records '())
          record)
      (while rest
        (setq record (bbdb-annotate-message-sender (car rest) t t t))
        (if record (setq records (cons record records)))
        (setq rest (cdr rest)))
      (message "Sorting...")
      (setq records (sort records (lambda (x y) (bbdb-record-lessp x y))))
      (bbdb-display-records records))))


;;; completion

;;;###autoload
(defun bbdb-completion-check-record (sym rec)
  (let ((name (or (bbdb-record-name rec)
                  (bbdb-record-company rec)
                  ""))
        (nets (bbdb-record-net rec))
        ok)

    (if (null bbdb-completion-type)
        (setq ok 't)

      (if (memq bbdb-completion-type
                '(name primary-or-name name-or-primary))
          (setq ok (string= sym (downcase name))))

      ;; #### handle AKA, mail-name or mail-alias here?
      (if ok '()
        (when (eq bbdb-completion-type 'net)
          (while (and nets (not ok))
            (setq ok (string= sym (downcase (car nets)))
                  nets (cdr nets))))
        (when (and nets (memq bbdb-completion-type
                              '(primary primary-or-name name-or-primary)))
          (setq ok (string= sym (downcase (car nets)))))))
    ok))


;;;###autoload
(defun bbdb-completion-predicate (symbol)
  "For use as the third argument to `completing-read'.
Obey the semantics of `bbdb-completion-type'."
  (cond ((null bbdb-completion-type)
         t)
        ((not (boundp symbol))
         nil)
        (t
         (let ((sym  (symbol-name symbol))
               (recs (symbol-value symbol))
               ok)
           (while (and recs (not ok))
             (setq ok   (bbdb-completion-check-record sym (car recs))
                   recs (cdr recs)))
           ok))))

(defun bbdb-completing-read-record (prompt &optional omit-records)
  "Prompt for and return a record from the bbdb.
Completion is done according to `bbdb-completion-type'.  If the user
just hits return, nil is returned.  Otherwise, a valid response is forced."
  (let* ((ht (bbdb-hashtable))
         (completion-ignore-case 't)
         (string (completing-read prompt ht 'bbdb-completion-predicate t))
         (symbol (and (not (= 0 (length string)))
                      (intern-soft string ht))))
    (if symbol
        (if (and (boundp symbol) (symbol-value symbol))
            (let ((recs (symbol-value symbol)) ret)
              (while recs
                (if (and (not (memq (car recs) omit-records))
                         (bbdb-completion-check-record (symbol-name symbol)
                                                       (car recs)))
                    (setq ret (cons (car recs) ret)))
                (setq recs (cdr recs)))
              ret)
          (error "selecting deleted (unhashed) record \"%s\"!" symbol))
      nil)))

(defun bbdb-completing-read-one-record (prompt &optional omit-records)
  "Prompt for and return a single record from the bbdb;
completion is done according to `bbdb-completion-type'.  If the user
just hits return, nil is returned. Otherwise, a valid response is forced.
if omit-records is non-nil it should be a list of records to dis-allow
completion with."
  (let ((records (bbdb-remove-memq-duplicates
                  (bbdb-completing-read-record prompt omit-records))))
    (cond
     ((eq (length records) 1)
      (car records))
     ((> (length records) 1)
      (let ((count (length records))
            prompts result)
        (bbdb-display-records records)
        (while (> count 0)
          (setq prompts (cons (list (number-to-string count) count) prompts)
                count (1- count)))
        (setq result
              (completing-read (format "Which duplicate record (1-%s): "
                                       (length records))
                               prompts nil t "1"))
        (nth (1- (string-to-number result)) records)))
     (t
      nil))))

(defvar bbdb-read-addresses-with-completion-map
  (let ((map (copy-keymap minibuffer-local-completion-map)))
    (define-key map " " 'self-insert-command)
    (define-key map "\t" 'bbdb-complete-name)
    (define-key map "\M-\t" 'bbdb-complete-name)
    map))

;;;###autoload
(defun bbdb-read-addresses-with-completion (prompt &optional default)
  "Like `read-string', but allows `bbdb-complete-name' style completion."
  (read-from-minibuffer prompt default
                        bbdb-read-addresses-with-completion-map))


;; Internal use. Store the window configuration before we pop up the
;; completion buffer.
(defvar bbdb-complete-name-saved-window-config nil)

;; Restore the saved window configuration
(defun bbdb-complete-name-cleanup ()
  (if bbdb-complete-name-saved-window-config
      (progn
        (if (get-buffer-window "*Completions*")
            (progn
              (set-window-configuration
               bbdb-complete-name-saved-window-config)
              (bury-buffer "*Completions*"))
          )
        (setq bbdb-complete-name-saved-window-config nil))))

(defvar bbdb-complete-name-callback-data nil
  "Stores the buffer and region start and end of the completed string.
This is set in the *Completions* buffer.
It is set in `bbdb-display-completion-list' and used in the advice
`choose-completion-string'.")

(make-variable-buffer-local 'bbdb-complete-name-callback-data)

(defun bbdb-display-completion-list (list &optional callback data)
  "Wrapper for `display-completion-list'.
GNU Emacs requires DATA to be in a specific format, viz. (nth 1 data) should
be a marker for the start of the region being completed."
  ;; disgusting hack to make GNU Emacs nuke the bit you've typed
  ;; when it inserts the completion.
  (setq bbdb-complete-name-callback-data data)
  (if (featurep 'xemacs)
      (display-completion-list list :activate-callback callback
                               :user-data data)
    (display-completion-list list)))

(defadvice choose-completion-string (before bbdb-complete-fix activate)
  "Deletes the completed string before replacing.
We need to do this as we are abusing completion and it was not meant to work
in buffer other than the mini buffer."
  (when bbdb-complete-name-callback-data
    (save-excursion
      (set-buffer (car bbdb-complete-name-callback-data))
      (apply 'delete-region (cdr  bbdb-complete-name-callback-data)))))

(defun bbdb-complete-clicked-name (event extent user-data)
  "Find the record for a name clicked in a completion buffer.
Currently only used by XEmacs."
  (let ((buffer (nth 0 user-data))
        (bbdb-complete-name-allow-cycling nil)
        (beg (nth 1 user-data))
        (end (nth 2 user-data)))
    (bbdb-complete-name-cleanup)
    (set-buffer buffer)
    (goto-char beg)
    (delete-region beg end)
    (insert (bbdb-extent-string extent))
    (bbdb-complete-name beg)))


(defun bbdb-list-overlap (l1 l2)
  (let (ok)
    (while (and (not ok) l1)
      (if (memq (car l1) l2) (setq ok t l1 '())
        (setq l1 (cdr l1))))
    ok))

(defun bbdb-remove-assoc-duplicates (l)
  (if (null l) '()
    (if (assoc (car (car l)) (cdr l))
        (bbdb-remove-assoc-duplicates (cdr l))
      (cons (car l) (bbdb-remove-assoc-duplicates (cdr l))))))

(defcustom bbdb-complete-name-allow-cycling nil
  "Whether to allow cycling of email addresses when calling
`bbdb-complete-name' on a completed address in a composition buffer."
  :group 'bbdb-mua-specific
  :type 'boolean)

(defcustom bbdb-complete-name-hooks nil
  "List of functions called after a sucessful completion."
  :group 'bbdb-mua-specific
  :type 'boolean)

(eval-when-compile (defvar auto-fill-hook))

;;;###autoload
(defun bbdb-complete-name (&optional start-pos)
  "Complete the user full-name or net-address before point (up to the
preceeding newline, colon, or comma, or the value of START-POS).  If
what has been typed is unique, insert an entry of the form \"User Name
<net-addr>\" (although see documentation for
bbdb-dwim-net-address-allow-redundancy).  If it is a valid completion
but not unique, a list of completions is displayed.

If the completion is done and `bbdb-complete-name-allow-cycling' is
true then cycle through the nets for the matching record.

When called with a prefix arg then display a list of all nets.

Completion behaviour can be controlled with `bbdb-completion-type'."
  (interactive)

  (let* ((end (point))
         (beg (or start-pos
                  (save-excursion
                    (re-search-backward "\\(\\`\\|[\n:,]\\)[ \t]*")
                    (goto-char (match-end 0))
                    (point))))
         (orig (buffer-substring beg end))
         (typed (downcase orig))
         (pattern (bbdb-string-trim typed))
         (ht (bbdb-hashtable))
         ;; make a list of possible completion strings
         ;; (all-the-completions), and a flag to indicate if there's a
         ;; single matching record or not (only-one-p)
         (only-one-p t)
         (all-the-completions nil)
         (pred
          (lambda (sym)
            (when (bbdb-completion-predicate sym)
              (if (and only-one-p
                       all-the-completions
                       (or
                        ;; not sure about this. more than one record
                        ;; attached to the symbol? does that happen?
                        (> (length (symbol-value sym)) 1)
                        ;; this is the doozy, though. multiple syms
                        ;; which all match the same record
                        (delete t (mapcar (lambda(x)
                                            (equal (symbol-value x)
                                                   (symbol-value sym)))
                                          all-the-completions))))
                  (setq only-one-p nil))
              (if (not (memq sym all-the-completions))
                  (setq all-the-completions (cons sym all-the-completions))))))
         (completion (progn (all-completions pattern ht pred) (try-completion pattern ht)))
         (exact-match (eq completion t)))

    (cond
     ;; No matches found OR you're trying completion on an
     ;; already-completed record. In the latter case, we might have to
     ;; cycle through the nets for that record.
     ((or (null completion)
          (and bbdb-complete-name-allow-cycling
               exact-match ;; which is a net of the record
               (member orig
                       (bbdb-record-net
                        (car (symbol-value (intern-soft pattern ht)))))))
      ;; Clean up the completion buffer, if it exists
      (bbdb-complete-name-cleanup)
      ;; Check for cycling
      (or (catch 'bbdb-cycling-exit
            ;; jump straight out if we're not cycling
            (or bbdb-complete-name-allow-cycling
                (throw 'bbdb-cycling-exit nil))

            ;; find the record we're working on.
            (let* ((addr (funcall bbdb-extract-address-components-func orig))
                   (rec
                    (if (listp addr)
                        ;; for now, we're ignoring the case where this
                        ;; returns more than one record. Ideally, the
                        ;; last expansion would be stored in a
                        ;; buffer-local variable, perhaps.
                        (car (bbdb-search-intertwingle (caar addr)
                                                       (cadar addr)))
                      nil)))
              (or rec
                  (throw 'bbdb-cycling-exit nil))

              (if current-prefix-arg
                  ;; use completion buffer
                  (let ((standard-output (get-buffer-create "*Completions*")))
                    ;; a previously existing buffer has to be cleaned first
                    (save-excursion (set-buffer standard-output)
                                    (setq buffer-read-only nil)
                                    (erase-buffer))
                    (display-completion-list
                     (mapcar (lambda (n) (bbdb-dwim-net-address rec n))
                             (bbdb-record-net rec)))
                    (delete-region beg end)
                    (switch-to-buffer standard-output))
                ;; use next address
                (let* ((addrs (bbdb-record-net rec))
                       (this-addr (or (cadr (member (car (cdar addr)) addrs))
                                      (nth 0 addrs))))
                  (if (= (length addrs) 1)
                      ;; no alternatives. don't signal an error.
                      (throw 'bbdb-cycling-exit t)
                    ;; replace with new mail address
                    (delete-region beg end)
                    (insert (bbdb-dwim-net-address rec this-addr))
                    (run-hooks 'bbdb-complete-name-hooks)
                    (throw 'bbdb-cycling-exit t))))))

          ;; FALL THROUGH
          ;; Check mail aliases
          (if (and bbdb-expand-mail-aliases (expand-abbrev))
              ()
            (when bbdb-complete-name-hooks
              (message "completion for \"%s\" unfound." pattern)
              (ding)))));; no matches, sorry!

     ;; Match for a single record. If cycling is enabled then we don't
     ;; care too much about the exact-match part.
     ((and only-one-p (or exact-match bbdb-complete-name-allow-cycling))
      (let* ((sym (if exact-match (intern-soft pattern ht) (car all-the-completions)))
             (recs (symbol-value sym))
             the-net match-recs lst primary matched)

        (while recs
          (when (bbdb-record-net (car recs))

            ;; Did we match on name?
            (let ((b-r-name (or (bbdb-record-name (car recs)) "")))
              (if (string= pattern
                           (substring (downcase b-r-name) 0
                                      (min (length b-r-name)
                                           (length pattern))))
                  (setq match-recs (cons (car recs) match-recs)
                        matched t)))

            ;; Did we match on aka?
            (when (not matched)
              (setq lst (bbdb-record-aka (car recs)))
              (while lst
                (if (string= pattern (substring (downcase (car lst)) 0
                                                (min (length (downcase
                                                              (car
                                                               lst)))
                                                     (length pattern))))
                    (setq match-recs (append match-recs (list (car recs)))
                          matched t
                          lst '())
                  (setq lst (cdr lst)))))

            ;; Name didn't match name so check net matching
            (when (not matched)
              (setq lst (bbdb-record-net (car recs)))
              (setq primary t) ;; primary wins over secondary...
              (while lst
                (if (string= pattern (substring (downcase (car lst))
                                                0 (min (length
                                                        (downcase (car
                                                                   lst)))
                                                       (length pattern))))
                    (setq the-net (car lst)
                          lst     nil
                          match-recs
                          (if primary (cons (car recs) match-recs)
                            (append match-recs (list (car recs))))))
                (setq lst     (cdr lst)
                      primary nil))))

          ;; loop to next rec
          (setq recs    (cdr recs)
                matched nil))

        (unless match-recs
          (error "only exact matching record unhas net field"))

        ;; now replace the text with the expansion
        (delete-region beg end)
        (insert (bbdb-dwim-net-address (car match-recs) the-net))

        ;; if we're past fill-column, wrap at the previous comma.
        (if (and
             (bbdb-auto-fill-function)
             (>= (current-column) fill-column))
            (let ((p (point))
                  bol)
              (save-excursion
                (beginning-of-line)
                (setq bol (point))
                (goto-char p)
                (if (search-backward "," bol t)
                    (progn
                      (forward-char 1)
                      (insert "\n   "))))))

        ;; Update the *BBDB* buffer if desired.
        (if bbdb-completion-display-record
            (let ((bbdb-gag-messages t))
              (bbdb-pop-up-bbdb-buffer)
              (bbdb-display-records-1 match-recs t)))
        (bbdb-complete-name-cleanup)

        ;; call the exact-completion hook
        (run-hooks 'bbdb-complete-name-hooks)))

     ;; Partial match
     ;; note, we can't use the trimmed version of the pattern here or
     ;; we'll recurse infinitely on e.g. common first names
     ((and (stringp completion) (not (string= typed completion)))
      (delete-region beg end)
      (insert completion)
      (setq end (point))
      (let ((last "")
            (bbdb-complete-name-allow-cycling nil))
        (while (and (stringp completion)
                    (not (string= completion last))
                    (setq last completion
                          pattern (downcase orig)
                          completion (progn (all-completions pattern ht pred) (try-completion pattern ht))))
          (if (stringp completion)
              (progn (delete-region beg end)
                     (insert completion))))
        (bbdb-complete-name beg)))

     ;; Exact match, but more than one record
     (t
      (or (eq (selected-window) (minibuffer-window))
          (message "Making completion list..."))

      (let (dwim-completions
            uniq nets net name akas)
        ;; Now collect all the dwim-addresses for each completion, but only
        ;; once for each record!  Add it if the net is part of the completions
        (bbdb-mapc
         (lambda (sym)
           (bbdb-mapc
            (lambda (rec)
              (when (not (member rec uniq))
                (setq uniq (cons rec uniq)
                      nets (bbdb-record-net rec)
                      name (downcase (or (bbdb-record-name rec) ""))
                      akas (mapcar 'downcase (bbdb-record-aka rec)))
                (while nets
                  (setq net (car nets))
                  (when (cond
                         ;; primary
                         ((and (member bbdb-completion-type
                                       '(primary primary-or-name))
                               (member (intern-soft (downcase net) ht)
                                       all-the-completions))
                          (setq nets nil)
                          t)
                         ;; name
                         ((and name (member bbdb-completion-type
                                            '(nil name primary-or-name))
                               (let ((cname (symbol-name sym)))
                                 (or (string= cname name)
                                     (member cname akas))))
                          (setq name nil)
                          t)
                         ;; net
                         ((and (member bbdb-completion-type
                                       '(nil net))
                               (member (intern-soft (downcase net) ht)
                                       all-the-completions)))
                         ;; (name-or-)primary
                         ((and (member bbdb-completion-type
                                       '(name-or-primary))
                               (let ((cname (symbol-name sym)))
                                 (or (string= cname name)
                                     (member cname akas))))
                          (setq nets nil)
                          t)
                         )
                    (setq dwim-completions
                          (cons (bbdb-dwim-net-address rec net)
                                dwim-completions))
                    (if exact-match (setq nets nil)))
                  (setq nets (cdr nets)))))
            (symbol-value sym)))
         all-the-completions)

        ;; if, after all that, we've only got one matching record...
        (if (and dwim-completions (null (cdr dwim-completions)))
            (progn
              (delete-region beg end)
              (insert (car dwim-completions))
              (message ""))
          ;; otherwise, pop up a completions window
          (if (not (get-buffer-window "*Completions*"))
              (setq bbdb-complete-name-saved-window-config
                    (current-window-configuration)))
          (let ((arg (list (current-buffer)
                           (set-marker (make-marker) beg)
                           (set-marker (make-marker) end))))
            (with-output-to-temp-buffer "*Completions*"
              (bbdb-display-completion-list
               dwim-completions
               'bbdb-complete-clicked-name
               arg)))
          (or (eq (selected-window) (minibuffer-window))
              (message "Making completion list...done"))))))))

;;;###autoload
(defun bbdb-yank ()
  "Insert the current contents of the *BBDB* buffer at point."
  (interactive)
  (insert (let ((b (current-buffer)))
            (set-buffer bbdb-buffer-name)
            (prog1 (buffer-string) (set-buffer b)))))


;;; interface to mail-abbrevs.el.

(defcustom bbdb-define-all-aliases-field 'mail-alias
  "*The field which `bbdb-define-all-aliases' searches for."
  :group 'bbdb
  :type 'symbol)

(defcustom bbdb-define-all-aliases-mode 'first
  "*The type of alias which are created.
first: Default is to generate an abbrev which is \"alias\" and expands to the
       primary net.
star:  Generate an extra alias \"<alias>*\" which expands to all nets of an
       record.
all:   Generate an alias all nets (as for 'star) and an alias for each net
       as \"<alias>n\" where n is the position of the net in the nets of the
       record."
  :group 'bbdb
  :type '(choice (symbol :tag "Only first" first)
                 (symbol :tag "<alias>* for all nets" star)
                 (symbol :tag "All aliases" all)))

;;;###autoload
(defun bbdb-define-all-aliases ()
  "Define mail aliases for some of the records in the database.
Every record which has a `mail-alias' field will have a mail alias
defined for it which is the contents of that field.  If there are
multiple comma-separated words in the `mail-alias' field, then all
of those words will be defined as aliases for that person.

If multiple entries in the database have the same mail alias, then
that alias expands to a comma-separated list of the network addresses
of all of those people."
  (interactive "")
  (let* ((target (cons bbdb-define-all-aliases-field "."))
         (use-abbrev-p (fboundp 'define-mail-abbrev))
         (mail-alias-separator-string (if (boundp 'mail-alias-separator-string)
                                          mail-alias-separator-string
                                        ", "))
         (records (bbdb-search (bbdb-records) nil nil nil target))
         result record aliases match)

    (if use-abbrev-p
        nil
      ;; clear abbrev-table
      (setq mail-aliases nil)
      ;; arrange rebuilt if necessary, this should be done by
      ;; mail-pre-abbrev-expand-hook, but there is none!
      (defadvice sendmail-pre-abbrev-expand-hook
        (before bbdb-rebuilt-all-aliases activate)
        (bbdb-rebuilt-all-aliases)))

    ;; collect an alist of (alias rec1 [rec2 ...])
    (while records
      (setq record (car records))
      (if (bbdb-record-net record)
          (setq aliases (bbdb-split
                         (bbdb-record-getprop record
                                              bbdb-define-all-aliases-field)
                         ","))
        (if (not bbdb-silent-running)
            (bbdb-warn "record %S has no network address, but the aliases: %s"
                        (bbdb-record-name record)
                        (bbdb-record-getprop record
                                             bbdb-define-all-aliases-field)))
        (setq aliases nil))

      (while aliases
        (if (setq match (assoc (car aliases) result))
            (nconc match (cons record nil))
          (setq result (cons (list (car aliases) record) result)))
        (setq aliases (cdr aliases)))
      (setq records (cdr records)))

    ;; iterate over the results and create the aliases
    (while result
      (let* ((aliasstem (caar result))
             (rec (cadar result))
             (group-alias-p (cddar result))
             (nets (if (not group-alias-p) (bbdb-record-net rec)))
             (expansions
              (if group-alias-p
                  (mapcar (lambda (r) (bbdb-dwim-net-address r)) (cdar result))
                (mapcar (lambda (net) (bbdb-dwim-net-address rec net))
                        (if (eq 'all bbdb-define-all-aliases-mode)
                            nets
                          (list (car nets))))))
             (count 1)
             alias expansion)

        (if group-alias-p
            ;; for group aliases we just take all the primary nets and define
            ;; just one expansion!
            (setq expansions (list (mapconcat 'identity expansions
                                              mail-alias-separator-string)))
          ;; this is an alias for a single person so deal with it according to
          ;; the bbdb-define-all-aliases-mode
          (when (or (not (eq 'first bbdb-define-all-aliases-mode))
                    (setq expansions
                          (cons (mapconcat 'identity
                                           (mapcar (lambda (net)
                                                     (bbdb-dwim-net-address
                                                      rec net))
                                                   nets)
                                           mail-alias-separator-string)
                                expansions)
                          count 0))))

        ;; create the aliases for each expansion
        (while expansions
          (cond ((= count 0);; all the nets of a record
                 (setq alias (concat aliasstem "*")))
                ((= count 1);; expansion as usual
                 (setq alias aliasstem))
                (t;; alias# for each net of a record
                 (setq alias (format "%s%s" aliasstem count))))
          (setq count (1+ count))
          (setq expansion (car expansions))

          (if use-abbrev-p
              (define-mail-abbrev alias expansion)
            (define-mail-alias alias expansion))
          (setq alias (or (intern-soft (downcase alias)
                                       (if use-abbrev-p
                                           mail-abbrevs mail-aliases))
                          (error "couldn't find the alias we just defined!")))

          (or (eq (symbol-function alias) 'mail-abbrev-expand-hook)
              (error "mail-aliases contains unexpected hook %s"
                     (symbol-function alias)))
          ;; The abbrev-hook is called with network addresses instead of bbdb
          ;; records to avoid keeping pointers to records, which would lose if
          ;; the database was reverted.  It uses -search-simple to convert
          ;; these to records, which is plenty fast.
          (fset alias (list 'lambda '()
                            (list 'bbdb-mail-abbrev-expand-hook
                                  alias
                                  (list 'quote
                                        (mapcar (lambda (x)
                                                  (car (bbdb-record-net x)))
                                                (cdr (car result)))))))
          (setq expansions (cdr expansions))))
      (setq result (cdr result)))

    (when (not use-abbrev-p)
      (if (boundp 'mail-mode-header-syntax-table)
          (modify-syntax-entry ?* "w" mail-mode-header-syntax-table))
      (sendmail-pre-abbrev-expand-hook))))

;; We should be cleverer here and instead of rebuilding all aliases we should
;; just do what's necessary, i.e. remove deleted records and add new records
(defun bbdb-rebuilt-all-aliases ()
  (let ((needs-rebuilt bbdb-define-all-aliases-needs-rebuilt))
    (when needs-rebuilt
      (if (not bbdb-silent-running)
          (message "Rebuilding aliases due to %s aliases." needs-rebuilt))
      (setq bbdb-define-all-aliases-needs-rebuilt nil)
      (bbdb-define-all-aliases))))

(defcustom bbdb-mail-abbrev-expand-hook nil
  "*Hook or hooks invoked each time an alias is expanded.
The hook is called with two arguments the alias and the list of records."
  :group 'bbdb-hooks
  :type 'hook)

(defun bbdb-mail-abbrev-expand-hook (alias records)
  (run-hook-with-args 'bbdb-mail-abbrev-expand-hook alias records)
  (mail-abbrev-expand-hook)
  (when bbdb-completion-display-record
    (if bbdb-use-pop-up
        (bbdb-pop-up-bbdb-buffer))
    (let ((bbdb-gag-messages t))
      (bbdb-display-records-1
       (mapcar (lambda (x) (bbdb-search-simple nil x)) records)
       t))))

(defun bbdb-get-mail-aliases ()
  "Return a list of mail aliases used in the BBDB.
The format is suitable for `completing-read'."
  (let* ((target (cons bbdb-define-all-aliases-field "."))
         (records (bbdb-search (bbdb-records) nil nil nil target))
         result aliases)
    (while records
      (setq aliases (bbdb-split
                     (bbdb-record-getprop (car records)
                                          bbdb-define-all-aliases-field)
                     ","))
      (while aliases
        (add-to-list 'result (list (car aliases)))
        (setq aliases (cdr aliases)))
      (setq records (cdr records)))
    result))

;;;###autoload
(defun bbdb-add-or-remove-mail-alias (&optional records newalias delete)
  "Add NEWALIAS in all RECORDS or remove it if DELETE it t.
When called with prefix argument it will remove the alias.
We honor `bbdb-apply-next-command-to-all-records'!
The new alias will only be added if it isn't there yet."
  (interactive (list (if (bbdb-do-all-records-p) 'all 'one)
                     (completing-read
                      (format "%s mail alias: " (if current-prefix-arg "Remove" "Add"))
                      (bbdb-get-mail-aliases))
                     current-prefix-arg))
  (setq newalias (bbdb-string-trim newalias))
  (setq newalias (if (string= "" newalias) nil newalias))
  (let* ((propsym bbdb-define-all-aliases-field)
         (do-all-p (if (equal records 'one) nil t))
         (records (cond ((equal records 'all) (mapcar 'car bbdb-records))
                        ((equal records 'one) (list (bbdb-current-record t)))
                        (t records))))
    (while records
      (let* ((record (car records))
             (oldaliases (bbdb-record-getprop record propsym)))
        (if oldaliases (setq oldaliases (bbdb-split oldaliases ",")))
        (if delete (setq oldaliases (delete newalias oldaliases))
          (add-to-list 'oldaliases newalias))
        (setq oldaliases (bbdb-join oldaliases ", "))
        (bbdb-record-putprop record propsym oldaliases))
      (setq records (cdr records)))
    (if do-all-p
        (bbdb-redisplay-records)
      (bbdb-redisplay-one-record (bbdb-current-record))))
  (setq bbdb-define-all-aliases-needs-rebuilt
        (if delete
            'deleted
          (if (bbdb-record-net (bbdb-current-record))
              'new
            nil))))

;;; Dialing numbers from BBDB
(defcustom bbdb-dial-local-prefix-alist
  '(((if (integerp bbdb-default-area-code)
         (format "(%03d)" bbdb-default-area-code)
       (or bbdb-default-area-code ""))
     ""))
  "Mapping to remove local prefixes from numbers.
If this is non-nil, it should be an alist of
(PREFIX REPLACEMENT) elements. The first part of a phone number
matching the regexp returned by evaluating PREFIX will be replaced by
the corresponding REPLACEMENT when dialing."
  :group 'bbdb-phone-dialing
  :type 'sexp)

(defcustom bbdb-dial-local-prefix nil
  "Local prefix digits.
If this is non-nil, it should be a string of digits which your phone
system requires before making local calls (for example, if your phone system
requires you to dial 9 before making outside calls.) In BBDB's
opinion, you're dialing a local number if it starts with a 0 after
processing bbdb-dial-local-prefix-alist."
  :group 'bbdb-phone-dialing
  :type '(choice (const :tag "No digits required" nil)
                 (string :tag "Dial this first" "9")))

(defcustom bbdb-dial-long-distance-prefix nil
  "Long distance prefix digits.
If this is non-nil, it should be a string of digits which your phone
system requires before making a long distance call (one not in your local
area code).  For example, in some areas you must dial 1 before an area
code. Note that this is used to replace the + sign in phone numbers
when dialling (international dialing prefix.)"
  :group 'bbdb-phone-dialing
  :type '(choice (const :tag "No digits required" nil)
                 (string :tag "Dial this first" "1")))

(defcustom bbdb-sound-player nil
  "The program to be used to play the sounds for the touch-tone digits."
  :group 'bbdb-phone-dialing
  :type '(choice (const :tag "No External Player" nil)
                 (file :tag "Sound Player" "/usr/local/bin/play")))

(defcustom bbdb-sound-files
  '["/usr/demo/SOUND/sounds/touchtone.0.au"
    "/usr/demo/SOUND/sounds/touchtone.1.au"
    "/usr/demo/SOUND/sounds/touchtone.2.au"
    "/usr/demo/SOUND/sounds/touchtone.3.au"
    "/usr/demo/SOUND/sounds/touchtone.4.au"
    "/usr/demo/SOUND/sounds/touchtone.5.au"
    "/usr/demo/SOUND/sounds/touchtone.6.au"
    "/usr/demo/SOUND/sounds/touchtone.7.au"
    "/usr/demo/SOUND/sounds/touchtone.8.au"
    "/usr/demo/SOUND/sounds/touchtone.9.au"
    "/usr/demo/SOUND/sounds/touchtone.pound.au"
    "/usr/demo/SOUND/sounds/touchtone.star.au"]
  "A vector of ten sound files to be used for dialing.  They
correspond to the 0, 1, 2, ... 9 digits, pound and star, respectively."
  :group 'bbdb-phone-dialing
  :type 'vector)

(defcustom bbdb-modem-dial nil
  "Type of dialing to use.
If this value is nil, the audio device is used for dialing. Otherwise,
this string is fed to the modem before the phone number digits."
  :group 'bbdb-phone-dialing
  :type '(choice (const  :tag "audio" nil)
                 (string :tag "tone dialing" "ATDT ")
                 (string :tag "pulse dialing" "ATDP ")))

(defcustom bbdb-modem-device "/dev/modem"
  "The name of the modem device.
This is only used if bbdb-modem-dial is set to something other than nil."
  :group 'bbdb-phone-dialing
  :type 'string)

(defcustom bbdb-sound-volume 50
  "The volume to play back dial tones at. The range is 0 to 100.
This is only used if bbdb-modem-dial is set to nil."
  :group 'bbdb-phone-dialing
  :type 'integer)

(defun bbdb-play-sound (num &optional volume)
  "Play the specified touchtone number NUM at VOLUME.
Uses external program `bbdb-sound-player' if set, otherwise
try to use internal sound if available."
  (if (and (not bbdb-sound-player) (featurep 'native-sound))
      ;; This requires the sound files to be loaded via bbdb-xemacs.
      (funcall 'play-sound (intern (format "touchtone%d" num))
               bbdb-sound-volume)
    (if (and (not (featurep 'xemacs))
             ;; We can't tell a priori if Emacs 21 facility will
             ;; actually work.
             (condition-case nil
                 (play-sound (list 'sound
                                   :file (aref bbdb-sound-files
                                               (string-to-number num))
                                   :volume (or volume bbdb-sound-volume)))
               (error nil)))
        (if (and bbdb-sound-player
                 (file-exists-p bbdb-sound-player))
            (call-process bbdb-sound-player nil nil nil
                          (aref bbdb-sound-files num))
          (error "BBDB has no means of playing sound.")))))

(eval-and-compile
  (if (fboundp 'next-event)
      (fset 'bbdb-next-event 'next-event)
    (fset 'bbdb-next-event 'read-event)))

(defun bbdb-dial-number (phone-string)
  "Dial the number specified by PHONE-STRING.
The number is dialed either by playing touchtones through the audio
device using bbdb-sound-player, or by sending a dial sequence to
bbdb-modem-device. # and * are dialed as-is, and a space is treated as
a pause in the dial sequence."
  (interactive "sDial number: ")
  (let ((dialed ""))
    (mapcar
     (lambda(d)
       (if bbdb-modem-dial
           (setq dialed
                 (concat dialed
                         (cond ((eq ?  d) ",")
                               ((memq d '(?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9 ?* ?#))
                                (format "%c" d))
                               (t ""))))
         (cond
          ((eq ?# d)
           (bbdb-play-sound 10))
          ((eq ?* d)
           (bbdb-play-sound 11))
          ((eq ?  d)
           ;; if we use sit-for, the user can interrupt!
           (sleep-for 1)) ;; configurable?
          ((memq d '(?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9))
           (bbdb-play-sound (- (char-int d) (char-int ?0))))
          (t)))) phone-string)

    ;; tell the user that we're dialed, if we're using the modem
    (if bbdb-modem-dial
        (with-temp-buffer
          (insert bbdb-modem-dial dialed ";\r\n")
          (write-region (point-min) (point-max) bbdb-modem-device t)
          (message "%s dialed. Pick up the phone now and hit any key ..."
                   phone-string)
          (bbdb-next-event)
          (erase-buffer)
          (insert "ATH\r\n")
          (write-region (point-min) (point-max) bbdb-modem-device t)))))

;;;###autoload
(defun bbdb-dial (phone force-area-code)
  "Dial the number at point.
If the point is at the beginning of a record, dial the first
phone number.  Does not dial the extension.  Does not apply the
transformations from bbdb-dial-local-prefix-alist if a prefix arg
is given."
  (interactive (list (bbdb-current-field)
                     current-prefix-arg))
  (if (eq (car-safe phone) 'name)
      (setq phone (car (bbdb-record-phones (bbdb-current-record)))))
  (if (eq (car-safe phone) 'phone)
      (setq phone (car (cdr phone))))
  (or (vectorp phone) (error "not on a phone field"))

  (let* ((number (bbdb-phone-string phone)) shortnumber)
    (when (not force-area-code)
      (let ((alist bbdb-dial-local-prefix-alist))
        (while alist
          (if (string-match (concat "^" (eval (caar alist))) number)
              (setq shortnumber (concat (car (cdar alist))
                                        (substring number (match-end 0)))
                    alist nil))
          (setq alist (cdr alist)))))

    ;; cut off the extension
    (if (string-match "x[0-9]+$" number)
        (setq number (substring number 0 (match-beginning 0))))

    ;; This is terrifically Americanized...
    ;; Leading 0 => local number (?)
    (if (and (not shortnumber) bbdb-dial-local-prefix
             (string-match "^0" number))
        (setq number (concat bbdb-dial-local-prefix number)))

    ;; Leading + => long distance/international number
    (if (and (not shortnumber) bbdb-dial-long-distance-prefix
             (string-match "^\+" number))
        (setq number (concat bbdb-dial-long-distance-prefix " "
                             (substring number 1))))

    ;; use the short number if it's available
    (setq number (or shortnumber number))
    (if (not bbdb-silent-running)
        (message "Dialing %s" number))
    (bbdb-dial-number number)))


;; not sure what this is doing here...
(defun bbdb-get-record (prompt)
  "Get the current record or ask the user.
To be used in `interactive' like this:
(interactive (list (bbdb-get-record \"look up ...\")))"
  (if (and (boundp 'bbdb-buffer-name)
(string= bbdb-buffer-name (buffer-name)))
(bbdb-current-record)
(let (re (pr ""))
  (while (not re)
    (setq re (bbdb-completing-read-record (concat pr prompt)))
    (unless re (ding)) (setq pr "Invalid response! ")) re)))

;;; Finger, based on code by Sam Cramer <cramer@sun.com>.
;;; Note that process-death bugs in 18.57 may make this eat up all the cpu...

(defcustom bbdb-finger-buffer-name "*finger*"
  "The buffer into which finger output should be directed."
  :group 'bbdb-utilities-finger
  :type 'string)

(defun bbdb-finger-internal (address)
  (message "Fingering %s..." address)
  (condition-case condition
      (let* ((@ (string-match "@" address))
             (stream (open-network-stream
                      "finger" bbdb-finger-buffer-name
                      (if @ (substring address (1+ @)) "localhost")
                      "finger")))
        (set-process-sentinel stream 'bbdb-finger-process-sentinel)
        (princ (concat "finger " address "\n"))
        (process-send-string
         stream (concat;;"/W " ; cs.stanford.edu doesn't like this...
                 (if @ (substring address 0 @) address) "\n"))
        (process-send-eof stream))
    (error
     (princ (format "error fingering %s: %s\n" address
                    (if (stringp condition) condition
                      (concat "\n" (nth 1 condition)
                              (if (cdr (cdr condition)) ": ")
                              (mapconcat '(lambda (x)
                                            (if (stringp x) x
                                              (prin1-to-string x)))
                                         (cdr (cdr condition)) ", ")))))
     (bbdb-finger-process-sentinel nil nil)))) ; hackaroonie

(defvar bbdb-remaining-addrs-to-finger)
(defun bbdb-finger-process-sentinel (process s)
  (save-excursion
    (set-buffer bbdb-finger-buffer-name)
    (goto-char (point-min))
    (while (search-forward "\r" nil t)
      (delete-char -1))
    (if (and (boundp 'bbdb-remaining-addrs-to-finger)
             bbdb-remaining-addrs-to-finger)
        (let ((addr (car bbdb-remaining-addrs-to-finger)))
          (setq bbdb-remaining-addrs-to-finger
                (cdr bbdb-remaining-addrs-to-finger))
          (goto-char (point-max))
          (let ((standard-output (current-buffer)))
            (princ "\n\n\^L\n")
            (bbdb-finger-internal addr)))
      (goto-char (point-max))
      (message "Finger done."))))

(defcustom bbdb-finger-host-field 'finger-host
  "*The field for special net addresses used by \"\\[bbdb-finger]\"."
  :group 'bbdb-utilities-finger
  :type 'symbol)

(defun bbdb-record-finger-host (record)
  (let ((finger-host (and bbdb-finger-host-field
                          (bbdb-record-getprop record bbdb-finger-host-field))))
    (if finger-host
        (bbdb-split finger-host ",")
      (bbdb-record-net record))))

;;;###autoload
(defun bbdb-finger (record &optional which-address)
  "Finger the network address of a BBDB record.
If this command is executed from the *BBDB* buffer, finger the network
address of the record at point; otherwise, it prompts for a user.
With a numeric prefix argument, finger the Nth network address of the
current record\; with a prefix argument of ^U, finger all of them.
The *finger* buffer is filled asynchronously, meaning that you don't
have to wait around for it to finish\; but fingering another user before
the first finger has finished could have unpredictable results.
\\<bbdb-mode-map>
If this command is executed from the *BBDB* buffer, it may be prefixed
with \"\\[bbdb-apply-next-command-to-all-records]\" \(as in \
\"\\[bbdb-apply-next-command-to-all-records]\\[bbdb-finger]\" instead of \
simply \"\\[bbdb-finger]\"\), meaning to finger all of
the users currently listed in the *BBDB* buffer instead of just the one
at point.  The numeric prefix argument has the same interpretation.

You can define a special network address to \"finger\" by defining a
field `finger-host' (default value of `bbdb-finger-host-field')."
  (interactive (list (bbdb-get-record "BBDB Finger: ")
                     current-prefix-arg))
  (if (not (consp record)) (setq record (list record)))
  (let ((addrs nil))
    (while record
      (cond ((null which-address)
             (setq addrs
                   (nconc addrs
                          (list (car (bbdb-record-finger-host (car record)))))))
            ((stringp which-address)
             (setq addrs (nconc addrs (list which-address))))
            ((numberp which-address)
             (setq addrs
                   (nconc addrs
                          (list (nth which-address
                                     (bbdb-record-finger-host (car record)))))))
            (t
             (setq addrs
                   (nconc addrs
                          (copy-sequence (bbdb-record-finger-host
                                          (car record)))))))
      (setq record (cdr record)))
    (if (car addrs)
        (save-excursion
          (with-output-to-temp-buffer bbdb-finger-buffer-name
            (set-buffer bbdb-finger-buffer-name)
            (make-local-variable 'bbdb-remaining-addrs-to-finger)
            (setq bbdb-remaining-addrs-to-finger (cdr addrs))
            (bbdb-finger-internal (car addrs))))
      (error "Nothing to finger!"))))


(defun bbdb-remove-duplicate-nets (records)
  "*Remove duplicate nets from a record."
  (interactive (if (bbdb-do-all-records-p)
                   (mapcar 'car bbdb-records)
                 (bbdb-current-record)))
  (let (nets cnets)
    (while records
      (setq nets (bbdb-record-net (car records))
            cnets nil)
      (while nets
        (add-to-list 'cnets (car nets))
        (setq nets (cdr nets)))
      (bbdb-record-set-net (car records) cnets)
      (setq records (cdr records)))))

(defun bbdb-find-duplicates (&optional fields)
  "Find all records that have duplicate entries for given FIELDS.
FIELDS should be a list of the symbols `name', `net', and/or `aka'.
Note that overlap between these fields is noted if either is selected,
most common case `aka' and `name'.  If FIELDS is not given it
defaults to all of them.

The results of the search is returned as a list of records."
  (setq fields (or fields '(name net aka)))
  (let ((records (bbdb-records))
        rec hash ret)
    (while records
      (setq rec (car records))

      (when (and (memq 'name fields)
                 (bbdb-record-name rec)
                 (setq hash (bbdb-gethash (downcase (bbdb-record-name rec))))
                 (> (length hash) 1))
        (setq ret (append hash ret))
        (message "BBDB record `%s' causes duplicates, maybe it is equal to a company name."
                 (bbdb-record-name rec))
        (sit-for 0))

      (if (memq 'net fields)
          (let ((nets (bbdb-record-net rec)))
            (while nets
              (setq hash (bbdb-gethash (downcase (car nets))))
              (when (> (length hash) 1)
                (setq ret (append hash ret))
                (message "BBDB record `%s' has duplicate net `%s'."
                         (bbdb-record-name rec) (car nets))
                (sit-for 0))
              (setq nets (cdr nets)))))

      (if (memq 'aka fields)
          (let ((aka (bbdb-record-aka rec)))
            (while aka
              (setq hash (bbdb-gethash (downcase (car aka))))
              (when (> (length hash) 1)
                (setq ret (append hash ret))
                (message "BBDB record `%s' has duplicate aka `%s'"
                         (bbdb-record-name rec) (car aka))
                (sit-for 0))
              (setq aka (cdr aka)))))

      (setq records (cdr records)))
    (reverse (bbdb-remove-memq-duplicates ret))))

(defun bbdb-show-duplicates (&optional fields)
  "*Find all records that have duplicate entries for given FIELDS.
FIELDS should be a list of the symbols `name', `net', and/or `aka'.
Note that overlap between these fields is noted if either is selected
(most common case `aka' and `name').  If FIELDS is not given it
defaults to all of them.

The results are displayed in the bbdb buffer."
  (interactive)
  (setq fields (or fields '(name net aka)))
  (bbdb-display-records (bbdb-find-duplicates fields)))

;;; Time-based functions
(defun bbdb-kill-older (date &optional compare function)
  "*Apply FUNCTION to all records with timestamps older than DATE.
The comparison is done with COMPARE.  If FUNCTION is not specified, the
selected records are deleted.  If COMPARE is not specified,
`string-lessp' is used.

Example:
        (bbdb-kill-older \"1997-01-01\")
will delete all records with timestamps older than Jan 1 1997.

Notes:  1. Records without timestamp fields will be ignored
2. DATE must be in yyyy-mm-dd format."
  (interactive "sKill records with timestamp older than (yyyy-mm-dd): \n")
  (let ((records (bbdb-records)) timestamp
        (fun (or function 'bbdb-delete-record-internal))
        (cmp (or compare 'string-lessp)))
    (while records
      (if (and (setq timestamp (bbdb-record-getprop (car records) 'timestamp))
               (funcall cmp timestamp date))
          (funcall fun (car records)))
      (setq records (cdr records)))))

(defmacro bbdb-compare-records (cmpval field compare)
  "Builds a lambda comparison function that takes one argument, REC.
REC is returned if
(COMPARE VALUE CMPVAL)
is true, where VALUE is the value of the FIELD field of REC."
  `(lambda (rec)
(let ((val (bbdb-record-getprop rec ,field)))
  (if (and val (,compare val ,cmpval))
      rec nil))))

;;;###autoload
(defun bbdb-timestamp-older (date)
  "*Display records with timestamp older than DATE.
DATE must be in yyyy-mm-dd format."
  (interactive "sOlder than date (yyyy-mm-dd): ")
  (bbdb-display-some (bbdb-compare-records date 'timestamp string<)))

;;;###autoload
(defun bbdb-timestamp-newer (date)
  "*Display records with timestamp newer than DATE.
DATE must be in yyyy-mm-dd format."
  (interactive "sNewer than date (yyyy-mm-dd): ")
  (bbdb-display-some (bbdb-compare-records date 'timestamp string>)))

;;;###autoload
(defun bbdb-creation-older (date)
  "*Display records with creation-date older than DATE.
DATE must be in yyyy-mm-dd format."
  (interactive "sOlder than date (yyyy-mm-dd): ")
  (bbdb-display-some (bbdb-compare-records date 'creation-date string<)))

;;;###autoload
(defun bbdb-creation-newer (date)
  "*Display records with creation-date newer than DATE.
DATE must be in yyyy-mm-dd format."
  (interactive "sNewer than date (yyyy-mm-dd): ")
  (bbdb-display-some (bbdb-compare-records date 'creation-date string>)))

;;;###autoload
(defun bbdb-creation-no-change ()
  "*Display records that have the same timestamp and creation-date."
  (interactive)
  (bbdb-display-some
   (bbdb-compare-records (bbdb-record-getprop rec 'timestamp)
                         'creation-date string=)))

;;; Help and documentation

(defcustom bbdb-info-file nil
  "*Set this to the location of the bbdb info file, if it's not in the
standard place."
  :group 'bbdb
  :type '(choice (const :tag "Standard location" nil)
                 (file :tag "New location")))

;;;###autoload
(defun bbdb-info ()
  (interactive)
  (require 'info)
  (if bbdb-inside-electric-display
      (bbdb-electric-throw-to-execute '(bbdb-info))
    (let ((file (or bbdb-info-file "bbdb")))
      (Info-goto-node (format "(%s)Top" file)))))

;;;###autoload
(defun bbdb-help ()
  (interactive)
  (message (substitute-command-keys "\\<bbdb-mode-map>\
new field: \\[bbdb-insert-new-field]; \
edit field: \\[bbdb-edit-current-field]; \
delete field: \\[bbdb-delete-current-field-or-record]; \
mode help: \\[describe-mode]; \
info: \\[bbdb-info]")))


(or (fboundp 'member);; v18 lossage
    (defun member (item list)
      (while (and list (not (equal item (car list)))) (setq list (cdr list)))
      list))


;;; If Sebastian Kremer's minibuffer history package is around, use it.
(if (and (fboundp 'gmhist-make-magic)
         (string-lessp emacs-version "19")) ; v19 has history built in
    (mapcar 'gmhist-make-magic
            '(bbdb bbdb-name bbdb-company bbdb-net bbdb-changed)))

;;;###autoload
(defcustom bbdb-update-records-mode 'annotating
  "Controls how `bbdb-update-records' processes email addresses.
Set this to an expression which evaluates either to 'searching or
'annotating.  When set to 'annotating email addresses will be fed to
`bbdb-annotate-message-sender' in order to update existing records or create
new ones.  A value of 'searching will search just for existing records having
the right net.

There is a version of this variable for each MUA, which overrides this variable
when set!

This variable is also used for inter-function communication between the
functions `bbdb-update-records' and `bbdb-prompt-for-create'."
  :group 'bbdb-mua-specific
  :type '(choice (const :tag "annotating all messages"
                        annotating)
                 (const :tag "annotating no messages"
                        searching)
                 (sexp   :tag "user defined")))

(defvar bbdb-offer-to-create nil
  "Used for inter-function communication between the functions
`bbdb-update-records' and `bbdb-prompt-for-create'.")
(defvar bbdb-address nil
  "Used for inter-function communication between the functions
`bbdb-update-records' and `bbdb-prompt-for-create'.")

(defvar bbdb-update-address-class nil
  "Class of currently processed address as in `bbdb-get-addresses-headers'.
The `bbdb-notice-hook' and `bbdb-create-hook' functions may utilize this to
treat updates in the right way.")

(defvar bbdb-update-address-header nil
  "Header the currently processed address was extracted from.
The `bbdb-notice-hook' and `bbdb-create-hook' functions may utilize this to
treat updates in the right way.")

;;;###autoload
(defun bbdb-update-records (addrs auto-create-p offer-to-create)
  "Returns the records corresponding to the list of addresses ADDRS,
creating or modifying them as necessary.  A record will be created if
AUTO-CREATE-P is non-nil or if OFFER-TO-CREATE is true and the user
confirms the creation.

The variable `bbdb/gnus-update-records-mode' controls what actions
are performed and it might override `bbdb-update-records-mode'.

When hitting C-g once you will not be asked any more for new people listed
in this message, but it will search only for existing records.  When hitting
C-g again it will stop scanning."
  (setq auto-create-p (bbdb-invoke-hook-for-value auto-create-p))

  (let ((bbdb-records (bbdb-records))
        (processed-addresses 0)
        (bbdb-offer-to-create (or offer-to-create (eq 'prompt auto-create-p)))
        (bbdb-update-records-mode
         (if offer-to-create 'annotating
           (if (listp bbdb-update-records-mode)
               (eval bbdb-update-records-mode)
             bbdb-update-records-mode)))
        (addrslen (length addrs))
        (bbdb-update-address-class nil)
        (bbdb-update-address-header nil)
        records hits)

    (while addrs

      (setq bbdb-address (car addrs)
            bbdb-update-address-class (car bbdb-address)
            bbdb-update-address-header (cadr bbdb-address)
            bbdb-address (caddr bbdb-address))

      (condition-case nil
          (progn
            (setq hits
                  (cond ((null (cadr bbdb-address))
                         ;; ignore emtpy addrs, e.g. (??? nil)
                         nil)
                        ((eq bbdb-update-records-mode 'annotating)
                         (list;; search might return a list
                          (bbdb-annotate-message-sender
                           bbdb-address t
                           (or offer-to-create;; force create
                               auto-create-p)
                           'bbdb-prompt-for-create)))
                        ((eq bbdb-update-records-mode 'searching)
                         ;; search for records having this net
                         (let ((net (concat "^"
                                            (regexp-quote
                                             (cadr bbdb-address))
                                            "$"))
                               ;; there is no case for nets
                               (bbdb-case-fold-search t))
                           (bbdb-search bbdb-records nil nil net))))
                  processed-addresses (+ processed-addresses 1))

            (when (and (not bbdb-silent-running)
                       (not bbdb-gag-messages)
                       (not (eq bbdb-offer-to-create 'quit))
                       (= 0 (% processed-addresses 5)))
              (let ((mess (format "Hit C-g to stop BBDB from %s.  %d of %d addresses processed."
                                  bbdb-update-records-mode processed-addresses addrslen)))
                (if (featurep 'xemacs)
                    (bbdb-display-message 'progress mess)
                  (message mess)))
              (sit-for 0)))

        ;; o.k. there was a quit signal so how should we proceed now?
        (quit (cond ((eq bbdb-update-records-mode 'annotating)
                     (setq bbdb-update-records-mode 'searching))
                    ((eq bbdb-update-records-mode 'searching)
                     nil)
                    ((eq bbdb-update-records-mode 'next)
                     (setq bbdb-update-records-mode 'annotating))
                    (t
                     (setq bbdb-update-records-mode 'quit)))
              nil))

      (while hits
        ;; people should be listed only once so we use add-to-list
        (if (car hits) (add-to-list 'records (car hits)))
        (setq hits (cdr hits)))

      (setq addrs (cdr addrs)))

    ;; add-to-list adds at the front so we have to reverse the list in order
    ;; to reflect the order of the records as they appear in the headers.
    (setq records (nreverse records))

    records))

(defun bbdb-get-help-window (message)
  "Display MESSAGE in a new window which is the last one in the current frame."
  (let ((b (get-buffer-create " *BBDB Help*"))
        (w (or (get-buffer-window " *BBDB Help*")
               (get-lru-window)))
        (lines (let ((l 2) (s 0))
                 (while (setq s (string-match "\n" message s))
                   (setq s (1+ s) l (1+ l)))
                 l)))

    (setq w (split-window w))
    (select-window w)
    (switch-to-buffer b)
    (erase-buffer)
    (insert message)
    (goto-char (point-min))
    (let ((window-min-height 1))
      (enlarge-window (- lines (window-height w))))
    w))

(defun bbdb-kill-help-window (window)
  "Kill the buffer corresponding to WINDOW and delete the WINDOW."
  (kill-buffer (window-buffer window))
  (delete-window window))

;; This is a hack.  The function is called by bbdb-annotate-message-sender and
;; uses the above variable in order to manipulate bbdb-update-records.
;; Some cases are handled with signals in order to keep the changes in
;; bbdb-annotate-message-sender as minimal as possible.

(defun bbdb-prompt-for-create ()
  "This function is used by `bbdb-update-records' to ask the user how to
proceed the processing of records.

It is called from `bbdb-annotate-message-sender' (PROMPT-FOR-CREATE arg) and
returns `t' if the record should be created or `nil' otherwise.  It honors a
previous answer, e.g. \"!\" add all ..."
  (let ((old-offer-to-create bbdb-offer-to-create)
        event prompt)
    (when bbdb-offer-to-create
      (when (not (integerp bbdb-offer-to-create))
        (setq prompt (format "%s is not in the db; add? (y,!,n,s,q,?) "
                             (or (car bbdb-address) (cadr bbdb-address))))
        (while (not event)
          (setq event (read-key-sequence prompt))
          (if (featurep 'xemacs)
              (setq event (bbdb-event-to-character (aref event 0)))
            (setq event (if (stringp event) (aref event 0)))))

        (setq bbdb-offer-to-create event))
      (message "");; clear the message buffer

      (cond ((eq bbdb-offer-to-create ?y)
             (setq bbdb-offer-to-create old-offer-to-create)
             t)
            ((eq bbdb-offer-to-create  ?!)
             t)
            ((or (eq bbdb-offer-to-create  ?n)
                 (eq bbdb-offer-to-create  ? ))
             (setq bbdb-update-records-mode 'next
                   bbdb-offer-to-create old-offer-to-create)
             (signal 'quit nil))
            ((eq bbdb-offer-to-create  ?q)
             (setq bbdb-update-records-mode 'quit)
             (signal 'quit nil))
            ((eq bbdb-offer-to-create  ?s)
             (setq bbdb-update-records-mode 'searching)
             (signal 'quit nil))
            (t
             (let ((w (bbdb-get-help-window
                       "Your answer controls how BBDB updates/searches for records.

Type ?  for this help.
Type y  to add the current record.
Type !  to add all remaining records.
Type n  to skip the current record. (You might also type space)
Type s  to switch from annotate to search mode.
Type q  to quit updating records.  No more search or annotation is done.")))
               (setq bbdb-offer-to-create nil)
               (condition-case error
                   (progn (bbdb-prompt-for-create)
                          (bbdb-kill-help-window w))
                 (t
                  (bbdb-kill-help-window w)
                  (apply 'signal error)))))))))

;;;###autoload
(defcustom bbdb-get-addresses-headers
  '((authors    . ("From" "Resent-From" "Reply-To"))
    (recipients . ("Resent-To" "Resent-CC" "To" "CC" "BCC")))
  "*List of headers to search for senders and recipients email addresses.
The headers are grouped into two classes, the authors and the senders headers."
  :group 'bbdb-mua-specific
  :type 'list)

;;;###autoload
(defcustom bbdb-get-only-first-address-p
  t
  "*If t `bbdb-update-records' will return only the first one.
Changing this variable will show its effect only after clearing the
`bbdb-message-cache' of a folder or closing and visiting it again."
  :group 'bbdb-mua-specific
  :type 'boolean)

(defun bbdb-get-addresses (only-first-address
                           uninteresting-senders
                           get-header-content-function
                           &rest get-header-content-function-args)
  ""
  (let ((headers bbdb-get-addresses-headers)
        (ignore-senders (or bbdb-user-mail-names uninteresting-senders))
        addrlist adlist fn ad
        header-type header-fields header-content)
    (while headers
      (setq header-type (caar headers)
            header-fields (cdar headers))
      (while header-fields
        (setq header-content (apply get-header-content-function
                                    (car header-fields)
                                    get-header-content-function-args))
        (when header-content
          (setq adlist (funcall bbdb-extract-address-components-func
                                header-content))
          (while adlist
            (setq fn (caar adlist)
                  ad (car (cdar adlist)))

            ;; ignore uninteresting addresses, this is kinda gross!
            (if (or (not (stringp ignore-senders))
                    (not (or (and fn (string-match ignore-senders fn))
                             (and ad (string-match ignore-senders ad)))))
                (add-to-list 'addrlist
                             (list header-type
                                   (car header-fields)
                                   (car adlist))))

            (if (and only-first-address addrlist)
                (setq adlist nil headers nil)
              (setq adlist (cdr adlist)))))
        (setq header-fields (cdr header-fields)))
      (setq headers (cdr headers)))
    (nreverse addrlist)))

(provide 'bbdb-com)
