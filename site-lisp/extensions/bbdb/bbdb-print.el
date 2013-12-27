;;; bbdb-print.el -- for printing BBDB databases using TeX.

;;; Authors: Boris Goldowsky <boris@cs.rochester.edu>
;;;          Dirk Grunwald <grunwald@cs.colorado.edu>
;;;          Luigi Semenzato <luigi@paris.cs.berkeley.edu>
;;; Copyright (C) 1993 Boris Goldowsky
;;; Version: 3.92; 4Jan95

;;; This file is part of the bbdb-print extensions to the Insidious
;;; Big Brother Database, which is for use with GNU Emacs.
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

;;; Commentary:
;;;
;;; In the *BBDB* buffer, type P to convert the listing to TeX
;;; format. It will prompt you for a filename.  Then run TeX on that
;;; file and print it out.
;;;
;;; Bbdb-print understands one new bbdb field: tex-name.  If it
;;; exists, this will be used for the printed listing instead of the
;;; name field of that record.  This is designed for entering names
;;; with lots of accents that would mess up mailers, or when for any
;;; reason you want the printed version of the name to be different
;;; from the version that appears on outgoing mail and in the *BBDB*
;;; buffer.  You may want to add tex-name to a omit list of the variable
;;; bbdb-display-layout-alist so you only see it in the printout.
;;; tex-name is exempted from the usual special-character quoting done by
;;; bbdb-print; it is used verbatim.
;;;
;;; Not all fields or records need be printed.  To not print a certain
;;; field, add it to `bbdb-print-omit-fields' (which see).  If after eliding
;;; fields a record contains no interesting information, it will not
;;; be printed at all; the variable `bbdb-print-require' determines
;;; what is meant by "interesting" information.  You can also restrict
;;; printing to just the records currently in the *BBDB* buffer by
;;; using *P instead of P.
;;;
;;; There are various options for the way the formatting is done; most
;;; are controlled by the variable bbdb-print-alist. See its
;;; documentation for the allowed options.

;;
;; $Id: bbdb-print.el,v 1.69 2005/08/11 03:23:48 waider Exp $
;;

;;; Installation:
;;;
;;; Put this file somewhere on your load-path.  Put bbdb-print.tex and
;;; bbdb-cols.tex somewhere on your TEXINPUTS path, or put absolute
;;; pathnames into the variable bbdb-print-format-files (which see). Put
;;; (add-hook 'bbdb-load-hook (function (lambda () (require 'bbdb-print))))
;;; into your .emacs, or autoload it.
;;;
;;; This program was adapted for BBDB by Boris Goldowsky
;;; <boris@cs.rochester.edu> and Dirk Grunwald
;;; <grunwald@cs.colorado.edu> using a TeX format designed by Luigi
;;; Semenzato <luigi@paris.cs.berkeley.edu>.
;;; We are also grateful to numerous people on the bbdb-info
;;; mailing list for suggestions and bug reports.

;;; Code:

(require 'bbdb)
(require 'bbdb-com)

;;; Variables:

(defcustom bbdb-print-file-name "~/bbdb.tex"
  "*Default file name for printouts of BBDB database."
  :group 'bbdb-utilities-print
  :type 'file)

(defcustom bbdb-print-omit-fields '(omit tex-name aka mail-alias)
  "*List of fields NOT to print in address list.
See also bbdb-print-require."
  :group 'bbdb-utilities-print
  :type '(repeat (symbol :tag "Field to exclude")))

(defcustom bbdb-print-require '(or address phone)
  "*What fields are required for printing a record.
This is evaluated for each record, and the record will be printed only
if it returns non-nil.  The symbols name, company, net, phone,
address, and notes will be set to appropriate values when this is
evaluated; they will be nil if the field does not exist or is elided.

The value of this variable can be any lisp expression, but typically
it will be used for a boolean combination of the field variables, as
in the following simple examples:

  Print only people whose phone numbers are known:
    (setq bbdb-print-require 'phone)
  Print people whose names AND companies are known:
    (setq bbdb-print-require '(and name company))
  Print people whose names, and either addresses OR phone numbers are known:
    (setq bbdb-print-require '(and name (or address phone)))."
  :group 'bbdb-utilities-print
  :type '(choice (const :tag "Print all records" t)
                 (symbol :tag "Print all records with this field" phone)
                 (sexp :tag "Print only when this evaluates to non-nil"
                       '(or phone address phone))))

(defun bbdb-print-field-shown-p (field)
  (not (memq field bbdb-print-omit-fields)))

(define-widget 'bbdb-print-alist-widget 'repeat
  "For use in Customize"
  :args `((choice
           (cons :tag "Column specification" :value (column . 1)
                 (const :tag "Column mode" column)
                 (radio-button-choice (const :tag "One column" 1)
                                      (const :tag "Two columns" 2)
                                      (const :tag "Three columns" 3)
                                      (const :tag "Four columns" 4)
                                      (const :tag "Quad" quad)
                                      (const :tag "Grid" grid)))
           (cons :tag "Separator specification" :value (separator . 0)
                 (const :tag "Separator" separator)
                 (radio-button-choice (const :tag "None" 0)
                                      (const :tag "Line" 1)
                                      (const :tag "Boxed letters" 2)
                                      (const :tag "Large boxed letters" 3)
                                      (const :tag "Large letters" 4)
                                      (const :tag "Letters with lines" 5)
                                      (const :tag "Letters with suits" 6)
                                      (const :tag "Boxed letters with suits" 7)))
           (cons :tag "Omit certain area codes"
                 :value (omit-area-code . ,(concat "^("
                                                   (if (integerp bbdb-default-area-code)
                                                       (int-to-string bbdb-default-area-code)
                                                     "000")  ") "))
                 (const :tag "Omit certain area codes" omit-area-code)
                 (regexp :tag "Pattern to omit"))
           (cons :tag "Phone number location" :value (phone-on-first-line . t)
                 (const :tag "Phone number location" phone-on-first-line)
                 (choice (const :tag "First home number on same line as name" t)
                         (const :tag "Don't put the phone number on the name line" nil)
                         (regexp :tag "Use phone number whose location matches" "^work$")))
           (cons :tag "Limit included phone numbers" :value (n-phones . 3)
                 (const :tag "Limit included phone numbers" n-phones)
                 (integer :tag "Maximum number to include" 3))
           (cons :tag "Limit included addresses" :value (n-addresses . 3)
                 (const :tag "Limit included addresses" n-addresses)
                 (integer :tag "Maximum number to include" 3))
           (cons :tag "Include additional TeX input files" :value (include-files . nil)
                 (const :tag "Additional TeX input files to include" include-files)
                 (repeat (file :tag "TeX file to include")))
           (cons :tag "Font type selection" :value (ps-fonts . nil)
                 (const :tag "Select font type" ps-fonts)
                 (choice (const :tag "Use standard TeX fonts" nil)
                         (const :tag "Use Postscript fonts" t)))
           (cons :tag "Font size selection" :value (font-size . 10)
                 (const :tag "Select font size" font-size)
                 (integer :tag "Font size in points" 10))
           (cons :tag "Page height selection" :value (v-size . nil)
                 (const :tag "Select page height" v-size)
                 (choice (const :tag "Use TeX default" nil)
                         (string :tag "Height (must be valid TeX dimension)" "9in")))
           (cons :tag "Page width selection" :value (h-size . nil)
                 (const :tag "Select page width" h-size)
                 (choice (const :tag "Use TeX default" nil)
                         (string :tag "Width (must be valid TeX dimension)" "6in")))
           (cons :tag "Vertical offset (top margin)" :value (voffset . nil)
                 (const :tag "Select vertical offset (top margin)" voffset)
                 (choice (const :tag "Use TeX default" nil)
                         (string :tag "Vertical offset (must be valid TeX dimension)" "1in")))
           (cons :tag "Horizontal offset (left margin)" :value (hoffset . nil)
                 (const :tag "Select horizontal offset (left margin)" hoffset)
                 (choice (const :tag "Use TeX default" nil)
                         (string :tag "Horizontal offset (must be valid TeX dimension)" "1in")))
           (cons :tag "Quad format height" :value (quad-vsize . "")
                 (const :tag "Select height for quad format pages" quad-vsize)
                 (string :tag "Height (must be valid TeX dimension)"))
           (cons :tag "Quad format width" :value (quad-hsize . "")
                 (const :tag "Select width for quad format pages" quad-hsize)
                 (string :tag "Width (must be valid TeX dimension)")))))

(defcustom bbdb-print-alist
  `((omit-area-code . ,(concat "^(" (if (integerp bbdb-default-area-code)
                                        (int-to-string bbdb-default-area-code)
                                      "000") ") "))
    (phone-on-first-line . "^[ \t]*$")
    (ps-fonts . nil)
    (font-size . 6)
    (quad-hsize . "3.15in")
    (quad-vsize . "4.5in"))
  "*Formatting options for `bbdb-print', all formats.
This is an alist of the form ((option1 . value1) (option2 . value2) ...)

You can have separate settings for brief and non-brief printouts;
see the variables `bbdb-print-brief-alist' and `bbdb-print-full-alist'.
Settings there will override the common settings in this variable.

The possible options and legal values are:
 - columns: 1, 2, 3, 4 or 'quad (4 little 2-column pages per sheet)
     or 'grid (12 credit-card-sized pages per sheet).
 - separator: 0-7, the style of heading for each letter.
     0=none, 1=line, 2=boxed letters, 3=large boxed letters, 4=large letters,
     5=letters with lines, 6=letters with suits, 7=boxed letters with suits.
 - omit-area-code: a regular expression matching area codes to omit.
 - phone-on-first-line: t means to put first phone number on the same
     line with the name, nil means just the name.  A string means to
     use the first phone number whose \"location\" matches that string,
     which should be a valid regular expression.
 - n-phones: maximum number of phone numbers to include.
 - n-addresses: maximum number of addresses to include.
 - include-files: list of TeX files to \\input.  If these filenames are not
   absolute, the files must be located somewhere that TeX will find them.
 - ps-fonts: nonnil means to use them, nil to use standard TeX fonts.
 - font-size: in points, any integer (assuming fonts in that size exist!).
 - hsize, vsize: horizontal dimension of pages.  String value can be any valid
   TeX dimension, or nil to use TeX's default.
 - hoffset, voffset: shift TeX's output rightward (downward) by this distance
   (any TeX dimension).  Nil or 0 uses TeX's default positioning.
 - quad-hsize, quad-vsize: for the quad format, horizontal and
     vertical size of the little pages.  These must be strings which
     are valid TeX dimensions, eg \"10cm\"."
  :group 'bbdb-utilities-print
  :type 'bbdb-print-alist-widget)

(defcustom bbdb-print-full-alist
  '((columns . 3)
    (separator . 2)
    (include-files "bbdb-print" "bbdb-cols"))
  "*Extra options for `bbdb-print' non-brief format.
These supplement or override entries in `bbdb-print-alist'; see description
of possible contents there."
  :group 'bbdb-utilities-print
  :type 'bbdb-print-alist-widget)

(defcustom bbdb-print-brief-alist
  '((columns . 1)
    (separator . 1)
    (n-phones . 2)
    (n-addresses . 1)
    (include-files "bbdb-print-brief" "bbdb-cols"))
  "*Extra Options for `bbdb-print', brief format.
These supplement or override entries in `bbdb-print-alist'; see description
of possible contents there."
  :group 'bbdb-utilities-print
  :type 'bbdb-print-alist-widget)

(defconst bbdb-print-filofax-alist
  (append '((font-size . 12)
            (columns . 2)
            (voffset . "-2cm")
            (hoffset . "-2cm")
            (vsize   . "27cm"))
          bbdb-print-full-alist)
  "Example setup for making pages for a Filofax binder.")


(defcustom bbdb-print-prolog
  (concat "%%%% ====== Phone/Address list in -*-TeX-*- Format =====\n"
          "%%%%        produced by bbdb-print, version 3.0\n\n")
  "*TeX statements to include at the beginning of the `bbdb-print' file."
  :group 'bbdb-utilities-print
  :type '(text :format "%t:\n%v"))

(defcustom bbdb-print-epilog "\\endaddresses\n\\bye\n"
  "*TeX statements to include at the end of the `bbdb-print' file."
  :group 'bbdb-utilities-print
  :type '(text :format "%t:\n%v"))

(defcustom bbdb-print-net 'primary
  "*Indicates whether only the primary or all email addresses are printed.
Symbol `primary' means print the primary email address only.
Symbol `all' means print all email addresses."
  :group 'bbdb-utilities-print
  :type '(choice (const primary)
         (const all)))

;;; Functions:

(defsubst bbdb-print-if-not-blank (string &rest more)
  "If STRING is not null, then return it concatenated
with rest of arguments.  If it is null, then all arguments are
ignored and the null string is returned."
  (if (or (null string) (equal "" string))
      ""
    (apply 'concat string more)))

;;;###autoload
(defun bbdb-print (visible-records to-file brief)
  "Make a TeX file for printing out the bbdb database.\\<bbdb-mode-map>
If \"\\[bbdb-apply-next-command-to-all-records]\\[bbdb-print]\" is \
used instead of simply \"\\[bbdb-print]\", then includes only the
people currently in the *BBDB* buffer.  With a prefix argument, makes
a brief \(one-line-per-entry) printout.

There are various variables for customizing the content & format of
the printout, notably the variables `bbdb-print-alist' and
`bbdb-print-require'.  See the file bbdb-print.el for more information."
  (interactive (list (bbdb-do-all-records-p)
                     (read-file-name "Print To File: "
                                     (file-name-directory bbdb-print-file-name)
                                     bbdb-print-file-name
                                     nil
                                     (file-name-nondirectory bbdb-print-file-name))
                     current-prefix-arg))
  (setq bbdb-print-file-name (expand-file-name to-file))
  (let* ((alist (append (if brief bbdb-print-brief-alist bbdb-print-full-alist)
                        bbdb-print-alist))
         (records (if (not visible-records)
                      (bbdb-records)
                    (set-buffer bbdb-buffer-name)
                    (mapcar 'car bbdb-records)))
         (psstring (if (cdr (assoc 'ps-fonts alist))
                       "ps" ""))
         (columns (cdr (assoc 'columns alist)))
         (current-letter t)
         (pofl (cdr (assoc 'phone-on-first-line alist)))
         (n-phones (cdr (assoc 'n-phones alist)))
         (n-addresses (cdr (assoc 'n-addresses alist))))
    (find-file bbdb-print-file-name)
    (widen) (erase-buffer)
    (insert bbdb-print-prolog)
    (let ((dimens '(hsize vsize hoffset voffset))
          val)
      (while dimens
        (setq val (cdr (assoc (car dimens) alist)))
        (if val
            (insert (format "\\%s=%s\n" (car dimens) val)))
        (setq dimens (cdr dimens))))
    (let ((infiles (cdr (assoc 'include-files alist))))
      (while infiles
        (insert (format "\\input %s\n" (car infiles)))
        (setq infiles (cdr infiles))))
    (insert (format "\n\\set%ssize{%d}\n"
                    psstring (cdr (assoc 'font-size alist)))
            (format "\\setseparator{%d}\n"
                    (cdr (assoc 'separator alist)))
            (cond ((eq 'quad columns)
                   (format "\\quadformat{%s}{%s}"
                           (cdr (assoc 'quad-hsize alist))
                           (cdr (assoc 'quad-vsize alist))))
                  ((eq 'grid columns) "\\grid")
                  ((= 4 columns) "\\fourcol")
                  ((= 3 columns) "\\threecol")
                  ((= 2 columns) "\\twocol")
                  ((= 1 columns) "\\onecol"))
            ;; catcodes are font-encoding specific !
            ;; Add more if you know them
            (if (equal psstring "ps")
                (concat "\n\n"
                        ;; Adobe Times and Courier
                        )
              (concat "\n\n"
                      ;; ec fonts
                      "\\catcode`ß=\\active\\chardefß=\"FF"))
            "\n\n\\beginaddresses\n")
    (while records
      (setq current-letter
            (bbdb-print-format-record (car records) current-letter
                                      brief pofl n-phones n-addresses))
      (setq records (cdr records)))
    (insert bbdb-print-epilog)
    (goto-char (point-min))))

(defvar bbdb-address-print-formatting-alist
  '((bbdb-address-is-continental . bbdb-print-format-address-continental)
    (nil . bbdb-print-format-address-default))
  "Alist of address identifying and address formatting functions for printing.
The key is an identifying function which accepts an address.  The
associated value is a formatting function which inserts the formatted
address in the current buffer.  If the identifying function returns
non-nil, the formatting function is called.  The nil key is a default
value will allways calls the associated formatting function.  Therefore
you should always have (nil . bbdb-print-format-address-default) as the
last element in the alist.

The functions must take one argument, the address.

See also `bbdb-address-formatting-alist'.")

(defun bbdb-print-format-address-continental (addr)
  "Insert formated continental address ADDR in current buffer for printing.
This format is used in western Europe, for example.

This function is a possible formatting function for
`bbdb-address-print-formatting-alist'.

The result looks like this:
 street
 street
 ...
 zip city, state
 country"
  (insert
   (format
    "\\address{%s}\n"
    (bbdb-print-tex-quote
     (if addr
         (concat
          (mapconcat (function (lambda(str)
                                 (if (= 0 (length (bbdb-string-trim str)))
                                     ()
                                   (concat str"\\\\\n"))))
                     (bbdb-address-streets addr)
                     "")
          (let ((c (bbdb-address-city addr))
                (s (bbdb-address-state addr))
                (z (bbdb-address-zip addr)))
            (if (or (> (length c) 0)
                    (> (length z) 0)
                    (> (length s) 0))
                (concat z (if (and (> (length z) 0)
                                   (> (length c) 0)) " " "")
                        c (if (and (or (> (length z) 0)
                                       (> (length c) 0))
                                   (> (length s) 0)) ", " "")
                        s "\\\\\n") ""))
          (bbdb-print-if-not-blank (bbdb-address-country addr) "\\\\\n"))
       "")))))

(defun bbdb-print-format-address-default (addr)
  "Insert formated address ADDR in current buffer for printing.
This is the default format; it is used in the US, for example.

This function is a possible formatting function for
`bbdb-address-print-formatting-alist'.

The result looks like this:
 street
 street
 ...
 city, state  zip
 country"
  (insert
   (format
    "\\address{%s}\n"
    (bbdb-print-tex-quote
     (if addr
         (concat
          (mapconcat (function (lambda(str)
                                 (if (= 0 (length (bbdb-string-trim str)))
                                     ()
                                   (concat str "\\\\\n"))))
                     (bbdb-address-streets addr)
                     "")
          (let ((c (bbdb-address-city addr))
                (s (bbdb-address-state addr))
                (z (bbdb-address-zip addr)))
            (if (or (> (length c) 0)
                    (> (length z) 0)
                    (> (length s) 0))
                (concat c (if (and (> (length c) 0)
                                   (> (length s) 0)) ", " "")
                        s (if (and (or (> (length c) 0)
                                       (> (length s) 0))
                                   (> (length z) 0)) "  " "")
                        z "\\\\\n") ""))
          (bbdb-print-if-not-blank (bbdb-address-country addr) "\\\\\n"))
       "")))))

(defun bbdb-print-format-record (record current-letter
                                        brief pofl n-phones n-addresses)
  "Insert the bbdb RECORD in TeX format.
Second arg CURRENT-LETTER is the first letter of the sortkey of the previous
record.  If this is non-nil and RECORD begins differently, a section heading is
output.  If CURRENT-LETTER is t always produces a heading.
3rd argument BRIEF is for 1-line-per-record printouts.
Args 3-5 PHONE-ON-FIRST-LINE, N-PHONES, and N-ADDRESSES are the respective
values from `bbdb-print-alist'.

The return value is the new CURRENT-LETTER."

  (bbdb-debug (if (bbdb-record-deleted-p record)
                  (error "plus ungood: tex formatting deleted record")))

  (let* ((first-letter
          (substring (concat (bbdb-record-sortkey record) "?") 0 1))
         (name    (and (bbdb-print-field-shown-p 'name)
                       (or (bbdb-record-getprop record 'tex-name)
                           (bbdb-print-tex-quote
                            (bbdb-record-name record)))))
         (company (and (bbdb-print-field-shown-p 'company)
                       (bbdb-record-company record)))
         (net     (and (bbdb-print-field-shown-p 'net)
                       (bbdb-record-net record)))
         (phone   (and (bbdb-print-field-shown-p 'phone)
                       (bbdb-record-phones record)))
         (address (and (bbdb-print-field-shown-p 'address)
                       (bbdb-record-addresses record)))
         (notes   (bbdb-record-raw-notes record)))

    (if (not (eval bbdb-print-require))
        nil                             ; lacks required fields

      ;; Section header, if neccessary.

      (if (and current-letter
               (not (string-equal first-letter current-letter)))
          (insert (format "\\goodbreak\n\\separator{%s}\n%%\n"
                          (bbdb-print-tex-quote (upcase first-letter)))))

      (insert "\\beginrecord\n")

      ;; if there is no name, use company instead
      (if (and (not name) company)
          (setq name (bbdb-print-tex-quote company)
                company nil))

      (let ((rightside ""))
        (cond ((null phone))
              ((eq t pofl)
               (setq rightside (bbdb-print-phone-string (car phone))
                     phone (cdr phone)))
              ((stringp pofl)
               (let ((p (bbdb-print-front-if
                         (function (lambda (ph)
                                     (string-match pofl (aref ph 0))))
                         phone)))
                 (if p
                     (setq rightside (bbdb-print-phone-string (car p))
                           phone (cdr p))))))
        (insert (format "\\firstline{%s}{%s}\n"
                        name
                        (bbdb-print-tex-quote rightside))))

      (if company
          (insert (format "\\comp{%s}\n" (bbdb-print-tex-quote company))))

      ;; Phone numbers

      (if n-phones
          (setq phone (bbdb-print-firstn (- n-phones (if pofl 1 0))
                                         phone brief)))
      (while phone
        (if (car phone)
            (let ((place (aref (car phone) 0))
                  (number (bbdb-print-phone-string (car phone))))
              (insert (format "\\phone{%s%s}\n"
                              (bbdb-print-tex-quote
                               (bbdb-print-if-not-blank place ": "))
                              (bbdb-print-tex-quote number))))
          (insert (format "\\phone{}\n")))
        (setq phone (cdr phone)))

      ;; Email address
      ;;  Make all dots legal line-breaks.

      (when net
    (let ((net-addrs
           (cond ((eq bbdb-print-net 'primary)
              (list (car net)))
             ((eq bbdb-print-net 'all)
              net)
             (t nil))))
      (insert
       (format
        "\\email{%s}\n"
        (mapconcat
         (lambda (net-addr)
           (setq net-addr (bbdb-print-tex-quote net-addr))
           (let ((start 0))
         (while (string-match "\\." net-addr start)
           (setq net-addr
             (concat (substring net-addr 0 (match-beginning 0))
                 ".\\-"
                 (substring net-addr (match-end 0))))
           (setq start (+ 2 (match-end 0)))))
           net-addr)
         net-addrs ", ")))))

      ;; Addresses.  FUTURE: If none left, should use phones instead.

      (if n-addresses
          (setq address
                (bbdb-print-firstn n-addresses address brief)))
      (while address
        (bbdb-format-address (car address) 'printing)
        (setq address (cdr address)))

      ;; Notes

      (if (stringp notes)
          (setq notes (list (cons 'notes notes))))
      (while notes
        (let ((thisnote (car notes)))
          (if (bbdb-print-field-shown-p (car thisnote))
              (progn
                (if (eq 'notes (car thisnote))
                    (insert (format "\\notes{%s}\n" (bbdb-print-tex-quote
                                                     (cdr thisnote))))
                  (insert (format "\\note{%s}{%s}\n"
                                  (bbdb-print-tex-quote (symbol-name
                                                         (car thisnote)))
                                  (bbdb-print-tex-quote (cdr thisnote))))))))
        (setq notes (cdr notes)))

      ;; Mark end of the record.

      (insert "\\endrecord\n%\n")
          (setq current-letter first-letter)))

  current-letter)

(defun bbdb-print-phone-string (phone)
  "Format PHONE-NUMBER as a string, obeying omit-area-code setting.
Omit-area-code is one of the allowed symbols in `bbdb-print-alist', which see."
  (let ((str (bbdb-phone-string phone))
        (omit (cdr (assoc 'omit-area-code bbdb-print-alist))))
    (if (and omit (string-match omit str))
        (substring str (match-end 0))
      str)))

(defun bbdb-print-front-if (func list)
  "Move first elt of LIST satisfying FUNC to front.
The car of the returned list is the first element that returned nonnil;
The cdr is the rest of the list.
But if the FUNC returns nil for every elements of the LIST, returns nil."
  (cond ((null list) nil)
        ((funcall func (car list))
         list)
        ((let ((rest (bbdb-print-front-if func (cdr list))))
           (if rest
               (cons (car rest)
                     (cons (car list) (cdr rest))))))))

(defun bbdb-print-firstn (n list force)
  "The first N elements of LIST.
If 3rd arg FORCE is nonnil, will extend the list to length N if necessary, by
adding nil's.  If N is nil, just returns LIST."
  (cond ((null n) list)
        ((null list) (if force (make-list n nil) nil))
        ((<= n 0) nil)
        (t (cons (car list) (bbdb-print-firstn (1- n) (cdr list) force)))))

(defun bbdb-print-tex-quote (string)
  "Quote any unquoted TeX special characters that appear in STRING.
In other words, # alone will be replaced by \\#, but \\^ will be left for
TeX to process as an accent."
  (if string
      (save-excursion
        (set-buffer (get-buffer-create " bbdb-print-tex-quote"))
        (erase-buffer)
        (insert string)
        (goto-char (point-min))
        (while (not (eobp))
          (cond ((looking-at "[<>=]+")
                 (replace-match "$\\&$"))
                ((and (looking-at "[#$%&_]")
                      (not (eq ?\\ (char-after (1- (point))))))
                 (insert "\\")
                 (forward-char 1))
                ((and (looking-at "~")
                      (not (eq ?\\ (char-after (1- (point))))))
                 (insert "\\")
                 (forward-char 1)
                 (insert "{}"))
                ((and (looking-at "[{}]")
                      (not (eq ?\\ (char-after (1- (point))))))
                 (insert "$\\")
                 (forward-char 1)
                 (insert "$"))
                (t (forward-char 1))))
        (buffer-string))))


(provide 'bbdb-print)

;;; bbdb-print ends here.

