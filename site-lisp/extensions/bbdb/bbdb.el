;;; -*- Mode:Emacs-Lisp -*-
;;; This file is the core of the Insidious Big Brother Database (aka BBDB),
;;; copyright (c) 1991, 1992, 1993, 1994 Jamie Zawinski <jwz@netscape.com>.
;;; See the file bbdb.texinfo for documentation.
;;;
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
;;;
;;;  ------------------------------------------------------------------------
;;; |  There is a mailing list for discussion of BBDB:                       |
;;; |         bbdb-info@lists.sourceforge.net                                |
;;; |  To join, send mail to bbdb-info-request@lists.sourceforge.net         |
;;; |  (don't forget the "-request" part or you'll look silly in front of    |
;;; |  lots of people who have the ability to remember it indefinitely...)   |
;;; |                                                                        |
;;; |  There is also a second mailing list, to which only bug fixes and      |
;;; |  new version announcements are sent; to be added to it, send mail to   |
;;; |  bbdb-announce-request@lists.sourceforge.net.  This is a very low      |
;;; |  volume list, and if you're using BBDB, you really should be on it.    |
;;; |                                                                        |
;;; |  When joining these lists or reporting bugs, please mention which      |
;;; |  version you have. The preferred method of reporting bugs is to use    |
;;; |  bbdb-submit-bug-report, which will include all useful version         |
;;; |  information plus state information about how you have BBDB set up.    |
;;;  ------------------------------------------------------------------------
;;;
;;; $Id: bbdb.el,v 1.234 2007/01/01 21:06:02 waider Exp $

(require 'timezone)
(eval-when-compile (require 'cl))

(eval-when-compile              ; pacify the compiler.
 (autoload 'widget-group-match "wid-edit")
 (autoload 'Electric-pop-up-window "electric")
 (autoload 'Electric-command-loop "electric")
 (autoload 'bbdb-migration-query "bbdb-migrate")
 (autoload 'bbdb-migrate "bbdb-migrate")
 (autoload 'bbdb-migrate-rewrite-all "bbdb-migrate")
 (autoload 'bbdb-migrate-update-file-version "bbdb-migrate")
 (autoload 'bbdb-unmigrate-record "bbdb-migrate")
 (autoload 'bbdb-create-internal "bbdb-com")
 (autoload 'bbdb-append-records-p "bbdb-com")
 (autoload 'bbdb-redisplay-records "bbdb-com")
 (autoload 'y-or-n-p-with-timeout "timer")
 (autoload 'mail-position-on-field "sendmail")
 (autoload 'bbdb-fontify-buffer "bbdb-gui")
 (autoload 'vm-select-folder-buffer "vm-folder")

 ;; can't use autoload for variables...
 (defvar bbdb-define-all-aliases-needs-rebuilt) ;; bbdb-com
 (defvar message-mode-map) ;; message.el
 (defvar mail-mode-map) ;; sendmail.el
 (defvar gnus-article-buffer) ;; gnus-art.el
 )

(defconst bbdb-version "2.35")
(defconst bbdb-version-date "$Date: 2007/01/01 21:06:02 $")

(defcustom bbdb-gui (if (fboundp 'display-color-p) ; Emacs 21
                        (display-color-p)
                      (not (null window-system))) ; wrong for XEmacs?
  "*Non-nil means fontify the *BBDB* buffer."
  :group 'bbdb
  :type 'boolean)

;; File format
(defconst bbdb-file-format 6)
(defvar bbdb-file-format-migration nil
  "A cons of two elements: the version read, and the version to write.
nil if the database was read in and is to be written in the current
version.")

(defvar bbdb-no-duplicates-p nil
  "Should BBDB allow entries with duplicate names.
This may lead to confusion when doing completion.  If non-nil, it will
prompt the users on how to merge records when duplicates are detected.")

;; Definitions for things that aren't in all Emacsen and that I really
;; would prefer not to live without.
(eval-and-compile
  (if (fboundp 'unless) nil
    (defmacro unless (bool &rest forms) `(if ,bool nil ,@forms))
    (defmacro when (bool &rest forms) `(if ,bool (progn ,@forms))))
  (unless (fboundp 'save-current-buffer)
    (defalias 'save-current-buffer 'save-excursion))
  (if (fboundp 'mapc)
      (defalias 'bbdb-mapc 'mapc)
    (defalias 'bbdb-mapc 'mapcar))
  )

(unless (fboundp 'with-current-buffer)
  (defmacro with-current-buffer (buf &rest body)
    `(save-current-buffer (set-buffer ,buf) ,@body)))

(unless (fboundp 'defvaralias)
  (defun defvaralias (&rest args)))

(defmacro string> (a b) (list 'not (list 'or (list 'string= a b)
                                         (list 'string< a b))))

(eval-and-compile
  (or (fboundp 'set-keymap-prompt)
      (fset 'set-keymap-prompt 'ignore)))

;; this should really be in bbdb-com
;;;###autoload
(defun bbdb-submit-bug-report ()
  "Submit a bug report, with pertinent information to the BBDB info list."
  (interactive)
  (require 'reporter)
  (delete-other-windows)
  (reporter-submit-bug-report
   "bbdb-info@lists.sourceforge.net"
   (concat "BBDB " bbdb-version)
   (append
    ;; non user variables
    '(emacs-version
      bbdb-version-date
      bbdb-file-format
      bbdb-no-duplicates-p)
    ;; user variables
    (sort (apropos-internal "^bbdb"
                            'user-variable-p)
          (lambda (v1 v2) (string-lessp (format "%s" v1) (format "%s" v2))))
    ;; see what the user had loaded
    (list 'features)
    )
   nil
   nil
   "Please change the Subject header to a concise bug description.\nIn this report, remember to cover the basics, that is, what you expected to\nhappen and what in fact did happen.  Please remove these\ninstructions from your message.")

  ;; insert the backtrace buffer content if present
  (let ((backtrace (get-buffer-create "*Backtrace*")))
    (when backtrace
      (goto-char (point-max))
      (insert "\n\n")
      (insert-buffer-substring backtrace)))

  (goto-char (point-min))
  (mail-position-on-field "Subject"))

;; Make custom stuff work even without customize
;;   Courtesy of Hrvoje Niksic <hniksic@srce.hr>
(eval-and-compile
  (condition-case ()
      (require 'custom)
    (error nil))
  (unless (and (featurep 'custom) (fboundp 'custom-declare-variable))
    ;; We have the old custom-library, hack around it!
    (defmacro defgroup (&rest args)
      nil)
    (defmacro defcustom (var value doc &rest args)
      `(defvar ,var ,value ,doc))
    (defmacro defface (var value doc &rest args)
      `(make-face ,var))
    (defmacro define-widget (&rest args)
      nil)))

(defconst bbdb-have-re-char-classes (string-match "[[:alpha:]]" "x")
    "Non-nil if this Emacs supports regexp character classes.
E.g. `[[:alnum:]]'.")

;; Custom groups

(defgroup bbdb nil
  "The Insidious Big Brother Database."
  :group 'news
  :group 'mail)

(put 'bbdb 'custom-loads '("bbdb-hooks" "bbdb-com"))

(defgroup bbdb-hooks nil
  "Hooks run at various times by the BBDB"
  :group 'bbdb)

(defgroup bbdb-record-display nil
  "Variables that affect the display of BBDB records"
  :group 'bbdb)

(defgroup bbdb-record-creation nil
  "Variables that affect the creation of BBDB records"
  :group 'bbdb)

(defgroup bbdb-noticing-records nil
  "Variables that affect the noticing of new authors"
  :group 'bbdb-record-creation)
(put 'bbdb-noticing-records 'custom-loads '("bbdb-hooks"))

(defgroup bbdb-record-use nil
  "Variables that affect the use of BBDB records"
  :group 'bbdb)

(defgroup bbdb-database nil
  "Variables that affect the database as a whole"
  :group 'bbdb)

(defgroup bbdb-saving nil
  "Variables that affect saving of the BBDB"
  :group 'bbdb-database)

(defgroup bbdb-mua-specific nil
  "MUA-specific customizations"
  :group 'bbdb)

(defgroup bbdb-mua-specific-gnus nil
  "Gnus-specific BBDB customizations"
  :group 'bbdb-mua-specific)

(put 'bbdb-mua-specific-gnus 'custom-loads '("bbdb-gnus"))

(defgroup bbdb-mua-specific-gnus-scoring nil
  "Gnus-specific scoring BBDB customizations"
  :group 'bbdb-mua-specific-gnus)

(put 'bbdb-mua-specific-gnus-scoring 'custom-loads '("bbdb-gnus"))

(defgroup bbdb-mua-specific-gnus-splitting nil
  "Gnus-specific splitting BBDB customizations"
  :group 'bbdb-mua-specific-gnus)

(put 'bbdb-mua-specific-gnus-splitting 'custom-loads '("bbdb-gnus"))

(defgroup bbdb-mua-specific-vm nil
  "VM-specific BBDB customizations"
  :group 'bbdb-mua-specific)

(put 'bbdb-mua-specific-vm 'custom-loads '("bbdb-vm"))

(defgroup bbdb-phone-dialing nil
  "Customizations for phone number dialing"
  :group 'bbdb)
(put 'bbdb-phone-dialing 'custom-loads '("bbdb-com"))

(defgroup bbdb-utilities nil
  "Customize BBDB Utilities"
  :group 'bbdb)

(defgroup bbdb-utilities-finger nil
  "Customizations for fingering from within the BBDB"
  :group 'bbdb-utilities
  :prefix "bbdb-finger")
(put 'bbdb-utilities-finger 'custom-loads '("bbdb-com"))

(defgroup bbdb-utilities-ftp nil
  "Customizations for using FTP sites stored in BBDB records."
  :group 'bbdb-utilities)
(put 'bbdb-utilities-ftp 'custom-loads '("bbdb-ftp"))

(defgroup bbdb-utilities-print nil
  "Customizations for printing the BBDB."
  :group 'bbdb-utilities
  :prefix "bbdb-print")
(put 'bbdb-utilities-print 'custom-loads '("bbdb-print"))

(defgroup bbdb-utilities-supercite nil
  "Customizations for using Supercite with the BBDB."
  :group 'bbdb-utilities
  :prefix "bbdb/sc")
(if (or (featurep 'supercite)
        (locate-library "supercite"))
    (put 'bbdb-utilities-supercite 'custom-loads '("bbdb-sc")))

(defgroup bbdb-utilities-server nil
  "Customizations for interfacing with the BBDB from external programs."
  :group 'bbdb-utilities
  :prefix "bbdb/srv")
(if (and (or (featurep 'gnuserv) (locate-library "gnuserv"))
         (or (featurep 'itimer)  (locate-library "itimer")))
    (put 'bbdb-utilities-server 'custom-loads '("bbdb-srv")))

;; BBDB custom widgets

(define-widget 'bbdb-alist-with-header 'group
  "My group"
  :match 'bbdb-alist-with-header-match
  :value-to-internal (lambda (widget value)
                       (if value (list (car value) (cdr value))))
  :value-to-external (lambda (widget value)
                       (if value (append (list (car value)) (cadr value)))))

(defun bbdb-alist-with-header-match (widget value)
  (widget-group-match widget
                      (widget-apply widget :value-to-internal value)))

;; Customizable variables

(defcustom bbdb-file "~/.bbdb"
  "*The name of the Insidious Big Brother Database file."
  :group 'bbdb-database
  :type 'file)

;; this should be removed, and the following put in place:
;; a hierarchical structure of bbdb files, some perhaps read-only,
;; perhaps caching in the local bbdb. This way you could have, e.g. a
;; company address book, with each person having access to it, and
;; then a local address book with personal stuff in it.
(defcustom bbdb-file-remote nil
  "*The remote file to save the database to.
When this is non-nil, it should be a file name.
When BBDB reads `bbdb-file', it checks this file,
 and if it is newer, downloads it.
When BBDB writes `bbdb-file', it also writes this file.

This feature allows one to keep the database in one place while using
different computers, thus reducing the need for merging different files."
  :group 'bbdb-database
  :type '(choice (const :tag "none" nil)
                 (file :tag "remote file name")))

(defcustom bbdb-file-remote-save-always t
  "*Should the `bbdb-file-remote' file be saved whenever the database is saved?
When nil, you will be asked."
  :group 'bbdb-database
  :type 'boolean)

(unless (fboundp 'primep)
  (defun primep (num)
    "Return t if NUM is a prime number."
    (and (numberp num) (> num 1) (= num (floor num))
         (let ((lim (sqrt num)) (nu 2) (prime t))
           (while (and prime (<= nu lim))
             (setq prime (/= 0 (mod num nu))
                   nu (1+ nu)))
           prime))))

(defcustom bbdb-hashtable-size 1021
  "*The size of the bbdb hashtable.
BBDB hashtable is an obarray, so this must be a prime integer.
Set this to a prime number (much) larger than the size of your database
before loading it.
If you change this variable outside `customize',
you should reload `bbdb-file'."
  :group 'bbdb-database
  :type 'integer
  :set (lambda (symb val)
         (unless (primep val)
           (error "`%s' must be prime, not %s" symb val))
         (set symb val)
         (when (fboundp 'bbdb-records)
           (bbdb-records))
         val))

(defcustom bbdb-default-area-code nil
  "*The default area code to use when prompting for a new phone number.
This variable also affects dialing."
  :group 'bbdb-record-creation
  :type '(choice (const :tag "none" nil)
                 (integer :tag "Default Area Code"))
  :set (lambda( symb val )
         (if (or (and (stringp val)
                      (string-match "^[0-9]+$" val))
                 (integerp val)
                 (null val))
             (set symb val)
           (error "%s must contain digits only." symb))))

(defcustom bbdb-lastname-prefixes
 '("von" "Von" "de" "De")
  "*List of lastname prefixes recognized in name fields. Used to
enhance dividing name strings into firstname and lastname parts."
  :group 'bbdb-record-creation
  :type '(repeat string))

(defcustom bbdb-default-domain nil
  "*The default domain to append when prompting for a new net address.
If the address entered does not contain `[@%!]', `@bbdb-default-domain'
will be appended to it.

The address will not be altered if bbdb-default-domain remains at its
default value of nil, or if one provides a prefix argument to the
bbdb-insert-new-field command."
  :group 'bbdb-record-creation
  :type '(choice (const :tag "none" nil)
                 (string :tag "Domain" :value nil)))

(defcustom bbdb-north-american-phone-numbers-p t
  "*Set this to nil if you want to enter phone numbers that aren't the same
syntax as those in North America (that is, [[1] nnn] nnn nnnn ['x' n*]).
If this is true, then some error checking is done so that you can't enter
incorrect phone numbers, and all phone numbers are pretty-printed the same
way.  European phone numbers don't have as strict a syntax, however, so
this is a harder problem for them (on which I am punting).

You can have both styles of phone number in your database by providing a
prefix argument to the bbdb-insert-new-field command."
  :group 'bbdb-record-creation
  :type 'boolean)

(defcustom bbdb-electric-p nil
  "*Whether bbdb mode should be `electric' like `electric-buffer-list'."
  :group 'bbdb-record-display
  :type 'boolean)

(defcustom bbdb-case-fold-search (default-value 'case-fold-search)
  "*This is the value of `case-fold-search' used by `bbdb' and friends.
This variable lets the case-sensitivity of ^S and of the bbdb
commands be different."
  :group 'bbdb
  :type 'boolean)

;; these variables both need to be enabled for gnus mailreading to
;; work right. that's probably a bug, or something.
(defcustom bbdb/mail-auto-create-p t
  "*If this is t, then Gnus, MH, RMAIL, and VM will automatically
create new bbdb records for people you receive mail from. If this
is a function name or lambda, then it is called with no arguments
to decide whether an entry should be automatically created. You
can use this to, for example, not create records for messages
which have reached you through a particular mailing list, or to
only create records automatically if the mail has a particular
subject."
  :group 'bbdb-noticing-records
  :type '(choice (const :tag "Automatically create" t)
                 (const :tag "Prompt before creating" prompt)
                 (const :tag "Do not automatically create" nil)
                 (function :tag "Create with function" bbdb-)))

(defcustom bbdb/news-auto-create-p nil
  "*If this is t, then Gnus will automatically create new bbdb
records for people you receive mail from.  If this is a function name
or lambda, then it is called with no arguments to decide whether an
entry should be automatically created.  You can use this to, for
example, create or not create messages which have a particular
subject.  If you want to autocreate messages based on the current
newsgroup, it's probably a better idea to set this variable to t or
nil from your `gnus-select-group-hook' instead."
  :group 'bbdb-noticing-records
  :type '(choice (const :tag "Automatically create" t)
                 (const :tag "Prompt before creating" prompt)
                 (const :tag "Do not automatically create" nil)
                 (function :tag "Create with function" bbdb-)))

(defcustom bbdb-quiet-about-name-mismatches nil
  "*If this is true, then BBDB will not prompt you when it notices a
name change, that is, when the \"real name\" in a message doesn't correspond
to a record already in the database with the same network address.  As in,
\"John Smith <jqs@frob.com>\" versus \"John Q. Smith <jqs@frob.com>\".
Normally you will be asked if you want to change it.
If set to a number it is the number of seconds to sit for while
displaying the mismatch message."
  :group 'bbdb-noticing-records
  :type '(choice (const :tag "Prompt for name changes" nil)
                 (const :tag "Do not prompt for name changes" t)
         (integer :tag
              "Instead of prompting, warn for this many seconds")))

(defcustom bbdb-use-alternate-names t
  "*If this is true, then when bbdb notices a name change, it will ask you
if you want both names to map to the same record."
  :group 'bbdb-noticing-records
  :type '(choice (const :tag "Ask to use alternate names field" t)
                 (const :tag "Use alternate names field without asking" nil)))

(defcustom bbdb-readonly-p nil
  "*If this is true, then nothing will attempt to change the bbdb database
implicitly, and you will be prevented from doing it explicitly.  If you have
more than one emacs running at the same time, you might want to arrange for
this to be set to t in all but one of them."
  :group 'bbdb-database
  :type '(choice (const :tag "Database is read-only" t)
                 (const :tag "Database is writable" nil)))

(defcustom bbdb-continental-zip-regexp "^\\s *[A-Z][A-Z]?\\s *-\\s *[0-9][0-9][0-9]"
  "Regexp matching continental zip codes.
Addresses with zip codes matching the regexp will be formated using
`bbdb-format-address-continental'.  The regexp should match zip codes
of the form CH-8052, NL-2300RA, and SE-132 54."
  :group 'bbdb-record-display
  :type 'regexp)

(defcustom bbdb-auto-revert-p nil
  "*If this variable is true and the BBDB file is noticed to have changed on
disk, it will be automatically reverted without prompting you first.  Otherwise
you will be asked. (But if the file has changed and you hae made changes in
memory as well, you will always be asked.)"
  :group 'bbdb-saving
  :type '(choice (const :tag "Revert unchanged database without prompting" t)
                 (const :tag "Ask before reverting database")))

(defcustom bbdb-notice-auto-save-file nil
  "*If this is true, then the BBDB will notice when its auto-save file is
newer than the file is was read from, and will offer to revert."
  :group 'bbdb-saving
  :type '(choice (const :tag "Check auto-save file" t)
                 (const :tag "Do not check auto-save file" nil)))

(defcustom bbdb-use-pop-up t
  "If true, display a continuously-updating bbdb window while in VM, MH,
RMAIL, or Gnus.  If 'horiz, stack the window horizontally if there is room."
  :group 'bbdb-record-display
  :type '(choice (const :tag "Automatic BBDB window, stacked vertically" t)
                 (const :tag "Automatic BBDB window, stacked horizontally" 'horiz)
                 (const :tag "No Automatic BBDB window" nil)))

(defcustom bbdb-pop-up-target-lines 5
  "*Desired number of lines in a VM/MH/RMAIL/Gnus pop-up bbdb window."
  :group 'bbdb-record-display
  :type 'integer)

(defcustom bbdb-completion-type nil
  "*Controls the behaviour of `bbdb-complete-name'.  If nil, completion is
done across the set of all full-names and user-ids in the bbdb-database;
if the symbol 'name, completion is done on names only; if the symbol 'net,
completion is done on network addresses only; if it is 'primary, then
completion is done only across the set of primary network addresses (the
first address in the list of addresses for a given user).  If it is
'primary-or-name, completion is done across primaries and real names."
  :group 'bbdb-record-use
  :type '(choice (const :tag "Complete across names and net addresses" nil)
                 (const :tag "Complete across names" name)
                 (const :tag "Complete across net addresses" net)
                 (const :tag "Complete across primary net addresses" primary)
                 (const :tag "Complete across names and primary net addresses"
                        primary-or-name)))

(defcustom bbdb-completion-display-record t
  "*Whether `bbdb-complete-name' (\\<mail-mode-map>\\[bbdb-complete-name]
in mail-mode) will update the *BBDB* buffer
to display the record whose email address has just been inserted."
  :group 'bbdb-record-use
  :type '(choice (const :tag "Update the BBDB buffer" t)
                 (const :tag "Don't update the BBDB buffer" nil)))

(defcustom bbdb-user-mail-names nil
  "*A regular expression identifying the addresses that belong to you.
If a message from an address matching this is seen, the BBDB record for
the To: line will be shown instead of the one for the From: line.  If
this is nil, it will default to the value of (user-login-name)."
  :group 'bbdb-noticing-records
  :type (list 'choice '(const :tag "Use value of (user-login-name)" nil)
              (list 'regexp :tag "Pattern matching your addresses"
                         (or (user-login-name) "address"))))

(defcustom bbdb-always-add-addresses 'ask
  "*If this is true, then when the Insidious Big Brother Database notices
a new email address for a person, it will automatically add it to the list of
addresses.  If it is 'ask, you will be asked whether to add it.  If it is nil
then new network addresses will never be automatically added nor the user will
be asked.

When set to a function name the function should return one of these values.

See also the variable `bbdb-new-nets-always-primary' for control of whether
the addresses go at the front of the list or the back."
  :group 'bbdb-noticing-records
  :type '(choice (const :tag "Automatically add new addresses" t)
                 (const :tag "Ask before adding new addresses" ask)
                 (const :tag "Never add new addresses" nil)
                 (const bbdb-ignore-some-messages-hook)
                 (const bbdb-ignore-most-messages-hook)))

(defcustom bbdb-new-nets-always-primary nil
  "*If this is true, then when the Insidious Big Brother Database adds a new
address to a record, it will always add it to the front of the list of
addresses, making it the primary address.  If this is nil, you will be asked.
If it is the symbol 'never (really, if it is any non-t, non-nil value) then
new network addresses will always be added at the end of the list."
  :group 'bbdb-noticing-records
  :type '(choice (const :tag "New address automatically made primary" t)
                (const :tag "Ask before making new address primary" nil)
                (const :tag "Never make new address primary" never)))

(defcustom bbdb-send-mail-style nil
  "*Specifies which package should be used to send mail.
Should be 'vm, 'mh, 'mail, 'message, or 'gnus (or nil, meaning guess.)"
  :group 'bbdb-record-use
  :type '(choice (const :tag "Use VM to send mail" vm)
                 (const :tag "Use MH-E to send mail" mh)
                 (const :tag "Use send-mail mode to send mail" mail)
                 (const :tag "Use Message to send mail" message)
                 (const :tag "Use Mew to send mail" mew)
                 (const :tag "Use compose-mail to send mail" compose-mail)
                 (const :tag "Use gnus to send mail" gnus)
                 (const :tag "Guess which package to use" nil)))

(defcustom bbdb-offer-save t
  "*If t, then certain actions will cause the BBDB to ask you whether
you wish to save the database.  If nil, then the offer to save will never
be made.  If not t and not nil, then any time it would ask you, it will
just save it without asking."
  :group 'bbdb-saving
  :type '(choice (const :tag "Offer to save the database" t)
                 (const :tag "Never offer to save the database" nil)
                 (const :tag "Save database without asking" savenoprompt)))

(defcustom bbdb-message-caching-enabled t
  "*Whether caching of the message->bbdb-record association should be used
for the interfaces which support it (VM, MH, and RMAIL).  This can speed
things up a lot.  One implication of this variable being true is that the
`bbdb-notice-hook' will not be called each time a message is selected, but
only the first time.  Likewise, if selecting a message would generate a
question (whether to add an address, change the name, etc) you will only
be asked that question the very first time the message is selected."
  :group 'bbdb
  :type '(choice (const :tag "Enable caching" t)
                 (const :tag "Disable caching" nil)))

(defcustom bbdb-silent-running nil
  "*If this is true, bbdb will suppress all its informational messages and
queries. Be very very certain you want to set this, because it will suppress
prompting to alter record names, assign names to addresses, etc."
  :group 'bbdb
  :type '(choice (const :tag "Run silently" t)
                 (const :tag "Disable silent running" nil)))

(defcustom bbdb-mode-hook nil
  "*Hook or hooks invoked when the *BBDB* buffer is created."
  :group 'bbdb-hooks
  :type 'hook)

(defcustom bbdb-list-hook nil
  "*Hook or hooks invoked after the `bbdb-list-buffer' is filled in.
Invoked with no arguments."
  :group 'bbdb-hooks
  :type 'hook)

(defcustom bbdb-create-hook 'bbdb-creation-date-hook
  "*Hook or hooks invoked each time a new BBDB record is created.  Invoked
with one argument, the new record.  This is called *before* the record is
added to the database.  Note that `bbdb-change-hook' will be called as well.

Hook functions can use the variable `bbdb-update-address-class' to determine
the class of an email address according to `bbdb-get-addresses-headers' and
the variable `bbdb-update-address-header' is set to the header the email
address was extracted from."
  :group 'bbdb-hooks
  :type 'hook)

(defcustom bbdb-change-hook 'bbdb-timestamp-hook
  "*Hook or hooks invoked each time a BBDB record is altered.  Invoked with
one argument, the record.  This is called *before* the bbdb-database buffer
is modified.  Note that if a new bbdb record is created, both this hook and
`bbdb-create-hook' will be called."
  :group 'bbdb-hooks
  :type 'hook)

(defcustom bbdb-after-change-hook nil
  "*Hook or hooks invoked each time a BBDB record is altered.  Invoked with
one argument, the record.  This is called *after* the bbdb-database buffer
is modified, so if you want to modify the record each time it is changed,
you should use the `bbdb-change-hook' instead.  Note that if a new bbdb
record is created, both this hook and `bbdb-create-hook' will be called."
  :group 'bbdb-hooks
  :type 'hook)

(defcustom bbdb-canonicalize-net-hook nil
  "*If this is non-nil, it should be a function of one arg: a network address
string.  Whenever the Insidious Big Brother Database \"notices\" a message,
the corresponding network address will be passed to this function first, as
a kind of \"filter\" to do whatever transformations upon it you like before
it is compared against or added to the database.  For example: it is the case
that CS.CMU.EDU is a valid return address for all mail originating at a
machine in the .CS.CMU.EDU domain.  So, if you wanted all such addresses to
be canonically hashed as user@CS.CMU.EDU, instead of as user@host.CS.CMU.EDU,
you might set this variable to a function like this:

 (setq bbdb-canonicalize-net-hook
       '(lambda (addr)
          (cond ((string-match \"\\\\`\\\\([^@]+@\\\\).*\\\\.\\\\(CS\\\\.CMU\\\\.EDU\\\\)\\\\'\"
                               addr)
                 (concat (substring addr (match-beginning 1) (match-end 1))
                         (substring addr (match-beginning 2) (match-end 2))))
                (t addr))))

You could also use this function to rewrite UUCP-style addresses into domain-
style addresses, or any number of things.

This function will be called repeatedly until it returns a value EQ to the
value passed in.  So multiple rewrite rules might apply to a single address."
  :group 'bbdb-hooks
  :type 'function)

(defcustom bbdb-canonicalize-redundant-nets-p t
  "*If this is non-nil, redundant network addresses will be ignored.
If a record has an address of the form foo@baz.com, setting this to t
will cause subsequently-noticed addresses like foo@bar.baz.com to be
ignored (since we already have a more general form of that address.)
This is similar in function to one of the possible uses of the variable
`bbdb-canonicalize-net-hook' but is somewhat more automatic.  (This
can't quite be implemented in terms of the canonicalize-net-hook because
it needs access to the database to determine whether an address is
redundant, and the canonicalize-net-hook is purely a textual manipulation
which is performed before any database access.)"
  :group 'bbdb-noticing-records
  :type '(choice (const :tag "Ignore redundant addresses" t)
                 (const :tag "Don't ignore redundant addresses" nil)))

(defcustom bbdb-notice-hook nil
  "*Hook or hooks invoked each time a BBDB record is \"noticed\", that is,
each time it is displayed by the news or mail interfaces.  Invoked with
one argument, the new record.  The record need not have been modified for
this to be called - use `bbdb-change-hook' for that.  You can use this to,
for example, add something to the notes field based on the subject of the
current message.  It is up to your hook to determine whether it is running
in Gnus, VM, MH, or RMAIL, and to act appropriately.

Also note that `bbdb-change-hook' will NOT be called as a result of any
modifications you may make to the record inside this hook.

Hook functions can use the variable `bbdb-update-address-class' to determine
the class of an email address according to `bbdb-get-addresses-headers' and
the variable `bbdb-update-address-header' is set to the header the email
address was extracted from.

Beware that if the variable `bbdb-message-caching-enabled' is true (a good
idea) then when you are using VM, MH, or RMAIL, this hook will be called only
the first time that message is selected.  (The Gnus interface does not use
caching.)  When debugging the value of this hook, it is a good idea to set
caching-enabled to nil."
  :group 'bbdb-hooks
  :type 'hook)

(defcustom bbdb-after-read-db-hook nil
  "*Hook or hooks invoked (with no arguments) just after the Insidious Big
Brother Database is read in.  Note that this can be called more than once if
the BBDB is reverted."
  :group 'bbdb-hooks
  :type 'hook)

(defcustom bbdb-load-hook nil
  "*Hook or hooks invoked when the BBDB code is first loaded.

WARNING:  This hook will be run the first time you traverse the Custom menus
          for the BBDB.  As a result, nothing slow should be added to
          this hook."
  :group 'bbdb-hooks
  :type 'hook)

(defcustom bbdb-initialize-hook nil
  "*Hook or hooks invoked (with no arguments) when the Insidious Big Brother
Database initialization function `bbdb-initialize' is run."
  :group 'bbdb-hooks
  :type 'hook)

;;;###autoload
(defcustom bbdb-multiple-buffers nil
  "When non-nil we create a new buffer of every buffer causing pop-ups.
You can also set this to a function returning a buffer name."
  :group 'bbdb-record-display
  :type '(choice (const :tag "Disabled" nil)
                 (function :tag "Enabled" bbdb-multiple-buffers-default)
                 (function :tag "User defined function")))

(defvar bbdb-mode-map nil
  "Keymap for Insidious Big Brother Database listings.")
(defvar bbdb-mode-search-map nil
  "Keymap for Insidious Big Brother Database searching")

;; iso-2022-7bit should be OK (but not optimal for Emacs, at least --
;; emacs-mule would be better) with both Emacs 21 and XEmacs.
(defconst bbdb-file-coding-system
  (if (fboundp 'coding-system-p)
      (cond ((coding-system-p 'utf-8-emacs)
             'utf-8-emacs)
            (t 'iso-2022-7bit)))
  "Coding system used for reading and writing `bbdb-file'.
This should not be changed by users.")

(defvar bbdb-suppress-changed-records-recording nil
  "Whether to record changed records in variable `bbdb-changed-records'.

If this is false, the BBDB will cease to remember which records are changed
as the change happens.  It will still remember that records have been changed,
so the file will still be saved, but the changed records list, and the `!!'
in the *BBDB* buffer modeline that it depends on, will no longer be updated.

You should bind this variable, not set it; the `!!' is a useful user-
interface feature, and should only be suppressed when changes need to be
automatically made to BBDB records which the user will not care directly
about.")


;;; These are the buffer-local variables we use.
;;; They are mentioned here so that the compiler doesn't warn about them
;;; when byte-compile-warn-about-free-variables is on.

(defvar bbdb-records nil)
(defvar bbdb-changed-records nil)
(defvar bbdb-end-marker nil)
(defvar bbdb-hashtable nil)
(defvar bbdb-propnames nil)
(defvar bbdb-message-cache nil)
(defvar bbdb-showing-changed-ones nil)
(defvar bbdb-modified-p nil)
(defvar bbdb-address-print-formatting-alist) ; "bbdb-print"

(defvar bbdb-debug t)
(defmacro bbdb-debug (&rest body)
  ;; ## comment out the next line to turn off debugging.
  ;; ## You really shouldn't do this!  But it will speed things up.
  (list 'and 'bbdb-debug (list 'let '((debug-on-error t)) (cons 'progn body)))
  )


;;; internal kludge to force queries to always happen with the mouse rather
;;; than basing the decision on the last-input-event; bind this, don't set it.
(defvar bbdb-force-dialog-boxes nil)

(defun bbdb-y-or-n-p (prompt)
  (prog1
      (funcall
       (cond ((and bbdb-force-dialog-boxes
                   (fboundp 'yes-or-no-p-dialog-box))
              (when (and (fboundp 'raise-frame)
                         (not (frame-visible-p (selected-frame))))
                (raise-frame (selected-frame)))
              'yes-or-no-p-dialog-box)
             (t 'y-or-n-p))
       prompt)
    (message " ")))

(defun bbdb-yes-or-no-p (prompt)
  (prog1
      (funcall (if (and bbdb-force-dialog-boxes
                        (fboundp 'yes-or-no-p-dialog-box))
                   'yes-or-no-p-dialog-box
                 'yes-or-no-p)
               prompt)
    (message " ")))

(defun bbdb-invoke-hook (hook arg)
  "Like `invoke-hooks', but invokes the given hook with one argument."
  (if (and (boundp hook) (setq hook (symbol-value hook)))
      (if (and (consp hook) (not (eq (car hook) 'lambda)))
          (while hook
            (funcall (car hook) arg)
            (setq hook (cdr hook)))
          (funcall hook arg))))

(defun bbdb-invoke-hook-for-value (hook &rest args)
  "If HOOK is a function, invoke it with ARGS. Otherwise return it as-is."
  (cond ((eq hook nil) nil)
        ((eq hook t) t)
        ((functionp hook) (apply hook args))
        (t hook)))

(defmacro bbdb-defstruct (conc-name &rest slots)
  "Make two functions, one for each slot.  The functions are:
        CONC-NAME + SLOT     and   CONC-NAME + `set-' + SLOT
The first one is to be used to read the element named in SLOT, and the
second is used to set it.  Also make a constant
        CONC-NAME + `length'
that holds the number of slots."
  (setq conc-name (symbol-name conc-name))
  (let ((body '())
        (i 0)
        (L (length slots)))
    (while slots
      (setq body
        (nconc body
          (let ((readname (intern (concat conc-name (symbol-name (car slots)))))
                (setname (intern (concat conc-name "set-" (symbol-name (car slots))))))
            (list
              (list 'defmacro readname '(vector)
                    (list 'list ''aref 'vector i))
              (list 'defmacro setname '(vector value)
                    (if (string= setname "bbdb-record-set-net")
                        (list 'setq
                              'bbdb-define-all-aliases-needs-rebuilt t))
                    (list 'list ''aset 'vector i 'value))
              ;(list 'put (list 'quote readname) ''edebug-form-hook ''(form))
              ;(list 'put (list 'quote setname) ''edebug-form-hook ''(form form))
              ))))
      (setq slots (cdr slots) i (1+ i)))
    (setq body (nconc body (list (list 'defconst
                                       (intern (concat conc-name "length"))
                                       L))))
    (cons 'progn body)))

;;; When reading this code, beware that "cache" refers to two things.
;;; It refers to the cache slot of bbdb-record structures, which is
;;; used for computed properties of the records; and it also refers
;;; to a message-id --> bbdb-record association list which speeds up
;;; the RMAIL, VM, and MH interfaces.

;; Build reading and setting functions for firstname, lastname, aka,
;; company, phones, addresses, net, raw-notes, and cache.  These are
;; for accessing the high-level forms for the record.
(bbdb-defstruct bbdb-record-
  firstname lastname aka company
  phones addresses net raw-notes
  cache
  )

;; HACKHACK
;;(defmacro bbdb-record-set-net (vector value)
;;  "We redefine the set-binding for 'net to detect changes"
;;  (list 'progn
;;        (list 'aset vector 6 value)
;;        (list 'setq 'bbdb-define-all-aliases-needs-rebuilt t)))

(put 'company 'field-separator "; ")
(put 'notes 'field-separator "\n")

;; Build reading and setting functions for location, area, exchange,
;; suffix, and extension.  These are for accessing the elements of the
;; individual phone number forms.
(bbdb-defstruct bbdb-phone-
  location area exchange suffix extension
  )

;; Build reading and setting functions for location, street, city,
;; state, zip and country.  These are for accessing the elements of
;; the individual address forms.
(bbdb-defstruct bbdb-address-
  location streets city state zip country
  )

;; Build reading and setting functions for namecache (the full name of
;; the person referred to by the record), sortkey (the concatenation
;; of the elements used for sorting the record), marker, and
;; deleted-p.  These are for accessing the elements of the cache form,
;; and are generally concatenations of data existing in separate parts
;; of the record, stored here prebuilt for speed.
(bbdb-defstruct bbdb-cache-
  namecache sortkey marker deleted-p
  )

;; Build the namecache for a record
(defsubst bbdb-record-name-1 (record)
  (bbdb-cache-set-namecache (bbdb-record-cache record)
    (let ((fname (bbdb-record-firstname record))
          (lname (bbdb-record-lastname record)))
      (if (> (length fname) 0)
          (if (> (length lname) 0)
              (concat fname " " lname)
            fname)
        lname))))

;; Return the full name from a record.  If the name is not available
;; in the namecache, the namecache value is generated (and stored).
(defun bbdb-record-name (record)
  (or (bbdb-cache-namecache (bbdb-record-cache record))
      (bbdb-record-name-1 record)))

;; Return the sortkey for a record, building (and storing) it if
;; necessary.
(defun bbdb-record-sortkey (record)
  (or (bbdb-cache-sortkey (bbdb-record-cache record))
      (bbdb-cache-set-sortkey (bbdb-record-cache record)
        (downcase
          (concat (bbdb-record-lastname record)
                  (bbdb-record-firstname record)
                  (bbdb-record-company record))))))

(defmacro bbdb-record-marker (record)
  (list 'bbdb-cache-marker (list 'bbdb-record-cache record)))

(defmacro bbdb-record-deleted-p (record)
  (list 'bbdb-cache-deleted-p (list 'bbdb-record-cache record)))

(defmacro bbdb-record-set-deleted-p (record val)
  (list 'bbdb-cache-set-deleted-p (list 'bbdb-record-cache record) val))

(defmacro bbdb-record-set-namecache (record newval)
  (list 'bbdb-cache-set-namecache (list 'bbdb-record-cache record) newval))

(defmacro bbdb-record-set-sortkey (record newval)
  (list 'bbdb-cache-set-sortkey (list 'bbdb-record-cache record) newval))

(defmacro bbdb-record-set-marker (record newval)
  (list 'bbdb-cache-set-marker (list 'bbdb-record-cache record) newval))


;; The "notes" and "properties" accessors don't need to be fast.

(defun bbdb-record-notes (record)
  (if (consp (bbdb-record-raw-notes record))
      (cdr (assq 'notes (bbdb-record-raw-notes record)))
      (bbdb-record-raw-notes record)))

;; this works on the 'company field as well.
(defun bbdb-record-getprop (record property)
  (if (memq property '(name address addresses phone phones net aka AKA))
      (error "bbdb: cannot access the %s field this way" property))
  (if (eq property 'company)
      (bbdb-record-company record)
    (if (consp (bbdb-record-raw-notes record))
        (cdr (assq property (bbdb-record-raw-notes record)))
      (if (and (eq property 'notes)
               (stringp (bbdb-record-raw-notes record)))
          (bbdb-record-raw-notes record)
        nil))))

(defun bbdb-get-field (rec field &optional nn)
  "Get the N-th element (or all if nil) of the notes FIELD of the REC.
If the note is absent, returns a zero length string."
  (let ((note (or (bbdb-record-getprop rec field) "")))
    (if nn
        (nth nn (split-string note " ,;\t\n\f\r\v"))
        note)))

;; this works on the 'company field as well.
(defun bbdb-record-putprop (record property newval)
  (if (memq property '(name address addresses phone phones net aka AKA))
      (error "bbdb: cannot annotate the %s field this way" property))
  (if (eq property 'company)
      (bbdb-record-set-company record
        (bbdb-record-set-company record newval))
    (if (and (eq property 'notes)
             (not (consp (bbdb-record-raw-notes record))))
        (bbdb-record-set-raw-notes record newval)
      (or (listp (bbdb-record-raw-notes record))
          (bbdb-record-set-raw-notes record
            (list (cons 'notes (bbdb-record-raw-notes record)))))
      (let ((old (assq property (bbdb-record-raw-notes record))))
        (if old
            (if newval
                (setcdr old newval)
              (bbdb-record-set-raw-notes record
                (delq old (bbdb-record-raw-notes record))))
          (and newval
               (bbdb-record-set-raw-notes record
                 (append (bbdb-record-raw-notes record)
                         (list (cons property newval))))))))
    ;; save some file space: if we ever end up with ((notes . "...")),
    ;; replace it with the string.
    (if (and (consp (bbdb-record-raw-notes record))
             (null (cdr (bbdb-record-raw-notes record)))
             (eq 'notes (car (car (bbdb-record-raw-notes record)))))
        (bbdb-record-set-raw-notes record
          (cdr (car (bbdb-record-raw-notes record)))))
    )
  ;; If we're changing the company, then we need to sort, since the company
  ;; is the sortkey for nameless records.  This should almost never matter...
  (bbdb-change-record record (eq property 'company))
  newval)

(defun bbdb-record-set-notes (record newval)
  (if (consp (bbdb-record-raw-notes record))
      (bbdb-record-putprop record 'notes newval)
    (bbdb-record-set-raw-notes record newval)
    (bbdb-change-record record nil)))

(defun bbdb-phone-string (phone)
  (if (= 2 (length phone)) ; euronumbers....
      (aref phone 1)
    ;; numbers should come in two forms:
    ;; ["where" 415 555 1212 99] or ["where" "the number"]
    (if (stringp (aref phone 1))
        (error "doubleplus ungood: euronumbers unwork"))
    (concat (if (/= 0 (bbdb-phone-area phone))
                (format "(%03d) " (bbdb-phone-area phone))
                "")
            (if (/= 0 (bbdb-phone-exchange phone))
                (format "%03d-%04d"
                        (bbdb-phone-exchange phone) (bbdb-phone-suffix phone))
                "")
            (if (and (bbdb-phone-extension phone)
                     (/= 0 (bbdb-phone-extension phone)))
                (format " x%d" (bbdb-phone-extension phone))
                ""))))

;; Legacy function.  Used to convert a zip datastructure string into a
;; formated string.  As zip codes are plain strings now, use
;; `bbdb-address-zip' instead.
(defalias 'bbdb-address-zip-string 'bbdb-address-zip)

(defmacro bbdb-record-lessp (record1 record2)
  (list 'string< (list 'bbdb-record-sortkey record1)
                 (list 'bbdb-record-sortkey record2)))

(defmacro bbdb-subint (string match-number)
  (list 'string-to-number
        (list 'substring string
              (list 'match-beginning match-number)
              (list 'match-end match-number))))

(eval-and-compile
  (if (fboundp 'display-error)
      (fset 'bbdb-display-error 'display-error)
    (defun bbdb-display-error(msg stream)
      (message "Error: %s" (nth 1 msg)))))

(defmacro bbdb-error-retry (form)
  (list 'catch ''--bbdb-error-retry--
        (list 'while ''t
              (list 'condition-case '--c--
                    (list 'throw ''--bbdb-error-retry-- form)
                    '(error
                      (ding)
                      (let ((cursor-in-echo-area t))
                        (bbdb-display-error --c-- nil)
                        (sit-for 2)))))))

;;; Completion on labels and field data

;;; Realistically speaking, it doesn't make sense to offer minibuffer
;;; completion for some fields - like ones that don't have labels!
;;;
;;; Also, I could probably do this with macros similar to the
;;; def-struct stuff.
(defcustom bbdb-default-label-list
 '("Home" "Office" "Mobile" "Other")
  "*Default list of labels for Address and Phone fields."
  :group 'bbdb-record-creation
  :type '(repeat string))

(defcustom bbdb-phones-label-list
  bbdb-default-label-list
  "*List of labels for Phone field.
The default value is `bbdb-default-label-list'."
  :group 'bbdb-record-creation
  :type '(repeat string))

(defcustom bbdb-addresses-label-list
  bbdb-default-label-list
  "*List of labels for Address field.
The default value is `bbdb-default-label-list'."
  :group 'bbdb-record-creation
  :type '(repeat string))

(defun bbdb-label-completion-list (field)
  "Figure out a completion list for the specified FIELD label.
This evaluates the variable bbdb-FIELD-label-list, such
as `bbdb-phones-label-list'."
  (if (boundp (intern (format "bbdb-%s-label-list" field)))
      (eval (intern (format "bbdb-%s-label-list" field)))
    ;; special-case out the ones it doesn't make sense for here?
    bbdb-default-label-list))

(defun bbdb-label-completion-default (field)
  "Figure out a default label from the completion list for FIELD.
This evaluates the variable bbdb-default-FIELD-label, such
as `bbdb-default-phones-label', if it exists, or it takes
the first item from the list of completions for FIELD as
returned by `bbdb-label-completion-list'."
  (if (boundp (intern (format "bbdb-default-%s-label" field)))
      (eval (intern (format "bbdb-default-%s-label" field)))
    (nth 0 (bbdb-label-completion-list field))))

;; These are so you can accumulate e.g. mail aliases or company names
;; and have BBDB offer completion on them.
(defun bbdb-data-completion-list (field)
  "Figure out a completion list for the specified FIELD value.
This evaluates the variable bbdb-FIELD-data-list, such
as `bbdb-mail-alias-data-list', if it exists, or it uses
`bbdb-default-label-list'."
  (if (boundp (intern (format "bbdb-%s-data-list" field)))
      (eval (intern (format "bbdb-%s-data-list" field)))
    ;; special-case out the ones it doesn't make sense for here?
    bbdb-default-label-list))

(defun bbdb-data-completion-default (field)
  "Figure out a default value from the completion list for FIELD.
This evaluates the variable bbdb-default-FIELD-data, such
as `bbdb-default-mail-alias-data', if it exists, or it takes
the first item from the list of completions for FIELD as
returned by `bbdb-data-completion-list'."
  (if (boundp (intern (format "bbdb-default-%s-data" field)))
      (eval (intern (format "bbdb-default-%s-data" field)))
    (nth 0 (bbdb-label-completion-list field))))

;;;
(defvar bbdb-buffer nil)
(defun bbdb-buffer ()
  (if (and bbdb-buffer (buffer-live-p bbdb-buffer))
      bbdb-buffer
    (when (and bbdb-file-remote
               (file-newer-than-file-p bbdb-file-remote bbdb-file))
      (let ((coding-system-for-write bbdb-file-coding-system))
    (copy-file bbdb-file-remote bbdb-file t t)))
    (setq bbdb-buffer (find-file-noselect bbdb-file 'nowarn))))

(defmacro bbdb-with-db-buffer (&rest body)
  (cons 'with-current-buffer
        (cons '(bbdb-buffer)
              (if (and (boundp 'bbdb-debug) bbdb-debug)
                  ;; if we're debugging, and the .bbdb buffer is visible in
                  ;; a window, temporarilly switch to that window so that
                  ;; when we come out, that window has been scrolled to the
                  ;; record we've just modified.  (make w-point = b-point)
                  (list
                    (list 'let '((w (and bbdb-debug
                                         (get-buffer-window
                                          (buffer-name
                                           (get-buffer bbdb-file))))))
                          (list 'save-excursion
                            (cons 'save-window-excursion
                                  (cons '(and w (select-window w))
                                        body)))))
                  body))))

(defsubst bbdb-string-trim (string)
  "Lose leading and trailing whitespace.  Also remove all properties
from string."
  (if (string-match "\\`[ \t\n]+" string)
      (setq string (substring string (match-end 0))))
  (if (string-match "[ \t\n]+\\'" string)
      (setq string (substring string 0 (match-beginning 0))))
  ;; This is not ideologically blasphemous.  It is a bad function to
  ;; use on regions of a buffer, but since this is our string, we can
  ;; do whatever we want with it. --Colin
  (set-text-properties 0 (length string) nil string)
  string)

(defun bbdb-read-string (prompt &optional default completions)
  "Reads a string, trimming whitespace and text properties."
  (bbdb-string-trim
   (if completions
       (completing-read prompt completions nil nil (cons default 0))
     (bbdb-string-trim (read-string prompt default)))))

;;; Address formatting.

(defcustom bbdb-time-display-format "%d %b %Y"
  "The format for the timestamp to be used in the creation-date and
timestamp fields.  See the documentation for `format-time-string'."
  :group 'bbdb :type 'string)

(defun bbdb-time-convert (date &optional format)
  "Convert a date from the BBDB internal format to the format
determined by FORMAT (or `bbdb-time-display-format' if FORMAT not
present).  Returns a string containing the date in the new format."
  (let ((parts (bbdb-split date "-")))
    (format-time-string (or format bbdb-time-display-format)
                        (encode-time 0 0 0 (string-to-number (caddr parts))
                                     (string-to-number (cadr parts))
                                     (string-to-number (car parts))))))

(defalias 'bbdb-format-record-timestamp 'bbdb-time-convert)
(defalias 'bbdb-format-record-creation-date 'bbdb-time-convert)

(defconst bbdb-gag-messages nil
  "Bind this to t to quiet things down - do not set it!")

(defconst bbdb-buffer-name "*BBDB*")

(defcustom bbdb-display-layout-alist
  '((one-line   (order     . (phones mail-alias net notes))
                (name-end  . 24)
                (toggle    . t))
    (multi-line (omit      . (creation-date timestamp))
                (toggle    . t))
    (pop-up-multi-line)
    (full-multi-line))
  "*An alist describing each display layout.
The format of an element is (LAYOUT-NAME OPTION-ALIST).

By default there are four different layout types used by BBDB, which are
`one-line', `multi-line', `pop-up-multi-line' (used for pop-ups) and
`full-multi-line' (showing all fields of a record).

OPTION-ALIST specifies the options for the layout.  Valid options are:

                           ------- Availability --------
    Format                  one-line        multi-line        default if unset
------------------------------------------------------------------------------
 (toggle . BOOL)                 +               +              nil
 (order . FIELD-LIST)            +               +              '(phones ...)
 (omit . FIELD-LIST)             +               +              nil
 (name-end . INTEGER)            +               -              40
 (indentation . INTEGER)         -               +              14
 (primary . BOOL)                -               +              nil
 (test . SEXP)                   +               +              nil

- toggle: controls if this layout is included when toggeling the display layout
- order: defines a user specific order for the fields, where `t' is a place
  holder for all remaining fields
- omit: is a list of fields which should not be displayed or `t' to exclude all
  fields except those listed in the order option
- name-end: sets the column where the name should end in one-line layout.
- indentation: sets the level of indentation for multi-line display.
- primary: controls wether only the primary net is shown or all are shown.
- test: a lisp expression controlling wether the record is to be displayed.

When you add a new layout FOO, you can write a corresponding layout
function bbdb-format-record-layout-FOO.  If you do not write your own
layout function, the multi-line layout will be used."
  :group 'bbdb
  :type
  `(repeat
    (cons :tag "Layout Definition"
          (choice :tag "Layout type"
                  (const one-line)
                  (const multi-line)
          (const pop-up-multi-line)
                  (const full-multi-line)
                  (symbol))
          (set :tag "Properties"
               (cons :tag "Order"
                     (const :tag "List of fields to order by" order)
                     (repeat (choice (const phones)
                                     (const addresses)
                                     (const net)
                                     (const AKA)
                                     (const notes)
                                     (symbol :tag "other")
                                     (const :tag "Remaining fields" t))))
               (choice :tag "Omit"
                       :value (omit . nil)
                       (cons :tag "List of fields to omit"
                             (const :tag "Fields not to display" omit)
                             (repeat (choice (const phones)
                                             (const addresses)
                                             (const net)
                                             (const AKA)
                                             (const notes)
                                             (symbol :tag "other"))))
                       (const :tag "Exclude all fields except those listed in the order property" t))
               (cons :tag "Indentation"
                     :value (indentation . 14)
                     (const :tag "Level of indentation for multi-line layout"
                            indentation)
                     (number :tag "Column"))
               (cons :tag "End of name field"
                     :value (name-end . 24)
                     (const :tag "The column where the name should end in one-line layout"
                            name-end)
                     (number :tag "Column"))
               (cons :tag "Toggle"
                     (const :tag "The layout is included when toggling display layout" toggle)
                     boolean)
           (cons :tag "Primary Net Only"
             (const :tag "Only the primary net address is included" primary)
             boolean)
           (cons :tag "Test"
             (const :tag "Show only records passing this test" test)
             (choice (const :tag "No test" nil)
                 (cons :tag "List of required fields"
                   (const :tag "Choose from the attributes in the following set:" and)
                   (set
                    (const name)
                    (const company)
                    (const net)
                    (const phones)
                    (const addresses)
                    (const notes)))
                 (sexp :tag "Lisp expression")))))))


(defcustom bbdb-display-layout 'multi-line
  "*The default display layout."
  :group 'bbdb
  :type '(choice (const one-line)
                 (const multi-line)
                 (const full-multi-line)
                 (symbol)))

(defcustom bbdb-pop-up-display-layout 'pop-up-multi-line
  "*The default display layout pop-up BBDB buffers, i.e. mail, news."
  :group 'bbdb
  :type '(choice (const one-line)
                 (const multi-line)
                 (const full-multi-line)
                 (symbol)))

(defun bbdb-display-layout-get-option (layout option)
  (let ((layout-spec (if (listp layout)
                         layout
                       (assoc layout bbdb-display-layout-alist)))
        option-value)
    (and layout-spec
         (setq option-value (assoc option layout-spec))
         (cdr option-value))))

(defcustom bbdb-address-formatting-alist
  '((bbdb-address-is-continental . bbdb-format-address-continental)
    (nil . bbdb-format-address-default))
  "Alist of address identifying and address formatting functions.
The key is an identifying function which accepts an address.  The
associated value is a formatting function which inserts the formatted
address in the current buffer.  If the identifying function returns
non-nil, the formatting function is called.  When nil is used as the
car, then the associated formatting function will always be called.
Therefore you should always have (nil . bbdb-format-address-default) as
the last element in the alist.

All functions should take two arguments, the address and an indentation.
The indentation argument may be optional.

This alist is used in `bbdb-format-address'.

See also `bbdb-address-print-formatting-alist'."
  :group 'bbdb-record-display
  :type '(repeat (cons function function)))

(defvar bbdb-address-print-formatting-alist) ; "bbdb-print"

(defun bbdb-address-is-continental (addr)
  "Return non-nil if the address ADDR is a continental address.
This is done by comparing the zip code to `bbdb-continental-zip-regexp'.

This is a possible identifying function for
`bbdb-address-formatting-alist' and
`bbdb-address-print-formatting-alist'."
  (string-match bbdb-continental-zip-regexp (bbdb-address-zip addr)))

(defun bbdb-format-streets (addr indent)
  "Insert street subfields of address ADDR in current buffer.
This may be used by formatting functions listed in
`bbdb-address-formatting-alist'."
  (bbdb-mapc (lambda(str)
           (indent-to indent)
           (insert str "\n"))
         (bbdb-address-streets addr)))

(defun bbdb-format-address-continental (addr &optional indent)
  "Insert formated continental address ADDR in current buffer.
This format is used in western Europe, for example.

This function is a possible formatting function for
`bbdb-address-formatting-alist'.

The result looks like this:
       location: street
                 street
                 ...
                 zip city, state
                 country"
  (setq indent (or indent 14))
  (let (;(fmt (format " %%%ds: " indent))
        (indent (+ 3 indent)))
    ;(insert (format fmt (bbdb-address-location addr)))
    (bbdb-format-streets addr indent)
    (let ((c (bbdb-address-city addr))
          (s (bbdb-address-state addr))
          (z (bbdb-address-zip addr)))
      (if (or (> (length c) 0)
              (> (length z) 0)
              (> (length s) 0))
          (progn
            (indent-to indent)
            (insert z (if (and (> (length z) 0)
                               (> (length c) 0)) " " "")
                    c (if (and (or (> (length z) 0)
                                   (> (length c) 0))
                               (> (length s) 0)) ", " "")
                    s "\n"))))
    (let ((str (bbdb-address-country addr)))
      (if (= 0 (length str)) nil
        (indent-to indent) (insert str "\n")))))

(defun bbdb-format-address-default (addr &optional indent)
  "Insert formated address ADDR in current buffer.
This is the default format; it is used in the US, for example.

This function is a possible formatting function for
`bbdb-address-formatting-alist'.

The result looks like this:
       location: street
                 street
                 ...
                 city, state  zip
                 country"
  (setq indent (or indent 14))
  (let (;(fmt (format " %%%ds: " indent))
        (indent (+ 3 indent)))
;    (insert (format fmt (bbdb-address-location addr)))
    (bbdb-format-streets addr indent)
    (let ((c (bbdb-address-city addr))
          (s (bbdb-address-state addr))
          (z (bbdb-address-zip addr)))
      (if (or (> (length c) 0)
              (> (length z) 0)
              (> (length s) 0))
          (progn
            (indent-to indent)
            (insert c (if (and (> (length c) 0)
                               (> (length s) 0)) ", " "")
                    s (if (and (or (> (length c) 0)
                                   (> (length s) 0))
                               (> (length z) 0)) "  " "")
                    z "\n"))))
    (let ((str (bbdb-address-country addr)))
      (if (= 0 (length str)) nil
        (indent-to indent) (insert str "\n")))))

(defun bbdb-format-address (addr &optional printing indent)
  "Call appropriate formatting function for address ADDR.

If optional second argument PRINTING is non-nil, this uses the alist
`bbdb-address-print-formatting-alist' to determine how the address is to
formatted and inserted into the current buffer. This is used by
`bbdb-print-format-record'.

If second argument PRINTING is nil, this uses the alist
`bbdb-address-formatting-alist' to determine how the address is to
formatted and inserted into the current buffer.  This is used by
`bbdb-format-record'."
  ;; alist contains functions ((ident1 . format1) (ident2 . format2) ...)
  ;; the first identifying-function is (caar alist)
  ;; the first formatting-function is  (cdar alist)
  (let ((alist (if printing bbdb-address-print-formatting-alist
                 bbdb-address-formatting-alist)))
    ;; while there a functions left and the current function does not
    ;; identify the address, try the next function.
    (while (and (caar alist)
                (null (funcall (caar alist) addr)))
      (setq alist (cdr alist)))
    ;; if we haven't reached the end of functions, we got a hit.
    (when alist
      (if printing
      (funcall (cdar alist) addr)
    (funcall (cdar alist) addr indent)))))

(defun bbdb-format-record-name-company (record)
  (let ((name (or (bbdb-record-name record) "???"))
        (company (bbdb-record-company record))
        (start (point)))

    (insert name)
    (put-text-property start (point) 'bbdb-field '(name))

    (when company
      (insert " - ")
      (setq start (point))
      (insert company)
      (put-text-property start (point) 'bbdb-field '(company)))))

(defun bbdb-format-record-one-line-phones (layout record phone)
  "Insert a formatted phone number for one-line display."
  (let ((start (point)))
    (insert (format "%s " (aref phone 1)))
    (put-text-property start (point) 'bbdb-field
                       (list 'phone phone (aref phone 0)))
    (setq start (point))
    (insert (format "(%s)" (aref phone 0)))
    (put-text-property start (point) 'bbdb-field
                       (list 'phone phone 'field-name))))

(defun bbdb-format-record-one-line-net (layout record net)
  "Insert a formatted list of nets for one-line display."
  (let ((start (point)))
    (insert net)
    (put-text-property start (point) 'bbdb-field (list 'net net))))

(defun bbdb-format-record-one-line-notes (layout record notes)
  "Insert formatted notes for one-line display.
Line breaks will be removed and white space trimmed."
  (let ((start (point)))
    (insert (bbdb-replace-in-string notes "[\r\n\t ]+" " "))
    (put-text-property start (point) 'bbdb-field (list 'notes notes))))

(defun bbdb-format-record-layout-one-line (layout record field-list)
  "Record formatting function for the one-line layout.
See `bbdb-display-layout-alist' for more."
  ;; name and company
  (bbdb-format-record-name-company record)
  (let ((name-end (or (bbdb-display-layout-get-option layout 'name-end)
                      40))
        start end)
    (save-excursion
      (setq end (point))
      (beginning-of-line)
      (setq start (point)))
    (when (> (- end start -1) name-end)
      (put-text-property (+ start name-end -4) end 'invisible t)
      (insert "..."))
    ;; guarantee one space after name - company
    (insert " ")
    (indent-to name-end))
  ;; rest of the fields
  (let (start field contentfun formatfun values value)
    (while field-list
      (setq field (car field-list)
            contentfun (intern (concat "bbdb-record-"
                                       (symbol-name field))))
      (if (fboundp contentfun)
          (setq values (eval (list contentfun record)))
        (setq values (bbdb-record-getprop record field)))
      (when (and (eq field 'net)
         (bbdb-display-layout-get-option layout 'primary))
    (setq values (list (car values))))
      (when values
        (if (not (listp values)) (setq values (list values)))
        (setq formatfun (intern (format "bbdb-format-record-%s-%s"
                                        layout field)))
        (while values
          (setq start (point)
                value (car values))
          (if (fboundp formatfun)
              (funcall formatfun layout record value)
            (insert (format "%s" value))
            (cond ((eq field 'addresses)
                   (put-text-property start (point) 'bbdb-field
                                      (list 'address value)))
                  ((eq field 'phones)
                   (put-text-property start (point) 'bbdb-field
                                      (list 'phone value)))
                  ((memq field '(name net aka))
                   (put-text-property start (point) 'bbdb-field
                                      (list field value )))
                  (t
                   (put-text-property start (point) 'bbdb-field
                                      (list 'property (list field value))))))
          (setq values (cdr values))
          (if values (insert ", ")))
        (insert "; "))
      (setq field-list (cdr field-list))))
  ;; delete the trailing "; "
  (backward-delete-char 2)
  (insert "\n"))

(defun bbdb-format-record-layout-multi-line (layout record field-list)
  "Record formatting function for the multi-line layout.
See `bbdb-display-layout-alist' for more."
  (bbdb-format-record-name-company record)
  (insert "\n")
  (let* ((notes (bbdb-record-raw-notes record))
         (indent (or (bbdb-display-layout-get-option layout 'indentation) 14))
         (fmt (format " %%%ds: " indent))
         start field)
    (if (stringp notes)
        (setq notes (list (cons 'notes notes))))
    (while field-list
      (setq field (car field-list)
            start (point))
      (cond ((eq field 'phones)
             (let ((phones (bbdb-record-phones record))
                   loc phone)
               (while phones
                 (setq phone (car phones)
                       start (point))
                 (setq loc (format fmt (bbdb-phone-location phone)))
                 (insert loc)
                 (put-text-property start (point) 'bbdb-field
                                    (list 'phone phone 'field-name))
                 (setq start (point))
                 (insert (bbdb-phone-string phone) "\n")
                 (put-text-property start (point) 'bbdb-field
                                    (list 'phone phone
                                          (bbdb-phone-location phone)))
                 (setq phones (cdr phones))))
             (setq start nil))
            ((eq field 'addresses)
             (let ((addrs (bbdb-record-addresses record))
                   loc addr)
               (while addrs
                 (setq addr (car addrs)
                       start (point))
                 (setq loc (format fmt (bbdb-address-location addr)))
                 (insert loc)
                 (put-text-property start (point) 'bbdb-field
                                    (list 'address addr 'field-name))
                 (setq start (point))
                 (bbdb-format-address addr nil indent)
                 (put-text-property start (point) 'bbdb-field
                                    (list 'address addr
                                          (bbdb-address-location addr)))
                 (setq addrs (cdr addrs))))
             (setq start nil))
            ((eq field 'net)
             (let ((net (bbdb-record-net record)))
               (when net
                 (insert (format fmt "net"))
                 (put-text-property start (point) 'bbdb-field
                                    '(net field-name))
                 (setq start (point))
         (if (bbdb-display-layout-get-option layout 'primary)
             (insert (car net) "\n")
           (insert (mapconcat (function identity) net ", ") "\n"))
                 (put-text-property start (point) 'bbdb-field '(net)))))
            ((eq field 'aka)
             (let ((aka (bbdb-record-aka record)))
               (when aka
                 (insert (format fmt "AKA"))
                 (put-text-property start (point) 'bbdb-field
                                    '(aka field-name))
                 (insert (mapconcat (function identity) aka ", ") "\n")
                 (setq start (point))
                 (put-text-property start (point) 'bbdb-field '(aka)))))
            (t
             (let ((note (assoc field notes))
                   (indent (length (format fmt "")))
                   p notefun)
               (when note
                 (insert (format fmt field))
                 (put-text-property start (point) 'bbdb-field
                                    (list 'property note 'field-name))
                 (setq start (point))
                 (setq p (point)
                       notefun (intern (format "bbdb-format-record-%s" field)))
                 (if (fboundp notefun)
                     (insert (funcall notefun (cdr note)))
                   (insert (cdr note)))
                 (save-excursion
                   (save-restriction
                     (narrow-to-region p (1- (point)))
                     (goto-char (1+ p))
                     (while (search-forward "\n" nil t)
                       (insert (make-string indent ?\ )))))
                 (insert "\n"))
               (put-text-property start (point) 'bbdb-field
                                  (list 'property note)))))
      (setq field-list (cdr field-list)))))

(defalias 'bbdb-format-record-layout-full-multi-line
  'bbdb-format-record-layout-multi-line)

(defalias 'bbdb-format-record-layout-pop-up-multi-line
  'bbdb-format-record-layout-multi-line)

(defun bbdb-format-record (record &optional layout)
  "Insert a formatted version of RECORD into the current buffer.

LAYOUT can be a symbol describing a layout in
`bbdb-display-layout-alist'.  For compatibility reasons, LAYOUT can
also be nil or t, where t stands for the one-line, and nil for the
multi-line layout."
  (bbdb-debug (if (bbdb-record-deleted-p record)
                  (error "plus ungood: formatting deleted record")))
  (setq layout (cond ((eq nil layout)
                      'multi-line)
                     ((eq t layout)
                      'one-line)
                     ((symbolp layout)
                      layout)
                     (t
                      (error "Unknown layout `%s'" layout))))
  (let* ((layout-spec (assoc layout bbdb-display-layout-alist))
     (test        (bbdb-display-layout-get-option layout-spec 'test))
         (omit-list   (bbdb-display-layout-get-option layout-spec 'omit))
         (order-list  (bbdb-display-layout-get-option layout-spec 'order))
         (all-fields  (append '(phones addresses net aka)
                              (let ((raw-notes (bbdb-record-raw-notes record)))
                                (if (stringp raw-notes)
                                    '(notes)
                                  (mapcar (lambda (r) (car r)) raw-notes)))))
         format-function field-list)
    (when (or (not test)
          ;; bind some variables for the test
          (let ((name (bbdb-record-name record))
            (company (bbdb-record-company record))
            (net (bbdb-record-net record))
            (phones (bbdb-record-phones record))
            (addresses (bbdb-record-addresses record))
            (notes (bbdb-record-raw-notes record)))
        ;; this must evaluate to non-nil if the record is to be shown
        (eval test)))
      (if (functionp omit-list)
      (setq omit-list (funcall omit-list record layout)))
      (if (functionp order-list)
      (setq order-list (funcall order-list record layout)))
      ;; first omit unwanted fields
      (when (and omit-list (or (not order-list) (memq t order-list)))
    (if (not (listp omit-list))
        ;; t => show nothing
        (setq all-fields nil)
      ;; listp => show all fields except those listed here
      (while omit-list
        (setq all-fields (delete (car omit-list) all-fields)
          omit-list (cdr omit-list)))))
      ;; then order them
      (if (not order-list)
      (setq field-list all-fields)
    (if (not (memq t order-list))
        (setq field-list order-list)
      (setq order-list (reverse order-list))
      (setq all-fields (delete nil (mapcar (lambda (f)
                         (if (memq f order-list)
                             nil
                           f))
                           all-fields)))
      (while order-list
        (if (eq t (car order-list))
        (setq field-list (append all-fields field-list))
          (setq field-list (cons (car order-list) field-list)))
        (setq order-list (cdr order-list)))))
      ;; call the actual format function
      (setq format-function
        (intern (format "bbdb-format-record-layout-%s" layout)))
      (if (functionp format-function)
      (funcall format-function layout record field-list)
    (bbdb-format-record-layout-multi-line layout record field-list)))))

(defun bbdb-frob-mode-line (n)
  (setq
   ;; identification
   mode-line-buffer-identification
   (if (> n 0)
       (list 24 (buffer-name) ": "
             (list 10 (format "%d/%d" n (length (bbdb-records))))
             '(bbdb-showing-changed-ones " !!" "   "))
     (list (buffer-name) ": Insidious Big Brother Database v" bbdb-version " "
           mode-line-modified "-"))
   ;; modified indicator
   mode-line-modified
   '(bbdb-readonly-p "--%%%%-" (bbdb-modified-p "--**-" "-----"))))

(defun bbdb-display-records-1 (records &optional append layout)
  (setq append (or append (bbdb-append-records-p)))

  (if (or (null records)
          (consp (car records)))
      nil

    ;; add layout and a marker to the local list of records
    (setq layout (or layout bbdb-display-layout))
    (setq records (mapcar (lambda (x)
                            (list x layout (make-marker)))
                          records)))

  (let ((b (current-buffer))
        (temp-buffer-setup-hook nil)
        (temp-buffer-show-hook nil)
        (first (car (car records))))

    (if bbdb-multiple-buffers (bbdb-pop-up-bbdb-buffer))

    (with-output-to-temp-buffer bbdb-buffer-name
      (set-buffer bbdb-buffer-name)

      ;; If append is set, clear the buffer, otherwise do clean up.
      (unless append (bbdb-undisplay-records))

      ;; If we're appending these records to the ones already displayed,
      ;; then first remove any duplicates, and then sort them.
      (if append
          (let ((rest records))
            (while rest
              (if (assq (car (car rest)) bbdb-records)
                  (setq records (delq (car rest) records)))
              (setq rest (cdr rest)))
            (setq records (append bbdb-records records))
            (setq records
                  (sort records
                        (lambda (x y) (bbdb-record-lessp (car x) (car y)))))))
      (make-local-variable 'mode-line-buffer-identification)
      (make-local-variable 'mode-line-modified)
      (set (make-local-variable 'bbdb-showing-changed-ones) nil)
      (let ((done nil)
            (rest records)
            (changed (bbdb-changed-records)))
        (while (and rest (not done))
          (setq done (memq (car (car rest)) changed)
                rest (cdr rest)))
        (setq bbdb-showing-changed-ones done))
      (bbdb-frob-mode-line (length records))
      (and (not bbdb-gag-messages)
           (not bbdb-silent-running)
           (message "Formatting..."))
      (bbdb-mode)
      ;; this in in the *BBDB* buffer, remember, not the .bbdb buffer.
      (set (make-local-variable 'bbdb-records) nil)
      (setq bbdb-records records)
      (let ((buffer-read-only nil)
            prs)
        (bbdb-debug (setq prs (bbdb-records)))
        (setq truncate-lines t)
        (while records
          (bbdb-debug (if (not (memq (car (car records)) prs))
                          (error "record doubleplus unpresent!")))
          (set-marker (nth 2 (car records)) (point))
          (bbdb-format-record (nth 0 (car records))
                              (nth 1 (car records)))
          (setq records (cdr records))))
      (and (not bbdb-gag-messages)
           (not bbdb-silent-running)
           (message "Formatting...done.")))
    (set-buffer bbdb-buffer-name)
    (if (and append first)
        (let ((cons (assq first bbdb-records))
              (window (get-buffer-window (current-buffer))))
          (if window (set-window-start window (nth 2 cons)))))
    (bbdbq)
    ;; this doesn't really belong here, but it's convenient ... and when
    ;; using electric display it would not be called otherwise.
    (save-excursion (run-hooks 'bbdb-list-hook))
    (if bbdb-gui (bbdb-fontify-buffer))
    (set-buffer-modified-p nil)
    (setq buffer-read-only t)
    (set-buffer b)))

(defun bbdb-undisplay-records ()
  (let ((bbdb-display-buffer (get-buffer bbdb-buffer-name)))
    (if (bufferp bbdb-display-buffer)
        (save-excursion
          (set-buffer bbdb-display-buffer)
          (setq bbdb-showing-changed-ones nil
                mode-line-modified nil
                bbdb-records nil
                buffer-read-only nil)
          (erase-buffer)
          (setq buffer-read-only t)
          (set-buffer-modified-p nil)))))

;;; Electric display stuff

(defconst bbdb-inside-electric-display nil)
;; hack hack: a couple of specials that the electric stuff uses for state.
(defvar bbdb-electric-execute-me)
(defvar bbdb-electric-completed-normally)

(defun electric-bbdb-display-records (records)
  (require 'electric)
  (let ((bbdb-electric-execute-me nil))   ; Hack alert!  throw-to-execute sets this!
   (let ((bbdb-inside-electric-display t)
         buffer
         bbdb-electric-completed-normally ; Hack alert!  throw-to-execute sets this!
         )
    (save-excursion
     (save-window-excursion
      (save-window-excursion (bbdb-display-records-1 records))
      (setq buffer (window-buffer (Electric-pop-up-window bbdb-buffer-name)))
      (set-buffer buffer)
      (if (not bbdb-gag-messages)
          (message "<<< Press Space to bury the Insidious Big Brother Database list >>>"))
      (catch 'Done
        (while t
          (catch 'Blow-off-the-error
            (setq bbdb-electric-completed-normally nil)
            (unwind-protect
                (progn
                   (catch 'electric-bbdb-list-select
                     (Electric-command-loop 'electric-bbdb-list-select
                                            "-> " t))
                   (setq bbdb-electric-completed-normally t))
              ;; protected
              (if bbdb-electric-completed-normally
                  (throw 'Done t)
                (ding)
                (message "BBDB-Quit")
                (throw 'Blow-off-the-error t)
                )))))
      (bury-buffer buffer))))
   (message " ")
   (if bbdb-electric-execute-me
       (eval bbdb-electric-execute-me)))
  nil)

(defun bbdb-electric-throw-to-execute (form-to-execute)
  "Exit the `electric-command-loop' and evaluate the given form."
  ;; Hack alert!  These variables are bound only within the scope of
  ;; bbdb-electric-display-records!
  (if (not (boundp 'bbdb-electric-execute-me))
      (error "plusungood: electrical short"))
  (setq bbdb-electric-execute-me form-to-execute
        bbdb-electric-completed-normally t)
  (throw 'electric-bbdb-list-select t))


(defun bbdb-done-command () (interactive)
  (throw 'electric-bbdb-list-select t))

(defun bbdb-bury-buffer ()
  (interactive)
  (if bbdb-inside-electric-display
      (bbdb-done-command)
    (bury-buffer)))

(defun bbdb-display-records (records &optional layout append)
  (let ((bbdb-window (get-buffer-window bbdb-buffer-name)))
    (if (and bbdb-electric-p
             ;; never be electric if the buffer is already on screen.
             (not bbdb-window))
        (progn
          (define-key bbdb-mode-map " " 'bbdb-done-command)
          (electric-bbdb-display-records records))
      (bbdb-display-records-1 records append layout)
      ;; don't smash keybinding if they invoked `bbdb-display'
      ;; from inside an electric loop.
      (unless bbdb-inside-electric-display
        (define-key bbdb-mode-map " " 'undefined))
      (if (and (not bbdb-gag-messages)
               (not bbdb-window))
          (message
           (substitute-command-keys
            (if (one-window-p t)
                (if pop-up-windows
                    "Type \\[delete-other-windows] to unshow the bbdb-list window."
                    "Type \\[switch-to-buffer] RET to unshow the bbdb-list window.")
                "Type \\[switch-to-buffer-other-window] RET to restore old contents of the bbdb-list window.")))))))

(defun bbdbq ()
  (if (not (zerop (logand (random) 31))) nil
    (let ((v '["\104\157\156\47\164\40\163\165\163\160\145\143\164\40\171\157\
\165\162\40\156\145\151\147\150\142\157\162\72\40\162\145\160\157\162\164\40\
\150\151\155\41" "\146\156\157\162\144" "\103\157\156\163\165\155\145\40\55\55\
\40\102\145\40\123\151\154\145\156\164\40\55\55\40\104\151\145" "\114\157\166\
\145\40\102\151\147\40\102\162\157\164\150\145\162" "\114\145\145\40\110\141\
\162\166\145\171\40\117\163\167\141\154\144\40\141\143\164\145\144\40\141\154\
\157\156\145" "\101\114\114\40\131\117\125\122\40\102\101\123\105\40\101\122\
\105\40\102\105\114\117\116\107\40\124\117\40\125\123" "\127\141\162\40\151\
\163\40\120\145\141\143\145" "\106\162\145\145\144\157\155\40\151\163\40\123\
\154\141\166\145\162\171" "\111\147\156\157\162\141\156\143\145\40\151\163\40\
\123\164\162\145\156\147\164\150" "\120\162\157\154\145\163\40\141\156\144\40\
\141\156\151\155\141\154\163\40\141\162\145\40\146\162\145\145"]))
      (message (aref v (% (logand 255 (random)) (length v))))
      (message " "))))

(defmacro bbdb-hashtable ()
  '(bbdb-with-db-buffer (bbdb-records nil t) bbdb-hashtable))

(defun bbdb-changed-records ()
  (bbdb-with-db-buffer (bbdb-records nil t) bbdb-changed-records))

(defmacro bbdb-build-name (f l)
  (list 'downcase
        (list 'if (list '= (list 'length f) 0) l
              (list 'if (list '= (list 'length l) 0) f
                    (list 'concat f " " l)))))

(defun bbdb-remove! (e l)
  (if (null l) l
    (let ((ret l)
          (n   (cdr l)))
      (while n
        (if (eq e (car n))
            (setcdr l (cdr n)) ; skip n
          (setq l n))          ; keep n
        (setq n (cdr n)))
      (if (eq e (car ret)) (cdr ret)
        ret))))

(defun bbdb-remove-memq-duplicates (l)
  (let (ret tail)
    (setq ret (cons '() '())
          tail ret)
    (while l
      (if (not (memq (car l) ret))
          (setq tail (setcdr tail (cons (car l) '()))))
      (setq l (cdr l)))
    (cdr ret)))

(defmacro bbdb-gethash (name &optional ht)
  (list 'symbol-value
        (list 'intern-soft name
              (or ht '(bbdb-hashtable)))))

(defmacro bbdb-puthash (name record &optional ht)
  (list 'let (list (list 'sym (list 'intern name (or ht '(bbdb-hashtable)))))
        (list 'set 'sym (list 'cons record
                              '(and (boundp sym) (symbol-value sym))))))

(defmacro bbdb-remhash (name record &optional ht)
  (list 'let (list (list 's (list 'intern-soft name
                                  (or ht '(bbdb-hashtable)))))
        (list 'and 's (list 'set 's (list 'bbdb-remove! record
                                          (list 'symbol-value 's))))))

(defsubst bbdb-search-intertwingle (name net)
  "Find bbdb records matching NAME and NET.

This is a more stringent version of bbdb-search-simple, which I am
not inclined to modify for fear of damaging other code that currently
relies on it. BBDB internals should be migrated to use this function
to identify which record is referred to by a name/net combination,
since search-simple has been overloaded with other functionality.

The name comes from
http://www.mozilla.org/blue-sky/misc/199805/intertwingle.html, which
any budding BBDB hacker should be at least vaguely familiar with."
  (bbdb-records t)
  (if name (setq name (downcase name)))
  (if net (setq net (downcase net))
    (setq net ""))
  (let ((net-recs (bbdb-gethash (downcase net)))
        recs)
    (while net-recs
      (if (or (and (not name) net)
              (string= name (downcase (bbdb-record-name (car net-recs)))))
          (add-to-list 'recs (car net-recs)))
      (setq net-recs (cdr net-recs)))
    recs))

(defsubst bbdb-search-simple (name net)
  "name is a string; net is a string or list of strings."
  (if (eq 0 (length name)) (setq name nil))
  (if (eq 0 (length net)) (setq net nil))
  (bbdb-records t) ; make sure db is parsed; don't check disk (faster)
  (let ((name-recs (if name ;; filter out companies from hash
                       (let ((recs (bbdb-gethash (downcase name)))
                             answer)
                         (while recs
                           (let ((n-rec (car recs)))
                             (if (string= (downcase name)
                                          (downcase
                                           (or (bbdb-record-name
                                                n-rec)
                                               (bbdb-record-company
                                                n-rec)
                                               "")))
                                 (setq answer (append recs (list n-rec))))
                             (setq recs (cdr recs))))
                         answer)))
        (net-recs  (if (stringp net) (bbdb-gethash (downcase net))
                     (let (answer)
                       (while (and net (null answer))
                         (setq answer (bbdb-gethash (downcase (car net)))
                               net (cdr net)))
                       answer)))
        ret)
    (if (not (and name-recs net-recs))
        (or (and name-recs (car name-recs))
            (and net-recs (car net-recs)))

      (while name-recs
        (let ((name-rec (car name-recs))
              (nets     net-recs))
          (while nets
            (if (eq (car nets) name-rec)
                (setq nets      '()
                      name-recs '()
                      ret name-rec)
              (setq nets (cdr nets))))
          (if name-recs (setq name-recs (cdr name-recs))
              name-rec)))
      ret)))

(defun bbdb-net-convert (record)
  "Given a record whose net field is a comma-separated string, convert it to
a list of strings (the new way of doing things.)  Returns the new list."
  (bbdb-record-set-net record (bbdb-split (bbdb-record-net record) ",")))

(defun bbdb-split (string separators)
  "Return a list by splitting STRING at SEPARATORS.
The inverse function of `bbdb-join'."
  (let (result
        (not-separators (concat "^" separators)))
    (save-excursion
      (set-buffer (get-buffer-create " *split*"))
      (erase-buffer)
      (insert string)
      (goto-char (point-min))
      (while (progn
               (skip-chars-forward separators)
               (skip-chars-forward " \t\n\r")
               (not (eobp)))
        (let ((begin (point))
              p)
          (skip-chars-forward not-separators)
          (setq p (point))
          (skip-chars-backward " \t\n\r")
          (setq result (cons (buffer-substring begin (point)) result))
          (goto-char p)))
      (erase-buffer))
    (nreverse result)))

(defun bbdb-join (list separator)
  "Join a LIST to a string where the list elements are separated by SEPARATOR.
The inverse function of `bbdb-split'."
  (when list
    (mapconcat 'identity list separator)))

(defsubst bbdb-hash-record (record)
  "Insert the record in the appropriate hashtables.  This must be called
while the .bbdb buffer is selected."
  (let ((name    (bbdb-record-name-1  record))  ; faster version
        (company (bbdb-record-company record))
        (aka     (bbdb-record-aka     record))
        (net     (bbdb-record-net     record)))
    (if (> (length name) 0)
        (bbdb-puthash (downcase name)    record bbdb-hashtable))
    (if (> (length company) 0)
        (bbdb-puthash (downcase company) record bbdb-hashtable))
    (while aka
      (bbdb-puthash (downcase (car aka)) record bbdb-hashtable)
      (setq aka (cdr aka)))
    (while net
      (bbdb-puthash (downcase (car net)) record bbdb-hashtable)
      (setq net (cdr net)))))


;;; Reading the BBDB

(defvar inside-bbdb-records nil
  "Internal variable.  Do not touch.")

(defvar bbdb-write-file-hooks '(bbdb-write-file-hook-fn)
  "*The list of functions added to `local-write-file-hooks' in `bbdb-file'.")

(defun bbdb-records (&optional dont-check-disk already-in-db-buffer)
  "Return a list of all bbdb records; read in and parse the db if necessary.
This also notices if the disk file has changed out from under us, unless
optional arg DONT-CHECK-DISK is non-nil (which is faster, but hazardous.)"
  (if inside-bbdb-records
      (let ((debug-on-error t))
        (error "catastrophic: bbdb-records recursed")))
  (let ((inside-bbdb-records t)
        (buf (if already-in-db-buffer (current-buffer) (bbdb-buffer)))
        shut-up)
    (with-current-buffer buf
      ;; make sure the BBDB in memory is not out of synch with disk.
      (cond (dont-check-disk nil)
            ((verify-visited-file-modtime buf) nil)
            ((and bbdb-auto-revert-p (not (buffer-modified-p buf)))
             (message "BBDB has changed on disk, reverting...")
             (setq shut-up t)
             (revert-buffer t t))
            ;; hassle the user
            ((bbdb-yes-or-no-p
              (if (buffer-modified-p buf)
                  "BBDB has changed on disk; flush your changes and revert? "
                  "BBDB has changed on disk; revert? "))
             (or (file-exists-p bbdb-file)
                 (error "bbdb: file %s no longer exists!!" bbdb-file))
             (revert-buffer t t))
            ;; this is the case where the .bbdb file has changed; the buffer
            ;; has changed as well; and the user has answered "no" to the
            ;; "flush your changes and revert" question.  The only other
            ;; alternative is to save the file right now.  If they answer
            ;; no to the following question, they will be asked the
            ;; preceeding question again and again some large (but finite)
            ;; number of times.  `bbdb-records' is called a lot, you see...
            ((buffer-modified-p buf)
             ;; this prompts
             (bbdb-save-db t t))
            ;; otherwise, the buffer and file are inconsistent, but we let
            ;; them stay that way.
            )
      (unless (assq 'bbdb-records (buffer-local-variables))
        (set (make-local-variable 'bbdb-records) nil)
        (set (make-local-variable 'bbdb-changed-records) nil)
        (set (make-local-variable 'bbdb-end-marker) nil)
        (set (make-local-variable 'bbdb-hashtable) nil)
        (set (make-local-variable 'bbdb-propnames) nil)
        (set (make-local-variable 'revert-buffer-function)
             'bbdb-revert-buffer)
        (bbdb-mapc (lambda (ff) (add-hook 'local-write-file-hooks ff))
              bbdb-write-file-hooks)
        (setq bbdb-hashtable (make-vector bbdb-hashtable-size 0)))
      (setq bbdb-modified-p (buffer-modified-p)
            buffer-read-only bbdb-readonly-p)
      (or bbdb-records
      (cond ((= (point-min) (point-max)) ; special-case empty db
             ;; this doesn't need to be insert-before-markers because
             ;; there are no db-markers in this buffer.
             (insert (format ";; -*-coding: %s;-*-\n;;; file-version: %d\n"
                 bbdb-file-coding-system bbdb-file-format))
             (bbdb-flush-all-caches)
             (setq bbdb-end-marker (point-marker))
             ;;(run-hooks 'bbdb-after-read-db-hook) ; run this?
             nil)
            (t
             (or shut-up bbdb-silent-running (message "Parsing BBDB..."))
             (bbdb-flush-all-caches)
             (cond ((and bbdb-notice-auto-save-file
                         (file-newer-than-file-p (make-auto-save-file-name)
                                                 buffer-file-name))
                    (if (bbdb-yes-or-no-p "BBDB auto-save file is newer; recover it? ")
                        (progn
                          (recover-file buffer-file-name)
                          (bury-buffer (current-buffer)) ; recover-file selects it
                          (auto-save-mode 1) ; turn autosave back on
                          (delete-file (make-auto-save-file-name))
                          (message "Auto-save mode is ON in BBDB buffer.  Suggest you save it soon.")
                          (sleep-for 2))
                      ;; delete auto-save anyway, so we don't keep asking.
                      (condition-case nil
                          (delete-file (make-auto-save-file-name))
                        (file-error nil)))
                    ;; tail-recurse and try again
                    (let ((inside-bbdb-records nil))
                      (bbdb-records)))
                   (t
                    ;; normal case
                    (fillarray bbdb-hashtable 0)
                    (parse-bbdb-internal)))))))))

(defun bbdb-revert-buffer (arg noconfirm)
  ;; The .bbdb file's revert-buffer-function.
  ;; Don't even think of calling this.
  (kill-all-local-variables)            ; clear db and caches.
  (if (get-buffer bbdb-buffer-name)     ; now contains invalid records; nukem.
      (bbdb-undisplay-records))
  (let ((revert-buffer-function nil))   ; don't loop.
    (revert-buffer arg noconfirm)))

(defun parse-bbdb-internal ()
  (bbdb-debug (message "Parsing BBDB... (reading...)"))
  (widen)
  (goto-char (point-min))
  ;; go to the point at which the first record begins
  (cond ((eq (following-char) ?\[) nil)
        ((search-forward "\n[" nil 0) (forward-char -1))
        (t nil)) ;; no records
  ;; look backwards for user-defined field names (for completion purposes.)
  (save-excursion
    (if (re-search-backward "^;+[ \t]*user-fields:[ \t]*\(" nil t)
        (progn
          (goto-char (1- (match-end 0)))
          (setq bbdb-propnames
                (mapcar (lambda (x) (list (symbol-name x)))
                        (read (point-marker)))))))
  ;; look backwards for file version, and convert if necessary.
  ;; (at least, I'll write this code if I ever change the file format again...)
  (let ((v (save-excursion
             (if (re-search-backward
                  "^;+[ \t]*file-version:[ \t]*\\([0-9]+\\)[ \t]*$" nil t)
                 (car (read-from-string
                       (buffer-substring
                        (match-beginning 1) (match-end 1))))))))
    (if (null v) ; current version, but no file-version: line. Bootstrap it.
        (let ((modp (buffer-modified-p)))
          ;; This should never happen (not any more, anyway...)
          (bbdb-debug (error "bbdb corrupted: no file-version line"))
          (setq v 2)
          (save-excursion
            (if (re-search-backward "^;" nil t)
                (forward-line 1)
                (goto-char 1))
            ;; remember, this goes before the begin-marker of the first
            ;; record in the database!
            (insert-before-markers
         (format ";; -*-coding: %s;-*-\n;;; file-version: %d\n"
             bbdb-file-coding-system bbdb-file-format)))
          (set-buffer-modified-p modp)))
    (cond ((< v bbdb-file-format)
           (if bbdb-file-format-migration
               ;; Sanity checking.
               (if (/= (car bbdb-file-format-migration) v)
                   (error
                    (format
                     "BBDB file format has changed on disk from %d to %d!"
                     (car bbdb-file-format-migration) v)))
               (setq bbdb-file-format-migration
                     (cons v (bbdb-migration-query v)))))
          ((> v bbdb-file-format)
           (error "BBDB version %s doesn't understand file format version %s."
                  bbdb-version v))
          (t (setq bbdb-file-format-migration (cons bbdb-file-format
                                                    bbdb-file-format)))))
  ;; A trap to catch a bug
  ;;(assert (not (null (car bbdb-file-format-migration))))

  (bbdb-debug
   (or (eobp) (looking-at "[\[]")
       (error "no following bracket: bbdb corrupted"))
   (if (save-excursion
         (save-restriction
           (widen)
           (save-excursion (search-backward "\n[" nil t))))
       (error "bbdb corrupted: records before point")))

  ;; Migrate only if we need to.  Change the .bbdb buffer only if it
  ;; is not to be saved in the newest version.
  (if (= (car bbdb-file-format-migration) bbdb-file-format)
      (parse-bbdb-frobnicate (parse-bbdb-read))
    (let ((newrecs (parse-bbdb-frobnicate (bbdb-migrate (parse-bbdb-read)))))
      (cond ((= (cdr bbdb-file-format-migration) bbdb-file-format)
             (bbdb-migrate-rewrite-all nil newrecs)
             (bbdb-migrate-update-file-version
              (car bbdb-file-format-migration)
              (cdr bbdb-file-format-migration))))
      newrecs)))

(defun parse-bbdb-read ()
  ;; narrow the buffer to skip over the rubbish before the first record.
  (narrow-to-region (point) (point-max))
  (let ((records nil))
    ;; insert parens so we can read the db in one fell swoop (down in C).
    (let ((buffer-read-only nil)
          (modp (buffer-modified-p))
          ;; Make sure those parens get cleaned up.
          ;; This code had better stay simple!
          (inhibit-quit t))
      (goto-char (point-min)) (insert "(\n")
      (goto-char (point-max)) (insert "\n)")
      (goto-char (point-min))
      (setq records (read (current-buffer)))
      (goto-char (point-min)) (delete-char 2)
      (goto-char (point-max)) (delete-char -2)
      (set-buffer-modified-p modp))
    records))

(defun parse-bbdb-frobnicate (records)
  ;; now we have to come up with a marker for each record.  Rather than
  ;; calling read for each record, we read them at once (already done) and
  ;; assume that the markers are at each newline.  If this isn't the case,
  ;; things can go *very* wrong.
  (goto-char (point-min))
  (while (looking-at "[ \t\n\f]*;")
    (forward-line 1))
  (widen)
  (bbdb-debug (message "Parsing BBDB... (frobnicating...)"))
  (setq bbdb-records records)
  (let* ((head (cons '() records))
         (rest head)
        record)
    (while (cdr rest)
      (setq record (car (cdr rest)))
      ;; yow, are we stack-driven yet??  Damn byte-compiler...
      ;; Make a cache.  Put it in the record.  Put a marker in the cache.
      ;; Add record to hash tables.
      (bbdb-cache-set-marker
       (bbdb-record-set-cache record (make-vector bbdb-cache-length nil))
       (point-marker))
      (forward-line 1)

      ;; frob the label completion lists (and data completion when I,
      ;; uh, get around to it, maybe. this stuff should probably be
      ;; conditional, in case you're not running a 42GHz Pentium 69
      ;; with chrome tailpipes)
      (let ((ps (bbdb-record-phones record))
            (pl (bbdb-label-completion-list "phones"))
            (as (bbdb-record-addresses record))
            (al (bbdb-label-completion-list "addresses")))
        (while ps
          (let ((l (bbdb-phone-location (car ps))))
            (or (member l pl)
                (setq bbdb-phones-label-list
                      (append (or bbdb-phones-label-list
                                  bbdb-default-label-list)
                              (list l))
                      pl bbdb-phones-label-list)))
          (setq ps (cdr ps)))
        ;; Yes, I cut and pasted.
        (while as
          (let ((l (bbdb-address-location (car as))))
            (or (member l al)
                (setq bbdb-addresses-label-list
                      (append (or bbdb-addresses-label-list
                                  bbdb-default-label-list)
                              (list l))
                      al bbdb-addresses-label-list)))
          (setq as (cdr as))))

      (if bbdb-no-duplicates-p
          ;; warn the user that there is a duplicate...
          (let* ((name (bbdb-record-name record))
                 (tmp  (and name (bbdb-gethash (downcase name)
                                               bbdb-hashtable))))
            (if tmp (message "Duplicate BBDB record encountered: %s" name))))

        (bbdb-hash-record record)
        (setq rest (cdr rest))

      (bbdb-debug
       (if (and (cdr rest) (not (looking-at "[\[]")))
           (error "bbdb corrupted: junk between records at %s" (point)))))
    ;; In case we removed some of the leading entries...
    (setq bbdb-records (cdr head)))
  ;; all done.
  (setq bbdb-end-marker (point-marker))
  (run-hooks 'bbdb-after-read-db-hook)
  (bbdb-debug (message "Parsing BBDB... (frobnicating...done)"))
  bbdb-records)

(defmacro bbdb-user-mail-names ()
  "Returns a regexp matching the address of the logged-in user."
  '(or bbdb-user-mail-names
    (setq bbdb-user-mail-names
     (concat "\\b" (regexp-quote (user-login-name)) "\\b"))))

(defun bbdb-write-file-hook-fn ()
  "This is just for `bbdb-write-file-hooks'.  Keep it there."
  ;; this is premature as the file isn't actually written yet; but it's just
  ;; for the benefit of the mode-line of the *BBDB* buffer, and there isn't
  ;; an after-write-file-hook, so it'll do.
  (save-restriction
    (widen)
    (goto-char (point-min))

    ;; this always rewrites the coding cookie, which is a bit
    ;; wasteful, but safer than alternatives
    (if (looking-at ";; *-\\*-coding:")
        (delete-region (point) (progn (forward-line) (point))))
    (insert-before-markers (format ";; -*-coding: %s;-*-\n"
                                   bbdb-file-coding-system)))
  (setq bbdb-modified-p nil
        bbdb-changed-records nil
        buffer-file-coding-system bbdb-file-coding-system)
  (let ((buf (get-buffer bbdb-buffer-name)))
    (when buf
      (with-current-buffer buf
        (setq bbdb-showing-changed-ones nil)
        (set-buffer-modified-p nil))))
  (when (and bbdb-file-remote
             (or bbdb-file-remote-save-always
                 (y-or-n-p (format "Save the remote BBDB file %s too? "
                                   bbdb-file-remote))))
    ;; write the current buffer, which is `bbdb-file' (since this is called
    ;; from its `local-write-file-hooks'), into the `bbdb-file-remote'.
    (let ((coding-system-for-write bbdb-file-coding-system))
      (write-region (point-min) (point-max) bbdb-file-remote))))

(defun bbdb-delete-record-internal (record)
  (if (null (bbdb-record-marker record)) (error "bbdb: marker unpresent"))
  (bbdb-with-db-buffer
    (if (or bbdb-suppress-changed-records-recording
            (memq record bbdb-changed-records))
        nil
      (setq bbdb-changed-records (cons record bbdb-changed-records)))
    (let ((tail (memq record bbdb-records)))
      (if (null tail) (error "bbdb: unfound %s" record))
      (setq bbdb-records (delq record bbdb-records))
      (delete-region (bbdb-record-marker record)
                     (if (cdr tail)
                         (bbdb-record-marker (car (cdr tail)))
                         bbdb-end-marker))
      (let ((name    (bbdb-record-name    record))
            (company (bbdb-record-company record))
            (aka     (bbdb-record-aka     record))
            (nets    (bbdb-record-net     record)))
        (if (> (length name) 0)
            (bbdb-remhash (downcase name) record bbdb-hashtable))
        (if (> (length company) 0)
            (bbdb-remhash (downcase company) record bbdb-hashtable))
        (while nets
          (bbdb-remhash (downcase (car nets)) record bbdb-hashtable)
          (setq nets (cdr nets)))
        (while aka
          (bbdb-remhash (downcase (car aka)) record bbdb-hashtable)
          (setq aka (cdr aka)))
        )
      (bbdb-record-set-sortkey record nil)
      (setq bbdb-modified-p t))))

(defun bbdb-insert-sorted (record records)
  "Inserts the RECORD into the list of RECORDS, in order.
Assumes the list is already sorted.  Returns the new head."
  (bbdb-debug (if (memq record records)
                  (error "doubleplus ununique: - %s" record)))
  (let* ((rest (cons nil records))
         (top rest))
    (while (and (cdr rest)
                (bbdb-record-lessp (nth 1 rest) record))
      (setq rest (cdr rest)))
    (setcdr rest (cons record (cdr rest)))
    (cdr top)))

(defun bbdb-insert-record-internal (record unmigrated)
  (if (null (bbdb-record-marker record))
      (bbdb-record-set-marker record (make-marker)))
  (bbdb-with-db-buffer
    (if (or bbdb-suppress-changed-records-recording
            (memq record bbdb-changed-records))
        nil
      (setq bbdb-changed-records (cons record bbdb-changed-records)))
    (let ((print-escape-newlines t))
      (bbdb-record-set-sortkey record nil) ; just in case...
      (setq bbdb-records
            (bbdb-insert-sorted record bbdb-records))
      (let ((next (car (cdr (memq record bbdb-records)))))
        (goto-char (if next
                       (bbdb-record-marker next)
                       bbdb-end-marker))
        ;; before printing the record, remove the cache \(we don't want that
        ;; written to the file.\)  Ater writing, put the cache back and update
        ;; the cache's marker.
        (let ((cache (bbdb-record-cache record))
              (point (point)))
          (bbdb-debug
           (if (= point (point-min))
               (error "doubleplus ungood: inserting at point-min (%s)" point))
           (if (and (/= point bbdb-end-marker)
                    (not (looking-at "[\[]")))
               (error "doubleplus ungood: not inserting before a record (%s)"
                      point)))
          (bbdb-record-set-cache record nil)
          (if unmigrated (bbdb-record-set-cache unmigrated nil))
          (insert-before-markers (prin1-to-string (or unmigrated record)) "\n")
          (set-marker (bbdb-cache-marker cache) point)
          (bbdb-record-set-cache record cache)
;;        (if (bbdb-record-name record)
;;            (bbdb-puthash (downcase (bbdb-record-name record)) record bbdb-hashtable))
;;        (let ((nets (bbdb-record-net record)))
;;          (while nets
;;            (bbdb-puthash (downcase (car nets)) record bbdb-hashtable)
;;            (setq nets (cdr nets))))
          ;; This is marginally slower because it rebuilds the namecache,
          ;; but it makes jbw's life easier. :-\)
          (bbdb-hash-record record))
        record))
    (setq bbdb-modified-p t)))

(defun bbdb-overwrite-record-internal (record unmigrated)
  (bbdb-with-db-buffer
    (if (or bbdb-suppress-changed-records-recording
            (memq record bbdb-changed-records))
        nil
      (setq bbdb-changed-records (cons record bbdb-changed-records)))
    (let ((print-escape-newlines t)
          (tail bbdb-records))
      ;; Look for record after RECORD in the database.  Use the
      ;; beginning marker of this record (or the marker for the end of
      ;; the database if no next record) to determine where to stop
      ;; deleting old copy of record
      (while (and tail (not (eq record (car tail))))
        (setq tail (cdr tail)))
      (if (null tail) (error "bbdb: unfound %s" record))
      (let ((cache (bbdb-record-cache record)))

        (bbdb-debug
         (if (<= (bbdb-cache-marker cache) (point-min))
             (error "doubleplus ungood: cache marker is %s"
                    (bbdb-cache-marker cache)))
         (goto-char (bbdb-cache-marker cache))
         (if (and (/= (point) bbdb-end-marker)
                  (not (looking-at "[\[]")))
             (error "doubleplus ungood: not inserting before a record (%s)"
                    (point))))

        (goto-char (bbdb-cache-marker cache))
        (bbdb-record-set-cache record nil)
        (if unmigrated (bbdb-record-set-cache unmigrated nil))

        (insert (prin1-to-string (or unmigrated record)) "\n")
        (delete-region (point)
                       (if (cdr tail)
                           (bbdb-record-marker (car (cdr tail)))
                         bbdb-end-marker))
        (bbdb-record-set-cache record cache)

        (bbdb-debug
         (if (<= (if (cdr tail)
                     (bbdb-record-marker (car (cdr tail)))
                   bbdb-end-marker)
                 (bbdb-record-marker record))
             (error "doubleplus ungood: overwrite unworks")))

        (setq bbdb-modified-p t)
        record))))

(defvar inside-bbdb-change-record nil "hands off")
(defvar inside-bbdb-notice-hook nil
  "Internal variable; hands off.
Set to t by the BBDB when inside the `bbdb-notice-hook'.

Calls to the `bbdb-change-hook' are suppressed when this is non-nil.")

(defun bbdb-change-record (record need-to-sort)
  "Update the database after a change to the given record.  Second arg
NEED-TO-SORT is whether the name has changed.  You still need to worry
about updating the name hash-table."
  (if inside-bbdb-change-record
      record
    (let ((inside-bbdb-change-record t)
          unmigrated)
      (or inside-bbdb-notice-hook
          (bbdb-invoke-hook 'bbdb-change-hook record))
      (bbdb-debug (if (bbdb-record-deleted-p record)
                      (error "bbdb: changing deleted record")))
      (if (/= (cdr bbdb-file-format-migration) bbdb-file-format)
             (bbdb-unmigrate-record (setq unmigrated (bbdb-copy-thing record))))
      ;; Do the changing
      (if (memq record (bbdb-records)) ; checks file synchronization too.
          (if (not need-to-sort) ;; If we don't need to sort, overwrite it.
              (progn
                (bbdb-overwrite-record-internal record unmigrated)
                (bbdb-debug
                 (if (not (memq record (bbdb-records)))
                     (error "Overwrite in change doesn't work"))))
            ;; Since we do need to sort, delete then insert
            (bbdb-delete-record-internal record)
            (bbdb-debug
             (if (memq record (bbdb-records))
                 (error "Delete in need-sort change doesn't work")))
            (bbdb-insert-record-internal record unmigrated)
            (bbdb-debug
             (if (not (memq record (bbdb-records)))
                 (error "Insert in need-sort change doesn't work"))))
        ;; Record isn't in database so add it.
        (bbdb-insert-record-internal record unmigrated)
        (bbdb-debug (if (not (memq record (bbdb-records)))
                        (error "Insert in change doesn't work"))))
      (setq bbdb-modified-p t)
      (bbdb-invoke-hook 'bbdb-after-change-hook record)
      record)))

(defun bbdb-copy-thing (thing)
  "Copy a thing.  Handles vectors, strings, markers, numbers, conses,
lists, symbols, and nil.  Raises an error if it finds something it
doesn't know how to deal with."
  (cond ((vectorp thing)
         (let ((i 0)
               (newvec (make-vector (length thing) nil)))
           (while (< i (length thing))
             (aset newvec i (bbdb-copy-thing (aref thing i)))
             (setq i (1+ i)))
           newvec))
        ((stringp thing)
         (copy-sequence thing))
        ((markerp thing)
         (copy-marker thing))
        ((numberp thing)
         thing)
        ((consp thing)
         (cons (bbdb-copy-thing (car thing))
               (bbdb-copy-thing (cdr thing))))
        ((listp thing)
         (let ((i 0) newlist)
           (while (< i (length thing))
             (setq newlist (append newlist (list (bbdb-copy-thing
                                                  (nth i thing))))
                   i (1+ i)))
           newlist))
        ((symbolp thing)
         thing)
        ((eq nil thing)
         nil)
        (t
         (error "Don't know how to copy %s" (prin1-to-string thing)))))

(defmacro bbdb-propnames ()
  '(bbdb-with-db-buffer bbdb-propnames))

(defun bbdb-set-propnames (newval)
  (bbdb-with-db-buffer
    (setq bbdb-propnames newval)
    (widen)
    (goto-char (point-min))
    (and (not (eq (following-char) ?\[))
         (search-forward "\n[" nil 0))
    (if (re-search-backward "^[ \t]*;+[ \t]*user-fields:[ \t]*\(" nil t)
        (progn
          (goto-char (1- (match-end 0)))
          (delete-region (point) (progn (end-of-line) (point))))
      (and (re-search-backward "^[ \t]*;.*\n" nil t)
           (goto-char (match-end 0)))
      ;; remember, this goes before the begin-marker of the first
      ;; record in the database!
      (insert-before-markers ";;; user-fields: \n")
      (forward-char -1))
    (prin1 (mapcar (lambda (x) (intern (car x)))
                   bbdb-propnames)
           (current-buffer))
    bbdb-propnames))


;;; BBDB mode

(defun bbdb-mode ()
  "Major mode for viewing and editing the Insidious Big Brother Database.
Letters no longer insert themselves.  Numbers are prefix arguments.
You can move around using the usual cursor motion commands.
\\<bbdb-mode-map>
\\[bbdb-add-or-remove-mail-alias]\t Add new mail alias to visible records or \
remove it.
\\[bbdb-edit-current-field]\t Edit the field on the current line.
\\[bbdb-record-edit-notes]\t Edit the `notes' field for the current record.
\\[bbdb-delete-current-field-or-record]\t Delete the field on the \
current line.  If the current line is the\n\t first line of a record, then \
delete the entire record.
\\[bbdb-insert-new-field]\t Insert a new field into the current record.  \
Note that this\n\t will let you add new fields of your own as well.
\\[bbdb-transpose-fields]\t Swap the field on the current line with the \
previous field.
\\[bbdb-dial]\t Dial the current phone field.
\\[bbdb-next-record], \\[bbdb-prev-record]\t Move to the next or the previous \
displayed record, respectively.
\\[bbdb-create]\t Create a new record.
\\[bbdb-toggle-records-display-layout]\t Toggle whether the current record is displayed in a \
one-line\n\t listing, or a full multi-line listing.
\\[bbdb-apply-next-command-to-all-records]\\[bbdb-toggle-records-display-layout]\t Do that \
for all displayed records.
\\[bbdb-refile-record]\t Merge the contents of the current record with \
some other, and then\n\t delete the current record.  See this command's \
documentation.
\\[bbdb-omit-record]\t Remove the current record from the display without \
deleting it from\n\t the database.  This is often a useful thing to do \
before using one\n\t of the `*' commands.
\\[bbdb]\t Search for records in the database (on all fields).
\\[bbdb-net]\t Search for records by net address.
\\[bbdb-company]\t Search for records by company.
\\[bbdb-notes]\t Search for records by note.
\\[bbdb-name]\t Search for records by name.
\\[bbdb-changed]\t Display records that have changed since the database \
was saved.
\\[bbdb-send-mail]\t Compose mail to the person represented by the \
current record.
\\[bbdb-apply-next-command-to-all-records]\\[bbdb-send-mail]\t Compose mail \
to everyone whose record is displayed.
\\[bbdb-finger]\t Finger the net address of the current record.
\\[bbdb-ftp]\t FTP to the curent records's `ftp' field.
\\[bbdb-apply-next-command-to-all-records]\\[bbdb-finger]\t Finger the \
net address of all displayed records.
\\[bbdb-save-db]\t Save the BBDB file to disk.
\\[bbdb-print]\t Create a TeX file containing a pretty-printed version \
of all the\n\t records in the database.
\\[bbdb-apply-next-command-to-all-records]\\[bbdb-print]\t Do that for the \
displayed records only.
\\[other-window]\t Move to another window.
\\[bbdb-info]\t Read the Info documentation for BBDB.
\\[bbdb-help]\t Display a one line command summary in the echo area.
\\[bbdb-www]\t Visit Web sites listed in the `www' field(s) of the current \
record.
\\[bbdb-whois]\t run whois on the current record.

For address completion using the names and net addresses in the database:
\t in Sendmail mode, type \\<mail-mode-map>\\[bbdb-complete-name].
\t in Message mode, type \\<message-mode-map>\\[bbdb-complete-name].

Variables of note:
\t bbdb-always-add-addresses
\t bbdb-auto-revert-p
\t bbdb-canonicalize-redundant-nets-p
\t bbdb-case-fold-search
\t bbdb-completion-type
\t bbdb-default-area-code
\t bbdb-default-domain
\t bbdb-electric-p
\t bbdb-display-layout
\t bbdb-file
\t bbdb-message-caching-enabled
\t bbdb-new-nets-always-primary
\t bbdb-north-american-phone-numbers-p
\t bbdb-notice-auto-save-file
\t bbdb-offer-save
\t bbdb-pop-up-display-layout
\t bbdb-pop-up-target-lines
\t bbdb-quiet-about-name-mismatches
\t bbdb-readonly-p
\t bbdb-use-alternate-names
\t bbdb-use-pop-up
\t bbdb-user-mail-names
\t bbdb/mail-auto-create-p
\t bbdb/news-auto-create-p

There are numerous hooks.  M-x apropos ^bbdb.*hook RET

The keybindings, more precisely:
\\{bbdb-mode-map}"
  (setq major-mode 'bbdb-mode)
  (setq mode-name "BBDB")
  (use-local-map bbdb-mode-map)
  (run-hooks 'bbdb-mode-hook))

;;; these should be in bbdb-com.el but they're so simple, why load it all.

(defun bbdb-next-record (p)
  "Move the cursor to the first line of the next BBDB record."
  (interactive "p")
  (if (< p 0)
      (bbdb-prev-record (- p))
    (forward-char)
    (while (> p 0)
      (or (re-search-forward "^[^ \t\n]" nil t)
          (progn (beginning-of-line)
                 (error "no next record")))
      (setq p (1- p)))
    (beginning-of-line)))

(defun bbdb-prev-record (p)
  "Move the cursor to the first line of the previous BBDB record."
  (interactive "p")
  (if (< p 0)
      (bbdb-next-record (- p))
    (while (> p 0)
      (or (re-search-backward "^[^ \t\n]" nil t)
          (error "no previous record"))
      (setq p (1- p)))))


(defun bbdb-maybe-update-display (bbdb-record)
  (save-excursion
    (save-window-excursion
      (let ((w (get-buffer-window bbdb-buffer-name))
            (b (current-buffer)))
        (if w
            (unwind-protect
                (progn (set-buffer bbdb-buffer-name)
                       (save-restriction
                         (if (assq bbdb-record bbdb-records)
                             (bbdb-redisplay-records))))
              (set-buffer b)))))))

(defcustom bbdb-notes-default-separator ", "
  "*The default separator inserted by `bbdb-annotate-notes'.
This is used for notes which do not have `field-separator' property set.
E.g., if you want URLs to be separated by newlines, you can put
  (put 'www 'field-separator \"\\n\")
into your .emacs."
  :group 'bbdb-noticing-records
  :type 'string)

(defun bbdb-annotate-notes (bbdb-record annotation &optional fieldname replace)
  "Add an annotation to a record.
Adds (or replaces, when the fourth argument REPLACE is non-nil)
an ANNOTATION to the note FIELDNAME in BBDB-RECORD.
Called by `bbdb-auto-notes-hook'."
  (or bbdb-record (error "unperson"))
  (setq annotation (bbdb-string-trim annotation))
  (if (memq fieldname '(name address addresses phone phones net aka AKA))
      (error "bbdb: cannot annotate the %s field this way" fieldname))
  (or fieldname (setq fieldname 'notes))
  (or (memq fieldname '(notes company))
      (assoc (symbol-name fieldname) (bbdb-propnames))
      (bbdb-set-propnames (append (bbdb-propnames)
                                  (list (list (symbol-name fieldname))))))
  (let ((notes (bbdb-string-trim
                (or (bbdb-record-getprop bbdb-record fieldname) ""))))
    (unless (or (string= "" annotation)
                (string-match (regexp-quote annotation) notes))
      (bbdb-record-putprop bbdb-record fieldname
                           (if (or replace (string= notes ""))
                               annotation
                               (concat notes
                                       (or (get fieldname 'field-separator)
                                           bbdb-notes-default-separator)
                                       annotation)))
      (bbdb-maybe-update-display bbdb-record))))

(defun bbdb-offer-save ()
  "Offer to save the Insidious Big Brother Database if it is modified."
  (if bbdb-offer-save
      (bbdb-save-db (eq bbdb-offer-save t))))

(defcustom bbdb-save-db-timeout nil
  "*If non-nil, then when `bbdb-save-db' is asking you whether to save the db,
it will time out to `yes' after this many seconds.  This only works if the
function `y-or-n-p-with-timeout' is defined."
  :group 'bbdb-save
  :type '(choice (const :tag "Don't time out" nil)
                 (integer :tag "Time out after this many seconds" 5)))

(defun bbdb-save-db (&optional prompt-first mention-if-not-saved)
  "Save the DB if it is modified."
  (interactive (list nil t))
  (bbdb-with-db-buffer
    (if (and (buffer-modified-p)
             (or (null prompt-first)
                 (if bbdb-readonly-p
                     (bbdb-y-or-n-p
                      "Save the BBDB, even though it's supposedly read-only? ")
                   (if (and bbdb-save-db-timeout
                            (fboundp 'y-or-n-p-with-timeout))
                       (y-or-n-p-with-timeout
                        "Save the BBDB now? " bbdb-save-db-timeout t)
                     (bbdb-y-or-n-p "Save the BBDB now? ")))))
        (save-buffer)
      (if mention-if-not-saved (message "BBDB not saved")))))


;;; mail and news interface

(defun bbdb-clean-username (string)
  "Strips garbage from the user full name string."
  ;; This function is called a lot, and should be fast.  But I'm loathe to
  ;; remove any of the functionality in it.
  (if (string-match "[@%!]" string)  ; ain't no user name!  It's an address!
      (bbdb-string-trim string)
   (let ((case-fold-search t))
     ;; Take off leading and trailing non-alpha chars \(quotes, parens,
     ;; digits, etc) and things which look like phone extensions \(like
     ;; "x1234" and "ext. 1234". \)
     ;; This doesn't work all the time because some of our friends in
     ;; northern europe have brackets in their names...
     (if (string-match (if bbdb-have-re-char-classes
               "\\`[^[:alpha:]]+"
             "\\`[^a-z]+")
               string)
         (setq string (substring string (match-end 0))))
     (while (string-match
             "\\(\\W+\\([Xx]\\|[Ee]xt\\.?\\)\\W*[-0-9]+\\|[^a-z]+\\)\\'"
             string)
       (setq string (substring string 0 (match-beginning 0))))
     ;; replace tabs, multiple spaces, dots, and underscores with a single
     ;; space, but don't replace ". " with " " because that could be an
     ;; initial.
     (while (string-match "\\(\t\\|  +\\|\\(\\.\\)[^ \t_]\\|_+\\)" string)
       (setq string (concat (substring string 0
                                       (or (match-beginning 2)
                                           (match-beginning 1)))
                            " "
                            (substring string (or (match-end 2)
                                                  (match-end 1))))))
     ;; If the string contains trailing parenthesized comments, nuke 'em.
     (if (string-match "[^ \t]\\([ \t]*\\((\\| -\\| #\\)\\)" string)
         (progn
           (setq string (substring string 0 (match-beginning 1)))
           ;; lose rubbish this may have exposed.
           (while
               (string-match
                "\\(\\W+\\([Xx]\\|[Ee]xt\\.?\\)\\W*[-0-9]+\\|[^a-z]+\\)\\'"
                string)
               (setq string (substring string 0 (match-beginning 0))))))
     string)))

;;; message-caching, to speed up the the mail interfaces

(defvar bbdb-buffers-with-message-caches '()
  "A list of all the buffers which have stuff on their `bbdb-message-cache'
local variable.  When we re-parse the `bbdb-file', we need to flush all of
these caches.")

(defun notice-buffer-with-cache (buffer)
  (or (memq buffer bbdb-buffers-with-message-caches)
      (progn
        ;; First remove any deleted buffers which may have accumulated.
        ;; This happens only when a buffer is added to the list, so it
        ;; ought not happen that frequently (each time you read mail, say.)
        (let ((rest bbdb-buffers-with-message-caches))
          (while rest
            (if (null (buffer-name (car rest)))
                (setq bbdb-buffers-with-message-caches
                      (delq (car rest) bbdb-buffers-with-message-caches)))
            (setq rest (cdr rest))))
        ;; now add this buffer.
        (setq bbdb-buffers-with-message-caches
              (cons buffer bbdb-buffers-with-message-caches)))))

(defvar bbdb-message-cache nil
  "alist of (MESSAGE-KEY BBDB-RECORDS) cached in order to avoid updating
messages each time they are visited.  This is used by all MUAs, while the
MESSAGE-KEY is specific to the MUA and the cache is local for each MUA or MUA
folder.")

(make-variable-buffer-local 'bbdb-message-cache)

(defun bbdb-message-cache-lookup (message-key)
  "Return cached BBDB records for MESSAGE-KEY.
If not present or when the records have been modified return nil."
  (bbdb-records)
  (if bbdb-message-caching-enabled
      (let ((records (assq message-key bbdb-message-cache))
            (invalid nil))
        (when records
          (setq records (cdr records))
          (bbdb-mapc (lambda (record)
                  (if (bbdb-record-deleted-p record)
                      (setq invalid t)))
                records))
        (if invalid nil records))))

(defun bbdb-encache-message (message-key bbdb-records)
  "Cache the BBDB-RECORDS for a message identified by MESSAGE-KEY and
return them."
  (and bbdb-message-caching-enabled
       (car bbdb-records)
       (add-to-list 'bbdb-message-cache (cons message-key bbdb-records))
       (notice-buffer-with-cache (current-buffer)))
  bbdb-records)

(defun bbdb-decache-message (message-key)
  "Remove an element form the cache."
  (and bbdb-message-caching-enabled
       (delq (assoc message-key bbdb-message-cache) bbdb-message-cache)))

(defun bbdb-flush-all-caches ()
  (bbdb-debug
    (and bbdb-buffers-with-message-caches
         (message "Flushing BBDB caches")))
  (save-excursion
    (while bbdb-buffers-with-message-caches
      (if (buffer-name (car bbdb-buffers-with-message-caches))
          (progn
            (set-buffer (car bbdb-buffers-with-message-caches))
            (setq bbdb-message-cache nil)))
      (setq bbdb-buffers-with-message-caches
            (cdr bbdb-buffers-with-message-caches)))))


(defconst bbdb-name-gubbish
  (concat "[-,. \t/\\]+\\("
          "[JjSs]r\\.?"
          "\\|V?\\(I\\.?\\)+V?"
          (concat "\\|"
                  (regexp-opt bbdb-lastname-prefixes))
          "\\)\\W*\\'"))

(defun bbdb-divide-name (string)
  "divide the string into a first name and a last name, cleverly."
  ;; ## This shouldn't be here.
  (if (string-match "\\W+\\([Xx]\\|[Ee]xt\\.?\\)\\W*[-0-9]+\\'" string)
      (setq string (substring string 0 (match-beginning 0))))
  (let* ((case-fold-search nil)
         (str string)
         (gubbish (string-match bbdb-name-gubbish string)))
    (if gubbish
        (setq gubbish (substring str gubbish)
              str (substring string 0 (match-beginning 0))))
    (if (string-match
     (concat " +\\("
         ;; start recognize some prefixes to lastnames
         (if bbdb-lastname-prefixes
             (concat "\\("
                 (regexp-opt bbdb-lastname-prefixes t)
                 "[ ]+\\)?"))
         ;; end recognize some prefixes to lastnames
         "\\([^ ]+ *- *\\)?[^ ]+\\)\\'") str)
        (list (substring str 0 (match-beginning 0))
              (concat
               (substring str (match-beginning 1))
               (or gubbish "")))
      (list string ""))))

(defun bbdb-check-alternate-name (possible-name record)
  (let (aka)
    (if (setq aka (bbdb-record-aka record))
        (let ((down-name (downcase possible-name))
              match)
          (while aka
            (if (equal down-name (downcase (car aka)))
                (setq match (car aka)
                      aka nil)
                (setq aka (cdr aka))))
          match))))


(defun bbdb-canonicalize-address (net)
  ;; call the bbdb-canonicalize-net-hook repeatedly until it returns a
  ;; value eq to the value passed in.  This implies that it can't
  ;; destructively modify the string.

  ;; Hysterical Raisins: This is a function, not a hook. In order to
  ;; make this hook a hook, we'll quietly convert a single function
  ;; into a hook list.  We should really warn the user that we're
  ;; doing this, and advise them to update their configuration
  ;; accordingly. For the release, maybe.
  (if (functionp bbdb-canonicalize-net-hook)
      (setq bbdb-canonicalize-net-hook (list bbdb-canonicalize-net-hook)))

  ;; Now, do the hook run. Note, if you mess up, it's possible that
  ;; BBDB will get stuck here oscillating between various definitions
  ;; of the canonical address.
  (while (not (equal net (setq net (run-hook-with-args
                                    'bbdb-canonicalize-net-hook net)))))

  net)

;; Mostly written by Rod Whitby.
(defun bbdb-net-redundant-p (net old-nets)
  "Returns non-nil if NET represents a sub-domain of one of the OLD-NETS.
The returned value is the address which makes this one redundant.
For example, \"foo@bar.baz.com\" is redundant w.r.t. \"foo@baz.com\",
and \"foo@quux.bar.baz.com\" is redundant w.r.t. \"foo@bar.baz.com\"."
  (let ((redundant-addr nil))
    (while (and (not redundant-addr) old-nets)
      ;; Calculate a host-regexp for each address in OLD-NETS
      (let* ((old (car old-nets))
             (host-index (string-match "@" old))
             (name (and host-index (substring old 0 host-index)))
             (host (and host-index (substring old (1+ host-index))))
             ;; host-regexp is "^<name>@.*\.<host>$"
             (host-regexp (and name host
                               (concat "\\`" (regexp-quote name)
                                       "@.*\\." (regexp-quote host)
                                       "\\'"))))
        ;; If NET matches host-regexp, then it is redundant
        (if (and host-regexp net
                 (string-match host-regexp net))
            (setq redundant-addr old)))
      (setq old-nets (cdr old-nets)))
    redundant-addr))


(defun bbdb-annotate-message-sender (from &optional loudly create-p
                                          prompt-to-create-p)
  "Fills the record corresponding to the sender with as much info as possible.
A record may be created by this; a record or nil is returned.
If `bbdb-readonly-p' is true, then a record will never be created.
If CREATE-P is true, then a record may be created, otherwise it won't.
If PROMPT-TO-CREATE-P is true, then the user will be asked for confirmation
before the record is created, otherwise it is created without confirmation
\(assuming that CREATE-P is true\).  "
  (let* ((data (if (consp from)
                   from ; if from is a cons, it's pre-parsed (hack hack)
                 (mail-extract-address-components from)))
         (name (car data))
         (net (car (cdr data))))
    (if (equal name net) (setq name nil))
    (bbdb-debug
     (if (equal name "") (error "mail-extr returned \"\" as name"))
     (if (equal net "") (error "mail-extr returned \"\" as net")))

    (if (and net bbdb-canonicalize-net-hook)
        (setq net (bbdb-canonicalize-address net)))

    (let ((change-p nil)
          (record (or (bbdb-search-simple nil net)
                      (bbdb-search-simple name nil)))
          (created-p nil)
          (fname name)
          (lname nil)
          old-name
          bogon-mode)
      (and record (setq old-name (bbdb-record-name record)))

      ;; This is to prevent having losers like "John <blat@foop>" match
      ;; against existing records like "Someone Else <john>".
      ;;
      ;; The solution implemented here is to never create or show records
      ;; corresponding to a person who has a real-name which is the same
      ;; as the network-address of someone in the db already.  This is not
      ;; a good solution.
      (let (down-name old-net)
        (if (and record name
                 (not (equal (setq down-name (downcase name))
                             (and old-name (downcase old-name)))))
            (progn
              (setq old-net (bbdb-record-net record))
              (while old-net
                (if (equal down-name (downcase (car old-net)))
                    (progn
                      (setq bogon-mode t
                            old-net nil)
                      (message
                       "Ignoring bogon %s's name \"%s\" to avoid name-clash with \"%s\""
                       net name old-name)
                      (sit-for 2))
                  (setq old-net (cdr old-net)))))))

      (if (or record
              bbdb-readonly-p
              (not create-p)
              (not (or name net))
              bogon-mode)
          ;; no further action required
          nil
        ;; otherwise, the db is writable, and we may create a record.
        ;; first try to get a reasonable default name if not given
        ;; often I get things like <firstname>.<surname>@ ...
        (if (or (null name) (and (stringp name) (string= "" name)))
            (if (string-match "^[^@]+" net)
                (setq name (bbdb-clean-username (match-string 0 net)))))
        (setq record (if (or (null prompt-to-create-p)
                             (eq create-p t) ;; don't skip if it's 'prompt!
                             (if (functionp prompt-to-create-p)
                                 (bbdb-invoke-hook-for-value
                                  prompt-to-create-p)
                               (bbdb-y-or-n-p
                                (format "%s is not in the db.  Add? "
                                        (or name net)))))
                         (make-vector bbdb-record-length nil))
              created-p (not (null record)))
        (if record
            (bbdb-record-set-cache record (make-vector bbdb-cache-length nil)))
        )
      (if (or bogon-mode (null record))
          nil
        (bbdb-debug (if (bbdb-record-deleted-p record)
                        (error "nasty nasty deleted record nasty.")))
        (if (and name
                 (not (equal (and name (downcase name))
                             (and old-name (downcase old-name))))
                 (or (null bbdb-use-alternate-names)
                     (not (bbdb-check-alternate-name name record)))
                 (let ((fullname (bbdb-divide-name name))
                       tmp)
                   (setq fname (car fullname)
                         lname (nth 1 fullname))
                   (not (and (equal (downcase fname)
                                    (and (setq tmp
                                               (bbdb-record-firstname record))
                                         (downcase tmp)))
                             (equal (downcase lname)
                                    (and (setq tmp
                                               (bbdb-record-lastname record))
                                         (downcase tmp)))))))

            ;; have a message-name, not the same as old name.
            (cond (bbdb-readonly-p nil);; skip if readonly

                  ;; ignore name mismatches?
                  ;; NB 'quiet' means 'don't ask', not 'don't mention'
                  ((and bbdb-quiet-about-name-mismatches old-name)
                   (let ((sit-for-secs
                          (if (numberp bbdb-quiet-about-name-mismatches)
                              bbdb-quiet-about-name-mismatches
                            2)))
                     (if (or bbdb-silent-running (= 0 sit-for-secs)) nil
                       (message "name mismatch: \"%s\" changed to \"%s\""
                                (bbdb-record-name record) name)
                       (sit-for sit-for-secs))))
                  ((or created-p
                       (if bbdb-silent-running t
                         (if (null old-name)
                             (bbdb-y-or-n-p
                              (format "Assign name \"%s\" to address \"%s\"? "
                                      name (car (bbdb-record-net record))))
                           (bbdb-y-or-n-p
                            (format "Change name \"%s\" to \"%s\"? "
                                    old-name name)))))
                   (setq change-p 'sort)

                   ;; Keep old name?
                   (and old-name bbdb-use-alternate-names
                        (not (member old-name (bbdb-record-aka record)))
                        ;; Silent mode: just add it.
                        (if bbdb-silent-running
                            (bbdb-record-set-aka record
                                                 (cons old-name
                                                       (bbdb-record-aka
                                                        record)))
                          ;; prompt user otherwise.
                          (if (bbdb-y-or-n-p
                               (format "Keep name \"%s\" as an AKA? "
                                       old-name))
                              (bbdb-record-set-aka record
                                                   (cons old-name
                                                         (bbdb-record-aka
                                                          record)))
                            (bbdb-remhash (downcase old-name) record))))

                   (bbdb-record-set-namecache record nil)
                   (bbdb-record-set-firstname record fname)
                   (bbdb-record-set-lastname record lname)
                   (bbdb-debug (or fname lname
                                   (error "bbdb: should have a name by now")))
                   (bbdb-puthash (downcase (bbdb-record-name record)) record))

                  ;; not quiet about mismatches
                  ((and old-name bbdb-use-alternate-names
                        ;; dedupe
                        (not (member old-name (bbdb-record-aka record)))
                        (if (not bbdb-silent-running)
                            (bbdb-y-or-n-p
                             (format "Make \"%s\" an alternate for \"%s\"? "
                                     name old-name))))
                   (setq change-p 'sort)
                   (bbdb-record-set-aka
                    record (cons name (bbdb-record-aka record)))
                   (bbdb-puthash (downcase name) record))))

        ;; It's kind of a kludge that the "redundancy" concept is built in.
        ;; Maybe I should just add a new hook here...  The problem is that the
        ;; canonicalize-net-hook is run before database lookup, and thus can't
        ;; refer to the database to determine whether a net is redundant.
        (if bbdb-canonicalize-redundant-nets-p
            (setq net (or (bbdb-net-redundant-p net (bbdb-record-net record))
                          net)))

        (if (and net (not bbdb-readonly-p))
            (if (null (bbdb-record-net record))
                ;; names are always a sure match, so don't bother prompting
                ;; here.
                (progn (bbdb-record-set-net record (list net))
                       (bbdb-puthash (downcase net) record) ; important!
                       (or change-p (setq change-p t)))
              ;; new address; ask before adding.
              (if (let ((rest-net (bbdb-record-net record))
                        (new (downcase net))
                        (match nil))
                    (while (and rest-net (null match))
                      (setq match (string= new (downcase (car rest-net)))
                            rest-net (cdr rest-net)))
                    match)
                  nil
                (if (let ((bbdb-always-add-addresses
                           bbdb-always-add-addresses))
                      (if (functionp bbdb-always-add-addresses)
                          (setq bbdb-always-add-addresses
                                (funcall bbdb-always-add-addresses)))
                      (cond
                       ;; add it automatically
                       ((eq bbdb-always-add-addresses t)
                        t)
                       ;; do not add it
                       ((null bbdb-always-add-addresses)
                        nil)
                       ;; ask the user if it should be added
                       (t
                        (and
                         (not (equal net "???"))
                         (let ((the-first-bit
                                (format "Add address \"%s\" to \"" net))
                               ;; this groveling is to prevent the "(y or n)"
                               ;; from falling off the right edge of the
                               ;; screen.
                               (the-next-bit (mapconcat 'identity
                                                        (bbdb-record-net
                                                         record)
                                                        ", "))
                               (w (window-width (minibuffer-window))))
                           (if (> (+ (length the-first-bit)
                                     (length the-next-bit) 15) w)
                               (setq the-next-bit
                                     (concat
                                      (substring
                                       the-next-bit
                                       0 (max 0 (- w (length the-first-bit)
                                                   20)))
                                      "...")))
                           (bbdb-display-records (list record))
                           (if (bbdb-y-or-n-p (concat the-first-bit
                                                      the-next-bit
                                                      "\"? "))
                               ;; then add the new net
                               t
                             ;; else add a new record with the same name
                             (if (and create-p
                                      (or (null prompt-to-create-p)
                                          (if (functionp prompt-to-create-p)
                                              (bbdb-invoke-hook-for-value
                                               prompt-to-create-p)
                                            (bbdb-y-or-n-p
                                             (format
                                              "Create a new record for %s? "
                                              (bbdb-record-name record))))))
                                 (setq record
                                       (bbdb-create-internal name nil net
                                                             nil nil nil)))
                             nil))))))
                    ;; then modify an existing record
                    (let ((front-p (cond ((null bbdb-new-nets-always-primary)
                                          (bbdb-y-or-n-p
                                           (format
                                            "Make \"%s\" the primary address? "
                                            net)))
                                         ((eq bbdb-new-nets-always-primary t)
                                          t)
                                         (t nil))))
                      (bbdb-record-set-net record
                                           (if front-p
                                               (cons net (bbdb-record-net
                                                          record))
                                             (nconc (bbdb-record-net record)
                                                    (list net))))
                      (bbdb-puthash (downcase net) record) ; important!
                      (or change-p (setq change-p t)))))))

        (bbdb-debug
         (if (and change-p bbdb-readonly-p)
             (error
              "doubleplus ungood: how did we change anything in readonly mode?"
              )))
        (if (and loudly change-p (not bbdb-silent-running))
            (if (eq change-p 'sort)
                (message "noticed \"%s\"" (bbdb-record-name record))
              (if (bbdb-record-name record)
                  (message "noticed %s's address \"%s\""
                           (bbdb-record-name record) net)
                (message "noticed naked address \"%s\"" net))))

        (if created-p
            (bbdb-invoke-hook 'bbdb-create-hook record))

        (if change-p
            (bbdb-change-record record (eq change-p 'sort)))

        ;; only invoke bbdb-notice-hook if we actually noticed something
        (if record
            (let ((inside-bbdb-notice-hook t))
              (bbdb-invoke-hook 'bbdb-notice-hook record)))

        record))))


;;; window configuration hackery
(defun bbdb-multiple-buffers-default ()
  "Default function for guessing a better name for new *BBDB* buffers."
  (cond ((memq major-mode '(vm-mode vm-summary-mode
                                    vm-presentation-mode
                                    vm-virtual-mode))
         (vm-select-folder-buffer)
         (buffer-name))
        ((memq major-mode '(gnus-summary-mode gnus-group-mode))
         (set-buffer gnus-article-buffer)
         (buffer-name))
        ((memq major-mode '(mail-mode vm-mail-mode message-mode))
         "message composition")))

(defun bbdb-multiple-buffers-set-name (&optional buffer-list new-name)
  (setq new-name (or new-name
                     (concat " *BBDB " (funcall bbdb-multiple-buffers) "*"))
        buffer-list (append (list (current-buffer)
                                  (get-buffer-create new-name))
                            buffer-list))

  (save-excursion
    (while buffer-list
      (set-buffer (car buffer-list))
      (make-local-variable 'bbdb-buffer-name)
      (setq bbdb-buffer-name new-name)
      (setq buffer-list (cdr buffer-list)))))

(defun bbdb-pop-up-bbdb-buffer (&optional horiz-predicate)
  "Find the largest window on the screen, and split it, displaying the
*BBDB* buffer in the bottom 'bbdb-pop-up-target-lines' lines (unless
the *BBDB* buffer is already visible, in which case do nothing.)

If 'bbdb-use-pop-up' is the symbol 'horiz, and the first window
matching HORIZ-PREDICATE is sufficiently wide (> 100 columns) then
the window will be split vertically rather than horizontally.

If `bbdb-multiple-buffers' is set we create a new BBDB buffer when not
already within one.  The new buffer-name starts with a space, i.e. it does
not clutter the buffer-list."

  (let ((b (current-buffer))
        new-bbdb-buffer-name)

    ;; create new BBDB buffer if multiple buffers are desired.
    (when (and bbdb-multiple-buffers (not (eq major-mode 'bbdb-mode)))
      (bbdb-multiple-buffers-set-name (list b)))
    (setq new-bbdb-buffer-name bbdb-buffer-name)

    ;; now get the pop-up
    (if (get-buffer-window new-bbdb-buffer-name)
        nil
      (if (and (eq bbdb-use-pop-up 'horiz)
               horiz-predicate
               (bbdb-pop-up-bbdb-buffer-horizontally horiz-predicate))
          nil
        (let* ((first-window (selected-window))
               (tallest-window first-window)
               (window first-window))
          ;; find the tallest window...
          (while (not (eq (setq window (previous-window window)) first-window))
            (if (> (window-height window) (window-height tallest-window))
                (setq tallest-window window)))
          ;; select it and split it...
          (select-window tallest-window)
          (let ((size (min
                       (- (window-height tallest-window)
                          window-min-height 1)
                       (- (window-height tallest-window)
                          (max window-min-height
                               (1+ bbdb-pop-up-target-lines))))))
            (split-window tallest-window
                          (if (> size 0) size window-min-height)))
          (if (memq major-mode
                    '(gnus-Group-mode gnus-Subject-mode gnus-Article-mode))
              (goto-char (point-min)))  ; make gnus happy...
          ;; goto the bottom of the two...
          (select-window (next-window))
          ;; make it display *BBDB*...
          (let ((pop-up-windows nil))
            (switch-to-buffer (get-buffer-create new-bbdb-buffer-name)))
          ;; select the original window we were in...
          (select-window first-window)))
      ;; and make sure the current buffer is correct as well.
      (set-buffer b)
      nil)))

(defun bbdb-pop-up-bbdb-buffer-horizontally (predicate)
  (if (<= (frame-width) 112)
      nil
    (let* ((first-window (selected-window))
           (got-it nil)
           (window first-window))
      (while (and (not (setq got-it (funcall predicate window)))
                  (not (eq first-window (setq window (next-window window)))))
        )
      (if (or (null got-it)
              (<= (window-width window) 112))
          nil
        (let ((b (current-buffer)))
          (select-window window)
          (split-window-horizontally 80)
          (select-window (next-window window))
          (let ((pop-up-windows nil))
            (switch-to-buffer (get-buffer-create bbdb-buffer-name)))
          (select-window first-window)
          (set-buffer b)
          t)))))

(defun bbdb-version (&optional arg)
  "Return string describing the version of the BBDB that is running.
When called interactively with a prefix argument, insert string at point."
  (interactive "P")
  (let ((version-string (format "BBDB version %s (%s)"
                               bbdb-version bbdb-version-date)))
    (cond
     (arg
      (insert (message version-string)))
     ((interactive-p)
      (message version-string))
     (t version-string))))

;;; resorting, which really shouldn't be necesary...

(defun bbdb-record-lessp-fn (record1 record2) ; for use as a funarg
  (bbdb-record-lessp record1 record2))

(defun bbdb-resort-database ()
  "*Resort BBDB database as a last resort.
This is not be needed when using BBDB itself.  It might be necessary
after having used inferior software to add entries to the BBDB, however."
  (interactive)
  (let* ((records (copy-sequence (bbdb-records))))
    (bbdb-with-db-buffer
     (setq bbdb-records (sort bbdb-records 'bbdb-record-lessp-fn))
     (if (equal records bbdb-records)
         nil
       (message "DANGER!  BBDB was mis-sorted; it's being fixed...")
       (goto-char (point-min))
       (cond ((eq (following-char) ?\[) nil)
             ((search-forward "\n[" nil 0) (forward-char -1)))
       (delete-region (point) bbdb-end-marker)
       (let ((print-escape-newlines t)
             (standard-output (current-buffer))
             (inhibit-quit t) ; really, don't fuck with this
             record cache)
         (setq records bbdb-records)
         (while records
           (setq record (car records)
                 cache (bbdb-record-cache record))
           (bbdb-record-set-cache record nil)
           (prin1 (car records))
           (bbdb-record-set-cache record cache)
           (insert ?\n)
           (setq records (cdr records))))
       (kill-all-local-variables)
       (error "the BBDB was mis-sorted: it has been repaired.")))))

(defvar bbdb-init-forms
  '((gnus                       ; gnus 3.15 or newer
     (add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus))
    (mh-e                       ; MH-E
     (add-hook 'mh-folder-mode-hook 'bbdb-insinuate-mh))
    (rmail                      ; RMAIL
     (add-hook 'rmail-mode-hook 'bbdb-insinuate-rmail))
    (sendmail                   ; the standard mail user agent
     (add-hook 'mail-setup-hook 'bbdb-insinuate-sendmail))
    (vm-old                     ; the alternative mail reader
     (add-hook 'vm-load-hook 'bbdb-insinuate-vm))
    (vm                        ; newer versions don't have vm-load-hook
     (progn (eval-after-load "vm" '(bbdb-insinuate-vm))))
    (message                    ; the gnus mail user agent
     (add-hook 'message-setup-hook 'bbdb-insinuate-message))
    (reportmail                 ; mail notification
     (add-hook 'reportmail-load-hook 'bbdb-insinuate-reportmail))
    (sc                         ; message citation
     (add-hook 'sc-load-hook 'bbdb-insinuate-sc))
    (supercite                  ; same
     (add-hook 'sc-load-hook 'bbdb-insinuate-sc))
    (w3                         ; WWW browser
     (add-hook 'w3-load-hook 'bbdb-insinuate-w3)))
  "The alist which maps features to insinuation forms.")

;;;###autoload
(defun bbdb-initialize (&rest to-insinuate)
"*Initialize the BBDB.  One or more of the following symbols can be
passed as arguments to initiate the appropriate insinuations.

 Initialization of mail/news readers:

   gnus       Initialize BBDB support for the gnus mail/news reader
              version 3.15 or newer.  If you pass the `gnus' symbol,
              you should probably also pass the `message' symbol.
   mh-e       Initialize BBDB support for the MH-E mail reader.
   rmail      Initialize BBDB support for the RMAIL mail reader.
   sendmail   Initialize BBDB support for sendmail (M-x mail).
   vm         Initialize BBDB support for the VM mail reader.
              NOTE: For the VM insinuation to work properly, you must
              either call `bbdb-initialize' with the `vm' symbol from
              within your VM initialization file (\"~/.vm\") or you
              must call `bbdb-insinuate-vm' manually from within your
              VM initialization file.

 Initialization of miscellaneous package:

   message    Initialize BBDB support for Message mode.
   reportmail Initialize BBDB support for the Reportmail mail
              notification package.
   sc or      Initialize BBDB support for the Supercite message
   supercite  citation package.
   w3         Initialize BBDB support for Web browsers."

  (defalias 'advertized-bbdb-delete-current-field-or-record
    'bbdb-delete-current-field-or-record)

  (require 'bbdb-autoloads)

  (while to-insinuate
    (let* ((feature (car to-insinuate))
           (init (assq feature bbdb-init-forms)))
      (setq to-insinuate (cdr to-insinuate))
      (if init
          (if (or (featurep feature) (locate-library (symbol-name feature)))
              (eval (cadr init))
            (bbdb-warn "cannot locate feature `%s'" feature))
          (bbdb-warn "don't know how to insinuate `%s'" feature))))

  ;; RMAIL, MHE, and VM interfaces might need these.
  (autoload 'mail-strip-quoted-names "mail-utils")
  (autoload 'mail-fetch-field "mail-utils")
  ;; All of the interfaces need this.
  (autoload 'mail-extract-address-components "mail-extr")

  (run-hooks 'bbdb-initialize-hook))

;; Initialize keymaps
(unless bbdb-mode-search-map
  (define-prefix-command 'bbdb-mode-search-map)
  (if (fboundp 'set-keymap-prompt)
      (set-keymap-prompt
       bbdb-mode-search-map
       "(Search [n]ame, [c]ompany, net [a]ddress, n[o]tes)?"))

  (define-key bbdb-mode-search-map [(n)] 'bbdb-name)
  (define-key bbdb-mode-search-map [(c)] 'bbdb-company)
  (define-key bbdb-mode-search-map [(a)] 'bbdb-net)
  (define-key bbdb-mode-search-map [(o)] 'bbdb-notes))

(unless bbdb-mode-map
  (setq bbdb-mode-map (make-keymap))
  (suppress-keymap bbdb-mode-map)

  (define-key bbdb-mode-map [(S)]          'bbdb-mode-search-map)

  (define-key bbdb-mode-map [(*)]          'bbdb-apply-next-command-to-all-records)
  (define-key bbdb-mode-map [(+)]          'bbdb-append-records)
  (define-key bbdb-mode-map [(!)]          'bbdb-search-invert-set)
  (define-key bbdb-mode-map [(a)]          'bbdb-add-or-remove-mail-alias)
  (define-key bbdb-mode-map [(e)]          'bbdb-edit-current-field)
  (define-key bbdb-mode-map [(n)]          'bbdb-next-record)
  (define-key bbdb-mode-map [(p)]          'bbdb-prev-record)
  (define-key bbdb-mode-map [(d)]          'bbdb-delete-current-field-or-record)
  (define-key bbdb-mode-map [(control k)]  'bbdb-delete-current-field-or-record)
  (define-key bbdb-mode-map [(control o)]  'bbdb-insert-new-field)
  (define-key bbdb-mode-map [(s)]          'bbdb-save-db)
  (define-key bbdb-mode-map [(control x) (control s)]
                                           'bbdb-save-db)
  (define-key bbdb-mode-map [(r)]          'bbdb-refile-record)
  (define-key bbdb-mode-map [(t)]          'bbdb-toggle-records-display-layout)
  (define-key bbdb-mode-map [(T)]          'bbdb-display-record-completely)
  (define-key bbdb-mode-map [(o)]          'bbdb-omit-record)
  (define-key bbdb-mode-map [(?\;)]        'bbdb-record-edit-notes)
  (define-key bbdb-mode-map [(m)]          'bbdb-send-mail)
  (define-key bbdb-mode-map "\M-d"         'bbdb-dial)
  (define-key bbdb-mode-map [(f)]          'bbdb-finger)
  (define-key bbdb-mode-map [(F)]          'bbdb-ftp)
  (define-key bbdb-mode-map [(i)]          'bbdb-info)
  (define-key bbdb-mode-map [(??)]         'bbdb-help)
  (define-key bbdb-mode-map [(q)]          'bbdb-bury-buffer)
  (define-key bbdb-mode-map [(control x) (control t)]
                                           'bbdb-transpose-fields)
  (define-key bbdb-mode-map [(w)]          'bbdb-www)
  (define-key bbdb-mode-map [(W)]         'bbdb-whois)
  (define-key bbdb-mode-map [(P)]          'bbdb-print)
  (define-key bbdb-mode-map [(h)]          'other-window)
  (define-key bbdb-mode-map [(=)]          'delete-other-windows)
  (define-key bbdb-mode-map [(c)]          'bbdb-create)
  (define-key bbdb-mode-map [(C)]          'bbdb-changed)
  (define-key bbdb-mode-map [(b)]          'bbdb)

  (define-key bbdb-mode-map [delete]       'scroll-down)
  (define-key bbdb-mode-map " "            'scroll-up)
  )


;;; Support for the various Emacsen.  This is for features that the
;;; BBDB adds to itself for different Emacsen.  For definitions of
;;; functions that aren't present in various Emacsen (for example,
;;; cadr for Emacs 19.34), see below
(when (string-match "XEmacs\\|Lucid" emacs-version)
  ;; Above
  (fset 'bbdb-warn 'warn)

  ;; bbdb-com.el
  (fset 'bbdb-display-completion-list 'bbdb-xemacs-display-completion-list))

(defun bbdb-insinuate-sendmail ()
  "Call this function to hook BBDB into sendmail (that is, M-x mail)."
  (define-key mail-mode-map "\M-\t" 'bbdb-complete-name))

;;;###autoload
(defun bbdb-insinuate-message ()
  "Call this function to hook BBDB into `message-mode'."
  (define-key message-mode-map "\M-\t" 'bbdb-complete-name))

;;; Erm. says here that (require...) can take a noerror flag; why do
;;; we have this function?
(defmacro safe-require (thing)
  (list 'condition-case nil (list 'require thing) '(file-error nil)))

;; Wrappers for things that change for different Emacsen.  Note: This
;; is for things that get redefined that don't belong elsewhere.  Some
;; functions that get redefined live elsewhere in the source because
;; it makes sense to put them there.

(defun bbdb-warn (&rest args)
  (beep 1)
  (apply 'message args))


(provide 'bbdb)  ; provide before loading things which might require

(run-hooks 'bbdb-load-hook)
