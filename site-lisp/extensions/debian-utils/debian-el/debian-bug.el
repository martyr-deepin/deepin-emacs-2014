;; debian-bug.el --- report a bug to Debian's bug tracking system

;; Copyright (C) 1998, 1999 Free Software Foundation, Inc.
;; Copyright (C) 2001, 2002, 2003, 2004 Peter S Galbraith <psg@debian.org>
;; Copyright (C) 2005, 2006, 2007, 2008 Peter S Galbraith <psg@debian.org>
;; Copyright (C) 2009, 2010 Peter S Galbraith <psg@debian.org>

;; Help texts from
;;  http://www.debian.org/Bugs/Developer#severities
;;  http://www.debian.org/Bugs/Developer#tags
;;  http://www.debian.org/Bugs/pseudo-packages
;; Copyright 1999 Darren O. Benham, 1994-1997 Ian Jackson,
;;  1997 nCipher Corporation Ltd.

;; Author (Up to version 1.7):           Francesco Potortì <pot@gnu.org>
;; Maintainer from version 1.8 onwards:  Peter S Galbraith <psg@debian.org>
;; Keywords: debian, bug, reporter

;; debian-bug.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; debian-bug.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; ----------------------------------------------------------------------------
;;; Commentary:
;;
;; Useful commands provided by this mode:
;;
;; debian-bug         - submit a bug report concerning a Debian package
;; debian-bug-web-bug - view a bug report on a web browser (via browse-url)
;; debian-bug-wnpp    - submit a Work Needed on Prospective Package bug report
;; debian-bug-request-for-package
;;                    - shortcut for a WNPP bug type.
;; debian-bug-ITP     - shortcut for a WNPP bug type
;;
;; debian-bug depends on either the bug package or the reportbug (>1.21)
;; package for best results.
;;
;; ----------------------------------------------------------------------------
;;; Change log:
;;
;; V1.5 23sep99 Francesco Potortì <pot@gnu.org>
;;  - V1.1 -> 1.5 versions had no changelogs; starting one now.
;; V1.6 and V1.7 by Francesco Potortì <pot@gnu.org> were unreleased.
;; V1.8 04aug01 Peter S Galbraith <psg@debian.org>
;;  - WNPP interface code added.  I'm unsure whether the functions useful
;;    only to Debian developpers should be in here.  Perhaps split into a
;;    second .el file bundled in dpkg-dev-el?
;; V1.9 10aug01 Peter S Galbraith <psg@debian.org>
;;  - gratuitous changes (sorry Francesco) while going through the code base:
;;    document defvars, s/debian-bug-program/debian-bug-helper-program/,
;;    s/debian-bug-init-program/debian-bug-helper-program-init/,
;;  - updated list of pseudo-packages
;; V1.10 11aug01 Peter S Galbraith <psg@debian.org>
;;   Apply most of patch (made against v1.4!) from
;;   Kim-Minh Kaplan <kmkaplan@vocatex.fr>, Dated 03 Oct 1999.
;;   (mostly, it inserts the package version numbers in the completion obarray
;;    eliminating the need to parse the status file later).
;; V1.11 11aug01 Peter S Galbraith <psg@debian.org>
;;  - Don't use external bug command when package name does not exist locally
;;    (fixes an old bug).
;;  - Add font-lock support (e.g. release-critical severities in red)
;; V1.12 13aug01 Peter S Galbraith <psg@debian.org>
;;  - generalize debian-bug-wnpp-email to debian-bug-use-From-address
;;  - debian-bug-ITP and debian-bug-request-for-package shortcuts.
;;  - Add menubar via minor-mode
;;    -> set address to send to (submit, quiet, maintonly)
;;    -> set/unset custom From header line.
;;    -> set/unset X-Debbugs-CC header line.
;;    -> change bug severity
;;    -> add/change Tags
;;    -> browse-url web interface to BTS
;;    -> various help texts
;;    -> customize
;; V1.13 14aug01 Peter S Galbraith <psg@debian.org>
;;  - Confirm when package is not in status file.
;;  - Fix for reportbug >= 1.22
;; V1.14 15aug01 Peter S Galbraith <psg@debian.org>
;;  - merge in wget BTS code from debian-changelog.el
;;  - clarify help texts about maintonly and ftp.debian.org
;;  - defaliases for -ITP and -RFP (closes: #108808)
;;  - Add menu option for X-Debbugs-CC to myself.
;;  - small format fixes.
;; V1.15 15aug01 Peter S Galbraith <psg@debian.org>
;;  - Change all address related menu comands to toggling radio switches.
;; V1.16 21sep01 Peter S Galbraith <psg@debian.org>
;;  - Temporary fix for XEmacs' lack of font-lock-add-keywords.
;;  - Add template as done by reportbug for ITP and RFP wnpp bugs.
;;    (closes: #111615)
;; V1.17 20oct01 Peter S Galbraith <psg@debian.org>
;;  - load poe for match-string-no-properties if using XEmacs.
;;  - Use only one arg with format-time-string for XEmacs compatibility.
;;  - After generating the bug list menu in XEmacs, remove both menus and add
;;    them again.  Otherwise the menu is not refreshed.  (Closes: #111332)
;; V1.18 11nov01 Peter S Galbraith <psg@debian.org>
;;  - customize debian-bug-helper-program so bug isn't necessarily used first.
;; V1.19 11nov01 Peter S Galbraith <psg@debian.org>
;;  - debian-bug: change from "Package; title" from reporter-submit-bug-report
;;    into "Package: title" (closes: #117976).
;;  - debian-bug-From-address-init: recognize all env vars used by reportbug
;;    and bug to set the From line.
;;  - debian-bug-use-From-address: default to t if any of the above env vars
;;    are set.  I could also try to detect is debian-bug-From-address is
;;    customized, but that's for another day.  (closes #117855).
;;  - debian-bug-prefill-report: don't add superfluous line at the beginning
;;    of the bug report body (closes #117842).
;; V1.20 11dec01 Peter S Galbraith <psg@debian.org>
;;  - debian-bug: display message that we are fetching system info (which can
;;    take a while). (Closes: #122033)
;;  - debian-bug: fix function doc string (Closes: #121932 if Roland fixes
;;    the corresponding autoload).
;; V1.21 11dec01 Peter S Galbraith <psg@debian.org>
;;  - debian-bug, debian-bug-wnpp: Use simpler 'reporter-compose-outgoing'
;;    instead of 'reporter-submit-bug-report'
;;  - debian-bug, debian-bug-wnpp: reset buffer to "*mail*" if mail-user-agent
;;    is gnus-user-agent (Closes: #121532).
;; V1.22 11dec01 Peter S Galbraith <psg@debian.org>
;;  - menu: Implement most of Bill Wohler's excellent suggestions to improve
;;    the main menu (Closes: #123476)
;; V1.23 12dec01 Peter S Galbraith <psg@debian.org>
;;  - use new option --template with reportbug for garanteed non-interactive
;;    use of reportbug.  The package must depend on reportbug >= 1.41.1
;;    (Closes: #122032)
;; V1.24 24Jan02 Peter S Galbraith <psg@debian.org>
;;    debian-bug-web-bugs: return all bugs for the source package.
;; V1.25 07Feb02 Peter S Galbraith <psg@debian.org>
;;    debian-bug-build-bug-menu: return all bugs for the source package.
;; V1.26 08Feb02 Peter S Galbraith <psg@debian.org>
;;    debian-bug-help-tags-text and debian-bug-help-severity-text: updated
;;    debian-bug-tags-alist: Added "upstream" to list
;; V1.27 11Jul02 Peter S Galbraith <psg@debian.org>
;;    reset buffer to "*mail*" only when in buffer " *nttpd*" (Closes: #151717)
;; V1.28 30Jul02 Peter S Galbraith <psg@debian.org>
;;    added debian-bug-filename (Closes: 117036)
;; V1.29 02Augl02 Peter S Galbraith <psg@debian.org>
;;    Add a few functions from debian-changelog, since we are taking over
;;    its duplicate commands.
;;     New: debian-bug-web-this-bug
;;     New: debian-bug-web-this-bug-under-mouse
;; V1.30 15Aug02 Peter S Galbraith <psg@debian.org>
;;    Kalle Olavi Niemitalo <kon@iki.fi> suggested the use of "toggle" buttons
;;    instead of "radio" buttons, where appropriate (Closes: #156297).
;; V1.31 15Aug02 Peter S Galbraith <psg@debian.org>
;;    Remove erroneous [] brackets around WNPP tags (Closes: #156391).
;; V1.32 13Sep02 Peter S Galbraith <psg@debian.org>
;;  - Deal with reportbug 1.99.54 (or so) that adds MIME stuff to mail headers.
;;    Patch from Brian Warner <warner@lothar.com> (Closes: #160750)
;;  - debian-bug-prefill-report: Don't pass desired severity to reportbug
;;    because it is interactive when high settings are passed.  Set after
;;    reportbug template is entered instead.  (Closes: #159625)
;; V1.33 27Sep02 Peter S Galbraith <psg@debian.org>
;;    Split long bug menus, first into categories, then into number ranges.
;;    (Closes: #161155)
;; V1.34 20Nov02 Peter S Galbraith <psg@debian.org>
;;    debian-bug-build-bug-menu: removed one character from the regexp for
;;    the bug menu.  I don't know if the web format changed, but
;;    debian-bug-alist was short by the last number in its bug numbers.
;; V1.35 19Mar03 Peter S Galbraith <psg@debian.org>
;;    debian-bug-build-bug-menu:  Adapted to change in BTS web page format.
;;    Bugs were no longer found by the old regexp.
;; V1.36 19Mar03 Peter S Galbraith <psg@debian.org>
;;    debian-bug: Call proper debian-bug--set-custom-From, which will delete
;;     an existing From line before inserting a new one.  Closes: #184954.
;;    debian-bug-prefill-report: Don't flake out on search if "\n\n" not
;;     found.  This might help with bug #165290, but I should really check
;;     that reportbug doesn't fail.
;;    debian-bug: Check if empty Subject field has trailing space.  Should
;;     fix bug #173040 and part of #177259.
;; V1.37 10Apr2003 Peter S Galbraith <psg@debian.org>
;;  - Switch priority of reportbug and bug, preferring reportbug.
;;  - send to maintonly if priority wishlist or minor.  Closes: #176429.
;; V1.38 14Apr2003 Peter S Galbraith <psg@debian.org>
;;  - Revert `send to maintonly if priority wishlist or minor' change.
;;    maintonly is for mass filings.
;;  - New buffer-local variable `debian-bug-open-alist' for open bugs.
;;    This will be used for completion in debian-changelog-mode.el
;;  - debian-bug: always build package list.  Closes: #186338
;;  - Use executable-find.  Patch contributed by Romain FRANCOISE
;;    <romain@orebokech.com>.  Closes: #189605
;;  - New actions in Bugs list menu: can now read bug reports as file or Email!
;;  - Apply checkdoc patch from Bill Wohler <wohler@newt.com>.  Thanks!
;;  - Byte-compilation cleanup.
;;  - Added debian-bug-menu-preload-flag.
;; V1.39 22Apr2003 Peter S Galbraith <psg@debian.org>
;;  - debian-bug-alltags-alist: new variable for complete Tags list.
;;  - debian-bug-help-control: new command for menu help for d-b-control
;;  - Minor doc string fixes.
;;  - renamed X-Debbugs-CC commands to simple CC, specifying the field to
;;    use as an new argument.  So it can be used in d-b-control.
;; V1.40 12May2003 Peter S Galbraith <psg@debian.org>
;;  - check if `debian-changelog-mode' is available as a feature, and not
;;    simply the if the autoloaded are fboundp (which is always true).
;; V1.41 15May2003 Peter S Galbraith <psg@debian.org>
;;  - Add `confirmed' tag.
;; V1.42 23May2003 Matt Swift <swift@alum.mit.edu>
;;    debian-bug-prefill-report: announce error if reportbug gives empty
;;    template.
;; V1.42 31May2003 Peter S Galbraith <psg@debian.org>
;;    Add `d-i', `ipv6' and `lfs' tags.
;; V1.43 01Sep2003 Peter S Galbraith <psg@debian.org>
;;    debian-bug-build-bug-menu: Create closing changlog entries in
;;    debian-bug-open-alist cdr's.  (Closes: #207852)
;; V1.44 03Sep2003 Peter S Galbraith <psg@debian.org>
;;  - Display help when prompting for package name and bug severity
;;    (Closes: #200058)
;;  - debian-bug-display-help: new defcustom.
;; V1.45 05Sep2003 Peter S Galbraith <psg@debian.org>
;;  - debian-bug-filename: Added File: in informational block.
;;  - debian-bug-search-file: Added message about system call to dpkg.
;;  - debian-bug-font-lock-keywords: added File:
;;  - debian-bug: make it a front-end to `debian-bug-package' (the old
;;    `debian-bug') and `debian-bug-filename' and make those non-interactive,
;;    reducing the number of interactive commands. (Closes: #167214)
;;  - checkdoc fixes.
;; V1.46 17Sep2003 Peter S Galbraith <psg@debian.org>
;;  - I think V1.43 added the # character before bug numbers in the menu
;;    and broke the splitting-up of large bug categories.  Fixed.
;;  - bugs.debian.org added HTML "name" tags which I need to exclude from
;;    titles.
;; V1.47 20Sep2003 Peter S Galbraith <psg@debian.org>
;;  - debian-bug-search-file: Use dlocate if available when filename is
;;    given. thanks to Jeff Sheinberg (Closes: #211598).
;; V1.48 01Oct2003 Peter S Galbraith <psg@debian.org>
;;  - Make debian-bug accept P or F without a carriage return.
;; V1.49 05Oct2003 Peter S Galbraith <psg@debian.org>
;;  - Add tags "sarge-ignore" and "fixed-uptsream".
;; V1.50 09Oct2003 Peter S Galbraith <psg@debian.org>
;;  - Add debian-bug-rfc2047-decode-string.
;; V1.51 28Oct2003 Peter S Galbraith <psg@debian.org>
;;  - Send to maintonly if priority minor.  Closes: #214242.
;;    See http://www.debian.org/Bugs/Reporting.en.html:
;;    "if a bug report is minor, for example, a documentation typo or a
;;     trivial build problem, please adjust the severity appropriately and
;;     send it to maintonly@bugs"
;; V1.52 27Nov2003 Kalle Olavi Niemitalo <kon@iki.fi>
;;  - Contain debian-bug's cursor-in-echo-area to when it's needed so the
;;    list of pseudo-packages can be scrolled. (Closes: #222332)
;;  - debian-bug-package: Let M-<next> and M-<prev> scroll the pseudo-package
;;    list window by making _it_ the other window. (Closes: #222333)
;; V1.53 27Nov2003 Peter S Galbraith <psg@debian.org>
;;  - Add menu entry for "Archived Bugs for this package" and for
;;    "Developer Page for This Package".  Create debian-bug-web-developer-page.
;; V1.54 02Aug2004 Peter S Galbraith <psg@debian.org>
;;  - Add RFH tag to wnpp.
;; V1.54 11Nov2004 Camm Maguire <camm@enhanced.com>
;;  - debian-bug:  Add "--list-cc=none" to call to reportbug after changes
;;    in new version of reportbug. (Closes: #280780)
;; V1.55 05Jan2005 Kevin Ryde <user42@zip.com.au>
;;  - adds gnus support to debian-bug-get-bug-as-email, bringing the bug
;;   messages up in a gnus group. (Closes: #288469)
;; V1.55 05Jan2005 Peter S Galbraith <psg@debian.org>
;;   debian-bug-package: skip over mml directives in new drafts.
;;   Thanks to Luca Capello <luca@pca.it> (Closes: #336466)
;; V1.56 03Nov2005 Peter S Galbraith <psg@debian.org>
;;   - debian-bug-prompt-bug-number: new function to prompt user for a bug
;;     number using number under point if any.
;;   - debian-bug-web-bug: use it.
;;   - debian-bug-web-this-bug: deleted (no longer needed).
;;   - debian-bug-get-bug-as-file: use it.
;;   - debian-bug-get-bug-as-email: use it.
;;     (Closes: #337233)
;; V1.57 03Nov2005 Peter S Galbraith <psg@debian.org>
;;   - Swap CC: for X-Debbugs-CC: in mail header (Closes: #208570)
;; V1.58 05Nov2005 Peter S Galbraith <psg@debian.org>
;;   - debian-bug-wnpp: skip over mml directives in new drafts.
;;   Thanks to Luca Capello <luca@pca.it> (Closes: #337659)
;; V1.59 14Nov2005 Peter S Galbraith <psg@debian.org>
;;   - Search for "^cc:" instead of simply "cc:" in Bug #208570 change.
;; V1.60 30May2006 Luca Capello <luca@pca.it>
;;   - Change the face of Tags: for experimental, (Closes: #357265)
;; V1.61 05Sep2006 Kevin Ryde <user42@zip.com.au>
;;   - word-at-point needs an autoload or a require statement (Closes: #384542)
;; V1.62 22Sep2006 Peter S Galbraith <psg@debian.org>
;;   - Added "Owner:" to ITP bugs. Thanks to Romain Francoise for bringing
;;     this to my attention (Closes: #388747)
;;   - Updated the list of valid tags.
;; V1.63 25Jul2007 Peter S Galbraith <psg@debian.org>
;;  - Adapt patch from Luca Capello <luca@pca.it> for bug #431091
;; V1.64 29Aug2007 Peter S Galbraith <psg@debian.org>
;;  - `debian-changelog-close-bug-statement' may not be bound (Closes: #440002)
;;    Thanks to my friend Bill Wohler for finding this bug.
;; V1.65 02Sep2007 Peter S Galbraith <psg@debian.org>
;;  - Implement pacakge lookup on http://packages.debian.org/
;;    See http://bugs.debian.org/87725
;; V1.66 24Sep2007 Luca Capello <luca@pca.it>
;;  - Add `debian-bug-get-bug-as-email-hook' and relative `run-hooks'
;;    (Closes: #392475)
;; V1.67 09Sept2008 Peter S Galbraith <psg@debian.org>
;;  - Bug fix: "Bug submenus have vanished", thanks to Bill Wohler for the
;;    report and to Camm Maguire for an initial patch (Closes: #463053).
;; V1.68 23Feb2009 Peter S Galbraith <psg@debian.org>
;;  - Bug fix: Adapted patch from Håkon Stordahl <haastord@online.no> to
;;    quote bug descriptions when building the bug menu. (Closes: #489786)
;;  - Bug fix: Applied patch from Håkon Stordahl <hakon@stordahl.org>
;;    for garbled Help buffer (Closes: #502426)
;; V1.69 13May2009 Peter S Galbraith <psg@debian.org>
;;  - Updated debian-bug-pseudo-packages (Closes: #526496)
;;  - [PATCH] using the "maintainer mbox" instead of "mbox folder".
;;    Thanks to Evgeny M. Zubok (Closes: #521571).
;;  - Fix "incomplete Bugs menu again", thanks to A Mennucc (Closes: #524043).
;; V1.70 11Nov2009 Peter S Galbraith <psg@debian.org>
;;  - Add `debian-bug-bts-URL' variable
;;  - Add `emacs-bug-web-bug', `emacs-bug-get-bug-as-email':
;;     New commands to interface with Emacs BTS
;; V1.71 19Dec2009 Peter S Galbraith <psg@debian.org>
;;  - Emacs BTS moved to debbugs.gnu.org
;; V1.72 27Apr2010 Peter S Galbraith <psg@debian.org>
;;  - debian-bug-build-bug-menu takes optional SOURCE argument to create a
;;    menu for source package.  The problem comes from the BTS that no longer
;;    finds source packages automatically, e.g. this won't work:
;;        http://bugs.debian.org/cgi-bin/pkgreport.cgi?src=debian-el
;;    but this is needed instead:
;;        http://bugs.debian.org/cgi-bin/pkgreport.cgi?src=emacs-goodes-el
;;    with the _real_ source package name.
;; V1.73 28Apr2010 H. Stordahl <hakon@stordahl.org>
;;    As of version 4.12 reportbug has a --no-bug-script option which can
;;    be used to work around bug #502317.
;; V1.74 07May2010 H. Stordahl <hakon@stordahl.org>
;;    A better way to run bug scripts... (Closes #422506)
;;    New functions: debian-bug-help-presubj, debian-bug-file-is-executable,
;;     debian-bug-find-bug-script, debian-bug-script-sentinel,
;;     debian-bug-run-bug-script, debian-bug-insert-bug-script-temp-file,
;;     debian-bug-compose-report
;;   Patch debian-bug-package to use them.
;; V1.74 07May2010 H. Stordahl <hakon@stordahl.org>
;;   Support "Bugs:" control field for unofficial packages (Closes #222392)
;;    New variable `debian-bug-bts-address'
;;    New functions:
;;     debian-bug-read-control-file-field
;;     debian-bug-read-bug-control-file-field
;;     debian-bug-find-bts-address
;;     debian-bug-bts-mail
;;    Patch debian-bug-prefill-report to use them
;; V1.75 13Mar2010 Peter S Galbraith <psg@debian.org>
;;   Updated `debian-bug-pseudo-packages'.
;;----------------------------------------------------------------------------

;;; Todo (Peter's list):
;;
;; - Add extra prompt for release-critical severities (e.g. "This indicates
;;   the package is not suitable for release.  Proceed?")
;; - Possibly add a pre-send-mail hook to check that all entries are
;;   validated.
;; - Help texts need a top-level general one (say where to look them up,
;;   and how to search by package, bug submitter, maintainer, etc)
;; - debian-bug-wnpp accepts empty package name!
;; - debian-bug-wnpp doesn't get a Bugs menu or web lookup.
;;   -> should lookup specified package to [O] and [ITO] ?
;;   -> or list all bugs for wnpp?
;; - add debian-bug-pseudo-package (with completion on those only, possibly
;;   with description)

;;; User customizable variables:

;;; Code:
(defgroup debian-bug nil "Debian Bug report helper"
  :group 'tools
  :prefix "debian-bug-")

(defcustom debian-bug-display-help t
  "Display help text when prompting for package name and bug severity."
  :group 'debian-bug
  :type 'boolean)

(defcustom debian-bug-helper-program nil
  "Helper program to use to generate bug report background info.
Possible values are 'bug, 'reportbug or nil (for neither).
If not customized, it will get set to at runtime to 'reportbug if the command
exists, or else to 'bug if that command exists, or else simply parse the
status file."
  :group 'debian-bug
  :type '(radio (const :tag "reportbug" reportbug)
                (const :tag "bug" bug)
                (const :tag "set at runtime" nil)))

(defcustom debian-bug-use-From-address
  (or (getenv "DEBEMAIL")               ; reportbug
      (getenv "REPORTBUGEMAIL")         ; reportbug
      (getenv "EMAIL"))                 ; reportbug and bug
  "Insert a custom From line in the bug report header.
Use it to specify what email your bugs will be archived under."
  :group 'debian-bug
  :type 'boolean)

(defcustom debian-bug-download-directory "~/"
  "Directory for mbox file downloads from the Debian BTS."
  :group 'debian-bug
  :type 'directory)

(defcustom debian-bug-mh-folder "+debian-bug"
  "The folder to put all bug folders into when using MH-E (7.3 or better)."
  :group 'debian-bug
  :type '(choice (string :tag "Folder name")
                 (const :tag "Don't use a folder"  nil)))

;;; Not implemented yet.
;; (defcustom debian-bug-create-package-directories-flag nil
;;   "Non-nil means to create a directory for each package.
;; For rmail, this means a directory beneath `debian-bug-download-directory'.
;; For MH-E, this means a folder beneath `debian-bug-mh-folder'."
;;   :group 'debian-bug
;;   :type 'boolean)

;; This function is from emacs/lisp/calendar/icalendar.el,
;; necessary to replace "%s" with the bug number in
;; `debian-changelog-close-bug-statement'
(defsubst debian-bug--rris (&rest args)
  "Replace regular expression in string.
Pass ARGS to `replace-regexp-in-string' (GNU Emacs) or to
`replace-in-string' (XEmacs)."
  ;; XEmacs:
  (if (fboundp 'replace-in-string)
      (save-match-data ;; apparently XEmacs needs save-match-data
        (apply 'replace-in-string args))
    ;; Emacs:
    (apply 'replace-regexp-in-string args)))

(defvar debian-bug-minor-mode nil)
(defvar debian-bug-minor-mode-map nil
  "Keymap for `debian-bug' minor mode.")
(if debian-bug-minor-mode-map
    nil
  (setq debian-bug-minor-mode-map (make-sparse-keymap)))

(if (not (fboundp 'match-string-no-properties))
    (load "poe" t t))                   ;XEmacs21.1 doesn't autoload this

;;; Guess From address initial value (if not set via customize)
(defun debian-bug-From-address-init ()
  "Return email to use for the From: line of the BTS email.
The full name is from the environment variable DEBFULLNAME or else the
variable `user-full-name'.
The email address is from the environment variable DEBEMAIL or EMAIL,
or else the `user-mail-address' variable."
  (let ((fullname (or (getenv "DEBFULLNAME")
                      (getenv "DEBNAME") ; reportbug
                      (getenv "NAME")    ;  reportbug
                      (user-full-name)))
        (mailing-address
         (or (getenv "DEBEMAIL")        ;  reportbug
             (getenv "REPORTBUGEMAIL")  ;  reportbug
             (getenv "EMAIL")           ;  reportbug and bug
             (and (boundp 'user-mail-address) user-mail-address)
             (and (fboundp 'user-mail-address) (user-mail-address)))))
    (cond
     ((and fullname mailing-address)
      (format "%s <%s>" fullname mailing-address))
     (mailing-address
      mailing-address)
     (t
      nil))))

(defcustom debian-bug-From-address (debian-bug-From-address-init)
  "Email address to use for the From: and CC: lines of Debian bug reports.
The default value is obtained from the function `debian-bug-From-address-init'."
  :group 'debian-bug
  :type 'string)

(defcustom debian-bug-always-CC-myself t
  "Insert a CC line to myself in the bug report header.
Will only actually do it if the variable `debian-bug-From-address' is set."
  :group 'debian-bug
  :type 'boolean)

;;(defvar debian-bug-menu-action)
;;(defvar debian-bug-menu-action-default)
;;(defun debian-bug-menu-action-set (symbol value)
;;  "Set SYMBOL to VALUE for
;;  (set-default symbol value)
;;  (setq-default debian-bug-menu-action debian-bug-menu-action-default)
;;  (setq debian-bug-menu-action debian-bug-menu-action-default))

(defcustom debian-bug-menu-action-default 'browse
  "Default action enabled at startup in Bugs menu-bar."
  :group 'debian-bug
  ;; :set 'debian-bug-menu-action-set
  :set (lambda (symbol value)
         (set-default symbol value)
         (setq-default debian-bug-menu-action debian-bug-menu-action-default)
         (setq debian-bug-menu-action debian-bug-menu-action-default))
  :type '(radio (const :tag "Browse" browse)
                (const :tag "Read as File" readfile)
                (const :tag "Read as Email" email)))

(defvar debian-bug-menu-action debian-bug-menu-action-default
  "Action to take when selecting a bug number from the Bugs menu-bar.")
(make-variable-buffer-local 'debian-bug-menu-action)

(defcustom debian-bug-menu-preload-flag nil
  "Non-nil means to fetch bug list from the web and populate Bugs menu.
Otherwise, simply use the menu entry to generate it."
  :group 'debian-bug
  :type 'boolean)

;; hooks
(defcustom debian-bug-get-bug-as-email-hook nil
  "Hook run when getting a bug through `mail-user-agent'."
  :group 'debian-bug
  :type 'hook)


;;; Internal variables:

(defvar debian-bug-bts-URL "http://bugs.debian.org/cgi-bin/bugreport.cgi?"
  "URL of the Bug Tracking System to query.")

(defvar debian-bts-control-for-emacs nil
  "Whether `debian-bts-control' is being called for Emacs BTS.")

(defvar debian-bug-mail-address
  "Debian Bug Tracking System <submit@bugs.debian.org>"
  "Email address that bugs are sent to.")

(defvar debian-bug-mail-quiet-address
  "Debian Bug Tracking System <quiet@bugs.debian.org>"
  "Address to use to send to the BTS but not forward to the maintainer.")

(defvar debian-bug-mail-maintonly-address
  "Debian Bug Tracking System <maintonly@bugs.debian.org>"
  "Address to use to send to the maintainer but not forward to the BTS.")

(defvar debian-bug-status-file "/var/lib/dpkg/status"
  "Debian installed package status file.
Used to get list of packages for prompt completion, and for report generation
when the shell commands \"bug\" and \"reportbug\" are not available")

(defvar debian-bug-severity-alist
  '(("critical") ("grave") ("serious") ("important")
    ("normal") ("minor") ("wishlist"))
  "Alist of possible bug severities used for prompt completion.")

(defvar debian-bug-tags-alist
  '(("patch") ("security") ("upstream"))
  ;;'(("patch") ("security") ("upstream") ("potato") ("woody") ("sarge") ("sid"))
  "Alist of valid Tags aimed at Debian users.
The complete list of valid tags is longer, but the others are for use by
Debian maintainers.")

(defvar debian-bug-alltags-alist
  '(("patch") ("wontfix") ("moreinfo") ("unreproducible") ("help") ("pending")
    ("fixed") ("fixed-in-experimental") ("fixed-upstream") ("security")
    ("upstream") ("confirmed") ("d-i") ("ipv6") ("lfs") ("l10n") ("potato")
    ("woody") ("sarge") ("sarge-ignore") ("etch") ("etch-ignore") ("sid")
    ("experimental"))
  "Alist of all valid Tags, aimed at Debian developpers.")

(defvar debian-bug-pseudo-packages
  '("base" "bugs.debian.org" "buildd.debian.org" "buildd.emdebian.org"
    "cdimage.debian.org" "cdrom" "debian-i18n" "debian-maintainers"
    "ftp.debian.org" "general" "installation-reports" "lists.debian.org"
    "mirrors" "nm.debian.org" "press" "project" "qa.debian.org" "release-notes"
    "release.debian.org" "security-tracker" "security.debian.org"
    "snapshot.debian.org" "tech-ctte" "upgrade-reports" "wiki.debian.org"
    "wnpp" "www.debian.org")

  "List of Debian pseudo-packages available for completion.
See http://www.debian.org/Bugs/pseudo-packages")

(defvar debian-bug-packages-obarray nil
  "List of Debian packages from status file used for completion.")

(defvar debian-bug-packages-date nil
  "Last modification time of status file used for internal package list.
Used to determine if internal list is uptodate.")

(defvar debian-bug-package-name nil
  "Buffer-local variable holding the package name for this submission.")
(make-variable-buffer-local 'debian-bug-package-name)

(defvar debian-bug-bts-address "bugs.debian.org"
  "Name of BTS to which the bug report will be submitted.")
(make-variable-buffer-local 'debian-bug-bts-address)

(defvar debian-bug-easymenu-list nil
  "Holds the dynamically built easymenu list.")
(defvar debian-bug-bugs-menu nil
  "Buffer local Bugs menu.")
(make-variable-buffer-local 'debian-bug-bugs-menu)
(defvar debian-bug-alist nil
  "Buffer local alist of bug numbers (and description) for this package.")
(make-variable-buffer-local 'debian-bug-alist)
(defvar debian-bug-open-alist nil
  "Buffer local alist of open bug numbers (and description) for this package.")
(make-variable-buffer-local 'debian-bug-open-alist)

(defalias 'report-debian-bug 'debian-bug)

;;; Functions:
(autoload 'reporter-compose-outgoing "reporter")
(autoload 'mail-header-end "sendmail")
(autoload 'match-string-no-properties "poe") ;XEmacs
(autoload 'debian-changelog-suggest-package-name "debian-changelog-mode")
(autoload 'debian-changelog-close-bug "debian-changelog-mode")
(autoload 'mh-find-path "mh-utils")
(autoload 'mh-expand-file-name "mh-utils")
(autoload 'mh-visit-folder "mh-e")
(autoload 'mh-exec-cmd-quiet "mh-utils")
(autoload 'mh-inc-folder "mh-e")

(defun debian-bug-intern (pair)
  "Simple function to intern PAIR of car cdr in `debian-bug-packages-obarray'."
  (set (intern (car pair) debian-bug-packages-obarray) (cdr pair)))

(defun debian-bug-fill-packages-obarray ()
  "Build `debian-bug-packages-obarray' and return its value.
The obarray associates each package with the installed version of the package."
  (if (not (and (vectorp debian-bug-packages-obarray)
                (equal debian-bug-packages-date
                       (nth 5 (file-attributes debian-bug-status-file)))))
      (let ((case-fold-search t)
            (packages (length debian-bug-pseudo-packages))
            (real-pkgs '())
            this-pkg this-ver)
        (message "Building list of installed packages...")
        (with-temp-buffer
          (insert-file-contents-literally debian-bug-status-file)
          (while (not (eobp))
            (cond ((looking-at "$")
                   (if (and this-pkg this-ver)
                       (setq real-pkgs (cons (cons this-pkg this-ver) real-pkgs)
                             packages (1+ packages)))
                   (setq this-pkg nil
                         this-ver nil))
                  ((looking-at "Package *: *\\([^ ]*\\)$")
                   (setq this-pkg (match-string 1)))
                  ((looking-at "Version *: *\\([^ ]*\\)$")
                   (setq this-ver (match-string 1))))
            (forward-line)))
        (setq debian-bug-packages-obarray
              (make-vector (1- (ash 4 (logb packages))) 0)
              debian-bug-packages-date
              (nth 5 (file-attributes debian-bug-status-file)))
        (mapcar 'debian-bug-intern (mapcar 'list debian-bug-pseudo-packages))
        (mapcar 'debian-bug-intern real-pkgs)
        (message "Building list of installed packages...done")))
  (if debian-bts-control-for-emacs
      '(("bzr") ("debbugs.gnu.org") ("gnus") ("octave")
        ("other") ("rmail"))
    debian-bug-packages-obarray))

(defun debian-bug-check-for-program (program)
  "Check if PROGRAM is installed on the system.
Done by calling `executable-find' or the external \"which\" utility."
  (if (fboundp 'executable-find)
      (executable-find program)
    (zerop (call-process "which" nil nil nil program))))

(defun debian-bug-helper-program ()
  "Return helper program found on system.
This can be removed at some point since `bug' is not released in sarge."
  (or debian-bug-helper-program
      (cond
       ((debian-bug-check-for-program "reportbug")
        'reportbug)
       ((debian-bug-check-for-program "bug")
        'bug)
       (t
        'none))))

(defun debian-bug-read-control-file-field (package field)
  "In the control file of PACKAGE, return the value of FIELD.
This is achieved by parsing the output of dpkg -s.  If the field
doesn't exist, nil is returned."
  (let ((case-fold-search t))
    (with-temp-buffer
      (call-process "dpkg" nil '(t nil) nil "-s" package)
      (goto-char (point-min))
      (if (re-search-forward
           (concat "^" field " *: *\\(.+\\)$") nil t)
          (match-string 1)))))

(defun debian-bug-read-bug-control-file-field (package field)
  "In the bug control file of PACKAGE, return the value of FIELD if it exists.
Otherwise nil is returned."
  (let ((control (concat "/usr/share/bug/" package "/control"))
        (case-fold-search t))
    (if (file-readable-p control)
        (with-temp-buffer
          (insert-file-contents-literally control)
          (goto-char (point-min))
          (if (re-search-forward
               (concat "^" field " *: *\\(.+\\)$") nil t)
              (match-string 1))))))

(defun debian-bug-find-bts-address (package)
  "Return address of BTS where bug reports on PACKAGE should be submitted.
This is specified by either the Bugs field in the control file for PACKAGE,
or the Send-To field in the file /usr/share/bug/PACKAGE/control.  If neither
of these fields have been specified, the address of the Debian BTS is
returned.  Note that the address returned can be either a complete e-mail
address or the host address of the BTS.  In the latter case the address
must be expanded, by prepending \"submit\", \"maintonly\" or \"quiet\", as
appropriate, followed by the at-sign, before it can be used to submit bug
reports."
  (let ((bugs-field (debian-bug-read-control-file-field
                     package "Bugs"))
        (send-to-field (debian-bug-read-bug-control-file-field
                        package "Send-To")))
    (cond
     ((and bugs-field (string-match "^\\(debbugs://\\|mailto:\\)\\(.+\\)$"
                                    bugs-field))
      (match-string 2 bugs-field))
     (send-to-field send-to-field)
     (t "bugs.debian.org"))))

(defun debian-bug-bts-mail (type bts-address)
  "Return the complete e-mail address which should be used to submit the bug.
The TYPE parameter is typically either of the strings \"submit\",
\"quiet\" or \"maintonly\".  However, if BTS-ADDRESS is already a
complete e-mail address, the TYPE parameter is ignored, and this
function simply returns BTS-ADDRESS."
  (if (string-match "@" bts-address)
      bts-address
    (concat type "@" bts-address)))

(defun debian-bug-prefill-report (package severity)
  "Prefill bug report for PACKAGE at SEVERITY, calling bug or reportbug."
  (cond
   ;; bug
   ((and (eq (debian-bug-helper-program) 'bug)
         (intern-soft package debian-bug-packages-obarray))
    (save-excursion
      (call-process "bug" nil '(t t) nil "-p" "-s" "" "-S" severity package))
    (forward-line 4))

   ;; reportbug
   ((eq (debian-bug-helper-program) 'reportbug)
    (save-excursion
      (call-process "reportbug" nil '(t t) nil
                    "--template" "-T" "none" "-s" "none" "-S" "normal" "-b"
                    "--list-cc=none" "--no-bug-script"
                    "-q" package)
      (debian-bug--set-severity severity))
    ;; delete the mail headers, leaving only the BTS pseudo-headers
    (delete-region
     (point)
     (or (search-forward "\n\n" nil t)
         ;; Fix from Matt Swift
         (error "Reportbug did not produce expected output!  Bailing out.
Reportbug may have sent an empty report!")))
    ;; and skip forward to them
    (search-forward "\n\n" nil t)
    )

   ;; neither reportbug nor bug
   (t
    (insert
     "Package: " (or (debian-bug-read-bug-control-file-field
                      package "Submit-As")
                     package)
     "\nVersion: " (let ((sym (intern-soft package debian-bug-packages-obarray)))
                     (or (if (boundp sym) (symbol-value sym))
                         (format-time-string "+N/A; reported %Y-%m-%d")))
     "\nSeverity: " severity
     "\n\n\n\n-- System Information"
     "\nDebian Release: ")

    (if (file-readable-p "/etc/debian_version")
        (forward-char (cadr
                       (insert-file-contents-literally "/etc/debian_version")))
      (insert "unknwown\n"))

    (insert "Kernel Version: ")
    (call-process "uname" nil '(t t) nil "-a")
    (forward-line -5))))

(defun debian-bug-help-presubj (package)
  "Display contents of /usr/share/bug/PACKAGE/presubj."
  (let ((presubj (concat "/usr/share/bug/" package "/presubj")))
    (if (file-readable-p presubj)
        (with-output-to-temp-buffer "*Help*"
          (with-current-buffer "*Help*"
            (insert-file-contents presubj))))))

(defun debian-bug-file-is-executable (file)
  "Return non-nil if FILE is executable.  Otherwise nil is returned."
  (and
   (file-regular-p file)
   (string-match "-..x..x..x" (nth 8 (file-attributes file)))))

(defun debian-bug-find-bug-script (package)
  "Return the full path name of the bug script of PACKAGE.
If such script exists, otherwise nil is returned."
  (let ((script-alt1 (concat "/usr/share/bug/" package "/script"))
        (script-alt2 (concat "/usr/share/bug/" package)))
    (cond
     ((debian-bug-file-is-executable script-alt1) script-alt1)
     ((debian-bug-file-is-executable script-alt2) script-alt2))))

(defun debian-bug-script-sentinel
  (process event package severity subject filename
           bug-script-temp-file win-config)
  "This function is the process sentinel for bug script processes.
When called, if the process has terminated, this function cleans
up the buffer used by the process and proceeds to the next step in the
bug reporting process by calling `debian-bug-compose-report'. Note that
this process sentinel is different from regular process sentinels in
that it requires more arguments. So, it cannot be assigned to a process
with `set-process-sentinel' directly, but requires some tweaking instead."
  (if (memq (process-status process) '(exit signal))
      (let* ((bug-script-buffer
              (process-buffer process))
             (bug-script-buffer-empty
              (= (buffer-size bug-script-buffer) 0)))

        ;; Call the process sentinel provided by the term module, to
        ;; clean up the terminal buffer. The sentinel will print a
        ;; message in the buffer, so we have been careful to check
        ;; whether the buffer is empty above, before this call.
        ;; Note, XEmacs' term module doesn't provide this sentinel.
        (if (fboundp 'term-sentinel)
            (term-sentinel process event))

        ;; The reportbug program doesn't seem to care about the exit
        ;; status of a bug script, so we won't do it either.
        ;; (if (/= (process-exit-status process) 0)
        ;;    (error (concat "Error occured while collecting"
        ;;                    " information about the package")))

        ;; If there is a window displaying the bug script buffer,
        ;; restore the original window configuration, because it
        ;; might have been changed when the bug script buffer was
        ;; displayed. Otherwise, if the buffer isn't visible,
        ;; assume that the window configuration hasn't changed, so
        ;; don't restore anything.
        (if (get-buffer-window bug-script-buffer)
            (set-window-configuration win-config))

        ;; If the process output buffer still exists, kill it if it's
        ;; empty, otherwise bury it.
        (if (buffer-name bug-script-buffer)
            (if bug-script-buffer-empty
                (kill-buffer bug-script-buffer)
              (bury-buffer bug-script-buffer)))

        (debian-bug-compose-report package severity subject filename
                                   bug-script-temp-file))))

(defun debian-bug-run-bug-script (package severity subject filename)
  "Run a script, if provided by PACKAGE, to collect information.
The information about the package which should be supplied with
the bug report, and then proceed to the next step in the bug
reporting process by calling `debian-bug-compose-report'."
  (let ((handler "/usr/share/reportbug/handle_bugscript")
        (bug-script (debian-bug-find-bug-script package)))
    (if (and bug-script
             (debian-bug-file-is-executable handler)
             (if (featurep 'xemacs)
                 (or (featurep 'term) (load "term" 'noerror))
               (require 'term nil 'noerror)))
        (let ((bug-script-buffer
               (get-buffer-create "*debian-bug-script*"))
              (bug-script-temp-file
               (cond ((fboundp 'make-temp-file)       ;; XEmacs doesn't know
                      (make-temp-file "debian-bug-")) ;; make-temp-file.
                     ((fboundp 'temp-directory)
                      (make-temp-name (expand-file-name
                                       "debian-bug-" (temp-directory))))
                     (t (error "Cannot create temporary file"))))
              (bug-script-process)

              ;; XEmacs' term module doesn't set the appropriate
              ;; coding system for process output from term-exec.
              ;; Thus the following workaround, otherwise the terminal
              ;; displayed by XEmacs can get messed up.
              (coding-system-for-read 'binary))

          (message (concat "Collecting information about the package."
                           " This may take some time."))
          (with-current-buffer bug-script-buffer
            (erase-buffer)
            (term-mode)
            (term-exec bug-script-buffer "debian-bug-script" handler nil
                       (list bug-script bug-script-temp-file))
            (setq bug-script-process
                  (get-buffer-process bug-script-buffer))

            ;; The process sentinel should handle process termination.
            ;; Note that we need to pass more information to the
            ;; process sentinel than just the process object and event
            ;; type. Ideally, the process property list seems suitable
            ;; for this purpose, but that is only supported in GNU
            ;; Emacs 22 and later. So, a hack is used to construct the
            ;; process sentinel with the required data on the fly.
            ;; However, I suspect there are better ways to do this,
            ;; perhaps to use lexical-let.
            (set-process-sentinel
             bug-script-process
             (list 'lambda '(process event)
                   (list 'debian-bug-script-sentinel 'process 'event
                         package severity subject filename
                         bug-script-temp-file
                         (current-window-configuration))))

            (term-char-mode)

            ;; The function set-process-query-on-exit-flag is only
            ;; available in GNU Emacs version 22 and later.
            (if (fboundp 'set-process-query-on-exit-flag)
                (set-process-query-on-exit-flag bug-script-process nil)))

          ;; Delay switching to the process output buffer by waiting
          ;; for output from the process, the process to terminate or
          ;; 200 seconds, because ideally we don't want to display the
          ;; buffer unless the process will be requesting input, but
          ;; it's no way to tell that in advance. If the process
          ;; prints to stdout, it's likely it will be expecting input,
          ;; so we display the buffer. If the process terminates with
          ;; no output, we simply don't do anything; the process
          ;; sentinel will kill the buffer, and proceed, upon process
          ;; termination.
          (accept-process-output bug-script-process 200)

          ;; Short wait required here for the process-status to be
          ;; updated. (Maybe a bug in Emacs?)
          (sleep-for 0.050)
          (if (not (memq (process-status bug-script-process)
                         '(exit signal)))
              (switch-to-buffer-other-window bug-script-buffer)))

      (debian-bug-compose-report package severity subject filename))))

(defun debian-bug-insert-bug-script-temp-file (temp-file)
  "Insert the output from the bug script, if any, into the current buffer."
  (when (and temp-file (file-readable-p temp-file))
    (save-excursion
      (next-line 1)
      (insert "\n")
      (insert "-- Package-specific info:\n")
      (let ((beg (point))
            (end (+ (point)
                    (nth 1 (insert-file-contents temp-file)))))
        (save-restriction
          (narrow-to-region beg end)
          (goto-char (point-max))
          (beginning-of-line)
          (when (not (looking-at "$"))
            (end-of-line)
            (insert "\n"))
          (when (or (and (boundp 'mml-mode) mml-mode)
                    (memq mail-user-agent '(mh-e-user-agent
                                            message-user-agent
                                            gnus-user-agent)))
            (mml-quote-region (point-min) (point-max))
            (goto-char (point-min))
            (insert "<#part type=\"text/plain\" disposition=attachment"
                    " description=\"Bug script output\">\n")
            (goto-char (point-max))
            (insert "<#/part>\n"))))
      (delete-file temp-file))))

(defun debian-bug-package (&optional package filename)
  "Submit a Debian bug report about PACKAGE."
  (if (or (not package) (string= "" package))
      (save-window-excursion
        (when debian-bug-display-help
          (debian-bug-help-pseudo-packages)
          ;; This lets M-<next> and M-<prev> scroll the pseudo-package list
          ;; window by making _it_ the other window.
          (if (get-buffer-window "*Help*")
              (select-window (get-buffer-window "*Help*"))))
        (setq package (completing-read
                       "Package name: "
                       (debian-bug-fill-packages-obarray)
                       nil nil nil nil (current-word)))))
  (if (string= package "wnpp")
      (debian-bug-wnpp)
    (debian-bug-fill-packages-obarray)
    (if (and (not (intern-soft package debian-bug-packages-obarray))
             (not (y-or-n-p
                   "Package does not appear to be installed.  Continue? ")))
        (error "Quitting"))
    (let ((severity (save-window-excursion
                      (if debian-bug-display-help
                          (debian-bug-help-severity))
                      (completing-read "Severity (default normal): "
                                       debian-bug-severity-alist
                                       nil t nil nil "normal")))
          (subject (save-window-excursion
                     (debian-bug-help-presubj package)
                     (read-string "(Very) brief summary of problem: "))))
      (debian-bug-run-bug-script package severity subject filename))))

(defun debian-bug-compose-report
  (package severity subject filename &optional bug-script-temp-file)
  "Compose the initial contents of the bug report and present it in a buffer.
The buffer will be completed by the user."
;;; (require 'reporter)
  (reporter-compose-outgoing)
  (if (and (equal mail-user-agent 'gnus-user-agent)
           (string-equal " *nntpd*" (buffer-name)))
      (set-buffer "*mail*"))            ; Bug in emacs21.1?  Moves to " *nntpd*"
  (goto-char (point-min))
  (when (re-search-forward "^cc:" nil t)
    (delete-region (match-beginning 0)(match-end 0))
    (insert "X-Debbugs-CC:"))
  (setq debian-bug-bts-address
        (debian-bug-find-bts-address package))
  (goto-char (point-min))
  (cond
   ((re-search-forward "To: " nil t)
    (insert debian-bug-mail-address))
   ((re-search-forward "To:" nil t)
    (insert " " debian-bug-mail-address))
   (t
    (insert "To: " debian-bug-mail-address)))
  (if (string-equal severity "minor")
      (debian-bug--set-bts-address
       (debian-bug-bts-mail "maintonly" debian-bug-bts-address))
    (debian-bug--set-bts-address
     (debian-bug-bts-mail "submit" debian-bug-bts-address)))
  (goto-char (point-min))
  (cond
   ((re-search-forward "Subject: " nil t)
    (insert package ": " subject))
   ((re-search-forward "Subject:" nil t)
    (insert " " package ": " subject))
   (t
    (insert "Subject: " package ": " subject)))
  (require 'sendmail)
  (goto-char (mail-header-end))
  (forward-line 1)
  (if (looking-at "^<#secure")          ;Skip over mml directives
      (forward-line 1))
  (message "Getting package information from reportbug...")
  (debian-bug-prefill-report package severity)
  (message "Getting package information from reportbug...done")
  (if debian-bug-use-From-address
      (debian-bug--set-custom-From))
  (if debian-bug-always-CC-myself
      (debian-bug--set-CC debian-bug-From-address "X-Debbugs-CC:"))
  (when filename
    (forward-char -1)
    (insert "File: " filename "\n")
    (forward-char 1))
  (debian-bug-insert-bug-script-temp-file bug-script-temp-file)
  (set-window-start (selected-window) (point-min) t)
  (setq debian-bug-package-name package)
  (debian-bug-minor-mode 1)
  (set-buffer-modified-p nil))

;;; ---------
;;; WNPP interface by Peter S Galbraith <psg@debian.org>, August 4th 2001
(defvar debian-bug-wnpp-alist
  '(("Intent to Package [ITP]" . "ITP")
    ("Orphaned [O]". "O")
    ("Request for Adoption [RFA]" . "RFA")
    ("Request For Package [RFP]" . "RFP")
    ("Request For Help [RFH]" . "RFH"))
  "Alist of WNPP possible bug reports.")

(defvar debian-bug-wnpp-severities
  '(("ITP" . "wishlist")
    ("O". "normal")
    ("RFA" . "normal")
    ("RFP" . "wishlist")
    ("RFH" . "normal"))
  "Bug severeties for each WNPP bug type.")

;;;###autoload
(defun debian-bug-wnpp (&optional action)
  "Submit a WNPP bug report to Debian.
Optional argument ACTION can be provided in programs."
  (interactive
   (list (completing-read
          "Action: (Press TAB) "  debian-bug-wnpp-alist nil t nil)))
  (if (or (not action) (string= action ""))
      (setq action (completing-read
                    "Action: (Press TAB) "  debian-bug-wnpp-alist nil t nil)))
  (if (or (not action) (string= action ""))
      (error "Nothing to do"))
  (require 'reporter)
  (debian-bug-fill-packages-obarray)
  (let* ((tag (cdr (assoc action debian-bug-wnpp-alist)))
         (severity (cdr (assoc tag debian-bug-wnpp-severities)))
         (package
          (completing-read
           (cond ((string-equal action "Intent to Package [ITP]")
                  "Proposed package name: ")
                 ((string-equal action "Request For Package [RFP]")
                  "Requested package name: ")
                 (t
                  "package name: "))
           debian-bug-packages-obarray nil nil nil))
         ;;FIXME: Should fetch description from system for "[O]" and "[ITO]"
         (description (read-string "Very short package description: "))
         (CC-devel (y-or-n-p "CC bug report to debian-devel? ")))
    (require 'reporter)
    (reporter-compose-outgoing)
    (if (and (equal mail-user-agent 'gnus-user-agent)
             (string-equal " *nntpd*" (buffer-name)))
        (set-buffer "*mail*"))          ; Bug in emacs21.1?  Moves to " *nntpd*"
    (goto-char (point-min))
    (if (re-search-forward "To:" nil t)
        (insert " " debian-bug-mail-address)
      (insert "To: " debian-bug-mail-address))
    (require 'sendmail)
    (goto-char (mail-header-end))
    (forward-line 1)
    (if (looking-at "^<#secure")        ;Skip over mml directives
        (forward-line 1))
    (save-excursion
      (goto-char (point-min))
      (if debian-bug-use-From-address
          (debian-bug--set-custom-From))
      (if debian-bug-always-CC-myself
          (debian-bug--set-CC debian-bug-From-address "X-Debbugs-CC:"))
      (if (re-search-forward "Subject: " nil t)
          (insert (format "%s: %s -- %s" tag package description))
        (re-search-forward "Subject:" nil t)
        (insert (format " %s: %s -- %s" tag package description)))
      (if CC-devel
          (debian-bug--set-CC "debian-devel@lists.debian.org"
                              "X-Debbugs-CC:")))
    (insert "Package: wnpp\n")
    (when (and (string-equal tag "ITP")
               debian-bug-From-address)
      (insert (format "Owner: %s\n" debian-bug-From-address)))
    (insert (format "Severity: %s\n\n" severity))
    (when (or (string-equal tag "ITP")
              (string-equal tag "RFP"))
      (insert
;;;    "< Enter some information about the package, upstream URL and license here. >\n"
       "* Package name    : " package "\n"
       "  Version         : \n"
       "  Upstream Author : \n"
       "* URL or Web page : \n"
       "* License         : \n"
       "  Description     : " description "\n")
      (forward-line -1))
    (set-window-start (selected-window) (point-min) t)
    (debian-bug-wnpp-minor-mode 1)
    (set-buffer-modified-p nil)))

;;;###autoload
(defun debian-bug-request-for-package ()
  "Shortcut for `debian-bug-wnpp' with RFP action."
  (interactive)
  (debian-bug-wnpp "Request For Package [RFP]"))
(defalias 'debian-bug-RFP 'debian-bug-request-for-package)

;;;###autoload
(defun debian-bug-intent-to-package ()
  "Shortcut for `debian-bug-wnpp' with ITP action (for Debian developers)."
  (interactive)
  (debian-bug-wnpp "Intent to Package [ITP]"))
(defalias 'debian-bug-ITP 'debian-bug-intent-to-package)

;;; font-lock by Peter S Galbraith <psg@debian.org>, August 11th 2001
(defvar debian-bug-font-lock-keywords
  '(("^ *\\(Package:\\) *\\([^ ]+\n\\)?"
     (1 font-lock-keyword-face)
     (2 font-lock-type-face nil t))
    ("^ *\\(Owner:\\) *\\(.+\n\\)?"
     (1 font-lock-keyword-face)
     (2 font-lock-type-face nil t))
    ("^ *\\(File:\\) *\\([^ ]+\n\\)?"
     (1 font-lock-keyword-face)
     (2 font-lock-type-face nil t))
    ("^ *\\(Version:\\) *\\([^ \n]+\n\\)?"
     (1 font-lock-keyword-face)
     (2 font-lock-type-face nil t))
    ("^ *\\(Tags:\\).*\\(\\(patch\\|experimental\\)\\)"
     (1 font-lock-keyword-face)
     (2 font-lock-type-face nil t))
    ("^ *\\(Tags:\\).*\\(security\\)"
     (1 font-lock-keyword-face)
     (2 font-lock-warning-face nil t))
    ("^ *\\(Severity:\\) *\\(\\(critical\\|grave\\|serious\\)\\|\\(important\\)\\|\\(normal\\)\\|\\(\\(minor\\)\\|\\(wishlist\\)\\)\\)"
     (1 font-lock-keyword-face)
     (3 font-lock-warning-face nil t)
     (4 font-lock-function-name-face nil t)
     (5 font-lock-type-face nil t)
     (6 font-lock-string-face nil t))
    ("^Subject: \\[\\(ITP\\|O\\|RFA\\|RFP\\)\\]"
     (1 font-lock-warning-face t t)))
  "Regexp keywords to fontify `debian-bug' reports.")

;;; ---------
;;; Menu-bar via minor-mode
;;  Peter S Galbraith <psg@debian.org>, August 12th 2001

(defun debian-bug--is-custom-From ()
  "Return t if first line begins in From:."
  (save-excursion
    (goto-char (point-min))
    (looking-at "^From:")))

(defun debian-bug--unset-custom-From ()
  "Remove From line in the mail header."
  (save-excursion
    (goto-char (point-min))
    (let ((header-end (re-search-forward "^-*$" nil t)))
      (goto-char (point-min))
      (when (re-search-forward "^From:" header-end t)
        (delete-region (progn (beginning-of-line)(point))
                       (progn (forward-line 1)(point)))))))

(defun debian-bug--set-custom-From ()
  "Set a From line using the `debian-bug-From-address' variable."
  (if (not debian-bug-From-address)
      (error "Variable debian-bug-From-address is unset, please customize it")
    (save-excursion
      (goto-char (point-min))
      (debian-bug--unset-custom-From)
      (insert "From: " debian-bug-From-address "\n"))))

(defun debian-bug--toggle-custom-From ()
  "Toggle the From line using the `debian-bug-From-address' variable."
  (if (debian-bug--is-custom-From)
      (debian-bug--unset-custom-From)
    (debian-bug--set-custom-From)))

(defun debian-bug--is-CC (address field)
  "Return t if ADDRESS is present in FIELD."
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search t))
      (re-search-forward
       (concat "^" field ".*" (regexp-quote address)) nil t))))

(defun debian-bug--remove-CC (address field &optional nocleanup)
  "Remove ADDRESS from FIELD.
Non-nil optional argument NOCLEANUP means remove empty field."
  (save-excursion
    (goto-char (point-min))
    (if (or (re-search-forward (concat "^" field ".*\\("
                                       (regexp-quote address) ", \\)") nil t)
            (re-search-forward (concat "^" field ".*\\(, "
                                       (regexp-quote address) "\\)") nil t)
            (re-search-forward (concat "^" field ".*\\("
                                       (regexp-quote address) "\\)") nil t))
        (delete-region (match-beginning 1)(match-end 1)))
    (goto-char (point-min))
    (if (and (not nocleanup)
             (re-search-forward (concat "^ *" field " *\n") nil t))
        (delete-region (match-beginning 0)(match-end 0)))))

(defun debian-bug--set-CC (address field)
  "Add ADDRESS to FIELD."
  (debian-bug--remove-CC address field t)
  (save-excursion
    (goto-char (point-min))
    (cond
     ((re-search-forward (concat "^" field " +$") nil t) ;Empty X-Debbugs-CC:
      (insert address))
     ((re-search-forward (concat "^" field "$") nil t) ;Empty X-Debbugs-CC:
      (insert " " address))
     ((re-search-forward (concat "^" field ".*$") nil t) ;Existing X-Debbugs-CC
      (insert ", " address))
     ((re-search-forward "^Subject:.*\n" nil t)
      (insert field " " address "\n"))
     ((re-search-forward "^To: .*\n" nil t)
      (insert field " " address "\n"))
     (t
      (insert field " " address "\n")))))

(defun debian-bug--toggle-CC (address field)
  "Add ADDRESS to FIELD or remove it if present."
  (if (debian-bug--is-CC address field)
      (debian-bug--remove-CC address field)
    (debian-bug--set-CC address field)))

(defun debian-bug--toggle-CC-myself ()
  "Toggle X-Debbugs-CC: or Cc: line for myself in the mail header."
  (when debian-bug-From-address
    (if debian-bug-minor-mode
        (debian-bug--toggle-CC debian-bug-From-address "X-Debbugs-CC:")
      (debian-bug--toggle-CC debian-bug-From-address "cc:"))))

(defun debian-bug--toggle-CC-devel ()
  "Toggle X-Debbugs-CC: or CC: line for debian-devel in the mail header."
  (if debian-bug-minor-mode
      (debian-bug--toggle-CC "debian-devel@lists.debian.org" "X-Debbugs-CC:")
    (debian-bug--toggle-CC "debian-devel@lists.debian.org" "cc:")))

(defun debian-bug--is-severity (severity)
  "Return t is current report has severity of SEVERITY."
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward "^ *Severity: +\\([a-zA-Z]+\\)" nil t)
        (let ((actualSeverity (match-string-no-properties 1)))
          (string= actualSeverity severity)))))

(defun debian-bug--set-severity (severity)
  "Set bug SEVERITY level."
  (interactive (list (completing-read "Severity: " debian-bug-severity-alist
                                      nil t nil nil)))
  (if (not severity)
      nil                               ; We're done!
    (save-excursion
      (goto-char (point-min))
      (cond
       ((re-search-forward "^ *Severity: \\([a-zA-Z]+\\)" nil t)
        (goto-char (match-beginning 1))
        (delete-region (match-beginning 1)(match-end 1))
        (insert severity))
       ((re-search-forward "^ *Version: .*\n" nil t)
        (insert "Severity: " severity))
       ((re-search-forward "^ *Package: .*\n" nil t)
        (insert "Severity: " severity))
       (t
        (forward-line 6)
        (insert "\nSeverity: " severity "\n"))))))


(defun debian-bug--is-tags (tag)
  "Return t if current report has a tags entry of TAG."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward (concat "^ *Tags:.*" tag) nil t)))

(defun debian-bug--remove-tags (tag &optional nocleanup)
  "Remove TAG.
Non-nil optional argument NOCLEANUP means remove empty field."
  (save-excursion
    (goto-char (point-min))
    (if (or (re-search-forward (concat "^ *Tags:.*\\(" tag ", \\)") nil t)
            (re-search-forward (concat "^ *Tags:.*\\(, " tag "\\)") nil t)
            (re-search-forward (concat "^ *Tags:.*\\(" tag "\\)") nil t))
        (delete-region (match-beginning 1)(match-end 1)))
    (goto-char (point-min))
    (if (and (not nocleanup)
             (re-search-forward "^ *Tags: *\n" nil t))
        (delete-region (match-beginning 0)(match-end 0)))))

(defun debian-bug--set-tags (tag)
  "Set TAG."
  (debian-bug--remove-tags tag t)
  (save-excursion
    (goto-char (point-min))
    (cond
     ((re-search-forward "^ *Tags: *$" nil t) ; Empty "Tags: "
      (insert tag))
     ((re-search-forward "^ *Tags:.*$" nil t) ; Existing "Tags: "
      (insert ", " tag))
     ((re-search-forward "^ *Severity: .*\n" nil t)
      (insert "Tags: " tag "\n"))
     ((re-search-forward "^ *Version: .*\n" nil t)
      (insert "Tags: " tag "\n"))
     ((re-search-forward "^ *Package: .*\n" nil t)
      (insert "Tags: " tag "\n"))
     (t
      (forward-line 6)
      (insert "\nTags: " tag "\n")))))

(defun debian-bug--toggle-tags (tag)
  "Toggle TAG."
  (interactive (list (completing-read "Tag: " debian-bug-tags-alist
                                      nil t nil nil)))
  (if (not tag)
      nil                               ; We're done!
    (if (debian-bug--is-tags tag)
        (debian-bug--remove-tags tag)
      (debian-bug--set-tags tag))))

(defun debian-bug--is-bts-address (address)
  "Return t if ADDRESS is present in address field."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward (concat "^To:.*" (regexp-quote address)) nil t)))

(defun debian-bug--set-bts-address (address)
  "Set \"To\" header field to ADDRESS."
  (interactive (list (completing-read "To: "
                                      '(("submit@bugs.debian.org")
                                        ("quiet@bugs.debian.org")
                                        ("maintonly@bugs.debian.org"))
                                      nil t nil nil)))
  (cond
   ((string= "submit@bugs.debian.org" address)
    (setq address debian-bug-mail-address))
   ((string= "quiet@bugs.debian.org" address)
    (setq address debian-bug-mail-quiet-address))
   ((string= "maintonly@bugs.debian.org" address)
    (setq address debian-bug-mail-maintonly-address)))
  (if (not address)
      nil                               ; We're done!
    (save-excursion
      (goto-char (point-min))
      (cond
       ((re-search-forward "^To: \\(.*\\)" nil t)
        (goto-char (match-beginning 1))
        (delete-region (match-beginning 1)(match-end 1))
        (insert address))
       (t
        (insert "To: " address "\n"))))))

(defun debian-bug-help-severity ()
  "Display severity help."
  (with-output-to-temp-buffer "*Help*"
    (princ  "Severity levels

 The bug system records a severity level with each bug report. This is set to
 normal by default, but can be overridden either by supplying a Severity line
 in the pseudo-header when the bug is submitted (see the instructions for
 reporting bugs), or by using the severity command with the control request
 server.

 The severity levels are:

 critical
      makes unrelated software on the system (or the whole system) break, or
      causes serious data loss, or introduces a security hole on systems
      where you install the package.
 grave
      makes the package in question unuseable or mostly so, or causes data
      loss, or introduces a security hole allowing access to the accounts of
      users who use the package.
 serious
      is a severe violation of Debian policy (that is, it violates a \"must\"
      or \"required\" directive), or, in the package maintainer's opinion,
      makes the package unsuitable for release.
 important
      a bug which has a major effect on the usability of a package, without
      rendering it completely unusable to everyone.
 normal
      the default value, applicable to most bugs.
 minor
      a problem which doesn't affect the package's usefulness, and is
      presumably trivial to fix.
 wishlist
      for any feature request, and also for any bugs that are very difficult
      to fix due to major design considerations.
 fixed
      for bugs that are fixed but should not yet be closed. This is an
      exception for bugs fixed by non-maintainer uploads. Note: the \"fixed\"
      tag should be used instead.

Certain severities are considered release-critical, meaning the bug will
have an impact on releasing the package with the stable release of
Debian.  Currently, these are critical, grave and serious.

Info from http://www.debian.org/Bugs/Developer#severities
Feb 8th 2002, checked Apr 22 2003.")))

(defun debian-bug-help-tags ()
  "Display tags help."
  (with-output-to-temp-buffer "*Help*"
    (princ
     "Tags for bug reports

 Each bug can have zero or more of a set of given tags. These tags are
 displayed in the list of bugs when you look at a package's page, and when
 you look at the full bug log.

 Tags can be set by supplying a Tags line in the pseudo-header when the bug
 is submitted (see the instructions for reporting bugs), or by using the
 tags command with the control request server. Separate multiple tags with
 commas, spaces, or both.

 The current bug tags are:

 patch
    A patch or some other easy procedure for fixing the bug is included in
    the bug logs. If there's a patch, but it doesn't resolve the bug adequately
    or causes some other problems, this tag should not be used.

 wontfix
    This bug won't be fixed.  Possibly because this is a choice between
    two arbitrary ways of doing things and the maintainer and submitter prefer
    different ways of doing things, possibly because changing the behaviour
    will cause other, worse, problems for others, or possibly for other
    reasons.

 moreinfo
    This bug can't be addressed until more information is provided by the
    submitter. The bug will be closed if the submitter doesn't provide more
    information in a reasonable (few months) timeframe. This is for bugs like
    \"It doesn't work\". What doesn't work?

 unreproducible
    This bug can't be reproduced on the maintainer's system. Assistance
    from third parties is needed in diagnosing the cause of the problem.

 help
    The maintainer is requesting help with dealing with this bug.

 pending
    A solution to this bug has been found and an upload will be made soon.

 fixed
    This bug is fixed or worked around (by a non-maintainer upload, for
    example), but there's still an issue that needs to be resolved. This tag
    replaces the old \"fixed\" severity.

 security
    This bug describes a security problem in a package (e.g., bad
    permissions allowing access to data that shouldn't be accessible, buffer
    overruns allowing people to control a system in ways they shouldn't be
    able to, denial of service attacks that should be fixed, etc). Most
    security bugs should also be set at critical or grave severity.

 upstream
    This bug applies to the upstream part of the package.

 confirmed
    The maintainer has looked at, understands, and basically agrees with
    the bug, but has yet to fix it. (Use of this tag is optional, it is
    intended mostly for maintainers who need to manage large numbers of open
    bugs.)  fixed-upstream

    The bug has been fixed by the upstream maintainer, but not yet in the
    package (for whatever reason: perhaps it is too complicated to backport
    the change or too minor to be worth bothering).

 fixed-in-experimental
    The bug has been fixed in the package of the experimental
    distribution, but not yet in the unstable distribution.

 d-i
    This bug is relevant to the development of debian-installer. It is
    expected that this will be used when the bug affects installer development
    but is not filed against a package that forms a direct part of the
    installer itself.

 ipv6
    This bug affects support for Internet Protocol version 6.

 lfs
    This bug affects support for large files (over 2 gigabytes).

 l10n
    This bug is relevant to the localisation of the package.

 potato
    This bug particularly applies to the potato release of Debian.

 woody
    This bug particularly applies to the woody distribution.

 sarge
    This bug should not be archived until it is fixed in sarge.

 sarge-ignore
    This release-critical bug is to be ignored for the purposes of
    releasing sarge. This tag should only be used by the release manager, do
    not set it yourself without explicit authorization from them.

 etch
    This bug should not be archived until it is fixed in etch.

 etch-ignore
    This release-critical bug is to be ignored for the purposes of
    releasing etch. This tag should only be used by the release manager, do
    not set it yourself without explicit authorization from them.

 sid
    This bug should not be archived until it is fixed in sid.

 experimental
    This bug should not be archived until it is fixed in experimental.

The meanings of the latter 6 tags have changed recently, the ignore tags
ignore the bug for the purpose of a testing propagation. The release tags,
which used to indicate which bugs affected a specific release now indicate
when a bug can be archived.
Info from http://www.debian.org/Bugs/Developer#tags
Sep 22, 2006")))

(defun debian-bug-help-pseudo-packages ()
  "Display pseudo-packages help."
  (with-output-to-temp-buffer "*Help*"
    (princ "List of Debian pseudo packages
 base — Base system general bugs
 bugs.debian.org — The bug tracking system, @bugs.debian.org
 buildd.debian.org — Problems and requests related to the Debian Buildds
 buildd.emdebian.org — Problems related to building packages for Emdebian
 cdimage.debian.org — CD Image issues
 cdrom — Installation system
 debian-i18n — Requests regarding Internationalization (i18n) of the distribution
 debian-maintainers — Problems and requests related to Debian Maintainers
 ftp.debian.org — Problems with the FTP site
 general — General problems (e.g. \"many manpages are mode 755\")
 installation-reports — Reports of installation problems with stable & testing
 lists.debian.org — The mailing lists, debian-*@lists.debian.org
 mirrors — Problems with the official mirrors
 nm.debian.org — New Maintainer process and nm.debian.org webpages
 press — Press release issues
 project — Problems related to project administration
 qa.debian.org — The Quality Assurance group
 release-notes — Problems with the Release Notes
 release.debian.org — Requests regarding Debian releases and release team tools
 security-tracker — The Debian Security Bug Tracker
 security.debian.org — The Debian Security Team
 snapshot.debian.org — Issues with the snapshot.debian.org service
 tech-ctte — The Debian Technical Committee (see the Constitution)
 upgrade-reports — Reports of upgrade problems for stable & testing
 wiki.debian.org — Problems with the Debian wiki
 wnpp — Work-Needing and Prospective Packages list
 www.debian.org — Problems with the WWW site

from http://www.debian.org/Bugs/pseudo-packages, May 13th 2010.
Copyright 1999 Darren O. Benham, 1997, 2003 nCipher Corporation Ltd,
1994-1997 Ian Jackson.
")))

(defun debian-bug-help-email ()
  "Display help about various bug report emails to use."
  (with-output-to-temp-buffer "*Help*"
    (princ "Info from http://www.debian.org/Bugs/Reporting
Aug 10th 2001

 If a bug report is minor, for example, a documentation typo or a
 trivial build problem, please adjust the severity appropriately and
 send it to maintonly@bugs instead of submit@bugs. maintonly will
 forward the report to the package maintainer only, it won't forward it
 to the BTS mailing lists.

 If you wish to report a bug to the bug tracking system that's already been
 sent to the maintainer, you can use quiet@bugs. Bugs sent to quiet@bugs
 will not be forwarded anywhere, only filed.

 Bugs sent to maintonly@bugs or to quiet@bugs are *still* posted to
 the Debian Bug Tracking System web site (--psg).")))


(easy-menu-define debian-bug-menu debian-bug-minor-mode-map
  "Debian Bug Mode Menu"
  '("Debian-Bug"
    ("Header"
     ["Custom From Address" (debian-bug--toggle-custom-From)
      :style toggle :active debian-bug-From-address
      :selected (debian-bug--is-custom-From)]
     "--"
     ["To BTS, Maintainer and Mailing Lists"
      (debian-bug--set-bts-address
       (debian-bug-bts-mail "submit" debian-bug-bts-address))
      :style radio
      :selected (debian-bug--is-bts-address
                 (debian-bug-bts-mail "submit" debian-bug-bts-address))]
     ["To BTS and Maintainer Only"
      (debian-bug--set-bts-address
       (debian-bug-bts-mail "maintonly" debian-bug-bts-address))
      :style radio
      :selected (debian-bug--is-bts-address
                 (debian-bug-bts-mail "maintonly" debian-bug-bts-address))]
     ["To BTS Only"
      (debian-bug--set-bts-address
       (debian-bug-bts-mail "quiet" debian-bug-bts-address))
      :style radio
      :selected (debian-bug--is-bts-address
                 (debian-bug-bts-mail "quiet" debian-bug-bts-address))]
     "--"
     ["CC debian-devel" (debian-bug--toggle-CC-devel)
      :style toggle
      :selected (debian-bug--is-CC
                 "debian-devel@lists.debian.org" "X-Debbugs-CC:")]
     ["CC me" (debian-bug--toggle-CC-myself)
      :style toggle :active debian-bug-From-address
      :selected (debian-bug--is-CC debian-bug-From-address "X-Debbugs-CC:")]
     )
    ("Severity"
     ["critical" (debian-bug--set-severity "critical")
      :style radio :selected (debian-bug--is-severity "critical")]
     ["grave" (debian-bug--set-severity "grave")
      :style radio :selected (debian-bug--is-severity "grave")]
     ["serious" (debian-bug--set-severity "serious")
      :style radio :selected (debian-bug--is-severity "serious")]
     ["important" (debian-bug--set-severity "important")
      :style radio :selected (debian-bug--is-severity "important")]
     ["normal" (debian-bug--set-severity "normal")
      :style radio :selected (debian-bug--is-severity "normal")]
     ["minor" (debian-bug--set-severity "minor")
      :style radio :selected (debian-bug--is-severity "minor")]
     ["wishlist" (debian-bug--set-severity "wishlist")
      :style radio :selected (debian-bug--is-severity "wishlist")]
     )
    ("Tags"
     ["Patch Included" (debian-bug--toggle-tags "patch")
      :style toggle :selected (debian-bug--is-tags "patch")]
     ["Security Issue!" (debian-bug--toggle-tags "security")
      :style toggle :selected (debian-bug--is-tags "security")]
     )
    ("Web View"
     ["Bugs for This Package" (debian-bug-web-bugs) t]
     ["Archived Bugs for This Package" (debian-bug-web-bugs t) t]
     ["Bug Number..." (debian-bug-web-bug) t]
     ["Package search for all releases" (debian-bug-web-packages) t]
     "-- Package Web Pages --"
     ["Stable" (debian-bug-web-package "stable") t]
     ["Testing" (debian-bug-web-package "testing") t]
     ["Unstable" (debian-bug-web-package "unstable") t]
     )
    ["Customize"
     (customize-group "debian-bug") (fboundp 'customize-group)]
    ("Help"
     ["Severities" (debian-bug-help-severity) t]
     ["Tags" (debian-bug-help-tags) t]
     ["Pseudo-Packages" (debian-bug-help-pseudo-packages) t]
     ["Addresses" (debian-bug-help-email) t]
     )
    ))

(defun debian-bug-minor-mode (arg)
  "Toggle `debian-bug' mode.
A positive prefix argument ARG turns on `debian-bug' mode\; a negative prefix
argument turn sit off."
  (interactive "P")
  (set (make-local-variable 'debian-bug-minor-mode)
       (if arg
           (> (prefix-numeric-value arg) 0)
         (not debian-bug-minor-mode)))
  (cond
   (debian-bug-minor-mode               ;Setup the minor-mode
    (if (fboundp 'font-lock-add-keywords)
        (font-lock-add-keywords nil debian-bug-font-lock-keywords t))
    (debian-bug-bug-menu-init debian-bug-minor-mode-map)
    (easy-menu-add debian-bug-menu))))

;; Install ourselves:
(or (assq 'debian-bug-minor-mode minor-mode-alist)
    (setq minor-mode-alist
          (cons '(debian-bug-minor-mode " DBug") minor-mode-alist)))
(or (assq 'debian-bug-minor-mode minor-mode-map-alist)
    (setq minor-mode-map-alist
          (cons (cons 'debian-bug-minor-mode debian-bug-minor-mode-map)
                minor-mode-map-alist)))

;;; ---------
;;; wnpp-minor-mode - like debian-bug-minor-mode but with limited menu

(defvar debian-bug-wnpp-minor-mode nil)
(defvar debian-bug-wnpp-minor-mode-map nil
  "Keymap for `debian-bug' minor mode.")
(if debian-bug-wnpp-minor-mode-map
    nil
  (setq debian-bug-wnpp-minor-mode-map (make-sparse-keymap)))

(easy-menu-define debian-bug-wnpp-menu debian-bug-wnpp-minor-mode-map
  "Debian Bug Mode Menu"
  '("Debian-Bug"
    ["Custom From address" (debian-bug--toggle-custom-From)
     :style radio :active debian-bug-From-address
     :selected (debian-bug--is-custom-From)]
    ["CC to debian-devel header line" (debian-bug--toggle-CC-devel)
     :style radio
     :selected (debian-bug--is-CC "debian-devel@lists.debian.org"
                                  "X-Debbugs-CC:")]
    ["CC to myself header line" (debian-bug--toggle-CC-myself)
     :style radio :active debian-bug-From-address
     :selected (debian-bug--is-CC debian-bug-From-address "X-Debbugs-CC:")]
    ["Customize debian-bug"
     (customize-group "debian-bug") (fboundp 'customize-group)]
    ))

(defun debian-bug-wnpp-minor-mode (arg)
  "Toggle `debian-bug' mode.
A positive prefix argument ARG turns on `debian-bug' mode\; a negative prefix
argument turn sit off."
  (interactive "P")
  (set (make-local-variable 'debian-bug-wnpp-minor-mode)
       (if arg
           (> (prefix-numeric-value arg) 0)
         (not debian-bug-wnpp-minor-mode)))
  (cond
   (debian-bug-wnpp-minor-mode          ;Setup the minor-mode
    (if (fboundp 'font-lock-add-keywords)
        (font-lock-add-keywords nil debian-bug-font-lock-keywords t))
    (easy-menu-add debian-bug-wnpp-menu))))

;; Install ourselves:
(or (assq 'debian-bug-wnpp-minor-mode minor-mode-alist)
    (setq minor-mode-alist
          (cons '(debian-bug-wnpp-minor-mode " WNPPBug") minor-mode-alist)))
(or (assq 'debian-bug-wnpp-minor-mode minor-mode-map-alist)
    (setq minor-mode-map-alist
          (cons (cons 'debian-bug-wnpp-minor-mode debian-bug-wnpp-minor-mode-map)
                minor-mode-map-alist)))

;;; ---------
;;; browse-url interfaces from debian-changelog-mode.el
;;  by Peter Galbraith, Feb 23 2001

;;;###autoload
(defun debian-bug-web-bugs (&optional archived)
  "Browse the BTS for this package via `browse-url'.
With optional argument prefix ARCHIVED, display archived bugs."
  (interactive "P")
  (if (not (featurep 'browse-url))
      (progn
        (load "browse-url" nil t)
        (if (not (featurep 'browse-url))
            (error "This function requires the browse-url elisp package"))))
  (let ((pkg-name (or debian-bug-package-name
                      (and (featurep 'debian-changelog-mode)
                           (debian-changelog-suggest-package-name))
                      (read-string "Package name: "))))
    (if (string-equal "" pkg-name)
        (message "No package name to look up")
      (if archived
          (browse-url
           (concat "http://bugs.debian.org/cgi-bin/pkgreport.cgi?src="
                   pkg-name "&archive=yes"))
        (browse-url (concat "http://bugs.debian.org/cgi-bin/pkgreport.cgi?src="
                            pkg-name)))
      (message "Looking up bugs for source package %s via browse-url"
               pkg-name))))

;;;FIXME - This might not be a source package name, and then the page
;;;        doesn't exist.
;;;###autoload
(defun debian-bug-web-developer-page ()
  "Browse the web for this package's developer page."
  (interactive)
  (if (not (featurep 'browse-url))
      (progn
        (load "browse-url" nil t)
        (if (not (featurep 'browse-url))
            (error "This function requires the browse-url elisp package"))))
  (let ((pkg-name (or debian-bug-package-name
                      (and (featurep 'debian-changelog-mode)
                           (debian-changelog-suggest-package-name))
                      (read-string "Package name: "))))
    (if (string-equal "" pkg-name)
        (message "No package name to look up")
      (or (string-match "^lib[a-zA-Z]" pkg-name)
          (string-match "^[a-zA-Z]" pkg-name))
      (browse-url (concat "http://packages.qa.debian.org/"
                          (match-string 0 pkg-name) "/" pkg-name ".html"))
      (message "Looking up developer web page for package %s via browse-url"
               pkg-name))))

(defun debian-bug-prompt-bug-number (prompt)
  "Prompt the user for a bug number using PROMPT."
  (require 'thingatpt)
  (let ((default-number)
        (item (word-at-point)))
    ;; First see if there's a number under point
    (if (and item
             (string-match "^[0-9]+[0-9]$" item))
        (setq default-number (match-string-no-properties 0 item))
      ;; If not, try for mail message header
      (save-excursion
        (goto-char (point-min))
        (if (re-search-forward "\\([0-9]+\\)@bugs.debian.org"
                               (mail-header-end) t)
            (setq default-number (match-string-no-properties 1)))))
    (list (completing-read (if default-number
                               (format "%s [%s]: " prompt default-number)
                             (format "%s: " prompt))
                           debian-bug-alist nil nil nil nil default-number))))

;;;###autoload
(defun debian-bug-web-bug (&optional bug-number)
  "Browse the BTS for BUG-NUMBER via `browse-url'."
  (interactive (debian-bug-prompt-bug-number "Bug number to lookup"))
  (if (not (featurep 'browse-url))
      (progn
        (load "browse-url" nil t)
        (if (not (featurep 'browse-url))
            (error "This function requires the browse-url elisp package"))))
  (if (or (not bug-number) (string-equal bug-number "none"))
      (setq bug-number (completing-read "Bug number to lookup: "
                                        debian-bug-alist nil nil)))
  (if (string-equal bug-number "")
      (message "No bug number to look up")
    (browse-url (concat debian-bug-bts-URL "archive=yes&bug=" bug-number))
    (message "Looking up bug number %s via browse-url" bug-number)))

;;;###autoload
(defun emacs-bug-web-bug (&optional bug-number)
  "Browse the Emacs BTS for BUG-NUMBER via `browse-url'."
  (interactive "NBug number: ")
  (let ((debian-bug-bts-URL
         "http://debbugs.gnu.org/cgi/bugreport.cgi?"))
    (debian-bug-web-bug (number-to-string bug-number))))

;;;###autoload
(defun debian-bug-web-this-bug-under-mouse (EVENT)
  "Browse the BTS via `browse-url' for the bug report number under mouse.
In a program, mouse location is in EVENT."
  (interactive "e")
  (mouse-set-point EVENT)
  (if (not (looking-at "[0-9]"))
      (error "Not a number under point/mouse"))
  (save-excursion
    (skip-chars-backward "0123456789")
    (if (looking-at "[0-9]+")
        (let ((bug-number (match-string 0)))
          (debian-bug-web-bug bug-number)))))

;;;###autoload
(defun debian-bug-web-packages ()
  "Search Debian web page for this package via `browse-url'."
  (interactive)
  (if (not (featurep 'browse-url))
      (progn
        (load "browse-url" nil t)
        (if (not (featurep 'browse-url))
            (error "This function requires the browse-url elisp package"))))
  (let ((pkg-name (or debian-bug-package-name
                      (and (featurep 'debian-changelog-mode)
                           (debian-changelog-suggest-package-name))
                      (read-string "Package name: "))))
    (if (string-equal "" pkg-name)
        (message "No package name to look up")
      (browse-url (concat "http://packages.debian.org/" pkg-name))
      ;; 2007-09-02 This URL is becoming obsolete...
      ;;       (concat
      ;;        "http://packages.debian.org/cgi-bin/search_packages.pl?keywords="
      ;;        pkg-name
      ;;        "&searchon=names&version=all&release=all")
      (message "Looking up web pages for package %s via browse-url" pkg-name))))

(defvar debian-bug-archive-alist
  '(("stable") ("testing") ("unstable"))
  "Alist of valid Debian archives for web interface (excludes experimental).")

(defvar debian-bug-archive-list
  '("stable" "testing" "unstable")
  "List of valid Debian archives.")

;;;###autoload
(defun debian-bug-web-package (archive)
  "Search Debian web page in ARCHIVE for this package via `browse-url'."
  (interactive "P")
  (if (not (featurep 'browse-url))
      (progn
        (load "browse-url" nil t)
        (if (not (featurep 'browse-url))
            (error "This function requires the browse-url elisp package"))))
  (let ((pkg-name (or debian-bug-package-name
                      (and (featurep 'debian-changelog-mode)
                           (debian-changelog-suggest-package-name))
                      (read-string "Package name: "))))
    (if (string-equal "" pkg-name)
        (message "No package name to look up")
      (if (not (member (list archive) debian-bug-archive-alist))
          (setq archive
                (completing-read "Debian archive: "
                                 debian-bug-archive-alist nil t nil)))
      (if (string-equal "" archive)
          (message "No archive name to look up")
        (browse-url (format "http://packages.debian.org/%s/%s"
                            archive pkg-name))
        (message "Looking up %s web page for package %s via browse-url"
                 archive pkg-name)))))

;;;-------------
;;; wget bug from BTS stuff - Peter Galbraith, August 2001
;;; from debian-changelog-mode.el

(defun debian-bug-menucount ()
  "Return the number of bug lines after wget process."
  (save-excursion
    (goto-char (point-min))
    (- (count-lines (point)(point-max)) 5)))

(defun debian-bug-menusplit-p (submenu)
  "Return t if we should split the menu, comparing bug numbers to frame size.
If SUBMENU is t, then check for current sexp submenu only."
  (let* ((menu-count (if submenu
                         (save-excursion
                           (count-lines (point)
                                        (progn (forward-sexp 1)(point))))
                       (debian-bug-menucount)))
         (frame-lines (cond ((< 60 (frame-height)) ;Big frames
                             (- (frame-height) 17))
                            ((< 40 (frame-height)) ;Med frames
                             (- (frame-height) 10))
                            (t
                             (- (frame-height) 6))))) ;Smaller frames
    (if (>= frame-lines menu-count)
        nil                             ; No split at all
      t)))

(defun debian-bug-submenusplit ()
  "Split this submenu, located in sexp."
  (save-excursion
    (save-restriction
      ;; First, narrow to submenu
      (narrow-to-region (point)
                        (progn (forward-sexp 1)(forward-char -2)(point)))
      (goto-char (point-min))
      (forward-line 1)
      ;; Now on first bug...
      (let ((lines (cond ((< 60 (frame-height)) ;Big frames
                          (- (frame-height) 25))
                         ((< 40 (frame-height)) ;Med frames
                          (- (frame-height) 20))
                         (t
                          (- (frame-height) 6)))) ;Smaller frames
            (start (point))
            (bugn-end)(bugn-beg))
        (while (< (point) (point-max))
          (forward-line lines)
          (beginning-of-line)
          (looking-at "^\\[\"\\(#?[0-9]+\\):")
          (setq bugn-end (match-string 1))
          (end-of-line)
          (insert ")")
          (goto-char start)
          (looking-at "^\\[\"\\(#?[0-9]+\\):")
          (setq bugn-beg (match-string 1))
          (insert (format "(\"%s-%s\"\n" bugn-beg bugn-end))
          (forward-line -1)
          (forward-sexp 1)
          (forward-line 1)
          (setq start (point))))))
  (forward-sexp 1)
  (beginning-of-line))

(defun debian-bug-wget-mbox (&optional bug-number)
  "Wget the mbox file for bug BUG-NUMBER and return the filename created."
  (if (not debian-bug-download-directory)
      (error "Please set ` debian-bug-download-directory'"))
  (if (and (not (file-exists-p debian-bug-download-directory))
           (make-directory debian-bug-download-directory)
           (not (file-exists-p debian-bug-download-directory)))
      (error "Please create directory %s" debian-bug-download-directory))
  (if (not bug-number)
      (setq bug-number (completing-read "Bug number to fetch: "
                                        debian-bug-alist nil nil)))
  (when bug-number
    (let ((filename (expand-file-name
                     (concat "debian-bug-"
                             (if debian-bug-package-name
                                 (concat debian-bug-package-name "-"))
                             bug-number)
                     debian-bug-download-directory))
          (status)
          (url (concat debian-bug-bts-URL "bug=" bug-number
                       "&mbox=yes&mboxmaint=yes")))
      (if (and (file-exists-p filename)
               (not (y-or-n-p "Bug file already exists.  Download again? ")))
          filename
        (message "Downloading bug %s..." bug-number )
        (setq status
              (call-process "wget" nil '(t t) nil "--quiet" "-O" filename url))
        (message "Downloading bug %s...done" bug-number)
        (if (= 0 status)
            filename
          (error "`wget' failed"))))))

;;;###autoload
(defun debian-bug-get-bug-as-file (&optional bug-number)
  "Read bug report #BUG-NUMBER as a regular file."
  (interactive (debian-bug-prompt-bug-number "Bug number to fetch"))
  (let ((filename (debian-bug-wget-mbox bug-number)))
    (find-file filename)
    (text-mode)))

;;;###autoload
(defun debian-bug-get-bug-as-email (&optional bug-number)
  "Read bug report #BUG-NUMBER via Email interface."
  (interactive (progn
                 ;; a second gnus in a second emacs can clobber .newsrc, ask
                 ;; the user to start gnus where they want it
                 (if (and (eq mail-user-agent 'gnus-user-agent)
                          (not (and (fboundp 'gnus-alive-p)
                                    (gnus-alive-p))))
                     (error "Please start `gnus' (or `gnus-slave') first"))
                 (debian-bug-prompt-bug-number "Bug number to fetch")))
  (run-hooks 'debian-bug-get-bug-as-email-hook)
  (cond
   ((and (eq mail-user-agent 'mh-e-user-agent)
         (featurep 'mh-inc))
    ;; MH-E
    (mh-find-path)
    (let* ((package-name (cond
                          (debian-bug-package-name
                           debian-bug-package-name)
                          ((featurep 'debian-changelog-mode)
                           (debian-changelog-suggest-package-name))
                          (t
                           (read-string "Package name: "))))
           (mh-e-folder (concat
                         (if debian-bug-mh-folder
                             (concat debian-bug-mh-folder "/")
                           "+debian-bug-")
                         (if package-name
                             (format "%s-" package-name))
                         bug-number)))
      (if (and (file-exists-p (mh-expand-file-name mh-e-folder))
               (not (y-or-n-p "Bug folder already exists.  Download again? ")))
          (mh-visit-folder mh-e-folder)
        (if (file-exists-p (mh-expand-file-name mh-e-folder))
            (mh-exec-cmd-quiet nil "rmf" mh-e-folder))
        (let ((filename (debian-bug-wget-mbox bug-number)))
          (mh-inc-folder filename mh-e-folder)
          (delete-file filename)))))
   ((eq mail-user-agent 'gnus-user-agent)
    (gnus-group-read-ephemeral-group
     bug-number `(nndoc "bug"
                        (nndoc-address ,(debian-bug-wget-mbox bug-number))
                        (nndoc-article-type mbox))
     nil
     ;; restore current window configuration after quitting the summary
     (cons (current-buffer) (current-window-configuration))))
   (t
    ;; rmail
    (let ((filename (debian-bug-wget-mbox bug-number)))
      (rmail filename)))))

;;;###autoload
(defun emacs-bug-get-bug-as-email (&optional bug-number)
  "Read Emacs bug report #BUG-NUMBER via Email interface."
  (interactive "NBug number: ")
  (let ((debian-bug-package-name "Emacs")
        (debian-bug-bts-URL "http://debbugs.gnu.org/cgi/bugreport.cgi?"))
    (debian-bug-get-bug-as-email (number-to-string bug-number))))

(defvar debian-changelog-menu)

(defun debian-bug-menu-action (bugnumber)
  "Do something with BUGNUMBER based on variable `debian-bug-menu-action'."
  (cond
   ((equal debian-bug-menu-action 'browse)
    (debian-bug-web-bug bugnumber))
   ((equal debian-bug-menu-action 'readfile)
    (debian-bug-get-bug-as-file bugnumber))
   ((equal debian-bug-menu-action 'email)
    (debian-bug-get-bug-as-email bugnumber))
   ((equal debian-bug-menu-action 'close)
    (debian-changelog-close-bug bugnumber))))

(defvar debian-changelog-mode-map)

(load "rfc2047" t t)
(defun debian-bug-rfc2047-decode-string (string)
  "Decode the quoted-printable-encoded STRING and return the results.
Only decodes if `rfc2047-decode-string' is available."
  (if (fboundp 'rfc2047-decode-string)
      (rfc2047-decode-string string)
    string))

(defvar debian-changelog-close-bug-statement)
(defun debian-bug-build-bug-menu (package &optional source)
  "Build a menu listing the bugs for PACKAGE.
Optionally, if SOURCE is t, make it a source package."
  (setq debian-bug-alist nil
        debian-bug-open-alist nil)
  (let ((debian-bug-tmp-buffer
         (get-buffer-create "*debian-bug-tmp-buffer*"))
        (bug-alist)
        (bug-open-alist)
        (bugs-are-open-flag t)
        (is-changelog-mode
         (and (equal major-mode 'debian-changelog-mode)
              (boundp 'debian-changelog-close-bug-takes-arg))))
    (save-excursion
      (set-buffer debian-bug-tmp-buffer)
      (insert "(setq debian-bug-easymenu-list\n'(\"Bugs\"\n")
      (insert "[\"* Regenerate list *\" (debian-bug-build-bug-this-menu) t]
      \"--\"
      [\"Browse\"
       (list (setq debian-bug-menu-action 'browse))
       :style radio :selected (equal debian-bug-menu-action 'browse)]
      [\"Read as a File\"
       (list (setq debian-bug-menu-action 'readfile))
       :style radio :selected (equal  debian-bug-menu-action 'readfile)]
      [\"Read as Email\"
       (list (setq debian-bug-menu-action 'email))
       :style radio :selected (equal  debian-bug-menu-action 'email)]\n")
      (if is-changelog-mode
          (insert "            [\"Close Bug\"
       (list (setq debian-bug-menu-action 'close))
       :style radio :selected (equal debian-bug-menu-action 'close)]\n"))
      (insert "      \"-\"\n")
      (with-temp-buffer
        (message "Fetching bug list...")
        (call-process "wget" nil '(t t) nil "--quiet" "-O" "-"
                      (concat
                       "http://bugs.debian.org/cgi-bin/pkgreport.cgi?"
                       (if source "src=" "pkg=")
                       package))
        (message "Fetching bug list...done")
        (goto-char (point-min))
        (while
            (re-search-forward
;;;          "\\(<H2.*</a>\\(.+\\)</H2>\\)\\|\\(<a href=\"\\(bugreport.cgi\\?bug=\\([0-9]+\\)\\)\">\\(.+: \\(.+\\)\\)</a>\\)"
             "\\(<H2.*</a>\\(.+\\)</H2>\\)\\|\\(<a href=\"\\(bugreport.cgi\\?bug=\\([0-9]+\\)\\)\">\\([^#].+\\)</a>\\)"
             nil t)
          (let ((type (match-string 2))
                ;;(URL (match-string 4))
                (bugnumber (match-string 5))
                (description (match-string 6))
                (shortdescription (match-string 6)))
            (cond
             ((string= type "-->"))     ;Do nothing
             (type
              (setq bugs-are-open-flag (not (string-match "resolved" type)))
              (save-excursion
                (set-buffer debian-bug-tmp-buffer)
                (insert "\"-\"\n\"" type "\"\n")))
             ((null description))                      ;Do nothing
             ((string-match "^#?[0-9]+$" description)) ;Do nothing
             (t
              (if (string-match "^[^ ]+: \\(.+\\)" description)
                  (setq shortdescription (match-string 1 description)))
              (setq bug-alist (cons (list bugnumber description) bug-alist))
              (when bugs-are-open-flag
                (when (and (re-search-forward
                            "Reported by: <a href=\"pkgreport.cgi\\?submitter=[^\"]+\">"
                            nil t)
                           (or (looking-at "&quot;\\(.*\\)&quot; &lt;")
                               (looking-at "\\(.*\\) &lt;")
                               (looking-at "\\(.*\\)<")))
                  (setq shortdescription
                        (concat "Bug fix: \"" shortdescription
                                "\", thanks to "
                                (debian-bug-rfc2047-decode-string
                                 (match-string 1))
                                " " (if (fboundp 'replace-regexp-in-string)
                                        (replace-regexp-in-string
                                         "%s" bugnumber
                                         (if (boundp 'debian-changelog-close-bug-statement)
                                             debian-changelog-close-bug-statement
                                           "(Closes: #%s)"))
                                      (debian-bug--rris
                                       "%s" bugnumber
                                       (if (boundp 'debian-changelog-close-bug-statement)
                                           debian-changelog-close-bug-statement
                                         "(Closes: #%s)"))))))
                (setq bug-open-alist
                      (cons
                       (list bugnumber shortdescription) bug-open-alist)))
              (save-excursion
                (set-buffer debian-bug-tmp-buffer)
                (insert
                 "["
                 (format "%S" (concat "#" bugnumber " "
                                      (if (< 60 (length description))
                                          (substring description 0 60)
                                        description)))
                 " (debian-bug-menu-action \"" bugnumber "\")"
                 " :active "
                 (if bugs-are-open-flag
                     "t"
                   "(not (eq debian-bug-menu-action 'close))")
                 "]\n")))))))
      (set-buffer debian-bug-tmp-buffer) ;Make sure we're here
      (insert "))")
      (when (debian-bug-menusplit-p nil)
        (goto-char (point-min))
        ;; First split on bug severities
        (when (and (re-search-forward "^\"-" nil t)
                   (re-search-forward "^\"" nil t))
          (when (search-forward " to upstream software authors"
                                (save-excursion (progn (end-of-line)(point)))
                                t)
            (replace-match " upstream"))
          (beginning-of-line)
          (insert "(")
          (while (and (re-search-forward "^\"-" nil t)
                      (re-search-forward "^\"" nil t))
            (when (search-forward " to upstream software authors"
                                  (save-excursion (progn (end-of-line)(point)))
                                  t)
              (replace-match " upstream"))
            (beginning-of-line)
            (insert ")("))
          (goto-char (point-max))
          (insert ")")
          ;; Next check for long menus, and split those again
          (goto-char (point-min))
          (while (re-search-forward "^)?(\"" nil t)
            (forward-char -2)
            (if (debian-bug-menusplit-p t)
                (debian-bug-submenusplit)
              (end-of-line)))
          ))
      (eval-buffer debian-bug-tmp-buffer)
      (kill-buffer nil)
      )
    (setq debian-bug-alist bug-alist)
    (setq debian-bug-open-alist bug-open-alist)
    (cond
     ((equal major-mode 'debian-changelog-mode)
      (easy-menu-define
        debian-bug-bugs-menu
        debian-changelog-mode-map "Debian Bug Mode Bugs Menu"
        debian-bug-easymenu-list)
      (cond
       ((string-match "XEmacs" emacs-version)
        (easy-menu-remove debian-bug-bugs-menu)
        (easy-menu-remove debian-changelog-menu)
        (easy-menu-add debian-bug-bugs-menu)
        (easy-menu-add debian-changelog-menu))))
     (t
      (easy-menu-define
        debian-bug-bugs-menu
        debian-bug-minor-mode-map "Debian Bug Mode Bugs Menu"
        debian-bug-easymenu-list)
      (cond
       ((string-match "XEmacs" emacs-version)
        (easy-menu-remove debian-bug-bugs-menu)
        (easy-menu-remove debian-bug-menu)
        (easy-menu-add debian-bug-bugs-menu)
        (easy-menu-add debian-bug-menu)))))))

(defun debian-bug-build-bug-this-menu ()
  "Regenerate Bugs list menu for this buffer's package."
  (if (and (featurep 'debian-changelog-mode)
           (debian-changelog-suggest-package-name))
      (debian-bug-build-bug-menu (debian-changelog-suggest-package-name) t)
    (let ((package (or (and (boundp 'debian-bug-package-name)
                            debian-bug-package-name)
                       (read-string "Package name: "))))
      (debian-bug-build-bug-menu package nil))))

(defun debian-bug-bug-menu-init (minor-mode-map)
  "Initialize empty bug menu.
Call this function from the mode setup with MINOR-MODE-MAP."
  (if debian-bug-menu-preload-flag
      (debian-bug-build-bug-this-menu)
    (easy-menu-define debian-bug-bugs-menu minor-mode-map
      "Debian Bug Mode Bugs Menu"
      '("Bugs"
        ["* Generate menu *" (debian-bug-build-bug-this-menu)
         (debian-bug-check-for-program "wget")])))
  (easy-menu-add debian-bug-bugs-menu))

;;;-------------
;;; debian-bug-filename - Peter Galbraith, July 2002.
;;;

(defun debian-bug-search-file (filename)
  "Search for FILENAME returning which package name it belongs to."
  (save-excursion
    (let ((tmp-buffer (get-buffer-create " *debian-bug-tmp*"))
          (expanded-file (expand-file-name filename))
          (package))
      (set-buffer tmp-buffer)
      (unwind-protect
          (progn
            (condition-case err
                (call-process "dlocate" nil '(t nil) nil "-S" expanded-file)
              (file-error
               (message "dlocate not installed...")))
            (goto-char (point-min))
            (when (re-search-forward
                   (concat "^\\(.*\\): " (regexp-quote expanded-file) "$")
                   nil t)
              ;; found one at least.  Try for another.
              (setq package (match-string 1))
              (when (re-search-forward
                     (concat "^.*: " (regexp-quote expanded-file) "$") nil t)
                (setq package nil)))
            (if package
                package
              (message "Calling dpkg for the search...")
              (erase-buffer)
              (call-process "dpkg" nil '(t nil) nil "-S"
                            (expand-file-name filename))
              (message "Calling dpkg for the search...done")
              (goto-char (point-min))
              (cond
               ((re-search-forward "not found.$" nil t)
                (message "%s not found in package list" filename)
                nil)
               ((re-search-forward "^\\(.*, .*\\): " nil t)
                (with-output-to-temp-buffer "*Help*"
                  (princ (format "Please refine your search,\nthere is more than one matched package:\n\n%s" (match-string 1))))
                nil)
               ((re-search-forward "^\\(.*\\): " nil t)
                (match-string 1))
               (t
                (message "%s not found in package list" filename)
                nil))))
        (kill-buffer tmp-buffer)))))

(defun debian-bug-filename ()
  "Submit a Debian bug report for a given filename's package."
  (let ((filename (read-file-name "Filename: " "/" nil t nil)))
    (cond
     ((string-equal "" filename)
      (message "Giving up"))
     (t
      (let ((package (debian-bug-search-file filename)))
        (if package
            (let ((answer (y-or-n-p (format "File is in package %s; continue? "
                                            package))))
              (when answer
                (debian-bug-package package filename)))))))))

;;;###autoload
(defun debian-bug ()
  "Submit a Debian bug report."
  (interactive)
  (let ((type (let ((cursor-in-echo-area t))
                (message
                 "Report a bug for a [P]ackage or [F]ile: (default P) ")
                (capitalize (read-char-exclusive)))))
    (cond
     ((or (equal 13 type)               ; <CR>
          (equal ?\r type)              ; <CR>
          (equal ?\  type)              ; <space>
          (equal 32 type)               ; <space>
          (equal ?p type)
          (equal ?P type))
      (debian-bug-package))
     ((equal ?F type)
      (debian-bug-filename))
     (t
      (message "Sorry, try that again")))))

(provide 'debian-bug)

;;; debian-bug.el ends here
