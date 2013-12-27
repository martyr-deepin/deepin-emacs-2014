;;; debian-el-loaddefs.el --- automatically extracted autoloads
;;
;;; Code:

(provide 'debian-el-loaddefs)

;;;### (autoloads (apt-sources-mode) "apt-sources" "apt-sources.el"
;;;;;;  (19215 18611))
;;; Generated autoloads from apt-sources.el

(autoload 'apt-sources-mode "apt-sources" "\
Major mode for editing apt's sources.list file.
Sets up command `font-lock-mode'.

\\{apt-sources-mode-map}

\(fn)" t nil)

;;;***

;;;### (autoloads (apt-utils-search apt-utils-show-package) "apt-utils"
;;;;;;  "apt-utils.el" (18850 53763))
;;; Generated autoloads from apt-utils.el

(autoload 'apt-utils-show-package "apt-utils" "\
Show information for a Debian package.
A selection of known packages is presented.  See `apt-utils-mode'
for more detailed help.  If NEW-SESSION is non-nil, generate a
new `apt-utils-mode' buffer.

\(fn &optional NEW-SESSION)" t nil)

(autoload 'apt-utils-search "apt-utils" "\
Search Debian packages for regular expression.
To search for multiple patterns use a string like \"foo && bar\".
The regular expression used to split the
terms (`apt-utils-search-split-regexp') is customisable.

\(fn)" t nil)

;;;***

;;;### (autoloads (deb-find deb-view-mode deb-view deb-view-dired-view)
;;;;;;  "deb-view" "deb-view.el" (19183 30392))
;;; Generated autoloads from deb-view.el

(autoload 'deb-view-dired-view "deb-view" "\
View Debian package control and data files.
Press \"q\" in either window to kill both buffers
and return to the dired buffer. See deb-view.

\(fn)" t nil)

(autoload 'deb-view "deb-view" "\
View Debian package DEBFILE's control and data files.
Press \"q\" in either window to kill both buffers.

In dired, press ^d on the dired line of the .deb file to view.
Or, execute: ESCAPE x deb-view RETURN, and enter the .deb file name
at the prompt.

\(fn DEBFILE)" t nil)

(autoload 'deb-view-mode "deb-view" "\
View mode for Debian Archive Files.

\(fn)" t nil)

(autoload 'deb-find "deb-view" "\
Search for deb files.
Use the method specified by the variable deb-find-method, and collect
output in a buffer.  See also the variable deb-find-directory.

This command uses a special history list, so you can
easily repeat a `deb-find' command.

\(fn)" t nil)

;;;***

;;;### (autoloads (debian-bug emacs-bug-get-bug-as-email debian-bug-get-bug-as-email
;;;;;;  debian-bug-get-bug-as-file debian-bug-web-package debian-bug-web-packages
;;;;;;  debian-bug-web-this-bug-under-mouse emacs-bug-web-bug debian-bug-web-bug
;;;;;;  debian-bug-web-developer-page debian-bug-web-bugs debian-bug-intent-to-package
;;;;;;  debian-bug-request-for-package debian-bug-wnpp) "debian-bug"
;;;;;;  "debian-bug.el" (19428 39961))
;;; Generated autoloads from debian-bug.el

(autoload 'debian-bug-wnpp "debian-bug" "\
Submit a WNPP bug report to Debian.
Optional argument ACTION can be provided in programs.

\(fn &optional ACTION)" t nil)

(autoload 'debian-bug-request-for-package "debian-bug" "\
Shortcut for `debian-bug-wnpp' with RFP action.

\(fn)" t nil)

(autoload 'debian-bug-intent-to-package "debian-bug" "\
Shortcut for `debian-bug-wnpp' with ITP action (for Debian developers).

\(fn)" t nil)

(autoload 'debian-bug-web-bugs "debian-bug" "\
Browse the BTS for this package via `browse-url'.
With optional argument prefix ARCHIVED, display archived bugs.

\(fn &optional ARCHIVED)" t nil)

(autoload 'debian-bug-web-developer-page "debian-bug" "\
Browse the web for this package's developer page.

\(fn)" t nil)

(autoload 'debian-bug-web-bug "debian-bug" "\
Browse the BTS for BUG-NUMBER via `browse-url'.

\(fn &optional BUG-NUMBER)" t nil)

(autoload 'emacs-bug-web-bug "debian-bug" "\
Browse the Emacs BTS for BUG-NUMBER via `browse-url'.

\(fn &optional BUG-NUMBER)" t nil)

(autoload 'debian-bug-web-this-bug-under-mouse "debian-bug" "\
Browse the BTS via `browse-url' for the bug report number under mouse.
In a program, mouse location is in EVENT.

\(fn EVENT)" t nil)

(autoload 'debian-bug-web-packages "debian-bug" "\
Search Debian web page for this package via `browse-url'.

\(fn)" t nil)

(autoload 'debian-bug-web-package "debian-bug" "\
Search Debian web page in ARCHIVE for this package via `browse-url'.

\(fn ARCHIVE)" t nil)

(autoload 'debian-bug-get-bug-as-file "debian-bug" "\
Read bug report #BUG-NUMBER as a regular file.

\(fn &optional BUG-NUMBER)" t nil)

(autoload 'debian-bug-get-bug-as-email "debian-bug" "\
Read bug report #BUG-NUMBER via Email interface.

\(fn &optional BUG-NUMBER)" t nil)

(autoload 'emacs-bug-get-bug-as-email "debian-bug" "\
Read Emacs bug report #BUG-NUMBER via Email interface.

\(fn &optional BUG-NUMBER)" t nil)

(autoload 'debian-bug "debian-bug" "\
Submit a Debian bug report.

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("debian-el.el" "gnus-BTS.el") (19428 40248
;;;;;;  570272))

;;;***

;;;### (autoloads (preseed-mode) "preseed" "preseed.el" (17245 35005))
;;; Generated autoloads from preseed.el

(autoload (quote preseed-mode) "preseed" "\
Major mode for editing debian-installer preseed files colourfully." t nil)

;;;***
