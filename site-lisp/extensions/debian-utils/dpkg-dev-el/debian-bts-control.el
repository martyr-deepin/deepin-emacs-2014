;;; debian-bts-control.el --- Create messages for Debian BTS control interface

;; Copyright (C) 2003, 2005, 2007, 2009 Peter S Galbraith
;;
;; Help text from http://www.debian.org/Bugs/server-control:
;; Debian BTS administrators <owner@bugs.debian.org>
;; Copyright 1999 Darren O. Benham, 1994-1997 Ian Jackson,
;;  1997 nCipher Corporation Ltd.
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; debian-bts-mode.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with your Debian installation, in /usr/share/common-licenses/GPL
;; If not, write to the Free Software Foundation, 675 Mass Ave,
;; Cambridge, MA 02139, USA.

;;; Commentary:
;;
;;  Use `M-x debian-bts-control' to create an initial message, and
;;  `M-x debian-bts-control' again (or `C-c C-b') to insert new directives.

;;; Change log:
;;
;; V1.00 30apr2003  Peter S Galbraith <psg@debian.org>
;;  - Initial release.
;; V1.01 23May2003  Peter S Galbraith <psg@debian.org>
;;  - Add `debian-bts-control-modes-to-reuse'.
;; V1.02 09Aug2003  Peter S Galbraith <psg@debian.org>
;;  - add `debian-bts-control-prompt' to Prompt for bug number using sensible
;;    default if found.
;; V1.03 03Sep2003  Peter S Galbraith <psg@debian.org>
;;  - Don't set `debian-bts-control-verbose-prompts-flag' to t for Emacs20
;;    since it can't display multi-line prompts. (Closes: #208553)
;; V1.04 05Sep2003  Peter S Galbraith <psg@debian.org>
;;  - debian-bts-help-control: was missing!
;; V1.05 18Sep2003  Peter S Galbraith <psg@debian.org>
;;  - Add `package', `owner' and `noowner'.
;; V1.06 05Oct2003 Peter S Galbraith <psg@debian.org>
;;  - Add tags "sarge-ignore" and "fixed-uptsream".
;; V1.07 03Nov2003 Peter S Galbraith <psg@debian.org>
;;  - Created defgroup debian-bts-control.
;; V1.08 20Nov2005 Peter S Galbraith <psg@debian.org>
;;  - patch from Jari Aalto <jari aalto A T cante net>:
;;     It is now possible to put point at "Bug#NNNN" e.g. in debian/changelog
;;     and use that as default number.
;;      (top level): Added '(require 'cl)
;;      (debian-bts-bug-number-at-point): New function.
;;      (debian-bts-control-prompt): Code structure slightly redesigned.
;;      (debian-bts-control): Use `debian-bts-bug-number-at-point' to
;;      set `number-default'.
;; V1.08 08Aug2007 Peter S Galbraith <psg@debian.org>
;;  - Use `C-c C-b' instead of `C-c c' (Closes: #435247).
;; V1.09 30Aug2007 Peter S Galbraith <psg@debian.org>
;;  - skip over mml directives (Closes: #392132)
;; V1.10 30Aug2007 Peter S Galbraith <psg@debian.org>
;;  - Add `fixed' `notfixed' `block' `unblock' `archive' `unarchive'
;;    `found' `notfound'.  (Closes: #391647)
;; V1.11 23Feb2009, Patch from Luca Capello <luca@pca.it>.
;;  - Add `debian-bts-control-cc-or-bcc' (Closes: #392494)
;; V1.12 11Nov2009 Peter S Galbraith <psg@debian.org>
;;  - Add `debian-bts-emailaddress' and `debian-bts-emaildomain'.
;;  - Add command `emacs-bts-control', new command to interface with Emacs BTS.
;; V1.13 21Nov2009 Peter S Galbraith <psg@debian.org>
;;  - Patches from Sven Joachim (Closes: #557408, #557412)
;; V1.14 19Dec2009 Peter S Galbraith <psg@debian.org>
;;  - Emacs BTS moved to debbugs.gnu.org
;; V1.15 22Feb2010 Peter S Galbraith <psg@debian.org>
;;  - add autoload cookie for `emacs-bts-control' (Closes: #565934)
;;; Code:

(eval-when-compile '(require 'cl))
(require 'debian-bug)
(autoload 'word-at-point "thingatpt")

(defgroup debian-bts-control nil
  "Create messages for Debian BTS control interface"
  :group 'debian-bug)

(defcustom debian-bts-control-verbose-prompts-flag t
  "Non-nil means to be very verbose for `debian-bts-control' prompts."
  :group 'debian-bts-control
  :type 'boolean
  :set (lambda (symbol value)
         (if (<= 21 emacs-major-version)
             (set-default symbol value)
           (message
            "debian-bts-control-verbose-prompts-flag overridden for Emacs20")
           (set-default symbol nil))))

(defcustom debian-bts-control-modes-to-reuse
  '(mh-letter-mode mail-mode message-mode)
  "List of modes in which calling `debian-bts-control' will reuse the buffer.
No new draft will be created.  Instead control@bugs.debian.org will be
added to the `debian-bts-control-cc-or-bcc' field and the commands added at
the top of the message."
  :group 'debian-bts-control
  :type '(repeat symbol))

(defcustom debian-bts-control-cc-or-bcc 'cc
  "Whether to use Cc: or Bcc: header."
  :group 'debian-bts-control
  :type '(choice (const cc) (const bcc)))

(defvar debian-bts-emailaddress "control@bugs.debian.org"
  "Email address to send control message to.")

(defvar debian-bts-emaildomain "bugs.debian.org"
  "Email address domain to send control message to.")

(defvar debian-bts-control-minor-mode nil)
(defvar debian-bts-control-minor-mode-map nil
  "Keymap for `debian-bts-control' minor mode.")
(if debian-bts-control-minor-mode-map
    nil
  (setq debian-bts-control-minor-mode-map (make-sparse-keymap))
  (define-key debian-bts-control-minor-mode-map "\C-c\C-b" 'debian-bts-control))

(easy-menu-define debian-bts-control-menu debian-bts-control-minor-mode-map
  "Debian Bug Mode Menu"
  '("Control"
    ("Header"
     ["Custom From Address" (debian-bug--toggle-custom-From)
      :style toggle :active debian-bug-From-address
      :selected (debian-bug--is-custom-From)]
     "--"
     ["CC debian-devel" (debian-bug--toggle-CC-devel)
      :style toggle
      :selected (debian-bug--is-CC "debian-devel@lists.debian.org" "cc:")]
     ["CC me" (debian-bug--toggle-CC-myself)
      :style toggle :active debian-bug-From-address
      :selected (debian-bug--is-CC debian-bug-From-address "cc:")]
     )
    "--"
    ["Package" (debian-bts-control "package") t]
    ["Reassign" (debian-bts-control "reassign") t]
    ["Reopen" (debian-bts-control "reopen") t]
    ["Owner" (debian-bts-control "owner") t]
    ["NoOwner" (debian-bts-control "noowner") t]
    ["Submitter" (debian-bts-control "submitter") t]
    ["Forwarded" (debian-bts-control "forwarded") t]
    ["NotForwarded" (debian-bts-control "notforwarded") t]
    ["Retitle" (debian-bts-control "retitle") t]
    ["Severity" (debian-bts-control "severity") t]
    ["Clone" (debian-bts-control "clone") t]
    ["Merge" (debian-bts-control "merge") t]
    ["UnMerge" (debian-bts-control "unmerge") t]
    ["Tags" (debian-bts-control "tags") t]
    ["Close" (debian-bts-control "close") t]
    "--"
    ("Web View"
     ["Bugs for a Package..." (debian-bug-web-bugs) t]
     ["Bug Number..." (debian-bug-web-bug) t]
     ["Package Info..." (debian-bug-web-packages) t]
     )
    ["Customize"
     (customize-group "debian-bug") (fboundp 'customize-group)]
    ("Help"
     ["Severities" (debian-bug-help-severity) t]
     ["Tags" (debian-bug-help-tags) t]
     ["Pseudo-Packages" (debian-bug-help-pseudo-packages) t]
     ;;   ["Addresses" (debian-bug-help-email) t]
     ["control commands" (debian-bts-help-control) t]
     )
    ))

;;  - Add `fixed' `notfixed' `block' `unblock' `archive' `unarchive'
;;    `found' `notfound'.  (Closes: #391647)

(defvar debian-bts-control-font-lock-keywords
  '(("#.*$" .  font-lock-comment-face)
    ("^ *thank.*$" . font-lock-function-name-face)
    ("^ *\\(found\\) +\\(-?[0-9]+\\) *\\(.*\\)$"
     (1 font-lock-function-name-face)
     (2 font-lock-type-face)
     (3 font-lock-string-face))
    ("^ *\\(notfound\\) +\\(-?[0-9]+\\) +\\(.+\\)$"
     (1 font-lock-function-name-face)
     (2 font-lock-type-face)
     (3 font-lock-string-face))
    ("^ *\\(archive\\) +\\(-?[0-9]+\\)$"
     (1 font-lock-function-name-face)
     (2 font-lock-type-face))
    ("^ *\\(unarchive\\) +\\(-?[0-9]+\\)$"
     (1 font-lock-function-name-face)
     (2 font-lock-type-face))
    ("^ *\\(block\\) +\\(-?[0-9]+\\) +\\(by\\) +\\(.+\\)$"
     (1 font-lock-function-name-face)
     (2 font-lock-type-face)
     (3 font-lock-function-name-face)
     (4 font-lock-string-face))
    ("^ *\\(unblock\\) +\\(-?[0-9]+\\) +\\(by\\) +\\(.+\\)$"
     (1 font-lock-function-name-face)
     (2 font-lock-type-face)
     (3 font-lock-function-name-face)
     (4 font-lock-string-face))
    ("^ *\\(fixed\\) +\\(-?[0-9]+\\) +\\(.+\\)$"
     (1 font-lock-function-name-face)
     (2 font-lock-type-face)
     (3 font-lock-string-face))
    ("^ *\\(notfixed\\) +\\(-?[0-9]+\\) +\\(.+\\)$"
     (1 font-lock-function-name-face)
     (2 font-lock-type-face)
     (3 font-lock-string-face))
    ("^ *\\(package\\)  +\\([a-z0-9\\.\\-]+\\)$"
     (1 font-lock-function-name-face)
     (2 font-lock-keyword-face nil t))
    ("^ *\\(owner\\) +\\(-?[0-9]+\\) +\\(\\(!\\)\\|\\(.+\\)\\)$"
     (1 font-lock-function-name-face)
     (2 font-lock-type-face)
     (4 font-lock-keyword-face nil t)
     (5 font-lock-string-face nil t))
    ("^ *\\(noowner\\) +\\(-?[0-9]+\\)"
     (1 font-lock-function-name-face)
     (2 font-lock-type-face))
    ("^ *\\(reassign\\) +\\(-?[0-9]+\\) +\\([a-z0-9\\.\\-]+\\)$"
     (1 font-lock-function-name-face)
     (2 font-lock-type-face)
     (3 font-lock-keyword-face nil t))
    ("^ *\\(reopen\\) +\\(-?[0-9]+\\) +\\(\\(!\\|=\\)\\|\\(.+\\)\\)$"
     (1 font-lock-function-name-face)
     (2 font-lock-type-face)
     (4 font-lock-keyword-face nil t)
     (5 font-lock-string-face nil t))
    ("^ *\\(submitter\\) +\\(-?[0-9]+\\) +\\(\\(!\\)\\|\\(.+\\)\\)$"
     (1 font-lock-function-name-face)
     (2 font-lock-type-face)
     (4 font-lock-keyword-face nil t)
     (5 font-lock-string-face nil t))
    ("^ *\\(forwarded\\) +\\(-?[0-9]+\\) +\\(.+\\)$"
     (1 font-lock-function-name-face)
     (2 font-lock-type-face)
     (3 font-lock-string-face))
    ("^ *\\(notforwarded\\) +\\(-?[0-9]+\\)"
     (1 font-lock-function-name-face)
     (2 font-lock-type-face))
    ("^ *\\(retitle\\) +\\(-?[0-9]+\\) +\\(.+\\)$"
     (1 font-lock-function-name-face)
     (2 font-lock-type-face)
     (3 font-lock-string-face))
    ("^ *\\(severity\\) +\\(-?[0-9]+\\) +\\(\\(critical\\|grave\\|serious\\)\\|\\(important\\)\\|\\(normal\\)\\|\\(\\(minor\\)\\|\\(wishlist\\)\\)\\)"
     (1 font-lock-function-name-face)
     (2 font-lock-type-face)
     (4 font-lock-warning-face nil t)
     (5 font-lock-keyword-name-face nil t)
     (6 font-lock-type-face nil t)
     (7 font-lock-string-face nil t))
    ("^ *\\(clone\\) +\\([0-9]+\\) +\\(-[0-9]+\\( +-[0-9]+\\)*\\)$"
     (1 font-lock-function-name-face)
     (2 font-lock-type-face)
     (3 font-lock-keyword-face))
    ("^ *\\(merge\\) +\\(-?[0-9]+ +-?[0-9]+\\( +-?[0-9]+\\)*\\)$"
     (1 font-lock-function-name-face)
     (2 font-lock-type-face)
     (3 font-lock-keyword-face))
    ("^ *\\(unmerge\\) +\\(-?[0-9]+\\)$"
     (1 font-lock-function-name-face)
     (2 font-lock-type-face))
    ("^ *\\(tags\\) +\\(-?[0-9]+\\) +\\([-+=]? +\\)?\\(security\\)"
     (1 font-lock-function-name-face)
     (2 font-lock-type-face)
     (3 font-lock-keyword-face nil t)
     (4 font-lock-warning-face))
    ("^ *\\(tags\\) +\\(-?[0-9]+\\) +\\([-+=]? +\\)?\\(patch\\|wontfix\\|moreinfo\\|unreproducible\\|help\\|pending\\|fixed-in-experimental\\|fixed-upstream\\|fixed\\|security\\|upstream\\|confirmed\\|d-i\\|ipv6\\|lfs\\|l10n\\|potato\\|woody\\|sarge-ignore\\|sarge\\|etch-ignore\\|etch\\|sid\\|experimental\\)"
     (1 font-lock-function-name-face)
     (2 font-lock-type-face)
     (3 font-lock-keyword-face nil t)
     (4 font-lock-keyword-face))
    ("^ *\\(close\\) +\\(-?[0-9]+\\)$"
     (1 font-lock-warning-face)
     (2 font-lock-type-face)))
  "Regexp keywords to fontify `debian-bts-control' reports.")

(defun debian-bts-control-minor-mode (arg)
  "Toggle `debian-bts-control' mode.
A positive prefix argument ARG turns on `debian-bts-control' mode\;
a negative prefix argument turns it off.
\\<debian-bts-control-minor-mode-map>
\\[debian-bts-control]\t\tAdd a control command to the current message."
  (interactive "P")
  (set (make-local-variable 'debian-bts-control-minor-mode)
       (if arg
           (> (prefix-numeric-value arg) 0)
         (not debian-bts-control-minor-mode)))
  (cond
   (debian-bts-control-minor-mode       ;Setup the minor-mode
    (if (fboundp 'font-lock-add-keywords)
        (font-lock-add-keywords nil debian-bts-control-font-lock-keywords t))
    )))

;; Install ourselves:
(or (assq 'debian-bts-control-minor-mode minor-mode-alist)
    (setq minor-mode-alist
          (cons '(debian-bts-control-minor-mode " DBugC") minor-mode-alist)))
(or (assq 'debian-bts-control-minor-mode minor-mode-map-alist)
    (setq minor-mode-map-alist
          (cons (cons 'debian-bts-control-minor-mode
                      debian-bts-control-minor-mode-map)
                minor-mode-map-alist)))

(defvar debian-bts-control-alist
  '(("reassign") ("severity") ("reopen") ("submitter") ("forwarded")
    ("notforwarded") ("retitle") ("clone") ("merge") ("unmerge")
    ("tags") ("close") ("package") ("owner") ("noowner") ("found")
    ("notfound") ("fixed") ("notfixed") ("block") ("unblock") ("archive")
    ("unarchive"))
  "List of available commands at control@bugs.debian.org.")

(defun debian-bts-bug-number-at-point ()
  "Read #NNNNNN from current point."
  (let ((item (word-at-point)))
    (if (and item
             (string-match "^[0-9]+[0-9]$" item))
        item)))

(defun debian-bts-control-prompt (prompt &optional number)
  "Prompt for bug number using sensible default if found."
  (let ((default-number number))
    (unless default-number
      (save-excursion
        (goto-char (point-min))
        (if (re-search-forward (concat "\\([0-9]+\\)@" debian-bts-emaildomain)
                               (mail-header-end) t)
            (setq default-number (match-string-no-properties 1)))))
    (if default-number
        (read-string (format "%s [%s]: " prompt default-number)
                     nil nil default-number)
      (read-string (format "%s: " prompt)))))

;;;###autoload
(defun debian-bts-control (action &optional arg)
  "Contruct a message with initial ACTION command for control@bugs.debian.org.
Contructs a new control command line if called from within the message
being constructed.

If prefix arg is provided, use the current buffer instead instead of
creating a new outgoing email message buffer.
The current buffer is also used if the current major mode matches one listed
in `debian-bts-control-modes-to-reuse'."
  (interactive (list (completing-read "Command: "
                                      debian-bts-control-alist nil nil)
                     current-prefix-arg))
  (let ((number-default (debian-bts-bug-number-at-point)))
    (cond
     ((or arg
          (and (car (memq t (mapcar '(lambda (item) (eq item major-mode))
                                    debian-bts-control-modes-to-reuse)))
               (not debian-bts-control-minor-mode)))
      (debian-bug--set-CC debian-bts-emailaddress
                          (concat
                           (symbol-name debian-bts-control-cc-or-bcc) ":"))
      (goto-char (point-min))
      (if (re-search-forward (concat "\\([0-9]+\\)@" debian-bts-emaildomain)
                             (mail-header-end) t)
          (setq number-default (match-string 1)))
      (goto-char (mail-header-end))
      (forward-line 1)
      (if (looking-at "^<#secure")      ;Skip over mml directives
          (forward-line 1))
      (insert "thanks\n\n")
      (debian-bts-control-minor-mode 1))
     ((not debian-bts-control-minor-mode)
      (reporter-compose-outgoing)
      (if (and (equal mail-user-agent 'gnus-user-agent)
               (string-equal " *nntpd*" (buffer-name)))
          (set-buffer "*mail*"))        ; Bug in emacs21.1?  Moves to " *nntpd*"
      (goto-char (point-min))
      (cond
       ((re-search-forward "To: " nil t)
        (insert debian-bts-emailaddress))
       ((re-search-forward "To:" nil t)
        (insert " " debian-bts-emailaddress))
       (t
        (insert "To: " debian-bts-emailaddress)))
      (if debian-bug-use-From-address
          (debian-bug--set-custom-From))
      (if debian-bug-always-CC-myself
          (debian-bug--set-CC debian-bug-From-address "cc:"))
      (goto-char (mail-header-end))
      (forward-line 1)
      (if (looking-at "^<#secure")      ;Skip over mml directives
          (forward-line 1))
      (insert "thanks\n")
      (debian-bts-control-minor-mode 1)))
    (goto-char (mail-header-end))
    (if (re-search-forward "^thank" nil t)
        (beginning-of-line)
      (goto-char (point-max)))
    (cond
     ((string-equal "package" action)
      (debian-bug-fill-packages-obarray)
      (let* ((verbose (if debian-bts-control-verbose-prompts-flag
                          "package [ packagename ... ]

 Limits the following commands so that they will only apply to bugs
 filed against the listed packages. You can list one or more
 packages. If you don't list any packages, the following commands will
 apply to all bugs. You're encouraged to use this as a safety feature
 in case you accidentally use the wrong bug numbers.

"
                        ""))
             (package (completing-read
                       (concat verbose "Package list to limit to: ")
                       (debian-bug-fill-packages-obarray) nil nil)))
        (insert (format "package %s\n" package))))
     ((string-equal "reassign" action)
      (debian-bug-fill-packages-obarray)
      (let* ((verbose (if debian-bts-control-verbose-prompts-flag
                          "reassign bugnumber package

 Records that bug #BUGNUMBER is a bug in PACKAGE. This can be used to
 set the package if the user forgot the pseudo-header, or to change an
 earlier assignment. No notifications are sent to anyone (other than the
 usual information in the processing transcript).

"
                        "Package to reassign to: "))
             (bug-number (debian-bts-control-prompt
                          (concat verbose "Bug number")
                          number-default))
             (package (completing-read
                       (concat verbose "Package to reassign to: ")
                       (debian-bug-fill-packages-obarray) nil nil)))
        (insert (format "reassign %s %s\n" bug-number package))))
     ((string-equal "reopen" action)
      (let* ((verbose (if debian-bts-control-verbose-prompts-flag
                          "reopen bugnumber [ originator-address | = | ! ]

 Reopens #BUGNUMBER if it is closed.

 By default, or if you specify =, the original submitter will remain the
 originator of the report.

 The originator will be set to the optional address you supply. If you wish
 to become the new originator of the reopened report you can use the !
 shorthand or specify your own email address.

 If the bug is not closed then \"reopen\" won't do anything, not even change
 the originator. To change the originator of an open bug report, use the
 \"submitter\" command; note that this will inform the original submitter of
 the change.

"
                        ""))
             (bug-number (debian-bts-control-prompt
                          (concat verbose "Bug number")
                          number-default))
             (originator (read-string
                          (concat verbose "Originator-address (optional): "))))
        (insert (format "reopen %s %s\n" bug-number originator))))
     ((string-equal "submitter" action)
      (let* ((verbose (if debian-bts-control-verbose-prompts-flag
                          "submitter bugnumber originator-address | !

 Changes the originator of #BUGNUMBER to ORIGINATOR-ADDRESS.

 If you wish to become the new originator of the report you can use the
 ! shorthand or specify your own email address.

 While the reopen command changes the originator of other bugs merged
 with the one being reopened, submitter does not affect merged bugs.

"
                        ""))
             (bug-number (debian-bts-control-prompt
                          (concat verbose "Bug number")
                          number-default))
             (originator (read-string
                          (concat verbose "Originator-address (optional): "))))
        (insert (format "submitter %s %s\n" bug-number originator))))
     ((string-equal "owner" action)
      (let* ((verbose (if debian-bts-control-verbose-prompts-flag
                          "owner bugnumber address | !

 Sets address to be the \"owner\" of #bugnumber. The owner of a bug
 claims responsibility for fixing it and will receive all mail
 regarding it. This is useful to share out work in cases where a
 package has a team of maintainers.

 If you wish to become the owner of the bug yourself, you can use the
 ! shorthand or specify your own email address.

"
                        ""))
             (bug-number (debian-bts-control-prompt
                          (concat verbose "Bug number")
                          number-default))
             (address (read-string
                       (concat verbose "address (optional): "))))
        (insert (format "owner %s %s\n" bug-number address))))
     ((string-equal "noowner" action)
      (let* ((verbose (if debian-bts-control-verbose-prompts-flag
                          "noowner bugnumber

 Forgets any idea that the bug has an owner other than the usual
 maintainer. If the bug had no owner recorded then this will do
 nothing.

"
                        ""))
             (bug-number (debian-bts-control-prompt
                          (concat verbose "Bug number")
                          number-default)))
        (insert (format "noowner %s\n" bug-number))))
     ((string-equal "forwarded" action)
      (let* ((verbose (if debian-bts-control-verbose-prompts-flag
                          "forwarded bugnumber address

 Notes that BUGNUMBER has been forwarded to the upstream maintainer at
 ADDRESS. This does not actually forward the report. This can be used to
 change an existing incorrect forwarded-to address, or to record a new
 one for a bug that wasn't previously noted as having been forwarded.

"
                        ""))
             (bug-number (debian-bts-control-prompt
                          (concat verbose "Bug number")
                          number-default))
             (address (read-string
                       (concat verbose "Forwarded-address: "))))
        (insert (format "forwarded %s %s\n" bug-number address))))
     ((string-equal "notforwarded" action)
      (let* ((verbose (if debian-bts-control-verbose-prompts-flag
                          "notforwarded bugnumber

 Forgets any idea that BUGNUMBER has been forwarded to any upstream
 maintainer. If the bug was not recorded as having been forwarded then
 this will do nothing.

"
                        ""))
             (bug-number (debian-bts-control-prompt
                          (concat verbose "Bug number")
                          number-default)))
        (insert (format "notforwarded %s\n" bug-number))))
     ((string-equal "retitle" action)
      (let* ((verbose (if debian-bts-control-verbose-prompts-flag
                          "retitle bugnumber new-title

 Changes the TITLE of a bug report to that specified (the default is the
 Subject mail header from the original report).

 Unlike most of the other bug-manipulation commands, when used on one of
 a set of merged reports this will change the title of only the
 individual bug requested, and not all those with which it is merged.

"
                        ""))
             (bug-number (debian-bts-control-prompt
                          (concat verbose "Bug number")
                          number-default))
             (title (read-string
                     (concat verbose "New title: "))))
        (insert (format "retitle %s %s\n" bug-number title))))
     ((string-equal "severity" action)
      (let* ((verbose (if debian-bts-control-verbose-prompts-flag
                          "severity bugnumber severity

 Set the severity level for bug report #BUGNUMBER to SEVERITY. No
 notification is sent to the user who reported the bug.

 Severities are critical, grave, serious, important, normal, minor, and
 wishlist.

 For their meanings, consult the Control->Help->Severities menu.

"
                        ""))
             (bug-number (debian-bts-control-prompt
                          (concat verbose "Bug number")
                          number-default))
             (severity (completing-read "Severity: " debian-bug-severity-alist
                                        nil t)))
        (insert (format "severity %s %s\n" bug-number severity))))
     ((string-equal "clone" action)
      (let* ((verbose (if debian-bts-control-verbose-prompts-flag
                          "clone bugnumber [ new IDs ]

 Duplicate a bug report. Useful when a single report indicates that
 multiple distinct bugs have occured. \"New IDs\" are negative numbers,
 separated by spaces, which may be used in subsequent control commands to
 refer to the newly duplicated bugs.
   Example usage:
     clone 12345 -1 -2
     reassign -1 foo
     retitle -1 foo: foo sucks
     reassign -2 bar
     retitle -2 bar: bar sucks when used with foo
     severity -2 wishlist
"
                        ""))
             (bug-number (debian-bts-control-prompt
                          (concat verbose "Bug number")
                          number-default))
             (ids (read-string (concat verbose "New IDs (e.g. -1 -2): "))))
        (insert (format "clone %s %s\n" bug-number ids))))
     ((string-equal "merge" action)
      (let* ((verbose (if debian-bts-control-verbose-prompts-flag
                          "merge bugnumber bugnumber ...

 Merges two or more bug reports. When reports are merged, opening, closing,
 marking or unmarking as forwarded and reassigning any of the bugs to a new
 package will have an identical effect on all of the merged reports.

 Before bugs can be merged they must be in exactly the same state.

"
                        ""))
             (bug-numbers (read-string (concat verbose "All bug numbers: "))))
        (insert (format "merge %s\n" bug-numbers))))
     ((string-equal "unmerge" action)
      (let* ((verbose (if debian-bts-control-verbose-prompts-flag
                          "unmerge bugnumber

 Disconnects a bug report from any other reports with which it may have
 been merged. If the report listed is merged with several others then
 they are all left merged with each other; only their associations with
 the bug explicitly named are removed.

"
                        ""))
             (bug-number (debian-bts-control-prompt
                          (concat verbose "Bug number")
                          number-default)))
        (insert (format "unmerge %s\n" bug-number))))
     ((string-equal "tags" action)
      (let* ((verbose (if debian-bts-control-verbose-prompts-flag
                          "tags bugnumber [ + | - | = ] tag

 Sets a particular tag for the bug report #BUGNUMBER to tag. No
 notification is sent to the user who reported the bug. + means adding, -
 means subtracting, and = means ignoring the current tags and setting them
 afresh. The default action is adding.

 Tags are patch, wontfix, moreinfo, unreproducible, help, pending, fixed,
 fixed-in-experimental, fixed-upstream, security, upstream, confirmed, d-i,
 ipv6, lfs, l10n, potato, woody, sarge, sarge-ignore, etch, etch-ignore,
 sid, and experimental.

 For their meanings, consult the Control->Help->Tags menu.

"
                        ""))
             (bug-number (debian-bts-control-prompt
                          (concat verbose "Bug number")
                          number-default))
             (add (completing-read "+, -, = (default +): "
                                   '(("+") ("-") ("=")) nil t nil nil "+"))
             (tag (completing-read "Tag: " debian-bug-alltags-alist nil t)))
        (insert (format "tags %s %s %s\n" bug-number add tag))))
     ((string-equal "close" action)
      (if (yes-or-no-p
           (concat "Deprecated in favor of #BUG-close@"
                   debian-bts-emaildomain ". Continue? "))
          (let* ((verbose (if debian-bts-control-verbose-prompts-flag
                              "close bugnumber

 Close bug report #BUGNUMBER.

 A notification is sent to the user who reported the bug, but (in contrast
 to mailing bugnumber-done@bugs) the text of the mail which caused the bug
 to be closed is not included in that notification. The maintainer who
 closes a report needs to ensure, probably by sending a separate message,
 that the user who reported the bug knows why it is being closed. The use of
 this command is therefore deprecated.

"
                            ""))
                 (bug-number (debian-bts-control-prompt
                              (concat verbose "Bug number")
                              number-default)))
            (insert (format "close %s\n" bug-number)))))
     ((string-equal "found" action)
      (let* ((verbose (if debian-bts-control-verbose-prompts-flag
                          "found bugnumber [version]

    Record that #bugnumber has been encountered in the given
    version of the package to which it is assigned.

    The BTS considers a bug to be open when it has no fixed
    version, or when it has been found more recently than it has
    been fixed.

    If no version is given, then the list of fixed versions for
    the bug is cleared. This is identical to the behaviour of
    reopen.

    This command will only cause a bug to be marked as not done
    if no version is specified, or if the version being marked
    found is equal to the version which was last marked
    fixed. (If you are certain that you want the bug marked as
    not done, use reopen in conjunction with found.)

"
                        ""))
             (bug-number (debian-bts-control-prompt
                          (concat verbose "Bug number")
                          number-default))
             (version (read-string (concat verbose "Version (if any): "))))
        (insert (format "found %s %s\n" bug-number version))))
     ((string-equal "notfound" action)
      (let* ((verbose (if debian-bts-control-verbose-prompts-flag
                          "notfound bugnumber version

    Remove the record that #bugnumber was encountered in the
    given version of the package to which it is assigned.

    This differs from closing the bug at that version in that the
    bug is not listed as fixed in that version either; no
    information about that version will be known. It is intended
    for fixing mistakes in the record of when a bug was found.

"
                        ""))
             (bug-number (debian-bts-control-prompt
                          (concat verbose "Bug number")
                          number-default))
             (version (read-string (concat verbose "Version: "))))
        (insert (format "notfound %s %s\n" bug-number version))))
     ((string-equal "fixed" action)
      (let* ((verbose (if debian-bts-control-verbose-prompts-flag
                          "fixed bugnumber version

    Indicate that bug #bugnumber was fixed in the given version
    of the package to which it is assigned.

    This does not cause the bug to be marked as closed, it merely
    adds another version in which the bug was fixed. Use the
    bugnumber-done address to close a bug and mark it fixed in a
    particular version.

"
                        ""))
             (bug-number (debian-bts-control-prompt
                          (concat verbose "Bug number")
                          number-default))
             (version (read-string (concat verbose "Version: "))))
        (insert (format "fixed %s %s\n" bug-number version))))
     ((string-equal "notfixed" action)
      (let* ((verbose (if debian-bts-control-verbose-prompts-flag
                          "notfixed bugnumber  version

    Remove the record that bug #bugnumber has been fixed in the
    given version.

    This command is equivalent to found followed by notfound (the
    found removes the fixed at a particular version, and notfound
    removes the found.)

"
                        ""))
             (bug-number (debian-bts-control-prompt
                          (concat verbose "Bug number")
                          number-default))
             (version (read-string (concat verbose "Version: "))))
        (insert (format "notfixed %s %s\n" bug-number version))))
     ((string-equal "block" action)
      (let* ((verbose (if debian-bts-control-verbose-prompts-flag
                          "block bugnumber by  bug ...

    Note that the fix for the first bug is blocked by the other
    listed bugs.

"
                        ""))
             (bug-number (debian-bts-control-prompt
                          (concat verbose "Bug number")
                          number-default))
             (by-bug (read-string (concat verbose "by bug number(s): "))))
        (insert (format "block %s by %s\n" bug-number by-bug))))
     ((string-equal "unblock" action)
      (let* ((verbose (if debian-bts-control-verbose-prompts-flag
                          "nblock bugnumber  by bug ...
    Note that the fix for the first bug is no longer blocked by the other listed bugs.

"
                        ""))
             (bug-number (debian-bts-control-prompt
                          (concat verbose "Bug number")
                          number-default))
             (by-bug (read-string (concat verbose "by bug number(s): "))))
        (insert (format "unblock %s by %s\n" bug-number by-bug))))
     ((string-equal "archive" action)
      (let* ((verbose (if debian-bts-control-verbose-prompts-flag
                          "archive bugnumber

    Archives a bug that had been archived at some point in the
    past but is currently not archived if the bug fulfills the
    requirements for archival, ignoring time.

"
                        ""))
             (bug-number (debian-bts-control-prompt
                          (concat verbose "Bug number")
                          number-default)))
        (insert (format "archive %s\n" bug-number))))
     ((string-equal "unarchive" action)
      (let* ((verbose (if debian-bts-control-verbose-prompts-flag
                          "unarchive bugnumber

    Unarchives a bug that was previously archived. Unarchival
    should generally be coupled with reopen and found/fixed as
    appropriate. Bugs that have been unarchived can be archived
    using archive assuming the non-time based archival
    requirements are met.

"
                        ""))
             (bug-number (debian-bts-control-prompt
                          (concat verbose "Bug number")
                          number-default)))
        (insert (format "unarchive %s\n" bug-number))))
     )))


(defun debian-bts-help-control ()
  (with-output-to-temp-buffer "*Help*"
    (princ
     "reassign bugnumber package

    Records that bug #bugnumber is a bug in package. This can be used to
    set the package if the user forgot the pseudo-header, or to change an
    earlier assignment. No notifications are sent to anyone (other than the
    usual information in the processing transcript).

reopen bugnumber [ originator-address | = | ! ]

    Reopens #bugnumber if it is closed.

    By default, or if you specify =, the original submitter is still as the
    originator of the report, so that they will get the ack when it is
    closed again.

    If you supply an originator-address the originator will be set to the
    address you supply. If you wish to become the new originator of the
    reopened report you can use the ! shorthand or specify your own email
    address.

    It is usually a good idea to tell the person who is about to be
    recorded as the originator that you're reopening the report, so that
    they will know to expect the ack which they'll get when it is closed
    again.

    If the bug is not closed then reopen won't do anything, not even change
    the originator. To change the originator of an open bug report, use the
    submitter command; note that this will inform the original submitter of
    the change.

found bugnumber [ version ]

    Record that #bugnumber has been encountered in the given
    version of the package to which it is assigned.

    The bug tracking system uses this information, in conjunction
    with fixed versions recorded when closing bugs, to display
    lists of bugs open in various versions of each package. It
    considers a bug to be open when it has no fixed version, or
    when it has been found more recently than it has been fixed.

    If no version is given, then the list of fixed versions for
    the bug is cleared. This is identical to the behaviour of
    reopen.

    This command will only cause a bug to be marked as not done
    if no version is specified, or if the version being marked
    found is equal to the version which was last marked
    fixed. (If you are certain that you want the bug marked as
    not done, use reopen in conjunction with found.)

    This command was introduced in preference to reopen because
    it was difficult to add a version to that command's syntax
    without suffering ambiguity.

notfound bugnumber version

    Remove the record that #bugnumber was encountered in the
    given version of the package to which it is assigned.

    This differs from closing the bug at that version in that the
    bug is not listed as fixed in that version either; no
    information about that version will be known. It is intended
    for fixing mistakes in the record of when a bug was found.

fixed bugnumber version

    Indicate that bug #bugnumber was fixed in the given version
    of the package to which it is assigned.

    This does not cause the bug to be marked as closed, it merely
    adds another version in which the bug was fixed. Use the
    bugnumber-done address to close a bug and mark it fixed in a
    particular version.

notfixed bugnumber version

    Remove the record that bug #bugnumber has been fixed in the given version.

    This command is equivalent to found followed by notfound (the found removes the fixed at a particular version, and notfound removes the found.)

submitter bugnumber originator-address | !

    Changes the originator of #bugnumber to originator-address.

    If you wish to become the new originator of the report you can use the
    ! shorthand or specify your own email address.

    While the reopen command changes the originator of other bugs merged
    with the one being reopened, submitter does not affect merged bugs.

forwarded bugnumber address

    Notes that bugnumber has been forwarded to the upstream maintainer at
    address. This does not actually forward the report. This can be used to
    change an existing incorrect forwarded-to address, or to record a new
    one for a bug that wasn't previously noted as having been forwarded.

notforwarded bugnumber

    Forgets any idea that bugnumber has been forwarded to any upstream
    maintainer. If the bug was not recorded as having been forwarded then
    this will do nothing.

retitle bugnumber new-title

    Changes the title of a bug report to that specified (the default is the
    Subject mail header from the original report.

    Unlike most of the other bug-manipulation commands when used on one of
    a set of merged reports this will change the title of only the
    individual bug requested, and not all those with which it is merged.

severity bugnumber severity

    Set the severity level for bug report #bugnumber to severity. No
    notification is sent to the user who reported the bug.

    Severities are critical, grave, serious, important, normal, minor, and
    wishlist.

    For their meanings please consult the general developers' documentation
    for the bug system.

clone bugnumber [ new IDs ]

    The clone control command allows you to duplicate a bug report. It is
    useful in the case where a single report actually indicates that
    multiple distinct bugs have occured. \"New IDs\" are negative numbers,
    separated by spaces, which may be used in subsequent control commands
    to refer to the newly duplicated bugs. A new report is generated for
    each new ID.

    Example usage:

        clone 12345 -1 -2
        reassign -1 foo
        retitle -1 foo: foo sucks
        reassign -2 bar
        retitle -2 bar: bar sucks when used with foo
        severity -2 wishlist
        clone 123456 -2
        reassign -2 foo
        retitle -2 foo: foo sucks
        merge -1 -2


merge bugnumber bugnumber ...

    Merges two or more bug reports. When reports are merged, opening,
    closing, marking or unmarking as forwarded and reassigning any of the
    bugs to a new package will have an identical effect on all of the
    merged reports.

    Before bugs can be merged they must be in exactly the same state:
    either all open or all closed, with the same forwarded-to upstream
    author address or all not marked as forwarded, all assigned to the same
    package or package(s) (an exact string comparison is done on the
    package to which the bug is assigned), and all of the same severity. If
    they don't start out in the same state you should use reassign, reopen
    and so forth to make sure that they are before using merge.

    If any of the bugs listed in a merge command is already merged with
    another bug then all the reports merged with any of the ones listed
    will all be merged together. Merger is like equality: it is reflexive,
    transitive and symmetric.

    Merging reports causes a note to appear on each report's logs; on the
    WWW pages this is includes links to the other bugs.

    Merged reports are all expired simultaneously, and only when all of the
    reports each separately meet the criteria for expiry.

unmerge bugnumber

    Disconnects a bug report from any other reports with which it may have
    been merged. If the report listed is merged with several others then
    they are all left merged with each other; only their associations with
    the bug explicitly named are removed.

    If many bug reports are merged and you wish to split them into two
    separate groups of merged reports you must unmerge each report in one
    of the new groups separately and then merge them into the required new
    group.

    You can only unmerge one report with each unmerge command; if you want
    to disconnect more than one bug simply include several unmerge commands
    in your message.

tags bugnumber [ + | - | = ] tag

    Sets a particular tag for the bug report #bugnumber to tag. No
    notification is sent to the user who reported the bug. + means adding,
    - means subtracting, and = means ignoring the current tags and setting
    them afresh. The default action is adding.

    Available tags currently include patch, wontfix, moreinfo,
    unreproducible, help, pending, fixed, security, upstream, fixed-upstream,
    potato, woody, sarge, sarge-ignore, sid and experimental.

    For their meanings, consult the Control->Help->Tags menu.

block bugnumber by  bug ...

    Note that the fix for the first bug is blocked by the other
    listed bugs.

unblock bugnumber by bug ...

    Note that the fix for the first bug is no longer blocked by
    the other listed bugs.

close bugnumber

    Close bug report #bugnumber.

    A notification is sent to the user who reported the bug, but (in
    contrast to mailing bugnumber-done@bugs) the text of the mail which
    caused the bug to be closed is not included in that notification. The
    maintainer who closes a report needs to ensure, probably by sending a
    separate message, that the user who reported the bug knows why it is
    being closed. The use of this command is therefore deprecated.

package [ packagename ... ]

    Limits the following commands so that they will only apply to bugs
    filed against the listed packages. You can list one or more packages. If
    you don't list any packages, the following commands will apply to all
    bugs. You're encouraged to use this as a safety feature in case you
    accidentally use the wrong bug numbers.

    Example usage:

        package foo
        reassign 123456 bar

        package bar
        retitle 123456 bar: bar sucks
        severity 123456 normal

        package
        severity 234567 wishlist

owner bugnumber address | !

    Sets address to be the \"owner\" of #bugnumber. The owner of a bug
    claims responsibility for fixing it and will receive all mail
    regarding it. This is useful to share out work in cases where a
    package has a team of maintainers.

    If you wish to become the owner of the bug yourself, you can use
    the ! shorthand or specify your own email address.

noowner bugnumber

    Forgets any idea that the bug has an owner other than the usual
    maintainer. If the bug had no owner recorded then this will do
    nothing.

archive bugnumber

    Archives a bug that had been archived at some point in the
    past but is currently not archived if the bug fulfills the
    requirements for archival, ignoring time.

unarchive bugnumber

    Unarchives a bug that was previously archived. Unarchival
    should generally be coupled with reopen and found/fixed as
    appropriate. Bugs that have been unarchived can be archived
    using archive assuming the non-time based archival
    requirements are met.

quit
stop
thank...
--...

    Tells the control server to stop processing the message; the remainder
    of the message can include explanations, signatures or anything else,
    none of it will be detected by the control server.

#...

    One-line comment. The # must be at the start of the line.

Help text from http://www.debian.org/Bugs/server-control, Apr 22nd 2003.
Copyright 1999 Darren O. Benham, 1994-1997 Ian Jackson,
 1997 nCipher Corporation Ltd.")))

;;;###autoload
(defun emacs-bts-control (action &optional arg)
  "Contruct a message with ACTION command for control@debbugs.gnu.org.
Contructs a new control command line if called from within the message
being constructed.

If prefix arg is provided, use the current buffer instead instead of
creating a new outgoing email message buffer.
The current buffer is also used if the current major mode matches one listed
in `debian-bts-control-modes-to-reuse'."
  (interactive (list (completing-read "Command: "
                                      debian-bts-control-alist nil nil)
                     current-prefix-arg))
  (let ((debian-bts-emailaddress "control@debbugs.gnu.org")
        (debian-bts-emaildomain "debbugs.gnu.org")
        (debian-bts-control-for-emacs t))
    (debian-bts-control action arg)))

(provide 'debian-bts-control)

;;; debian-bts-control.el ends here
