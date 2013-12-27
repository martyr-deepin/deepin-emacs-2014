;;; debian-control-mode.el --- major mode for Debian control files

;; Copyright (C) 2001, 2003 Free Software Foundation, Inc.
;; Copyright (C) 2003-2005, 2007-2011 Peter S Galbraith <psg@debian.org>

;; Author: Colin Walters <walters@debian.org>
;; Maintainer: Peter S Galbraith <psg@debian.org>
;; Created: 29 Nov 2001
;; Version: 1.5
;; X-RCS: $Id: debian-control-mode.el,v 1.18 2011-08-17 04:00:18 psg Exp $
;; Keywords: convenience

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; debian-control-mode.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with your Debian installation, in /usr/share/common-licenses/GPL
;; If not, write to the Free Software Foundation, 675 Mass Ave,
;; Cambridge, MA 02139, USA.

;;; Commentary:

;; debian-control-mode.el is developed under Emacs 21, and is targeted
;; for use in Emacs 21 and relatively recent versions of XEmacs.

;;; Change Log:

;; V1.5 (2011-08-16) Added "Multi-Arch:" (Closes #634162)

;; V1.4 (2011-06-24) Added "XS-Python-Version" to debian-control-source-fields
;;  (Closes #591697)

;; V1.3 (2010-05-07) Added "Breaks" to debian-control-binary-fields
;;  (Closes #580501)

;; V1.2a (2009-02-23) Applied patch from Morten Kjeldgaard changing
;;      Dm-Upload-Allowed to DM-Upload-Allowed (Closes: #508748)

;; V1.2 (2008-01-17)  Cyril Brulebois <cyril.brulebois@enst-bretagne.fr>
;; - Add "Dm-Upload-Allowed" field to source fields.

;; V1.1 (2007-10-18)  Cyril Brulebois <cyril.brulebois@enst-bretagne.fr>
;; - Renamed "XS-Vcs-*" fields into "Vcs-*", officially supported since
;;   dpkg/1.14.7.

;; V1.0 (2007-10-01)  Cyril Brulebois <cyril.brulebois@enst-bretagne.fr>
;; - Add "Homepage" field to source fields.
;; - Add "XS-Vcs-*" fields to source fields, patch contributed by
;;   Rafael Laboissiere <rafael@debian.org> (Closes: #422491).

;; V0.9 (2005-11-22)  Peter S Galbraith <psg@debian.org>
;; - Make # the comment character. (Closes: #339868)

;; V0.8 (2005-02-07)  Peter S Galbraith <psg@debian.org>
;; - Change mouse-2 binding to C-mouse-2 (Closes: #293629)
;; - Fix debian-control-mode-bugs-mouse-click to create correct
;;   text-properties of package names.

;; V0.7 (2004-03-27)  Peter S Galbraith <psg@debian.org>
;;
;; * Apply patch from Jhair Tocancipa Triana <jhair_tocancipa@gmx.net>
;;   in http://bugs.debian.org/226770.  Fixes an after-change-functions race.

;; V0.6 (2003-11-27)  Peter S Galbraith <psg@debian.org>
;;
;; * Only fontify known fields (to better catch misspellings) (Closes: #213779)
;; * Add "Uploaders" field; Add "Section" and "Priority" also to binary fields.
;; * Call `goto-address' in major-mode to clickify URLs.
;; * http://cvs.verbum.org/debian/debian-control-mode link removed.

;; V0.5 (2003/10/16)  Peter S Galbraith <psg@debian.org>
;;
;; * Add "View upgrading-checklist" to control menu.
;; * Added debian-control-find-file to make this work on XEmacs.

;; Changes from 0.3 to 0.4:
;;
;; * Don't depend on face properties to find names of packages.
;; * Use an after-change-function to put special text properties on,
;;   instead of using font-lock to do it.  That way they'll be added
;;   regardless of the value of `font-lock-mode'.
;; * Fix up portable definition of `with-auto-compression-mode'.

;; Changes from 0.2 to 0.3:
;;
;; * Fix bug in filling description lines.
;; * Clicking on a source or binary package name shows bugs for that
;;   package.
;; * New function `debian-control-mode-add-field', bound to 'C-c C-a'
;;   by default.
;; * New function `debian-control-visit-policy', bound to 'C-c C-p'
;;   by default.
;; * New function `debian-control-view-package-bugs', bound to 'C-c C-b'
;;   by default.
;; * Initial menu support.
;; * Initial customize support.
;; * Imenu support.
;; * Initial attempts at XEmacs support.
;; * Use the term "field" instead of "header".

;; Changes from 0.1 to 0.2:
;;
;; * Tighten up regexps; whitespace before and after a field value is
;;   insignificant.  Also, package names may contain '+' and '.'.
;; * Add more comments for compliance with Emacs Lisp coding standards.
;; * Allow filling of a regular field to work.
;; * Provide `debian-control-mode'.

;;; Bugs:

;; Filling doesn't work on XEmacs.  I have no idea why.
;; Mouse stuff doesn't work on XEmacs.
;; Emacs 20 isn't supported.

;;; Code:

(require 'easymenu)
(require 'font-lock)
(eval-when-compile
  (require 'cl))

;; XEmacs compatibility
(eval-and-compile
  (unless (fboundp 'line-beginning-position)
    (defun line-beginning-position ()
      (save-excursion
        (beginning-of-line)
        (point))))
  (unless (fboundp 'line-end-position)
    (defun line-end-position ()
      (save-excursion
        (end-of-line)
        (point))))
  (unless (fboundp 'match-string-no-properties)
    (defalias 'match-string-no-properties 'match-string)))

(defgroup debian-control nil "Debian control file maintenance"
  :group 'tools)

(defcustom debian-control-source-package-face 'font-lock-type-face
  "The face to use for highlighting source package names."
  :type 'face
  :group 'debian-control)

(defcustom debian-control-binary-package-face 'font-lock-variable-name-face
  "The face to use for highlighting binary package names."
  :type 'face
  :group 'debian-control)

(defvar debian-control-syntax-table nil
  "Syntax table used in debian-control-mode buffers.")

(if debian-control-syntax-table
    ()
  (setq debian-control-syntax-table (make-syntax-table))
  ;; Support # style comments
  (modify-syntax-entry ?#  "<"  debian-control-syntax-table)
  (modify-syntax-entry ?\n "> " debian-control-syntax-table))

;; FIXME: As of policy 3.5.6.0, the allowed characters in a field name
;; are not specified.  So we just go with "word constituent" or '-'
;; characters before a colon.
(defvar debian-control-field-regexp "^\\(\\(\\sw\\|-\\)+:\\)")
(defvar debian-control-package-name-regexp "\\([-a-zA-Z0-9+.]+?\\)")

(defvar debian-control-mode-package-name-keymap (make-sparse-keymap))

;; An uptodate list can be found at:
;;   http://svn.debian.org/wsvn/qa/trunk/pts/www/bin/common.py?op=file
(defvar debian-control-vcs-names
  '("Arch" "Bzr" "Cvs" "Darcs" "Git" "Hg" "Mtn" "Svn")
  "Valid VCS names for the Vcs-* field.")

(defvar debian-control-source-fields
  (append
   '("Section" "Priority" "Maintainer" "Build-Depends" "Build-Depends-Indep"
     "Build-Conflicts" "Build-Conflicts-Indep" "Standards-Version" "Uploaders"
     "DM-Upload-Allowed" "Homepage" "Vcs-Browser" "XS-Python-Version")
   (mapcar (lambda (elt) (concat "Vcs-" elt))
           debian-control-vcs-names))
  "Valid source package field names, collected from several policy sections.")

(defvar debian-control-binary-fields
  '("Section" "Priority" "Architecture" "Depends" "Conflicts" "Pre-Depends"
    "Essential" "Provides" "Recommends" "Suggests" "Replaces" "Enhances"
    "Description" "Breaks")
  "Valid binary package field names, collected from several policy sections.")

(defvar debian-control-source-fields-regexp
  (concat
   "^\\("
   (let ((max-specpdl-size 1000))
     (regexp-opt debian-control-source-fields t))
   "\\):")
  "font-lock regexp matching known fields in the source section.")

(defvar debian-control-binary-fields-regexp
  (concat
   "^\\("
   (let ((max-specpdl-size 1000))
     (regexp-opt debian-control-binary-fields t))
   "\\):")
  "font-lock regexp matching known fields in the binary section.")

(defvar debian-control-font-lock-keywords
  `((,(concat "^\\(Source:\\)\\s-*"
              debian-control-package-name-regexp
              "\\s-*$")
     (1 font-lock-keyword-face)
     ,(list 2
            (if (featurep 'xemacs)
                '(symbol-value debian-control-source-package-face)
              '(list 'face debian-control-source-package-face))
            nil nil))
    ("^\\(Multi-Arch:\\)\\s-*\\(same\\|foreign\\|allowed\\)"
     (1 font-lock-function-name-face)
     (2 font-lock-keyword-face))
    (,debian-control-source-fields-regexp
     (1 font-lock-keyword-face))
    (,debian-control-binary-fields-regexp
     (1 font-lock-function-name-face))))

(defvar debian-control-mode-menu nil)

;;;###autoload
(define-derived-mode debian-control-mode fundamental-mode "Debian Control"
  "A major mode for editing Debian control files (i.e. debian/control)."
  (if (< emacs-major-version 21)
      (message "debian-control-mode only supports emacsen version >= 21; disabling features")
    (progn
      (set-syntax-table debian-control-syntax-table)
      ;; Comments
      (make-local-variable 'comment-start-skip)        ;Need this for font-lock...
      (setq comment-start-skip "\\(^\\|\\s-\\);?#+ *") ;;From perl-mode
      (make-local-variable 'comment-start)
      (make-local-variable 'comment-end)
      (setq comment-start "#"
            comment-end "")

      (make-local-variable 'font-lock-defaults)
      (setq font-lock-defaults
            '(debian-control-font-lock-keywords
              nil ;;; Keywords only? No, let it do syntax via table.
              nil ;;; case-fold?
              nil ;;; Local syntax table.
              nil ;;; Use `backward-paragraph' ? No
              ))
      (set (make-local-variable 'fill-paragraph-function)
           #'debian-control-mode-fill-paragraph)
      (make-local-variable 'after-change-functions)
      (push 'debian-control-mode-after-change-function after-change-functions)
      (set (make-local-variable 'imenu-generic-expression)
           '((nil "^\\(Package\\|Source\\):\\s-*\\([-a-zA-Z0-9+.]+?\\)\\s-*$" 2)))

      (define-key debian-control-mode-map (kbd "C-c C-b") 'debian-control-view-package-bugs)
      (define-key debian-control-mode-map (kbd "C-c C-p") 'debian-control-visit-policy)
      (define-key debian-control-mode-map (kbd "C-c C-a") 'debian-control-mode-add-field)
      (define-key debian-control-mode-package-name-keymap (if (featurep 'xemacs)
                                                              [(control down-mouse-2)]
                                                            [(C-mouse-2)])
        'debian-control-mode-bugs-mouse-click)
      (easy-menu-add debian-control-mode-menu)
      (if (and (featurep 'goto-addr) goto-address-highlight-p)
          (goto-address))
      (let ((after-change-functions nil))
        (debian-control-mode-after-change-function (point-min) (point-max) 0)))))

(defun debian-control-mode-after-change-function (beg end len)
  (save-excursion
    (let ((modified (buffer-modified-p))
          (buffer-read-only nil)
          (data (match-data)))
      (unwind-protect
          (progn
            (goto-char beg)
            (beginning-of-line)
            (while (< (point) end)
              (cond ((looking-at (concat "^\\(Source:\\)\\s-*"
                                         debian-control-package-name-regexp
                                         "\\s-*$"))
                     (add-text-properties
                      (match-beginning 2) (match-end 2)
                      `(mouse-face
                        highlight
                        debian-control-mode-package ,(match-string 2)
                        help-echo "C-mouse-2: View bugs for this source package"
                        keymap ,debian-control-mode-package-name-keymap)))
                    ((looking-at (concat "^\\(Package:\\)\\s-*"
                                         debian-control-package-name-regexp
                                         "\\s-*$"))
                     (add-text-properties
                      (match-beginning 2) (match-end 2)
                      `(mouse-face
                        highlight
                        debian-control-mode-package ,(match-string 2)
                        help-echo "C-mouse-2: View bugs for this binary package"
                        keymap ,debian-control-mode-package-name-keymap)))
                    (t nil))
              (forward-line 1)))
        (set-match-data data)
        (set-buffer-modified-p modified)))))

(easy-menu-define
  debian-control-mode-menu debian-control-mode-map "Debian Control Mode Menu"
  '("Control"
    ["Add field at point" debian-control-mode-add-field t]
    "--"
    "Policy"
    ["View upgrading-checklist" (debian-control-visit-policy 'checklist)
     (file-exists-p "/usr/share/doc/debian-policy/upgrading-checklist.txt.gz")]
    ["View policy (text)" (debian-control-visit-policy 'text)
     (file-exists-p "/usr/share/doc/debian-policy/policy.txt.gz")]
    ["View policy (HTML)" (debian-control-visit-policy 'html) t]
    "--"
    "Access www.debian.org"
    ["Bugs for package" debian-control-view-package-bugs t]
    ["Specific bug number" (debian-changelog-web-bug) nil]
    ;;   ["Package list (all archives)" (debian-changelog-web-packages) t]
    ;;  ("Package web pages..."
    ;;   ["stable" (debian-changelog-web-package "stable") t]
    ;;   ["testing" (debian-changelog-web-package "testing") t]
    ;;   ["unstable" (debian-changelog-web-package "unstable") t])
    "--"
    ["Customize" (customize-group "debian-control") t]))

(defun debian-control-mode-fill-paragraph (&rest args)
  (let (beg end)
    (save-excursion
      ;; Are we looking at a field?
      (if (save-excursion
            (beginning-of-line)
            (looking-at debian-control-field-regexp))
          (setq beg (match-end 0)
                end (line-end-position))
        ;; Otherwise, we're looking at a description; handle filling
        ;; areas separated with "."  specially
        (setq beg (save-excursion
                    (beginning-of-line)
                    (while (not (or (bobp)
                                    (looking-at "^\\sw-*$")
                                    (looking-at "^ \\.")
                                    (looking-at debian-control-field-regexp)))
                      (forward-line -1))
                    (unless (eobp)
                      (forward-line 1))
                    (point))
              end (save-excursion
                    (beginning-of-line)
                    (while (not (or (eobp)
                                    (looking-at "^\\sw-*$")
                                    (looking-at debian-control-field-regexp)
                                    (looking-at "^ \\.")))
                      (forward-line 1))
                    (unless (bobp)
                      (forward-line -1)
                      (end-of-line))
                    (point))))
      (let ((fill-prefix " "))
        (apply #'fill-region beg end args)))))

(defun debian-control-mode-add-field (binary field)
  "Add a field FIELD to the current package; BINARY means a binary package."
  (interactive
   (let* ((binary-p (if (or (save-excursion
                              (beginning-of-line)
                              (looking-at "^\\(Package\\|Source\\)"))
                            (re-search-backward "^\\(Package\\|Source\\)" nil t))
                        (not (not (string-match "Package" (match-string 0))))
                      (error "Couldn't find Package or Source field")))
          (fields (if binary-p
                      debian-control-binary-fields
                    debian-control-source-fields)))
     (list
      binary-p
      (capitalize
       (completing-read (format "Add %s package field: " (if binary-p "binary" "source"))
                        (mapcar #'(lambda (x) (cons x nil)) fields))))))
  (require 'cl)
  (let ((fields (if binary
                    debian-control-binary-fields
                  debian-control-source-fields))
        (beg (save-excursion
               (beginning-of-line)
               (while (not (or (bobp)
                               (looking-at "^\\s-*$")))
                 (forward-line -1))
               (forward-line 1)
               (point)))
        (end (save-excursion
               (beginning-of-line)
               (while (not (or (eobp)
                               (looking-at "^\\s-*$")))
                 (forward-line 1))
               (point))))
    (save-restriction
      (narrow-to-region beg end)
      (let ((curfields (let ((result nil))
                         (goto-char (point-min))
                         (while (not (eobp))
                           (when (looking-at debian-control-field-regexp)
                             (push (cons (subseq
                                          ;; Text properties are evil
                                          (match-string-no-properties 1)
                                          0
                                          ;; Strip off the ':'
                                          (- (match-end 1)
                                             (match-beginning 1)
                                             1))
                                         (match-beginning 0))
                                   result))
                           (forward-line 1))
                         result))
            (x nil))
        ;; If the field is already present, just jump to it
        (if (setq x (assoc field curfields))
            (goto-char (cdr x))
          (let* ((pos (position field fields :test #'string-equal))
                 (prevfields (subseq fields 0 pos))
                 (nextfields (subseq fields (1+ pos)))
                 (cur nil))
            (while (or prevfields
                       nextfields)
              (when prevfields
                (when (setq x (assoc (pop prevfields) curfields))
                  (setq prevfields nil nextfields nil)
                  (goto-char (cdr x))))
              (when nextfields
                (when (setq x (assoc (pop nextfields) curfields))
                  (setq prevfields nil nextfields nil)
                  (goto-char (cdr x)))))
            ;; Hack: we don't want to add fields after Description
            (beginning-of-line)
            (when (looking-at "^Description")
              (forward-line -1))
            (end-of-line)
            (insert "\n" field ": ")))))))

(defun debian-control-visit-policy (format)
  "Visit the Debian Policy manual in format FORMAT.
Currently valid FORMATs are `html', `text' and `checklist'.
The last one is not strictly a format, but visits the upgrading-checklist.txt
text file."
  (interactive
   (list (intern
          (completing-read "Policy format: "
                           (mapcar #'(lambda (x) (cons x 0))
                                   '("html" "text" "checklist"))
                           nil t))))
  (case format
    (text
     (debian-control-find-file "/usr/share/doc/debian-policy/policy.txt.gz"))
    (checklist
     (debian-control-find-file
      "/usr/share/doc/debian-policy/upgrading-checklist.txt.gz"))
    (html
     (require 'browse-url)
     (browse-url
      (if (file-exists-p "/usr/share/doc/debian-policy/policy.html/index.html")
          "file:///usr/share/doc/debian-policy/policy.html/index.html"
        (prog1
            "http://www.debian.org/doc/debian-policy"
          (message "Note: package `debian-policy' not installed, using web version")))))
    (t
     (error "Unknown format %s for policy" format))))

(defun debian-control-find-file (file)
  "Find-file a possibly compressed FILE"
  (require 'jka-compr)
  (let ((installed (jka-compr-installed-p)))
    (if (not installed)
        (auto-compression-mode t))
    (find-file file)
    (if (not installed)
        (auto-compression-mode -1))))

(defun debian-control-mode-bugs-mouse-click (event)
  "Display the bugs for the package name clicked on."
  (interactive "e")
  (mouse-set-point event)
  (let ((prop (get-text-property (point) 'debian-control-mode-package)))
    (unless prop
      (error "Couldn't determine package name at point"))
    (debian-control-view-package-bugs prop)))

(defun debian-control-mode-bug-package-names ()
  (let ((result nil))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (when (looking-at "^\\(Package\\|Source\\):\\s-*\\([-a-zA-Z0-9+.]+?\\)\\s-*$")
          (push (concat
                 (if (save-match-data (string-match "Source" (match-string 1)))
                     "src:"
                   "")
                 (match-string-no-properties 2)) result))
        (forward-line 1)))
    result))

(defun debian-control-view-package-bugs (package)
  "View bugs for package PACKAGE via http://bugs.debian.org."
  (interactive
   (list
    (completing-read "View bugs for package: "
                     (mapcar #'(lambda (x) (cons x 0))
                             (debian-control-mode-bug-package-names))
                     nil t)))
  (browse-url (concat "http://bugs.debian.org/" package)))

(add-to-list 'auto-mode-alist '("/debian/control\\'" . debian-control-mode))
;;;###autoload(add-to-list 'auto-mode-alist '("/debian/control\\'" . debian-control-mode))

(provide 'debian-control-mode)

;;; debian-control-mode.el ends here
