;;; ff-paths.el --- searches certain paths to find files.

;; Copyright (C) 1994-2005 Peter S. Galbraith
 
;; Author:    Peter S. Galbraith <psg@debian.org>
;; Created:   16 Sep 1994
;; Version:   3.23 (Jul 08 2005)
;; Keywords:  find-file, ffap, paths, search

;;; This file is not part of GNU Emacs.

;; This package is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This package is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;; ----------------------------------------------------------------------------
;;; Commentary:

;; New versions of this package (if they exist) may be found at:
;;   http://people.debian.org/~psg/elisp/ff-paths.el
;; or in the Debian package `emacs-goodies-el'.

;; This code allows you to use C-x C-f normally most of the time, except that
;; if the requested file doesn't exist, it is checked against a list of
;; patterns for special paths to search for a file of the same name.
;;
;; Examples:
;;  - a file extension of .bib will cause to search the path defined in
;;    $BSTINPUTS or $BIBINPUTS for the file you requested.
;;  - a file extension of .h will cause the /usr/include/ and
;;    /usr/local/include/ directory trees to be searched.
;;  - a file extension of .sty causes a search of TEXINPUTS and of all
;;    directories below /usr/lib/texmf/tex/
;;  - a file extension of .el causes a search of the path set in the
;;    emacs variable load-path.
;;  - If the aboves searches don't return a match, the filename is searched
;;    for using the `locate' command (if available on your system).
;;  - gzip-compressed files (.gz) will also be found by ff-paths if
;;    the package jka-compr is present.  If you use some other package,
;;    simply set the ff-paths-gzipped variable to t:

;; If one file is found, or many files of the same name are found, then the
;; *completions* buffer is displayed with all possibilities, including the
;; non-existing path you first provided.  Selecting it creates the new
;; file.
;;
;; This package runs as a find-file-not-found-hooks hook, and so will
;; happily live alongside other such file-finding mechanisms (e.g.
;; PC-look-for-include-file PC-try-load-many-files vc-file-not-found-hook)

;; The patterns to test against filenames and the associated paths to search
;; for these files can be modified by the user by editing the variable
;; ff-paths-list defined below.

;; I suggest that you use ffap.el by Michelangelo Grigni <mic@cs.ucsd.edu>,
;; now part of GNU Emacs.  His package will guess the filename from the
;; text under the editing point.  It will search for an existing file in
;; various places before you even get the "File: " prompt.  ff-paths will
;; provide itself to ffap as an additional tool to locate the file before
;; you ever see a prompt.  ff-paths behaves slightly differently with ffap
;; than it does with find-file: if the file path selected under point by
;; ffap does not exist, it is not shown in the completions buffer along
;; with existing paths.  If only one existing path is found for said file,
;; it is placed in the minibuffer at the ffap prompt.  Also, since using
;; the `locate' command is fairly aggressive, it is not used in the ffap
;; toolkit.

;;; Installation:
;;
;;  ff-paths installs itself as a hook in find-file-not-found-hooks for
;;  find-file.  If ffap is installed, ff-paths installs itself as a toolbox
;;  hook in ffap-alist (so load ff-paths after ffap).
;;
;;  All you need to do is add this in ~/.emacs:
;;   (require 'ff-paths)
;;   (ff-paths-install)
;;  or customize the variable `ff-paths-install' to enable it.
;;
;;  NOTE: ff-paths used to install itself when it was loaded.  It no longer
;;        does so because that is against the Emacs coding conventions.
;;
;;
;;  You may alter the value of the variables:
;;
;;   ff-paths-list
;;   ff-paths-use-locate
;;   ff-paths-locate-max-matches
;;   ff-paths-using-ms-windows
;;   ff-paths-display-non-existent-filename
;;   ff-paths-prompt-for-only-one-match
;;   ff-paths-require-match
;;   ff-paths-gzipped
;;
;;  To see their documentation and current settings, do:
;;    C-h v ff-paths-list
;;  because that variable is _not_ customized, and also for all other
;;  variables:
;;    M-x customize-group ff-paths.

;; ----------------------------------------------------------------------------
;;; Change log:
;;
;; V1.01  16sep94 - created by Peter S. Galbraith,
;;                             rhogee@bathybius.meteo.mcgill.ca
;; V1.02  20sep94 - by Peter S. Galbraith
;;      Change TeX-split-string to dired-split (thanks to Michelangelo Grigni)
;;      Change variable name psg-ff-list to ff-paths-list
;;      Added find-file-noselect-using-paths for ffap.el
;;      Added ff-paths-prompt variable
;; V1.03  12oct94 - by Peter S. Galbraith
;;      Fixed:
;;      - error when nil appeared in ff-paths-list translation
;;        (meaning current default)
;;      - find-file-at-point would switch buffer if new file were not created.
;; V1.04  24oct94 - by Peter S. Galbraith
;;      Added patch from Ziv Gigus <ziv@sgi.com> to let environment variables
;;       have trailing directory paths:
;;            ("^foo_.*\\.[ch]$" "$FOO1:$FOO/bar:$FOO/barnone")
;; V2.00  05Jul95 - by Peter S. Galbraith
;;       Reworked interface
;;   Tremendous thanks to Bill Brodie <wbrodie@panix.com> for telling me how
;;   to make completing-read start off with the completions buffer displayed.
;;   It made this version possible without a kludge.  Thanks Bill!
;; V2.01  05Jul95 - by Peter S. Galbraith
;;      - Followed Bill Brodie's suggestions to make ff-paths-list not
;;        necessarilly a colon-separated string, but rather usually a list
;;        of strings:    ("\\.bib$" "$BSTINPUTS:$BIBINPUTS")
;;                    -> ("\\.bib$" "$BSTINPUTS" "$BIBINPUTS")
;;      - Also his suggestion to not quote symbols.
;;      - Also his suggestion to include leftmost matches as initial string
;;        to completing-read.
;;      - Also, I substitute ~/ for the home directory if possible in the
;;        matches displayed in the completions buffer.
;; V2.02  Jul 19 95 - Peter Galbraith
;;   - Had introduced bug in search-directory-tree. synced with bib-cite.el.
;; V3.00  Jul 26 95 - Peter Galbraith
;;   - Now a hook to find-file and ffap.  Removed `create buffer?' prompt.
;; V3.01  Sep 13 95
;;   - dired-aux may not be loaded - Yoichi Konno <itokon@ssel.toshiba.co.jp>
;;   - added ff-paths-display-non-existent-filename
;;      Jason Hatch <jhatch@matra.demon.co.uk>
;;   - psg-translate-ff-list was reversing directory order
;;      Juergen Vollmer <vollmer@ipd.info.uni-karlsruhe.de>
;; V3.02  March 20 96
;;     dired-aux not in XEmacs - Vladimir Alexiev <vladimir@cs.ualberta.ca>
;; V3.03  August 19 96
;;     ff-paths-prompt-for-only-one-match added.
;;     Havard Fosseng <havardf@usit.uio.no>
;; V3.04  August 26 96 Sudish Joseph <sudish@MindSpring.COM>  (RCS 1.4)
;;   - Use unread-command-events instead of unread-command-char.
;; V3.05  December 31 96 - Christoph Wedler <wedler@fmi.uni-passau.de>
;    (RCS 1.5)
;;   - Use minibuffer-setup-hook instead of unread-command-events.
;;   - Better minibuffer-quit.
;;   - New variable `ff-paths-prompt'
;;   - New variable `ff-paths-require-match'
;;   - Changed from `dired-split' to copying AUCTeX's code.
;; V3.06  Janury 18 97  (RCS 1.6)
;;   - Added the `locate' command functionality.
;; V3.07  July 16 97  (RCS 1.8)
;;   - Added gzipped files
;;   - Fixed infinite loop in recursive search with directory soft links
;;     such as:  /usr/include/ncurses -> .
;; V3.08  December 15 97
;;   - Hacked simpler create-alist-from-list  (RCS 1.9)
;;   - Handle file that exists but are not readable  (RCS 1.10)
;; V3.09  December 17 97 (RCS 1.11)
;;   - Added special face to completion buffer for non-existent filename.
;; V3.10  December 18 97 (RCS 1.13)
;;   - Made V3.09 change work in XEmacs also.
;; V3.11  August 08 1998 (RCS 1.15)
;;   - Compatible with GNU/Emacs compiled on NT/Win95
;; V3.12  September 28 1998 (RCS 1.16)
;;   - ff-paths-list can contain many entries for a filename match.
;;   - ffap calls ff-paths on any filename (so users can modify ff-paths-list).
;;   - ff-paths-locate validates filenames in case they have since been deleted
;; V3.13  November 12 1998 (RCS 1.17)
;;   - Added ff-paths-use-locate equals 1 for high priority use.
;;   - self-detection of locate for ntemacs.
;; V3.14  December 29 1999 (RCS 1.18)
;;   - switch to GPL.
;;   - psg-convert-homedir-to-tilde uses files.el's abbreviate-file-name
;; V3.15  October 02 2000 (RCS 1.19)
;;    - spelling error: changed existant -> existent everywhere, affecting
;;       user variables.  Sorry.
;; V3.16  January 08 2001 (RCS 1.20)
;;    - Added ff-paths-locate-max-matches, defaults to 20 matches.
;; V3.17  January 17 2001 (RCS 1.22)
;;    - Oops! defvar ff-paths-locate-max-matches.
;; V3.18  January 07 2002 (RCS 1.24) Michael Ernst <mernst@alum.mit.edu>
;;   Quote filenames before passing them to locate.  Without this change,
;;   ff-paths may return many irrelevant matches.  More seriously, the
;;   locate command may take a very long time to complete, if some portion
;;   of the the filename matches many files.  (I was given a file named
;;   "procedure - version 1", and locate went to town on the "-".)
;; V3.19  April 21st 2003 PSG
;;   - checkdoc cleaning.
;;   - customization (still lacking the main variable `ff-paths-list'!)
;;   - byte-compiles clean!
;; V3.20  June 16 2003 PSG
;;   - Add /usr/X11R6/include// to ff-paths-list
;;   - Add ff-paths-install to install this package (instead of doing so
;;     automatically at load time).
;;   - Add ff-paths-install defcustom to enable package.
;; V3.21  Aug 14 2003 PSG
;;   - ff-paths-list-env: code cleanup.
;; V3.22 Nov 21 2003 PSG
;;   - Add defcustoms `ff-paths-locate-ignore-filenames-default',
;;     `ff-paths-locate-ignore-filenames' and `ff-paths-locate-ignore-regexps' 
;;     and support infracstructure to skip using locate for certain
;;     (common) filenames.
;; V3.23 Jul 08 2005  Heath Morgan
;;   - Reinsert `ff-paths-prompt-for-only-one-match' in XEmacs code.
;; ----------------------------------------------------------------------------
;;; Code:

(eval-when-compile (require 'cl))

(defgroup ff-paths nil
  "Find file using paths."
  :group 'ffap
  :group 'matching
  :group 'convenience)

;; The following variable may be edited to suit your site:
;; Send me your interesting add-ons too!

(defvar ff-paths-list
  '(("\\.awk$" "$AWKPATH")              ; awk files in AWKPATH env variable.
    ("\\.bib$" "$BSTINPUTS" "$BIBINPUTS") ; bibtex files.
    ("\\.\\(sty\\|cls\\)$" "$TEXINPUTS" "/usr/share/texmf/tex//") ;LaTeX files
    ("\\.[h]+$" "/usr/local/include//" "/usr/include//" "/usr/X11R6/include//")
    ("^\\." "~/")                       ; .* (dot) files in user's home
    ("\\.el$" load-path))               ; el extension in load-path elisp var
  "*List of paths to search for given file extension regexp's.
The directories can be:
  - colon-separated directories and ENVIRONMENT variables
    (which may also translate to colon-separated directories)
  - list of strings representing directories or environment variables.
  - a symbol object evaluating to a list of strings (e.g. `load-path')

You may mix environment variables and directory paths together.
You may add trailing directoty paths to environment variables, e.g. $HOME/bin
You may not mix strings with elisp lists (like `load-path').
You may terminate a directory name with double slashes // indicating that
 all subdirectories beneath it should also be searched.")

;; Other variables

(defvar ff-paths-prompt "Find File: "
  "Prompt used by ff-paths.")

(defvar ff-paths-have-reached-locate-max nil
  "Internal to ff-paths to remember if max count is reached on this search.")

(defvar ff-paths-in-ffap-name ""
  "Filename used when `ff-paths-in-ffap' called.
Find-file-using-paths-hook does nothing if called with this same name to avoid
searching twice for a non-existing file the user actually wants to create")

(defvar ff-paths-non-existent-filename nil
  "Internal holder for a filename that doesn't exist on the filesystem.")

;; ----------------------------------------------------------------------------
;;; Installs itself as hooks at the end of the file
;;  (so it won't if error in byte-compiling)

;; ----------------------------------------------------------------------------
;;  Notes about ffap
;;
;;  This defines two hooks:
;;  - ff-paths-in-ffap used by ffap if it found a filename around point
;;    which doesn't exist in the specified path or default directory.
;;  - find-file-using-paths-hook used by find-file when the specific file
;;    path does not exist.
;;
;;  If ffap doesn't find a filename around point and prompts the user for a
;;  filename and that file doesn't exist, ffap will not use its bag of
;;  tricks to find the file (which would include ff-paths-in-ffap), but
;;  will rather pass the filename directly to find-file, which will call
;;  find-file-using-paths-hook.  So both hooks are actually used. This is
;;  ok, but I'll have to change things if ffap changes this behaviour.
;;
;;  If ffap finds a filename around point but said file does not exit, ffap
;;  will use ff-paths-in-ffap (as part of its toolbox) to locate the file.
;;  I do not include the non-existent file as a possible completion because
;;  ffap cannot readily deal with this.  If only one file is found it is
;;  returned to ffap, which will prompt the user using it as an initial
;;  string.  If no files are found, ff-paths-in-ffap recurses through
;;  directory paths ending in // to try again. If two or more files are
;;  found, ff-paths-in-ffap will use the completions buffer to ask which
;;  the user wants, and returns it to ffap.  Unfortunately, ffap doesn't
;;  know any better than to prompt the user again with this filename.

;;  If ffap and ff-paths-in-ffap both fail, ffap will pass the argument to
;;  vanilla find-file and find-file-using-paths-hook will be called down
;;  the line because the file does not exist.  find-file-using-paths-hook
;;  checks if called with same filename (which will also be same as
;;  ffap-string-at-point) and doesn't do anything if it is. This handles
;;  the case where the user actually wanted to create this new file.

;;  ff-paths-in-ffap can't let the user edit completions to some
;;  non-existing file because ffap will check for existence, crush the
;;  choice and display a fresh prompt.

(defvar ff-paths-is-XEmacs
  (not (null (save-match-data (string-match "XEmacs\\|Lucid" emacs-version)))))

;;FIXME: Should use defface if using Emacs-20
(defvar ff-paths-non-existent-file-face 'ff-paths-non-existent-file-face
  "Face to use for message marked for deletion in mh-e folder-mode.")
(make-face 'ff-paths-non-existent-file-face)
(if ff-paths-is-XEmacs
    (make-face-bold 'ff-paths-non-existent-file-face nil)
  (make-face-bold 'ff-paths-non-existent-file-face nil t))
(set-face-foreground 'ff-paths-non-existent-file-face "NavyBlue" nil)

(defvar buf)
(defvar truename)
(defvar number)
(defvar filename)

(defvar ff-paths-use-locate)
(defvar ff-paths-display-non-existent-filename)
(defvar ff-paths-require-match)
(defvar ff-paths-locate-max-matches)
(defvar ff-paths-gzipped)
(defvar ff-paths-using-ms-windows)
(defvar ff-paths-locate-ignore-filenames-compiled)
(defvar ff-paths-locate-ignore-filenames-default)
(defvar ff-paths-locate-ignore-filenames)
(defvar ff-paths-locate-ignore-regexps)

(defun find-file-using-paths-hook ()
  "Search for file not found in path specified by the variable `ff-paths-list'."
  ;; This is called by find-file after it fails.
  ;; find-file can itself be called by ffap if no string was under point.
  (if (or (ff-paths-file-exists-but-cannot-be-read buffer-file-name)
          (string-equal buffer-file-name ff-paths-in-ffap-name))
      nil
    (let* ((the-name (file-name-nondirectory buffer-file-name))
           (matches
            (or (if (and (equal ff-paths-use-locate '1)
                         (ff-paths-locate-filename-p the-name))
                    (ff-paths-locate the-name))
                (psg-filename-in-directory-list
                 the-name (ff-paths-from-list the-name))
                (if (and (equal ff-paths-use-locate 't)
                         (ff-paths-locate-filename-p the-name))
                    (ff-paths-locate the-name))))
           (bufname (buffer-name buf)) ; compute before uniquify hits!
           newbuf)
      (if (null matches)
          nil                             ;Return nil
        (if (not ff-paths-display-non-existent-filename)
            (setq matches (psg-convert-homedir-to-tilde matches))
          (setq matches (psg-convert-homedir-to-tilde
                         (cons (expand-file-name buffer-file-name) matches)))
          (setq ff-paths-non-existent-filename
                (car (psg-convert-homedir-to-tilde (list buffer-file-name)))))

;;From: Christoph Wedler <wedler@fmi.uni-passau.de>
;; * The code of automatically displaying the *Completion* Buffer doesn't work
;;   in XEmacs 19.13 (this is fixed in the patch below, ffap did something
;;   similar--but I prefer `cons'ing to `minibuffer-setup-hook' instead of
;;   setting this hook)

;; Replace this:
;;        (let ((unread-command-char ??))
;;          (setq the-name
;;                (if (and (not ff-paths-prompt-for-only-one-match)
;;                         (null (cdr matches)))
;;                    (car matches)
;;                  (or (and (string-equal "18" (substring emacs-version 0 2))
;;                           (completing-read "Find file: "
;;                                            (create-alist-from-list matches)
;;                                            nil nil
;;                                            (psg-common-in-list matches)))
;;                      (completing-read "Find file: "
;;                                       (create-alist-from-list matches)
;;                                       nil nil
;;                                       (psg-common-in-list matches)
;;                                       'file-name-history)))))
;;

;; With this:
        (condition-case nil
            (let ((minibuffer-setup-hook (cons 'minibuffer-completion-help
                                               minibuffer-setup-hook))
                  (completion-setup-hook
                   (append (symbol-value 'completion-setup-hook)
                           (list 'ff-paths-fontify-non-existent-filename
                                 'ff-paths-display-locate-max-reached))))
              (setq the-name
                    ;; Heath Morgan pointed out that
                    ;; `ff-paths-prompt-for-only-one-match' had been dropped.
                    ;; Added back in V3.23
                    (if (and (not ff-paths-prompt-for-only-one-match)
                             (null (cdr matches)))
                      (car matches)
                      (or (and (string-equal "18" (substring emacs-version 0 2))
                             (completing-read ff-paths-prompt
                                              (create-alist-from-list matches)
                                              nil ff-paths-require-match
                                              (psg-common-in-list matches)))
                        (completing-read ff-paths-prompt
                                         (create-alist-from-list matches)
                                         nil ff-paths-require-match
                                         (psg-common-in-list matches)
                                         'file-name-history)))))
          (quit (setq the-name nil)))
;; End of Christoph Wedler's change.

        (if (or (not the-name)
                (string-equal "" the-name)
                (not (file-exists-p the-name)))
            nil                           ;Return nil
          (let ((find-file-hooks))        ;Don't call hooks twice
;           (funcall 'find-file (expand-file-name the-name))))))))
            (setq newbuf (set-buffer (find-file-noselect the-name))))
          (kill-buffer buf)
          (rename-buffer bufname)
          ;; Side-effect variables of parent find-file-noselect
          (setq buf newbuf
                filename buffer-file-name
                truename buffer-file-truename
                number buffer-file-number)
          t)))))

(defun ff-paths-fontify-non-existent-filename ()
  "Fontify the non-existing filename in *Completions* if using `window-system'."
  (cond
   ((and window-system
         ff-paths-display-non-existent-filename
         (boundp 'ff-paths-non-existent-filename)
         ff-paths-non-existent-filename)
    (save-excursion
      (set-buffer standard-output)
      (goto-char (point-min))
      (if (search-forward ff-paths-non-existent-filename nil t)
          (progn
            (put-text-property (match-beginning 0) (match-end 0)
                               'face 'ff-paths-non-existent-file-face)
            (goto-char (point-min))
            (if (search-forward "Possible completions are:" nil t)
                (forward-line -1))
            (let ((the-start (point))
                  (buffer-read-only nil))
              (insert "The filename in this face is the path you requested and does not exist.\n")
              (put-text-property the-start (point)
                                 'face 'ff-paths-non-existent-file-face))))))))

(defun ff-paths-display-locate-max-reached ()
  "Add a line in completions buffer to say that locate maximum is reached."
  (if ff-paths-have-reached-locate-max
      (save-excursion
        (set-buffer standard-output)
        (goto-char (point-min))
        (if (search-forward "Possible completions are:" nil t)
            (forward-line -1))
        (let ((buffer-read-only nil))
          (insert "Only the first "
                  (int-to-string ff-paths-locate-max-matches)
                  " matches are listed.\n"))))
  (setq ff-paths-have-reached-locate-max nil))

(defun ff-paths-file-exists-but-cannot-be-read (file-name)
  "Return t if FILE-NAME exists but cannot be Read.
`find-file' calls `find-file-not-found-hooks' when this is the case, but I
don't think it should.  ff-paths should deal with it anyway..."
  (and (file-exists-p file-name)
       (not (file-readable-p file-name))))

(defun ff-paths-in-ffap (name)
  "Search for NAME in path specified in `ff-paths-list'."
  ;; This is called by ffap before it prompts.
  (setq ff-paths-in-ffap-name (expand-file-name name))
  (let* ((the-name (file-name-nondirectory name))
         (matches (psg-filename-in-directory-list
                   the-name (ff-paths-from-list the-name))))
    (cond
     ((null matches)                    ; No match, Return nil
      nil)
     ((null (cdr matches))              ; Single matche
      (car matches))
     (t
      (setq matches (psg-convert-homedir-to-tilde matches))
      (condition-case nil
	  (let ((minibuffer-setup-hook (cons 'minibuffer-completion-help
					     minibuffer-setup-hook)))
	    (setq the-name
		  (or (and (string-equal "18" (substring emacs-version 0 2))
			   (completing-read ff-paths-prompt
					    (create-alist-from-list matches)
					    nil t
					    (psg-common-in-list matches)))
		      (completing-read ff-paths-prompt
				       (create-alist-from-list matches)
				       nil t
				       (psg-common-in-list matches)
				       'file-name-history))))
	(quit (setq the-name nil)))
      (if (and the-name
               (not (string-equal "" the-name)))
          the-name
        nil)))))

(defvar ffap-alist)

;;(defun ff-paths-in-ffap-install ()
;;  "Install ff-paths in ffap toolbox to find files from name under point"
;;  (cond
;;   ((and (boundp 'ffap-alist)
;;         (not (member
;;               (cons "\\(^\\.\\)\\|\\.\\(awk\\|bib\\|sty\\|cls\\|[h]+\\|el\\)$"
;;                     'ff-paths-in-ffap)
;;               ffap-alist)))
;;    (setq ffap-alist
;;          (nconc
;;           ffap-alist
;;           (list
;;            (cons "\\(^\\.\\)\\|\\.\\(awk\\|bib\\|sty\\|cls\\|[h]+\\|el\\)$"
;;                  'ff-paths-in-ffap)))))))

;; FIXME: Either make ffap call ff-paths on any file like here, or build a
;;        regexp from ff-paths-list
(defun ff-paths-in-ffap-install ()
  "Install ff-paths in ffap toolbox to find files from name under point."
  (cond
   ((and (boundp 'ffap-alist)
         (not (member '("." . ff-paths-in-ffap) ffap-alist)))
    (setq ffap-alist (append ffap-alist '(("." . ff-paths-in-ffap)))))))

;; There must be a command to do this!
(defun psg-common-in-list (list)
  "Return STRING with same beginnings in all strings in LIST."
  (let* ((first-string (car list))
         (work-list (cdr list))
         (match-len (length first-string)))
    (while work-list
      (let ((i 1))
        (while (and (<= i match-len)
                    (<= i (length (car work-list)))
                    (string-equal (substring first-string 0 i)
                                  (substring (car work-list) 0 i))
                    (setq i (1+ i))))
        (setq match-len (1- i)))
      (setq work-list (cdr work-list)))
    (substring first-string 0 match-len)))

(defun psg-convert-homedir-to-tilde (list)
  "Shorten LIST elements by substituting teh home directory by tilde."
  (let* ((work-list list)(result-list)
         (homedir (concat "^" (file-name-as-directory
                               (expand-file-name "~"))))
         (the-length (1- (length homedir))))
    (while work-list
      (if (fboundp 'abbreviate-file-name)
          (setq result-list
                (cons (abbreviate-file-name (car work-list)) result-list))
        (if (string-match homedir (car work-list))
            (setq result-list
                  (cons (concat "~/" (substring (car work-list) the-length))
                        result-list))
          (setq result-list (cons (car work-list) result-list))))
      (setq work-list (cdr work-list)))
    (nreverse result-list)))
    
;; Defined in bib-cite.el !
(defun create-alist-from-list (the-list)
  (mapcar 'list the-list))

(defun psg-filename-in-directory-list (filename list)
  "Check for presence of FILENAME in directory LIST.  Return all found.
If none found, recurse through directory tree of directories ending in //
and return all matches."
  ;;USAGE: (psg-filename-in-directory-list "emacs" (ff-paths-list-env "PATH"))
  ;;USAGE: (psg-filename-in-directory-list "ff-paths.el" load-path)
  ;;USAGE: (psg-filename-in-directory-list "ff-paths.el" (ff-paths-from-list "ff-paths.el"))
  (let ((the-list list) (filespec-list))
    (while the-list
      (let* ((directory (or (and (not (car the-list)) ; list item is nil -> ~/
                                 "~/")
                            (substring (car the-list)
                                       0
                                       (string-match "//$" (car the-list)))))
             ;; This removed trailing // if any
             (filespec (expand-file-name filename directory)))
        (if (file-exists-p filespec)
            (setq filespec-list (cons filespec filespec-list)))
        (if (and ff-paths-gzipped
                 (file-exists-p (concat filespec ".gz")))
            (setq filespec-list (cons (concat filespec ".gz") filespec-list))))
      (setq the-list (cdr the-list)))
    (if filespec-list
        filespec-list
      ;; If I have not found a file yet, then check if some directories
      ;; ended in // and recurse through them.
      (let ((the-list list))
        (while the-list
          (if (or (not (car the-list))  ; `nil' case
                  (not (string-match "//$" (car the-list)))) nil
            (setq filespec-list
                  (append
                   filespec-list
                   (search-directory-tree
                    (substring (car the-list) 0 (match-beginning 0))
                    (if ff-paths-gzipped
                        (concat "^" filename "\\(.gz\\)?$")
                      (concat "^" filename "$"))
                    t
                    nil))))
          (setq the-list (cdr the-list))))
      filespec-list)))
      
;;; search-directory-tree is heavily based on TeX-search-files
;;  which recursively searches a list of directories for files
;;  matching a list of extensions.  This simplified version should
;;  be a wee bit faster and will suit my purposes (for bib-cite's
;;  need to search directories listed in BIBINPUTS recursively
;;  if they end in //).
;;  TeX-search-files is part of auc-tex:
;;    Maintainer: Per Abrahamsen <auc-tex@iesd.auc.dk>
     
;;    Copyright (C) 1985, 1986 Free Software Foundation, Inc.
;;    Copyright (C) 1987 Lars Peter Fischer
;;    Copyright (C) 1991 Kresten Krab Thorup
;;    Copyright (C) 1993, 1994 Per Abrahamsen

;; Also defined in bib-cite.el !
(defun search-directory-tree (directories extension-regexp recurse first-file)
  "Return recursive list of files in DIRECTORIES ending with EXTENSION-REGEXP.
DIRECTORIES is a list or a single-directory string
EXTENSION-REGEXP is actually (any) regexp, usually \\\\.bib$
If RECURSE is t, then we will recurse into the directory tree,
              nil, we will only search the list given.
If FIRST-FILE is t, stop after first file is found."
  (or (listp directories)
      (setq directories (list directories)))
    
  (let ((match)
        (directories-done))
    (while directories
      (let* ((directory (file-name-as-directory (car directories)))
             (content (and directory
			   (file-readable-p directory)
			   (ff-paths-file-directory-p directory)
			   (directory-files directory))))
        (setq directories (cdr directories))
        (setq directories-done (cons directory directories-done))
        (while content
          (let ((file (expand-file-name (car content) directory)))
            (cond ((string-match "[.]+$" (car content))) ;This or parent dir
                  ((not (file-readable-p file)))
                  ((and recurse
                        (ff-paths-file-directory-p file))
                   (if (not (member
                             (file-name-as-directory (file-chase-links file))
                             directories-done))
                       (setq directories
                             (cons
                              (file-name-as-directory (file-chase-links file))
                              directories))))
                  ((string-match extension-regexp
                                 (file-name-nondirectory file))
                   (and first-file
                        (setq content nil
                              directories nil))
                   (setq match (cons file match)))))
          (setq content (cdr content)))))
    match))


(defun ff-paths-split-path (string)
  "Split a path STRING such as \"/some/directory:/some/other\".
The returned list is like (\"/some/directory\" \"/some/other\"."
  (let ((splitter (or (and ff-paths-using-ms-windows ";") ":")))
    (ff-paths-split-string splitter string)))

;; copied from auctex's TeX-split-string
(defun ff-paths-split-string (regexp string)
  "Return a list of strings given a REGEXP and a STRING.
The string is split into sections which were seperated by REGEXP.

Examples:

      (ff-paths-split-string \"\:\" \"abc:def:ghi\")
          -> (\"abc\" \"def\" \"ghi\")

      (ff-paths-split-string \" *\" \"dvips -Plw -p3 -c4 testfile.dvi\")

          -> (\"dvips\" \"-Plw\" \"-p3\" \"-c4\" \"testfile.dvi\")

If REGEXP is nil, or \"\", an error will occur."

  (let ((start 0)
	(result '()))
    (while (string-match regexp string start)
      (let ((match (string-match regexp string start)))
	(setq result (cons (substring string start match) result))
	(setq start (match-end 0))))
    (setq result (cons (substring string start nil) result))
    (nreverse result)))
   
;; `ff-paths-from-list' and `ff-paths-expand-path' together replace
;; the old `psg-translate-ff-list'
(defun ff-paths-from-list (filename)
  "Given a FILENAME, return corresponding directory list from `ff-paths-list'.
Return nil if file name extension is not listed in `ff-paths-list'.
So translate the cdr of the `ff-paths-list' entry to a directory list.
NOTE: returned nil means no match, but nil as an element of the returned list
      is valid, meaning current-directory!"
  (let ((local-ff-list ff-paths-list)(the-path))
    (while local-ff-list
      (let ((the-pair (car local-ff-list)))
        (cond
         ((string-match (car the-pair) filename)
          (setq the-path
                (append the-path (ff-paths-expand-path (cdr the-pair))))))
        (setq local-ff-list (cdr local-ff-list))))
    the-path))

(defun ff-paths-expand-path (unexpanded-path)
  "UNEXPANDED-PATH is expanded.
It should hold a list of:
      no match          ->  nil
      symbol            ->  (load-path)
      stringed PATH     ->  (\"/usr/local/include//:/usr/include//\")
      many such strings ->  (\"/usr/local/include//\" \"/usr/include//\")
      appended env var  ->  (\"$FOO/bar\")"
  (cond
   ((not unexpanded-path)             ; nil case, and we're done.
    nil)
   ((symbolp (car unexpanded-path))   ; load-path type symbol
    (eval (car unexpanded-path)))     ; ->Return it, and we're done.
   (t                                 ;string case, expand each element
    (let ((the-list))
      (while unexpanded-path
        (let ((the-elements (ff-paths-split-path (car unexpanded-path)))
              (path-list) (element))
          (while the-elements
            (setq element (car the-elements))
            (setq the-elements (cdr the-elements))
            (if (string-match "^\\$" element) ; an ENVIRONMENT var?
                (setq path-list
                      (nconc path-list
                             (ff-paths-list-env (substring element 1))))
              (if (ff-paths-file-directory-p element) ;  Add only if it exists
                  (setq path-list (cons element path-list)))))
          (if path-list
              (setq the-list (append the-list path-list))))
        (setq unexpanded-path (cdr unexpanded-path)))
      the-list))))

(defun ff-paths-list-env (env)
  "Return a list of directory elements in ENV variable (w/o leading $)
argument may consist of environment variable plus a trailing directory, e.g.
HOME or HOME/bin"
  (let* ((slash-pos (string-match "/" env))
         (value (if (not slash-pos)
                    (getenv env)
                  (concat (getenv (substring env 0 slash-pos))
                          (substring env slash-pos))))
	 (entries (and value (ff-paths-split-path value))))
    (loop for x in entries if (ff-paths-file-directory-p x) collect x)))

(defun ff-paths-file-directory-p (file)
  "Like default `file-directory-p' but allow FILE to end in // for ms-windows."
  (save-match-data
    (if (string-match "\\(.*\\)//$" file)
	(file-directory-p (match-string 1 file))
      (file-directory-p file))))

;;; `locate' stuff

(defun ff-paths-locate (filename)
  "Try finding FILENAME using the locate command.
Return a string if a single match, or a list if many matches."
  (let ((ff-buffer (get-buffer-create "*ff-paths-locate*"))
        status matches
        (count 0))
    (save-excursion
      (set-buffer ff-buffer)
      (setq status
            (call-process "sh" nil t nil "-c"
                          (concat "locate " (shell-quote-argument filename))))
    (goto-char 1)
      (if (eq status 1)
          nil                           ;Not found...
        (while (and (or (not (boundp 'ff-paths-locate-max-matches))
                        (not ff-paths-locate-max-matches)
                        (> ff-paths-locate-max-matches count))
                    (re-search-forward (if (and (boundp 'ff-paths-gzipped)
                                                ff-paths-gzipped)
                                           (concat "/" filename "\\(.gz\\)?$")
                                         (concat "/" filename "$"))
                                       nil t))
          (let ((the-file (buffer-substring (progn (beginning-of-line)(point))
                                            (progn (end-of-line)(point)))))
            (setq count (1+ count))
            (if (and (file-exists-p the-file)
                     (not (file-directory-p the-file)))
                (setq matches (cond ((not matches)
                                     (list the-file))
                                    (t
                                     (cons the-file matches))))))))
      (if (and (boundp 'ff-paths-locate-max-matches)
               ff-paths-locate-max-matches
               (<= ff-paths-locate-max-matches count))
          (setq ff-paths-have-reached-locate-max t))
      (kill-buffer ff-buffer)
      matches)))

(defun ff-paths-locate-filename-p (filename)
  "Return t if ff-paths should try to find FILENAME using locate command.
Checks FILENAME against `ff-paths-locate-ignore-filenames',
`ff-paths-locate-ignore-filenames-default' and
`ff-paths-locate-ignore-regexps'."
  (cond
   ((string-match ff-paths-locate-ignore-filenames-compiled filename)
    nil)
   (t  
    (not (car (memq t
                    (mapcar (lambda (x) (not (null (string-match x filename))))
                            ff-paths-locate-ignore-regexps)))))))

(defun ff-paths-have-locate ()
  "Determine if the `locate' command exists on this system."
  (if (not (condition-case nil
               (not (call-process "sh" nil 0 nil))
             (error)))
      nil                               ;No `sh' command on system
    (cond
     ((and (fboundp 'executable-find)
           (executable-find "locate"))
      t)
     ((ff-paths-locate "bin/locate")
      t)
     ((ff-paths-locate "locate.exe")
      t)
     (t      
      nil))))

;;;###autoload
(defun ff-paths-install ()
  "Install ff-paths as a `find-file-not-found-hooks' and to ffap package."
  (add-hook 'find-file-not-found-hooks 'find-file-using-paths-hook t)
  (ff-paths-in-ffap-install))

(defcustom ff-paths-install nil
  "Whether to setup ff-paths for use.
find-file-using-paths searches certain paths to find files."
  :type 'boolean
  :set (lambda (symbol value)
         (set-default symbol value)
         (when value
           (ff-paths-install)))
  :require 'ff-paths
  :group 'ff-paths)

(defcustom ff-paths-use-ffap nil
  "Whether to setup ffap and its key bindings for use.

Usually packages don't advertise or try to setup other packages, but
ff-paths works well in combination with ffap (Find FILENAME, guessing a
default from text around point) and so I recommend it here.

find-file-using-paths searches certain paths to find files."
  :type 'boolean
  :set (lambda (symbol value)
         (set-default symbol value)
         (when value
           (require 'ffap)
           (ffap-bindings)
           (ff-paths-in-ffap-install)))
  :require 'ff-paths
  :group 'ff-paths)

(defcustom ff-paths-use-locate (ff-paths-have-locate)
  "*Determines whether the `locate' command is used by ff-paths.
If nil don't use it.
If t use it but only if other ff-paths methods have failed.
If 1 use it before any other mechanism (because it's faster).

To set it to 1, add this to your ~/.emacs file:

  (setq ff-paths-use-locate '1)

By default, this is set to t if it can be determined that your system has
the locate command.
Using locate is fairly aggressive, and so is *not* added to the ffap toolkit."
  :group 'ff-paths
  :type 'boolean)

(defcustom ff-paths-display-non-existent-filename t
  "*find-file-using-paths-hook displays the prompted-for non-existent filename.
If you use \"C-x C-f article.sty\" in a path where it does not exists,
find-file-using-paths-hook will presumably find it for you. If this variable
is set, then this non-existent filename will be displayed in the completions
buffer along with the existing found file.  This makes it more intuitive
in case you really wanted to create the new file (instead of pressing C-g
to create the new file)."
  :group 'ff-paths
  :type 'boolean)

(defcustom ff-paths-prompt-for-only-one-match t
  "*If non-nil, prompt the user for filename even if there is only one match.
If nil and `ff-paths-display-non-existent-filename' is also nil, then dispense
with confirmation prompt when a single match is found for a non-existent file
and edit that single matched file immediately."
  :group 'ff-paths
  :type 'boolean)

(defvar ff-paths-locate-ignore-filenames-compiled nil
  "*Regexp matching files not searched for using locate.
Do not alter this variable directly. Instead, customize
`ff-paths-locate-ignore-filenames-default' checking off filenames normally
not searched that you would like searched, and add extra filenames to
not search for in `ff-paths-locate-ignore-filenames'.")

(defun ff-paths-locate-ignore-filenames-compile ()
  "Make or remake the variable `ff-paths-locate-ignore-filenames-compiled'.
Done using `ff-paths-locate-ignore-filenames' and
`ff-paths-locate-ignore-filenames-default' as input."
  (let ((list (cond
               ((and (boundp 'ff-paths-locate-ignore-filenames)
                     ff-paths-locate-ignore-filenames
                     (boundp 'ff-paths-locate-ignore-filenames-default)
                     ff-paths-locate-ignore-filenames-default)
                (append ff-paths-locate-ignore-filenames
                        ff-paths-locate-ignore-filenames-default))
               ((and (boundp 'ff-paths-locate-ignore-filenames)
                     ff-paths-locate-ignore-filenames)
                ff-paths-locate-ignore-filenames)
               ((and (boundp 'ff-paths-locate-ignore-filenames-default)
                     ff-paths-locate-ignore-filenames-default)
                ff-paths-locate-ignore-filenames-default))))
    (if list
        (setq ff-paths-locate-ignore-filenames-compiled
              (concat
               "^"
               ;; workaround for insufficient default
               (let ((max-specpdl-size 1000))
                 (regexp-opt list t))
               "$"))
      (setq ff-paths-locate-ignore-filenames-compiled nil))))

(defcustom ff-paths-locate-ignore-filenames-default
  '("ChangeLog"
    "changelog"
    "changelog.gz"
    "changelog.Debian.gz"
    "copyright"
    "README"
    "README.Debian"
    "README.Debian.gz")
  "A customizable list of filenames to not search for using locate.
Usually a list of very common filenames.

See also `ff-paths-locate-ignore-filenames' and
`ff-paths-locate-ignore-regexps'"
  :type '(set 
          (const "ChangeLog")
          (const "changelog")
          (const "changelog.gz")
          (const "changelog.Debian.gz")
          (const "copyright")
          (const "README")
          (const "README.Debian")
          (const "README.Debian.gz"))
  :set (lambda (symbol value)
         (set-default symbol value)
         (ff-paths-locate-ignore-filenames-compile))
  :group 'ff-paths)

(defcustom ff-paths-locate-ignore-filenames nil
  "*Additional filenames to not search for using locate.
Filenames that you would like the locate search to skip that aren't listed in
`ff-paths-locate-ignore-filenames-default' can be added to this option with the
caveat that regular expressions are not allowed.

See also `ff-paths-locate-ignore-regexps'"
  :type '(repeat (string :tag "Filename:"))
  :set (lambda (symbol value)
         (set-default symbol value)
         (ff-paths-locate-ignore-filenames-compile))
  :group 'ff-paths)

(defcustom ff-paths-locate-ignore-regexps nil
  "*Additional regexps matching filenames to not search for using locate.
Add regular expressions matching filenames that are not to be
searched suing the system locate command here (because the names
are too common to be useful).

See also `ff-paths-locate-ignore-filenames-default' and
`ff-paths-locate-ignore-filenames'."
  :type '(repeat (regexp :tag "Regular expression:"))
  :group 'ff-paths)

(defcustom ff-paths-require-match nil
  "*Whether user has to choose one of the listed files.
This is the argument REQUIRE-MATCH of `completing-read'."
  :group 'ff-paths
  :type 'boolean)

(defcustom ff-paths-gzipped (featurep 'jka-compr)
  "*Search for gzipped-compressed file as well."
  :group 'ff-paths
  :type 'boolean)

(defcustom ff-paths-using-ms-windows (and (boundp 'system-type)
                                          (equal system-type 'windows-nt))
  "*Set to t if using DOS, win95, winNT, etc.
The effect is to set path splitting on the \";\" character instead of \":\""
  :group 'ff-paths
  :type 'boolean)

(defcustom ff-paths-locate-max-matches 20
  "*Maximum number of matches to extract from locate command.
Only this number of mtaches will be displayed and all next matches will be
ignored.  If set to nil, any number of matches will be processed but be
warned that this can take some time (for example, I have 939 files called
changelog.Debian.gz on my system)"
  :group 'ff-paths
  :type 'integer)

(provide 'ff-paths)
;;; ff-paths.el ends here
