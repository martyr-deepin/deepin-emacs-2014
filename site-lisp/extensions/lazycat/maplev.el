;;; maplev.el --- Maple mode for GNU Emacs
;;
;;
;; Copyright (C) 2001,2003 Joseph S. Riel

;; Authors:    Joseph S. Riel <joer@k-online.com>
;;             and Roland Winkler <Roland.Winkler@physik.uni-erlangen.de>
;; Time-stamp: "2003-10-09 13:32:59 jriel"
;; Created:    June 1999
;; Version:    2.154
;; Keywords:   Maple, languages
;; X-URL:      http://www.k-online.com/~joer/maplev/maplev.html
;; X-RCS:      $$

;;{{{ License
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of the
;; License, or (at your option) any later version.
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
;; 02111-1307, USA.
;;}}}
;;{{{ Introduction
;;; Commentary:
;;
;; This package defines five major modes:
;;
;;   maplev-mode:        for editing Maple code
;;   maplev-cmaple-mode: for running Maple
;;   maplev-mint-mode:   for displaying the output of mint
;;   maplev-help-mode:   for displaying Maple help pages
;;   maplev-proc-mode:   for displaying Maple procedures

;;; Features:

;; font-lock (highlighting) of Maple keywords
;; automatic indentation
;; syntax checking (via Mint)
;; online Maple help
;; online display of Maple procedures
;; outlining (not yet)
;; narrowing (nothing here)
;; tags
;; imenu support
;; auto-fill support

;;; Installation:

;; Put this file into your Emacs load path and byte compile it.  Add
;; the following to your `.emacs':
;;
;;   (autoload 'maplev-mode "maplev" "Maple editing mode" t)
;;   (autoload 'cmaple      "maplev" "Start maple process" t)
;;
;; To have Emacs automagically start in MapleV mode when editing Maple
;; source, add the following to your .emacs, modifying the regexp
;; `.mpl' to an extension appropriate for your usage:
;;
;;   (setq auto-mode-alist (cons `("\\.mpl\\'" . maplev-mode) auto-mode-alist))
;;
;; YOU MUST customize some of the default settings to be appropriate
;; for your installation.  You can do this in several ways.  The most
;; user friendly way is to use `customize'.  You can do this with:
;;
;;   M-x load-library RET maplev RET
;;   M-x customize-group RET maplev RET
;;
;; The important options are in the subgroup `maplev-important'.  After
;; setting and testing these options, save them to your .emacs by
;; clicking on the `Save for Future Sessions' button.
;;
;;
;;; History:

;; Oct 99:  Initial release.
;;}}}
;;{{{ To Do
;; High Priority:
;; - make `maplev-beginning-of-proc' and `maplev-end-of-proc' more reliable.
;;
;; Medium Priority:
;; - add comment-out functions
;; - pass `maplev-beginning-of-proc' (or faster) to `font-lock-defaults'.
;;   That should speed up fontification with lazy(?) lock.  Testing.
;; - add clean up routine to kill buffers and processes
;;   when exiting maplev-mode
;; - indent continued assignments (this could be tricky)
;; - more complete definition of maplev-completion-alist based on
;;   the maple help node `index[package]'
;;
;; Low Priority:
;; - font lock local variables
;; - fix problem with folding
;;}}}

;;; Code:

;;{{{ Information
(defconst maplev-version 
  "2.154"
  "Version of MapleV mode.")

(defconst maplev-developer 
  "Joseph S. Riel <joer@k-online.com> and Roland Winkler <Roland.Winkler@physik.uni-erlangen.de>"
  "Developers/maintainers of maplev-mode.")

(defun maplev-about ()
  (interactive)
  (sit-for 0)
  (message "maplev-mode version %s, (C) %s" maplev-version maplev-developer))
;;}}}


(require 'abbrevlist)
(require 'font-lock)
(require 'imenu)
(require 'comint)
(require 'info)
(require 'align)


;;{{{ Compatibility assignments

(eval-and-compile
  (if (not (boundp 'folding-mode)) (defvar folding-mode nil))
  (if (not (fboundp 'folding-open-buffer)) (defun folding-open-buffer ()))

  (defvar maplev-xemacsp
    (or (featurep 'xemacs)
        (string-match "XEmacs\\|Lucid" (emacs-version)))
    "*Non-nil when running under under Lucid Emacs or Xemacs.")

  (when (or (string< emacs-version "20.4") maplev-xemacsp)
    (defun line-beginning-position (&optional n)
      "Return the character position of the first character on the current line.
With argument N not nil or 1, move forward N - 1 lines first.
If scan reaches end of buffer, return that position.
This function does not move point."
      (save-excursion
        (beginning-of-line n)
        (point)))

    (defun line-end-position (&optional n)
      "Return the character position of the last character on the current line.
With argument N not nil or 1, move forward N - 1 lines first.
If scan reaches end of buffer, return that position.
This function does not move point."
      (save-excursion
        (end-of-line n)
        (point))))

  (if maplev-xemacsp
      (defun match-string-no-properties (num &optional string)
        "Return string of text matched by last search, without text properties.
NUM specifies which parenthesized expression in the last regexp.
 Value is nil if NUMth pair didn't match, or there were less than NUM pairs.
Zero means the entire text matched by the whole regexp or whole string.
STRING should be given if the last search was by `string-match' on STRING."
        (if (match-beginning num)
            (if string
                (let ((result
                       (substring string (match-beginning num) (match-end num))))
                  (set-text-properties 0 (length result) nil result)
                  result)
              (buffer-substring-no-properties (match-beginning num)
                                              (match-end num))))))

  ;; The following two inline functions are needed by GNU emacs.
  ;; They mimic the builtin Xemacs functions.
  (unless maplev-xemacsp
    (defun event-window (event)
      "Return the window over which mouse EVENT occurred."
      (nth 0 (nth 1 event)))
    (defun event-point (event)
      "Return the character position of the mouse EVENT."
      (posn-point (event-start event))))

  (defun maplev--mouse-keymap (keys)
    "Generate vector keymap for KEYS corresponding to a mouse button.
It handles the difference between Emacs and Xemacs.  KEYS is a list, the last item
is an integer correspond to the button number; preceding items are optional modifiers"
    (let ((rkeys (reverse keys)))
      (setcar rkeys (intern (concat (if maplev-xemacsp "button" "mouse-")
                                    (number-to-string (car rkeys)))))
      (vector (reverse rkeys)))))

;;}}}
;;{{{ Group definitions
(defgroup maplev nil
  "Major mode for editing Maple source in Emacs"
  :group 'languages)

(defgroup maplev-important nil
  "STUFF THAT MUST BE CONFIGURED."
  :group 'maplev)

(defgroup maplev-faces nil
  "Faces for highlighting text in MapleV mode."
  :group 'maplev)

(defgroup maplev-executables nil
  "Maple and Mint location and configuration."
  :group 'maplev)

(defgroup maplev-templates nil
  "Procedure template and other shortcuts."
  :group 'maplev)

(defgroup maplev-misc nil
  "Miscellaneous options."
  :group 'maplev)

(defgroup maplev-align nil
  "Alignment variables."
  :group 'maplev)

;;}}}
;;{{{ Configurable options
;;{{{   executables
(defcustom maplev-executable-alist
  (if (string-match "windows-nt\\|ms-dos" (symbol-name system-type))
      '(
        ("9" . ("c:/Program Files/Maple Release 9/bin.wnt/cmaple9.exe"
                nil
                "c:/Program Files/Maple Release 9/bin.wnt/mint9.exe"))
        ("8" . ("c:/Program Files/Maple Release 8/bin.wnt/cmaple.exe"
                nil
                "c:/Program Files/Maple Release 8/bin.wnt/mint.exe"))
        ("7" . ("c:/Program Files/Maple Release 7/bin.wnt/cmaple.exe"
                nil
                "c:/Program Files/Maple Release 7/bin.wnt/mint.exe"))
        ("6" . ("c:/Program Files/Maple Release 6/bin.wnt/cmaple.exe"
                nil
                "c:/Program Files/Maple Release 6/bin.wnt/mint.exe"))
        ("5.1" . ("c:/Program Files/MapleV Release 5.1/bin.wnt/cmaple.exe"
                  nil
                  "c:/Program Files/MapleV Release 5.1/bin.wnt/mint.exe"))
        ("5" . ("c:/Program Files/MapleV Release 5/bin.wnt/cmaple.exe"
                nil
                "c:/Program Files/MapleV Release 5/bin.wnt/mint.exe"))
        ("4"   . ("c:/maplev4/bin.win/cmaple.exe"
                  nil
                  "c:/maplev4/bin.win/mint.exe")))
    '(
      ("9"   . ("maple" nil "mint"))
      ("8"   . ("maple" nil "mint"))
      ("7"   . ("maple" nil "mint"))
      ("6"   . ("maple" nil "mint"))
      ("5.1" . ("maple" nil "mint"))
      ("5"   . ("maple" nil "mint"))
      ("4"   . ("maple" nil "mint"))))
  "*Assoc list specifying the available executables.
Each item has the form \(RELEASE MAPLE MAPLE-INIFILE MINT\)
where RELEASE is the Maple release corresponding to the
executables MAPLE and MINT.  MAPLE must be the command line
\(non-GUI\) version of Maple.  MAPLE-INIFILE is the maple
initialization file for running Maple under Emacs;
if nil the default initialization file is used."
  :type '(repeat (list (string :tag "Maple Release")
                       (file   :tag "Maple Executable")
                       (choice :tag "Maple Initialization File"
                               file (const :tag "none" nil))
                       (file   :tag "Mint Executable ")))
  :group 'maplev-executables
  :group 'maplev-important)

;; this isn't quite right, it doesn't permit assigning
;; a new release.

(defcustom maplev-default-release "9"
  "*Release of Maple used as the default executable.
It must be a key in `maplev-executable-alist'."
  :type `(choice ,@(mapcar (lambda (item)
                             (list 'const (car item)))
                           maplev-executable-alist))
  :group 'maplev-executables
  :group 'maplev-important)

(defvar maplev-release maplev-default-release
  "Buffer local string variable assigned the selected release of Maple.
Used to index `maplev-executable-alist'.")
(make-variable-buffer-local 'maplev-release)

(defcustom maplev-init-string-alist
  (let ((maplev-print-R5-
         (concat
          "if not assigned(maplev_print) then\n"
          "  maplev_print := proc(n)\n"
          "    print(`if`(type(evaln(n),procedure),eval,readlib)(n))\n"
          "  end;\n"
          "fi:\n"))
        (maplev-print-R6+
         (concat
          "if not assigned(maplev_print) then"
          "  maplev_print := print "
          "fi:\n"))
        (maplev-interface-string
         (concat
          "prettyprint=1,"
          "verboseproc=2,"
          "errorbreak=0,\n"
          "screenheight=9999,"
          "warnlevel=2"))
        (maplev-kernelopts "kernelopts(printbytes=false):\n"))
    `(
      ("9" . ,(concat maplev-print-R6+
                      "interface(" maplev-interface-string
                      ",errorcursor=false):\n"
                      maplev-kernelopts))
      ("8" . ,(concat maplev-print-R6+
                      "interface(" maplev-interface-string
                      ",errorcursor=false):\n"
                      maplev-kernelopts))
      ("7" . ,(concat maplev-print-R6+
                      "interface(" maplev-interface-string
                      ",errorcursor=false):\n"
                      maplev-kernelopts))
      ("6" . ,(concat maplev-print-R6+
                      "interface(" maplev-interface-string
                      ",errorcursor=false):\n"
                      maplev-kernelopts))
      ("5.1" . ,(concat maplev-print-R5-
                        "interface(" maplev-interface-string
                        ",errorcursor=false):\n"
                        maplev-kernelopts))
      ("5" . ,(concat maplev-print-R5-
                      "interface(" maplev-interface-string
                      ",errorcursor=false):\n"
                      maplev-kernelopts))
      ("4" . ,(concat maplev-print-R5-
                      "interface(" maplev-interface-string "):\n"
                      maplev-kernelopts))
      ))
  "*Assoc list of Maple commands initializing a maple session.
Each item has the form \(RELEASE COMMANDS\) where RELEASE is the
Maple release.  COMMANDS must be a string of Maple commands."
  :type '(repeat (cons (string :tag "Maple Release")
                       (string :tag "Maple Commands")))
  :group 'maplev-executables
  :group 'maplev-important)

(defcustom maplev-mint-info-level 3
  "*Integer controlling amount of information that Mint outputs."
  :type '(choice (const :tag "no info" 0)
                 (const :tag "severe errors" 1)
                 (const :tag "+ serious errors" 2)
                 (const :tag "+ warnings" 3)
                 (const :tag "full report" 4))
  :group 'maplev-mint)

(defcustom maplev-mint-error-level 1
  "*Integer controlling Mint error checking in Maple input."
  :type '(choice (const :tag "no info" 0)
                 (const :tag "severe errors" 1)
                 (const :tag "+ serious errors" 2)
                 (const :tag "+ warnings" 3)
                 (const :tag "full report" 4))
  :group 'maplev-mint)

(defcustom maplev-mint-start-options (list "-q")
  "*List of mint command line options.  
Do not include the info level or the include path,
they are handled by `maplev-mint-info-level' and `maplev-include-path'."
  :type 'list
  ;;   :type '(repeat (choice (const :tag "no logo" " -q")
  ;;                       (const :tag "suppress startup" " -s")
  ;;                       (const :tag "syntax only" " -S")
  ;;                       (const :tag "cross reference" " -x")
  ;;                       (list :tag "library" (const " -b") directory)
  ;;                       (list :tag "append database" (const " -a ") file)
  ;;                       (list :tag "use database" (const " -d ") file)
  ;;                       (list :tag "toggle error" (const " -t ") (string :tag "error number"))))

  :group 'maplev-mint)

(defcustom maplev-include-path nil
  "*List of directories to search for files to include.
Each element is a string (directory name) or nil.
The directories are passed to maple and to mint 
via the \"-I\" option; they are searched for files
specified in Maple preprocessor $include directives."
  :type '(choice (const nil) (repeat string))
  :group 'maplev-executables
  :group 'maplev-mint)


;;}}}
;;{{{   comments
(defcustom maplev-comment-column 40
  "*Column for inline comments.
Use \\[indent-for-comment] to insert or align an inline comment."
  :type 'integer
  :group 'maplev-comments)

(defcustom maplev-comment-start "#"
  "*String to insert to start a Maple inline comment."
  :type 'string
  :group 'maplev-comments)

;; not used by GNU emacs 21
(defcustom maplev-block-comment-start "# "
  "*String to insert to start a Maple standalone comment."
  :type 'string
  :group 'maplev-comments)

(defcustom maplev-auto-fill-comment-flag t
  "*Non-nil means initially enable `auto-fill-mode' in a Maple buffer."
  :type 'boolean
  :group 'maplev-comments)
;;}}}
;;{{{   indentation

(defcustom maplev-indent-level 4
  "*Indentation of Maple statements with respect to containing block."
  :type 'integer
  :group 'maplev-indentation)

(defcustom maplev-indent-declaration 0
  "*Indentation of Maple declarations \(local, global, option, description\)."
  :type 'integer
  :group 'maplev-indentation)

(defcustom maplev-dont-indent-re "[#$]"
  "*Lines starting with this regular expression will not be auto-indented."
  :type '(choice string (const :tag "default" nil))
  :group 'maplev-indentation)

;;}}}
;;{{{   templates

(defcustom maplev-copyright-owner "John Q. Public"
  "*Copyright owner inserted in the copyright string by `maplev-template-proc'."
  :type 'string
  :group 'maplev-templates
  :group 'maplev-important)

(defcustom maplev-comment-end-flag t
  "*Non-nil means add a template's name as a comment following the end.
See `maplev-template-proc'."
  :type 'boolean
  :group 'maplev-templates)

;;; The reason for making this [the following] customizable is to
;;; support mapledoc, a LaTeX package.  To hide the name of the
;;; template in the the typeset output, I use the string " #% ".  To
;;; display it I might use " #\# ", which also prints the hash.

(defcustom maplev-template-end-comment " # "
  "*String prepended to the name of a template at the end,
following the \"end\".  See `maplev-comment-end-flag'."
  :type 'string
  :group 'maplev-templates)

(defcustom maplev-insert-copyright-flag t
  "*Non-nil means insert `maplev-copyright-owner' in a template.
See `maplev-template'."
  :type 'boolean
  :group 'maplev-templates)

(defcustom maplev-description-quote-char ?\`
  "*Quote character for the description statement.
Maple uses a backquote; however, in R5 it makes more sense to use a
double quote.  Procbody, alas, does not handle a double quote."
  :type 'character
  :group 'maplev-templates)

(defcustom maplev-variable-spacing 0
  "*Spaces to insert after a comma in declarations and argument lists."
  :type 'integer
  :group 'maplev-templates)

(defcustom maplev-assignment-operator " := "
  "*Maple assignment operator.  Used by `maplev-insert-assignment-operator'."
  :type 'string
  :group 'maplev-templates)
;;}}}
;;{{{   completion

(defcustom maplev-completion-longdelim-p nil
  "*If non-nil use the long delimiter when completing a Maple control structure.
For example, if non-nil, a `do' loop is completed with `end do',
otherwise it is completed with `od'.  If the maple release is less than 6
than the long delimiter is never used."
  :type 'boolean
  :group 'maplev-completions)
;;}}}
;;{{{   miscellaneous
;; Abbrev mode

(defcustom maplev-initial-abbrev-mode-flag nil
  "*Non-nil means initially enable function `abbrev-mode' in a Maple buffer."
  :type 'boolean
  :group 'maplev-misc)

(defcustom maplev-expand-abbrevs-in-comments-and-strings-flag nil
  "*Non-nil means expand Maple abbreviations in comments and strings.
Nil means do not expand in either."
  :type 'boolean
  :group 'maplev-misc
  :group 'maplev-comments)

;; Saving

(defcustom maplev-clean-buffer-before-saving-flag t
  "*Non-nil means run `maplev-remove-trailing-spaces' before saving."
  :type 'boolean
  :group 'maplev-misc)
;;}}}
;;{{{   align rules

;; Define the maplev alignment rules.
;; Align the assignment operator (`:='), equals signs,
;; columns (`|'), commas, and comments.  
;; Columns and commas are aligned only if the
;; the prefix argument is active (i.e. C-u M-x align).
;; The comment rule is the last rule so that comments are properly aligned.

(defcustom maplev-align-rules-list
  '((maple-assignment-rule
     (regexp   . "\\s-*\\w+\\(\\s-*:\\)=\\(\\s-*\\)")
     (group    . (1 2))
     (justify  . t)
     (tab-stop . nil))
    (maple-equals-rule
     (regexp   . "\\s-*\\w+\\(\\s-*\\)=\\(\\s-*\\)")
     (group    . (1 2))
     (repeat   . t)
     (tab-stop . nil))
    (maple-column-delimiter
     (regexp . "\\(\\s-*\\)\|\\(\\s-*\\)")
     (group  . (1 2))
     (repeat . t)
     (run-if lambda nil current-prefix-arg))
    (maple-comma-delimiter
     (regexp . ",\\(\\s-*\\)\\S-")
     (repeat . t)
     (run-if lambda nil current-prefix-arg))
    (maple-comment
     (regexp . "\\(\\s-+\\)\\s<")
     (column . comment-column)))
  "*A list describing the maplev alignment rules.
See the documentation for `align-rules-list' for more info on the format."
  :type align-rules-list-type
  :group 'maplev-align)

;; Define the alignment exclusion rules.
;; The prevent changing quoted material and comments.

(defcustom maplev-align-exclude-rules-list
  `((exc-dq-string
     (regexp . "\"\\([^\"\n]+\\)\"")
     (repeat . t))
    (exc-sq-string
     (regexp . "'\\([^'\n]+\\)'")
     (repeat . t))
    (exc-bq-string
     (regexp . "`\\([^`\n]+\\)`")
     (repeat . t))
     (exc-open-comment
      (regexp . ,(function
 	  (lambda (end reverse)
 	    (funcall (if reverse 're-search-backward
 		       're-search-forward)
 		     (concat "[^ \t\n\\\\]"
 			     (regexp-quote comment-start)
 			     "\\(.+\\)$") end t))))))
  "*A list describing text that should be excluded from alignment.
See the documentation for `align-exclude-rules-list' for more info."
  :type align-rules-list-type
  :group 'maplev-align)


;;}}}
;;}}}
;;{{{ Internal variables

(defvar maplev-mint--code-buffer nil
  "Buffer containing source code that was passed to Mint.")

(defvar maplev-mint--code-beginning nil
  "Marker at beginning of region in `maplev-mint--code-buffer' that was passed to Mint.")

(defvar maplev-mint--code-end nil
  "Marker at end of region in `maplev-mint--code-buffer' that was passed to Mint.")

(defvar maplev-completion-alist nil
  "Alist for minibuffer completion.")

(defvar maplev-completion-release nil
  "Maple release for which completion has been requested.")

(defvar maplev-history-list nil
  "History list used by maplev.")

(defvar complete-symbol-function nil
  "Mode-specific function to complete a symbol at point.")

;;}}}
;;{{{ Regular expressions
(defconst maplev--declaration-re
  "\\<\\(local\\|options?\\|global\\|description\\|export\\)\\>"
  "Regular expression for a Maple procedure declaration statement.")

(defconst maplev--simple-name-re  "\\<[a-zA-Z_][a-zA-Z0-9_]*\\>"
  "Regular expression for a simple name.")

(defconst maplev--quoted-name-re  "`[^`\n\\\\]*\\(\\\\.[^`\n\\\\]*\\)*`"
  "Regular expression for a Maple quoted name.
It correctly handles escaped backquotes in a name, but not doubled
backquotes.  It intentionally fails for the exceptional case where a
name has a newline character.")

(defconst maplev--symbol-re (concat maplev--simple-name-re "\\|"
                                    maplev--quoted-name-re)
  "Regular expression for a Maple symbol.")

(defconst maplev--indexed-name-re
  (concat "\\("  maplev--symbol-re "\\)" ; base name
          "\\([ \t]*\\[[^][]*\\]\\)+"    ; mandatory indices
          "\\([ \t]*([^)(]*)\\)*")       ; optional arguments
  "Regular expression for a Maple indexed name.
Does not allow a square bracket in the index expression,
nor a parenthesis in the optional arguments.")

(defconst maplev--name-re
  (concat "\\("  maplev--symbol-re "\\)"  ; base name
          "\\([ \t\n\f]*\\[[^][]*\\]\\)*" ; optional indices
          "\\([ \t\n\f]*([^)(]*)\\)*")    ; optional arguments
  "Regular expression for Maple names.")

(defconst maplev--comment-re "#.*$"
  "Regular expression for Maple comments.
A backslash at the end of the line does not continue the comment.")

(defconst maplev--defun-re "\\<proc\\>\\|\\<module\\>"
  "Regular expression at start of a Maple procedure or module.")

(defconst maplev--assignment-re
  (concat maplev--name-re "[ \t\n]*:=[ \t\n]*")
  "Regex that matches a Maple assignment.")

(defconst maplev--defun-begin-re
  (concat maplev--assignment-re
          "\\(" maplev--comment-re "\\)?"
          "[ \t\f\n]*\\(" maplev--defun-re "\\)")
  "Regular expression for Maple procedure assignments.")

(defconst maplev--top-defun-begin-re
  (concat "^" maplev--defun-begin-re)
  "Regular expression for top level Maple procedure assignments.")

(defconst maplev--defun-end-re
  (concat "\\<end\\>"
          ;; "\\([ \t]+" maplev--defun-re "\\)?"
          "[ \t]*[:;]")
  "Regex for \"end\" statement in a Maple procedure assignment.
Does not allow linebreaks as this messes up searching.")

(defconst maplev--top-defun-end-re
  (concat "^\\(" maplev--defun-end-re "\\)"           ; flush left end
          "\\|"                                       ; or
          maplev--top-defun-begin-re "[^#\n]*"        ; one line proc
          maplev--defun-end-re)
  "Regex for \"end\" statement in a top level Maple procedure assignment.
It matches either a flush left \"end\" or a one line procedure assignment.")

(defconst maplev--space-dot-quote-re "\\s-*\\.[`\"]") ; space could be allowed 'twixt dot and quote

;;;(defconst maplev--quote-re "\"[^\"]*\"\\|`[^`]*`")    ; fails when a quote contains a quote.

(defconst maplev--string-re "\"[^\"\\\\]*\\(\\\\[[:ascii:]][^\"\\\\]*\\)*\""
  "Regex that matches a double-quoted Maple string.
It matches even when a string contains newlines or escaped characters, 
including double-quotes.")


(defconst maplev--quote-re
  (concat maplev--quoted-name-re
          "\\|"
          maplev--string-re))

(eval-and-compile
  (defun maplev--list-to-word-re (words)
    "Generate a regular expression that matches one of WORDS, a list."
    (concat "\\<\\(" (regexp-opt words) "\\)\\>")))

;;}}}

;;{{{ Indentation

;; The indentation functions handle the indentation of Maple code.
;; They are based on the Maple-mode package written by Nicholas
;; Thie'ry.  Considerable changes have been made to handle the
;; extended syntax introduced in Maple R6.  Following is a brief
;; description of the algorithm.
;; 
;; The buffer local list variable `maplev--update-indent-info' stores
;; the indentation information at a particular point, call it the
;; `known-indent-point' (the point position is stored in the list).
;; When a line is indented, the algorithm checks whether the current
;; position is greater than `known-indent-point'; if so, it only needs
;; to check between that point and the current position.  If not, it
;; needs to search backwards for a known valid indentation point.  The
;; function `maplev--validate-indent-info' handles this.
;;
;; The amount that a particular line is indented is determined by the
;; grammar defined by the constant assoc list `maplev--grammar-alist'. 
 


;;{{{   module
;; Define variables and functions for handling indentation information.

(defvar maplev--indent-info nil
  "Buffer local variable storing previous indent information.
Nil when there is no previous, or valid, indent information.
Otherwise it's a list: \(POINT STATE STACK\).  
POINT is the character position at which the information applies.  
STATE is the output of `parse-partial-sexp' 
\(valid from the start of the buffer to POINT\).
STACK is a list of lists, each list having the form 
\(KEYWORD INDENT-CLOSE INDENT-FOLLOW\).  
KEYWORD is a keyword or parenthesis in the source.  
INDENT-CLOSE is the indentation amount for the closing keyword.  
INDENT-FOLLOW is the indentation amount for source between
KEYWORD and the closing keyword.")

;; Procedures for accessing the contents of `maplev--indent-info'.

(defsubst maplev--indent-info-point ()
  "Return position of last valid indent."
  (nth 0 maplev--indent-info))

(defsubst maplev--indent-info-state ()
  "Return output of `parse-partial-sexp' from last indent."
  (nth 1 maplev--indent-info))

(defsubst maplev--indent-info-stack ()
  "Return indentation stack."
  (nth 2 maplev--indent-info))

(defsubst maplev--indent-info-assign (point state stack)
  "Assign POINT, STATE, and STACK to the variable `maplev--indent-info'."
  (setq maplev--indent-info (list point state stack)))

(defsubst maplev--clear-indent-info ()
  "Clear the indent information."
  (setq maplev--indent-info nil))

(defun maplev--validate-indent-info ()
  "Update the variable `maplev--indent-info' if nil.
Set POINT in variable to closest valid starting point.
Set STATE and STACK in variable to nil."
  (unless (and maplev--indent-info
               (>= (point) (maplev--indent-info-point)))
    ;; Set POINT to (point) if we're at the beginning of a top level
    ;; procedure assignment, otherwise search backwards for the
    ;; beginning or end of a top level procedure assignment and put
    ;; point outside it.  If neither is found, move point to the start
    ;; of the buffer.  WHAT ABOUT NARROWING AND/OR FOLDING?
    (maplev--indent-info-assign
     (or (and (looking-at maplev--top-defun-begin-re) (point))
         (save-excursion
           (when (re-search-backward
                  (concat "\\(" maplev--top-defun-begin-re "\\)\\|"
                          "\\(" maplev--top-defun-end-re "\\)") nil t)
             (if (nth 2 (match-data)) ; found proc?
                 (match-beginning 0)  ;   goto start of proc
               (match-end 0))))       ;   goto end of proc
         (point-min))                 ; goto top of buffer
     nil nil)))

(defun maplev--before-change-function (beg &rest unused)
  "Clear indent info if the buffer change is before the last info location.
This function is called whenever the buffer is changed.  BEG is the
character position of the beginning of the change.  UNUSED is not used."
  (and maplev--indent-info
       (< beg (maplev--indent-info-point))
       (maplev--clear-indent-info)))
;;}}}
;;{{{   grammar
(defconst maplev--grammar-alist nil
  "Assoc list defining the grammar for Maple indentation.
Each entry has the form \(KEY . \(MATCH-RE OPEN-P INDENT ADJUST-FUNC
POST-FUNC\)\).  KEY is a Maple keyword or parenthesis.  MATCH-RE is a
regular expression that matches any of the keys that follow KEY; nil
means that KEY closes a Maple statement.  OPEN-P is a boolean flag
that is non-nil if KEY can initiate a Maple statement.  INDENT is the
relative indentation for the block immediately following KEY; nil
means that the indentation is handled in an ad hoc fashion.
ADJUST-FUNC is optional, if non-nil it is a function that moves point
to the position from where the indent is computed.  POST-FUNC is
optional, if non-nil it is a function that is called after the keyword
is handled.  Currently it is only used by the keyword `end'.")

;; Removed "in" from grammar to allow its use as a binary operator in Maple R8.
;; The change in the indentation is minor; rarely is there a line break between 
;; an "in" and the "do" in a loop.

(unless maplev--grammar-alist
  (let ((alist
         (list
          (list "proc" . ("\\<end\\>" t maplev-indent-level 'maplev--indent-point-of-proc))
          (list "module" . ("\\<end\\>" t maplev-indent-level 'maplev--indent-point-of-proc))
          (list "end"  . (nil nil 0 nil 'maplev--skip-optional-end-keyword))
;;;          (list "for"  . ((maplev--list-to-word-re '("from" "to" "by" "while" "in" "do")) t 0))
          (list "for"  . ((maplev--list-to-word-re '("from" "to" "by" "while" "do")) t 0))
          (list "for"  . ((maplev--list-to-word-re '("from" "to" "by" "while""do")) t 0))
          (list "from" . ((maplev--list-to-word-re '("to" "by" "while" "do")) t 0))
          (list "to"   . ((maplev--list-to-word-re '("by" "while" "do")) t 0))
          (list "by"   . ((maplev--list-to-word-re '("from" "to" "while" "do")) t 0))
          (list "while" . ((maplev--list-to-word-re '("from" "to" "by" "do")) t 0))
;;;          (list "in"   . ((maplev--list-to-word-re '("while" "do" "end")) t maplev-indent-level))
          (list "do"   . ((maplev--list-to-word-re '("od" "end")) t maplev-indent-level))
          (list "od"   . (nil nil 0))

          (list "if"   . ("\\<then\\>" t 0))
          (list "elif" . ("\\<then\\>" nil 0))
          (list "else" . ((maplev--list-to-word-re '("fi" "end")) nil maplev-indent-level))
          (list "then" . ((maplev--list-to-word-re '("elif" "else" "fi" "end")) nil maplev-indent-level))
          (list "fi"   . (nil nil 0))

;;;          (list "use"  . ("\\<in\\>" t maplev-indent-level))
          (list "use"  . ("\\<end\\>" t maplev-indent-level))
          (list "try"  . ((maplev--list-to-word-re '("catch" "finally" "end")) t maplev-indent-level))
          (list "catch". ((maplev--list-to-word-re '("catch" "finally" "end")) t maplev-indent-level))
          (list "finally". ((maplev--list-to-word-re '("end")) t maplev-indent-level))

          (list "{"    . ("}" t nil))
          (list "["    . ("]" t nil))
          (list "("    . (")" t nil))
          (list "}"    . (nil nil 0))
          (list "]"    . (nil nil 0))
          (list ")"    . (nil nil 0)))))
    (setq maplev--grammar-alist alist)))


(defconst maplev--grammar-keyword-re
  (eval-when-compile
    (concat
     (maplev--list-to-word-re
      '("proc" "module" "end"
;;;      "for" "from" "to" "by" "while" "in" "do" "od"
        "for" "from" "to" "by" "while" "do" "od"
        "if" "elif" "else" "then" "fi"
        "use" "try" "catch" "finally"))
     "\\|\\("
     (regexp-opt '("{" "}" "[" "]" "(" ")" ))
     "\\)"))
  "Regular expression of keywords used in Maple grammar for indentation.")

(defun maplev--skip-optional-end-keyword ()
  "Skip the optional keyword following an end statement."
  (if (looking-at (concat "[ \t]+"
                          (maplev--list-to-word-re '("proc" "module" "do" "use" "if" "try"))))
      (goto-char (match-end 0))))
;;}}}
;;{{{   errors

;; Create a new error symbol, `keyword-out-of-sequence', for handling
;; keywords and parentheses that appear out of sequence during an
;; indentation.  It isn't clear to me that this is the proper way to
;; handle this rather special condition; but I'll go with it for now.

(put 'keyword-out-of-sequence
     'error-conditions
     '(error keyword-out-of-sequence))

(put 'keyword-out-of-sequence 'error-message "Keyword out of sequence")

(defun maplev--handle-grammar-error (err)
  "Handle a grammar error ERR.
Push the mark \(so that we can return to it with \\[universal-argument] \\[set-mark-command]\),
ding the bell, display a message, and move point to the
start of the offending keyword."
  (push-mark)
  (ding)
  (message "Keyword `%s' out of sequence" (nth 1 err))
  (goto-char (nth 2 err)))

;;}}}
;;{{{   functions
(defun maplev-goto-previous-codeline ()
  "Move point to the start of the previous line of Maple code.
Blank lines and comment lines are skipped.
THIS WILL FAIL IN A STRING."
  (interactive)
  (while (and (= (forward-line -1) 0)
              (looking-at "\\s-*\\(#\\|$\\)"))))

(defsubst maplev--indent-point-of-proc ()
  "Move point to position from where a procedure is indented.
Point must originally be just to the left of the \"proc\".
If procedure is anonymous, point is not moved and nil is returned.
Otherwise point is moved to left of assignee and point is returned."
  ;; Regexp does not include possible comments.
  (re-search-backward (concat maplev--assignment-re "\\=") nil t))

(defun maplev--indent-line-with-info ()
  "Indent the current line as Maple code.  Point must be at the left margin."
  (unless (or (and maplev-dont-indent-re
                   (looking-at maplev-dont-indent-re))
              (let ((state (maplev--indent-info-state)))
                (or (nth 3 state) (nth 4 state))))
    (delete-region (point) (progn (skip-chars-forward " \t") (point)))
    (indent-to (maplev--compute-indent (car (maplev--indent-info-stack))))))
;;}}}
;;{{{   algorithm

;; Algorithm:

;; The indentation algorithm is intended to provide rapid indentation
;; both for interactive use, that is, using `maplev-indent-newline',
;; and for global use, that is, using `maplev-indent-region'.
;;
;; To rapidly indent a region, previous indentation information is
;; stored in data structure, `maplev--indent-info'.  See its docstring
;; for a description of the structure.  To interactively indent, the
;; data is checked to see if there is usable information.  If so, it
;; is used, otherwise the nearest preceding syntactically
;; grammatically point (the start or end of a top level procedure
;; assignment) is found and the indentation information computed from
;; that point.


(defun maplev--update-indent-info ()
  "Update the variable `maplev--indent-info' at point.
Scan the source for keywords and parentheses from the previous valid
indent position to point.  Update the stack and state according to the
syntax table and the grammar, `maplev--grammar-alist'.  Restore point.
The calling function must ensure that the previous info point is not
beyond \(point\)."
  (save-excursion
    (let ((point (maplev--indent-info-point))
          (stack (maplev--indent-info-stack))
          (state (maplev--indent-info-state))
          (end (point))
          keyword keyword-beginning key-list indent indent-close
          adjust-func post-func top-stack old-keyword match-re
          case-fold-search)
      (goto-char point)
      (while (re-search-forward maplev--grammar-keyword-re end 'move)

        ;; Assign loop variables.  KEY-POINT is assigned the position
        ;; after the next keyword.  If no keyword exists in the line,
        ;; KEY-POINT is nil.

        (setq keyword (match-string-no-properties 0)
              key-list (cdr (assoc keyword maplev--grammar-alist))
              indent (nth 2 key-list)
              adjust-func (nth 3 key-list)
              post-func (nth 4 key-list)
              top-stack (car stack)
              indent-close (nth 1 top-stack)
              old-keyword (car top-stack) ; Don't set to (old) KEYWORD, it might have been matched
              match-re (and old-keyword
                            (car (cdr (assoc old-keyword maplev--grammar-alist))))
              keyword-beginning (match-beginning 0)
              state (parse-partial-sexp point (point) nil nil state)
              point (point))
        (cond

         ;; If KEYWORD is in a comment or a quote, do nothing.
         ((or (nth 4 state) (nth 3 state))) ; comments are more frequent, so check first

         ;; Does KEYWORD pair with the top one on STACK?
         ((and match-re (string-match match-re keyword))
          ;; Should more keywords follow KEYWORD?
          (if (nth 0 key-list)
              ;; If so, replace the top of STACK with a new list.  The
              ;; new list has the new KEYWORD, the INDENT-CLOSE from
              ;; the old list, and
              (setcar stack (list keyword
                                  indent-close
                                  (+ indent-close indent)))
            ;; otherwise pop the top of STACK.
            (and post-func (funcall post-func))
            (setq stack (cdr stack))))

         ;; Is KEYWORD an opening keyword?  Push a new item onto
         ;; STACK.

         ((nth 1 key-list)
          (setq stack
                (cons
                 (cons
                  keyword
                  ;; Handle keywords and parentheses
                  ;; differently. Indentation for keywords that start
                  ;; a Maple statement is from `keyword-beginning';
                  ;; however, if the keyword is an assigned proc then
                  ;; the actual beginning of the keyword is the start
                  ;; of the assigned name.
                  (if indent
                      (save-excursion
                        (goto-char keyword-beginning)
                        (and adjust-func (funcall adjust-func))
                        (list (current-column) ; alignment for closing keyword
                              (+ (current-column) indent))) ; alignment for subblock

                    ;; Handle an open parenthesis.  INDENT-CLOSE is
                    ;; set to the same column as the parerenthesis so
                    ;; that the closing parenthesis is aligned.  If
                    ;; space or a a comment follows the parenthesis,
                    ;; then the following block of code is indented
                    ;; from the current indentation.  Otherwise
                    ;; following code indents to first character
                    ;; following the parenthesis.
                    (list
                     (1- (current-column)) ; INDENT-CLOSE
                     (progn
                       (skip-chars-forward " \t")
                       (if (looking-at "#\\|$") ; no code on remainder of line
                           (+ (current-indentation) maplev-indent-level)
                         (current-column))))))
                 stack)))

         ;; KEYWORD is out of sequence.  Move point before KEYWORD and
         ;; signal an error.
         (t (re-search-backward keyword)
            (signal 'keyword-out-of-sequence (list keyword (point))))))
      (if (< point end)
          (setq state (parse-partial-sexp point (point) nil nil state)))
      (maplev--indent-info-assign end state stack))))

;;}}}
;;{{{   commands

(defun maplev--compute-indent (indent-info)
  "Return the indentation required for a Maple code line.
INDENT-INFO is the indentation information applicable to this line;
it it is a list of three items: \(KEYWORD INDENT-CLOSE INDENT-FOLLOW\).
See `maplev--indent-info' for details.  If INDENT-INFO is nil then 0
is returned.  Point must be at current indentation."
  (if (not indent-info)
      0
    (save-excursion
      (let ((point (point)))
        (cond
         ;; Handle declarations in procedures (and modules)
         ((and (string-match maplev--defun-re (car indent-info))
               (looking-at maplev--declaration-re))
          (+  maplev-indent-declaration
              (nth 1 indent-info)))
         ;; Continued dotted quotes, e.g. ``."a string".''
         ;; They are aligned with previous quoted material.
         ;; There should be a flag to disable this.
         ((and
           (looking-at maplev--space-dot-quote-re)
           (not (bobp))
           (save-excursion
             (maplev-goto-previous-codeline)
             (setq point (point))
             (end-of-line)
             (setq point (re-search-backward maplev--quote-re point 'move))))
          (goto-char point)
          (max 0 (1- (current-column))))

         ;; We've handled the special cases.
         ;; Now to tackle regular statements.
         (t
          (or
           (let* ((old-keyword (car indent-info))
                  (match (and old-keyword (nth 1 (assoc old-keyword maplev--grammar-alist)))))
             (nth (if (and match (looking-at match))
                      1
                    2)
                  indent-info))
           0)))))))                     ; maplev--compute-indent

(defun maplev-indent-region (beg end)
  "Indent the region between POINT and MARK.
BEG and END may also be passed to the function."
  (interactive "r")
  (condition-case err
      (save-excursion
        (let ((before-change-functions nil)
              (after-change-functions nil))
          ;; Clear the indent stack.  Goto to the start of the region.
          ;; Set up a marker for the end of the region (it is used to
          ;; compute the percent completed).
          (goto-char beg)
          (beginning-of-line)
          (setq end (set-marker (make-marker) end))
          (maplev--clear-indent-info)  ; temporary
          (maplev--validate-indent-info)

          ;; THE FOLLOWING LINE IS EXPERIMENTAL BUT SEEMS NECESSARY
          (maplev--update-indent-info)
          ;; Indent each line in the region

          (while (and (<= (point) end) (not (eobp)))
            (maplev--indent-line-with-info)
            (forward-line)
            (maplev--update-indent-info)
            (message "Indenting...(%d%%)"
                     (min 100 (* 10 (/ (* 10 (- (point) beg)) (- end beg))))))

          (message "Indenting...done")
          (set-marker end nil)))

    (keyword-out-of-sequence
     (maplev--handle-grammar-error err)))) ; {end} maplev-indent-region


(defun maplev-indent-buffer ()
  "Indent the buffer."
  (interactive)
  (save-restriction
    (widen)
    (maplev-indent-region (point-min) (point-max))))

(defun maplev-indent-procedure (&optional level)
  "Indent the current procedure or module."
  (interactive "p")
  (apply 'maplev-indent-region (maplev-current-proc level)))

(defun maplev-indent-line ()
  "Indent current line according to grammar.
If point was to the left of the initial indentation, it moves to the
final indentation; otherwise it remains in the same position relative
to the indentation."
  (interactive)
;; 25-Feb-2001: Added condition-case to move cursor to an out of sequence keyword.
  (condition-case err
      (let ((before-change-functions nil))
        (goto-char (max (save-excursion
                          (beginning-of-line)
                          (maplev--validate-indent-info)
                          (maplev--update-indent-info)
                          (maplev--indent-line-with-info)
                          (point))
                        (point))))
    (keyword-out-of-sequence
     (maplev--handle-grammar-error err))))

;; This is used by `indent-for-comment' to decide how much to indent a
;; comment in Maple code based on its context.

(defun maplev-comment-indentation ()
  "Return the column at which a comment should be started or moved to.
If the line starts with a flush left comment, return 0."
  (if (looking-at "^#")
      0                 ; Existing comment at bol stays there.
    comment-column))

;; Xmaple doesn't support selections
(defun maplev-insert-cut-buffer (&optional arg)
  "Inserts the value of the X server cut-buffer 0.
Text string is added to kill ring. Prefix arguments are interpreted as
with \\[yank]."
  (interactive "*P")
  (kill-new (x-get-cut-buffer 0))
  (setq this-command 'yank)
  (yank arg))

;; borrowed from mouse-yank-at-click
(defun maplev-mouse-yank-cut-buffer (click arg)
  "Inserts the value of the X server cut-buffer 0 at the position clicked on.
Also move point to one end of the text thus inserted (normally the end),
and set mark at the beginning.
Prefix arguments are interpreted as with \\[yank].
If `mouse-yank-at-point' is non-nil, insert at point
regardless of where you click."
  (interactive "e\nP")
  (kill-new (x-get-cut-buffer 0))
  ;; Give temporary modes such as isearch a chance to turn off.
  (run-hooks 'mouse-leave-buffer-hook)
  (or mouse-yank-at-point (mouse-set-point click))
  (setq this-command 'yank)
  (setq mouse-selection-click-count 0)
  (yank arg))

;;}}}

;;}}}

;;{{{ Mode map

(defvar maplev-mode-map nil
  "Keymap used in Maple mode.")

(unless maplev-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [(tab)]                      'maplev-electric-tab)
    (define-key map [(meta tab)]                 'maplev-complete-symbol)
    (define-key map [(backspace)]                'backward-delete-char-untabify)
    (define-key map [(control backspace)]        'maplev-untab)
    (define-key map [(control ?\;)]              'maplev-insert-assignment-operator)
    (define-key map [(control c) (control t) ?p] 'maplev-template-proc)
    (define-key map [(control c) (control t) ?m] 'maplev-template-module)
    (define-key map [(control j)]                'maplev-indent-newline)
    (define-key map [(control return)]           'maplev-newline-and-comment)
    (define-key map [(meta control h)]           'maplev-mark-proc)
    (define-key map [(meta control a)]           'maplev-beginning-of-proc)
    (define-key map [(meta control e)]           'maplev-end-of-proc)
    (define-key map [(control x) ?n ?d]          'maplev-narrow-to-proc)

    ;; These two bindings are needed only under linux / unix
    (define-key map [(meta control y)]          'maplev-insert-cut-buffer)
    (define-key map (maplev--mouse-keymap '(control meta 2)) 'maplev-mouse-yank-cut-buffer)

    (define-key map [(control c) (control l)] 'maplev-add-local-variable)
    (define-key map [(control c) (control g)] 'maplev-add-global-variable)
    (define-key map [(control c) (control e)] 'maplev-add-export-variable)

    ;; Indent commands
    (define-key map [(control c) (tab) ?b]  'maplev-indent-buffer)
    (define-key map [(control c) (tab) tab] 'maplev-indent-buffer)
    (define-key map [(control c) (tab) ?p]  'maplev-indent-procedure)
    (define-key map [(control c) (tab) ?r]  'maplev-indent-region)

    ;; Cmaple commands
    (define-key map [(control c) (control c) ?b]      'maplev-cmaple-send-buffer)
    (define-key map [(control c) (control c) ?p]      'maplev-cmaple-send-procedure)
    (define-key map [(control c) (control c) ?r]      'maplev-cmaple-send-region)
    (define-key map [(control c) (control c) return] 'maplev-cmaple-send-line)
    (define-key map [(control c) (control c) ?g]      'maplev-cmaple-pop-to-buffer)
    (define-key map [(control c) (control c) ?i]      'maplev-cmaple-interrupt)
    (define-key map [(control c) (control c) ?k]      'maplev-cmaple-kill)
    (define-key map [(control c) (control c) ?s]      'maplev-cmaple-status)

    ;; Mint commands    

    (define-key map [(control c) (return) ?b] 'maplev-mint-buffer)
    (define-key map [(control c) (return) ?p] 'maplev-mint-procedure)
    (define-key map [(control c) (return) ?r] 'maplev-mint-region)
    (define-key map [(control c) (return) return] 'maplev-mint-rerun)

    ;; Help and proc comma    
      
    (define-key map [(control ?\?)] 'maplev-help-at-point)
    (define-key map [(meta ?\?)]    'maplev-proc-at-point)

    ;; Xemacs and FSF Emacs use different terms for mouse buttons

    (define-key map (maplev--mouse-keymap '(control shift 2)) 'maplev-help-follow-mouse)
    (define-key map (maplev--mouse-keymap '(meta shift 2))    'maplev-proc-follow-mouse)

    (define-key map [(control c) (control s) ?h] 'maplev-switch-buffer-help)
    (define-key map [(control c) (control s) ?l] 'maplev-switch-buffer-proc)
    (define-key map [(control c) (control s) ?c] 'maplev-switch-buffer-cmaple)

    (setq maplev-mode-map map)))
;;}}}
;;{{{ Menu

(defvar maplev--menu-decoration
  '(["reserved words"  (maplev-reset-font-lock 1) :style radio
     :selected (equal font-lock-maximum-decoration 1)]
    ["+ special words"  (maplev-reset-font-lock 2) :style radio
     :selected (equal font-lock-maximum-decoration 2)]
    ["+ builtin functions"  (maplev-reset-font-lock 3) :style radio
     :selected (or (equal font-lock-maximum-decoration 3)
                   (equal font-lock-maximum-decoration t))])
  "Menu items for changing the decoration level in Maple mode.")

(defvar maplev-menu nil)
(unless maplev-menu
  (easy-menu-define
    maplev-menu maplev-mode-map
    "Menu for MapleV mode."
    `("MapleV"
      ("Indent"
       ["Buffer"    maplev-indent-buffer t]
       ["Procedure" maplev-indent-procedure t]
       ["Region"    maplev-indent-region t])
      ("Mint"
       ["Buffer"    maplev-mint-buffer t]
       ["Procedure" maplev-mint-procedure t]
       ["Region"    maplev-mint-region t]
       ["Rerun"     maplev-mint-rerun :active maplev-mint--code-beginning]
       "---"
       ("Mint level"
        ["severe errors"  (setq maplev-mint-info-level   1) :style radio :selected (= maplev-mint-info-level 1)]
        ["+ serious errors" (setq maplev-mint-info-level 2) :style radio :selected (= maplev-mint-info-level 2)]
        ["+ warnings"       (setq maplev-mint-info-level 3) :style radio :selected (= maplev-mint-info-level 3)]
        ["full report"    (setq maplev-mint-info-level   4) :style radio :selected (= maplev-mint-info-level 4)]))
      ("Maple"
       ["Goto buffer"    maplev-cmaple-pop-to-buffer t]
       ["Send buffer"    maplev-cmaple-send-buffer t]
       ["Send procedure" maplev-cmaple-send-procedure t]
       ["Send region"    maplev-cmaple-send-region t]
       ["Send line"      maplev-cmaple-send-line t]
       "---"
       ["Interrupt"   maplev-cmaple-interrupt t]
       ["Kill"        maplev-cmaple-kill t])
      ("Help"
       ["Word"        maplev-help-at-point t]
       ["Highlighted" maplev-help-region t])
      "---"
      ("Setup"
       ("Maple Release"
        ,@(mapcar (lambda (item)
                    (let ((key (car item)))
                      `[,key (maplev-set-release ,key)
                             :style radio
                             :selected (string= maplev-release ,key)]))
                  maplev-executable-alist))
       ("Abbrevs"
        ["Enable abbrevs" abbrev-mode
         :style toggle :selected abbrev-mode]
        ["List abbrevs" maplev-abbrev-help t])
       ["Enable auto fill" auto-fill-mode
        :style toggle :selected auto-fill-function]
       ("Decoration" ,@maplev--menu-decoration))
      "---"
      ["Add Index" maplev-add-imenu (not (and (boundp 'imenu--index-alist)
                                              imenu--index-alist))]

      "---"
      ["Quit"      quit-window t]
      "---"
      ["Info"  maplev-goto-info-node t]
      ["About" maplev-about t])))

;;}}}
;;{{{ Abbreviations

(defun maplev--abbrev-hook ()
  "Unexpand an abbreviation in a string or a comment.
The variable `maplev-expand-abbrevs-in-comments-and-strings-flag'
controls the expansion."
  (unless maplev-expand-abbrevs-in-comments-and-strings-flag
    ;; Searching can be expensive:
    ;; We assume that strings do not span more than one line
    (let ((state (parse-partial-sexp (maplev-safe-position) (point))))
      (if (or (nth 4 state) (nth 3 state))
          (unexpand-abbrev)))))

(defvar maplev-mode-abbrev-table nil
  "Abbrev table used in MapleV mode buffers.")

(unless maplev-mode-abbrev-table
  (let ((ac abbrevs-changed))
    (define-abbrev-table
      'maplev-mode-abbrev-table
      '(("ar"    "array"      maplev--abbrev-hook 0)
        ("ass"   "assigned"   maplev--abbrev-hook 0)
        ("co"    "convert"    maplev--abbrev-hook 0)
        ("err"   "ERROR"      maplev--abbrev-hook 0)
        ("fail"  "FAIL"       maplev--abbrev-hook 0)
        ("fr"    "from"       maplev--abbrev-hook 0)
        ("gl"    "global"     maplev--abbrev-hook 0)
        ("inf"   "infinity"   maplev--abbrev-hook 0)
        ("lib"   "libname"    maplev--abbrev-hook 0)
        ("lo"    "local"      maplev--abbrev-hook 0)
        ("ma"    "matrix"     maplev--abbrev-hook 0)
        ("npf"   "nprintf"    maplev--abbrev-hook 0)
        ("null"  "NULL"       maplev--abbrev-hook 0)
        ("pi"    "Pi"         maplev--abbrev-hook 0)
        ("pnam"  "procname"   maplev--abbrev-hook 0)
        ("pf"    "printf"     maplev--abbrev-hook 0)
        ("remem" "remember"   maplev--abbrev-hook 0)
        ("ret"   "RETURN"     maplev--abbrev-hook 0)
        ("rlib"  "readlib"    maplev--abbrev-hook 0)
        ("stext" "searchtext" maplev--abbrev-hook 0)
        ("stxt"  "SearchText" maplev--abbrev-hook 0)
        ("ta"    "table"      maplev--abbrev-hook 0)
        ("th"    "then"       maplev--abbrev-hook 0)
        ("trap"  "traperror"  maplev--abbrev-hook 0)
        ("ty"    "type"       maplev--abbrev-hook 0)
        ("user"  "userinfo"   maplev--abbrev-hook 0)
        ("wh"    "while"      maplev--abbrev-hook 0)))
    (setq abbrevs-changed ac)))

(defun maplev-abbrev-help ()
  "List the currently defined abbreviations."
  (interactive)
  (list-one-abbrev-table maplev-mode-abbrev-table "*Abbrevs*"))

;;}}}
;;{{{ Syntax table

(defvar maplev-mode-syntax-table nil
  "Syntax table used in MapleV mode buffers \(except R4\).")

(unless maplev-mode-syntax-table
  (setq maplev-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?_  "_"  maplev-mode-syntax-table) ; symbol constituent
  (modify-syntax-entry ?\\ "\\" maplev-mode-syntax-table) ; escape
  (modify-syntax-entry ?#  "<"  maplev-mode-syntax-table) ; comment starter
  (modify-syntax-entry ?\n ">"  maplev-mode-syntax-table) ; newline = comment ender
  (modify-syntax-entry ?\f ">"  maplev-mode-syntax-table) ; formfeed = comment ender
  (modify-syntax-entry ?\r " "  maplev-mode-syntax-table) ; return = whitespace
  (modify-syntax-entry ?\t " "  maplev-mode-syntax-table) ; tab = whitespace

  (modify-syntax-entry ?*  "."  maplev-mode-syntax-table) ; punctuation
  (modify-syntax-entry ?/  "."  maplev-mode-syntax-table)
  (modify-syntax-entry ?+  "."  maplev-mode-syntax-table)
  (modify-syntax-entry ?-  "."  maplev-mode-syntax-table)
  (modify-syntax-entry ?=  "."  maplev-mode-syntax-table)
  (modify-syntax-entry ?<  "."  maplev-mode-syntax-table)
  (modify-syntax-entry ?>  "."  maplev-mode-syntax-table)
  (modify-syntax-entry ?.  "."  maplev-mode-syntax-table)

  (modify-syntax-entry ?\' "\"" maplev-mode-syntax-table) ; string quotes
  (modify-syntax-entry ?\` "\"" maplev-mode-syntax-table) ; string quotes
  (modify-syntax-entry ?\{ "(}" maplev-mode-syntax-table) ; balanced brackets
  (modify-syntax-entry ?\[ "(]" maplev-mode-syntax-table)
  (modify-syntax-entry ?\( "()" maplev-mode-syntax-table)
  (modify-syntax-entry ?\} "){" maplev-mode-syntax-table)
  (modify-syntax-entry ?\] ")[" maplev-mode-syntax-table)
  (modify-syntax-entry ?\) ")(" maplev-mode-syntax-table)

  ;; Entries for R5 and later
  (modify-syntax-entry ?%  "."  maplev-mode-syntax-table)
  (modify-syntax-entry ?\" "\"" maplev-mode-syntax-table)
  )

(defvar maplev-mode-4-syntax-table nil
  "Syntax table used in MapleV mode buffers for R4.")

;; In R4 the ditto operator is `"'
(unless maplev-mode-4-syntax-table
  (setq maplev-mode-4-syntax-table
        (copy-syntax-table maplev-mode-syntax-table))
  (modify-syntax-entry ?\" "." maplev-mode-4-syntax-table))

(defvar maplev-help-mode-syntax-table nil
  "Syntax table used in Maple help buffer.")

(unless maplev-help-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?_ "w" table)
    ;;    (modify-syntax-entry ?[ "w" table)
    ;;    (modify-syntax-entry ?] "w" table)
    ;;    (modify-syntax-entry ?/ "w" table)
    (setq maplev-help-mode-syntax-table table)))

;;}}}
;;{{{ Imenu support
;; Index all the procedure assignments.  Other possiblities to index
;; are global variable assignments, macros and aliases; however,
;; selecting them is difficult.

(defvar maplev-imenu-generic-expression
  `(("Procedures" ,maplev--top-defun-begin-re 1)
    ("Variables" ,(concat "^\\(" maplev--name-re "\\)"
                          "[ \t\n]*:=[ \t\n]*"
                          "\\([^ \t\np]\\|p\\([^r]\\|r\\([^o]\\|o\\([^c]\\|c[^ \t\n(]\\)\\)\\)\\)") 1)
    ("Macros" ,(concat "^macro([ \t]*\\([^ \t=]*\\)") 1))
  "Imenu expression for MapleV mode.  See `imenu-generic-expression'.")

(defun maplev--imenu-goto-function (name position &rest ignore)
  "Move point to POSITION.  Ignore NAME and IGNORE.
This works with `folding-mode', but crudely.  Folding mode appears to
have an error; `folding-goto-char' does not work reliably.  Until that
is fixed the solution is to open the entire buffer."
  (and (or (< position (point-min))
           (> position (point-max)))
       (widen))
  (if folding-mode (folding-open-buffer))
  (goto-char position))

(defun maplev-add-imenu ()
  "Add an imenu of Maple procedures."
  (interactive)
  (imenu-add-to-menubar "Index")
  (menu-bar-mode 1))

(defun maplev--imenu-create-index-function ()
  "Create an index for `imenu'.
Check whether `folding-mode' is active."
  (if folding-mode (folding-open-buffer))
  (imenu-default-create-index-function))
;;}}}
;;{{{ Buffer edit functions

;; Does this work with folding-mode?
(defun maplev-remove-trailing-spaces  ()
  "Remove trailing spaces in the whole buffer."
  (interactive)
  (save-match-data
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (while (re-search-forward "[ \t]+$" (point-max) t)
          (replace-match "" nil nil))))))


(defun maplev-goto-comment ()
  "Move point just after comment character in line.
If there is no comment character in the line, move point to end of line
and return nil, otherwise return t."
  (interactive)
  (beginning-of-line)
  (maplev--validate-indent-info)
  (let ((state  (parse-partial-sexp
                 (maplev--indent-info-point)
                 (point)
                 nil nil (maplev--indent-info-state))))
    (nth 4 (parse-partial-sexp
            (point)
            (line-end-position)
            nil nil state 'comment-stop))))

(defun maplev-fill-paragraph (&optional justify)
  "Like \\[fill-paragraph], but handles Maple comments.
Assigned to `fill-paragraph-function'.  If any of the current line is
a comment, fill the comment or the paragraph of it that point is in,
preserving the comment's indentation and initial comment symbol.
Prefix JUSTIFY means justify as well."
  (interactive "P")
  (let (has-code             ; Non-nil if line contains code (possibly blank)
        comment-fill-prefix) ; Appropriate fill-prefix for a comment.

    ;; Figure out what kind of comment we are looking at.

    (save-excursion
      (beginning-of-line)
      (setq has-code (looking-at "[ \t]*[^ \t#]"))
      (when (maplev-goto-comment)
        (backward-char)
        (looking-at "#+[\t ]*")
        (setq comment-fill-prefix
              (concat (if indent-tabs-mode
                          (progn
                            (make-string (/ (current-column) tab-width) ?\t)
                            (make-string (% (current-column) tab-width) ?\ ))
                        (make-string (current-column) ?\ ))
                      (buffer-substring (match-beginning 0) (match-end 0))))
        (save-restriction
          (beginning-of-line)
          (narrow-to-region
           ;; Find the first line we should include in the region to fill.
           (save-excursion
             (while (and (zerop (forward-line -1))
                         (looking-at "^[ \t]*#")))
             ;; We may have gone too far.  Go forward again if there
             ;; is no comment on this line.
             (or (looking-at ".*#")
                 (forward-line 1))
             (point))
           ;; Find the beginning of the first line past the region to fill.
           (save-excursion
             (while (progn (forward-line 1)
                           (looking-at "^[ \t]*#")))
             (point)))

          ;; Lines with only comment characters on them
          ;; can be paragraph boundaries.
          (let* ((paragraph-start    (concat paragraph-start "\\|[ \t#]*$"))
                 (paragraph-separate (concat paragraph-start "\\|[ \t#]*$"))
                 (paragraph-ignore-fill-prefix nil)
                 (fill-prefix comment-fill-prefix)
                 (after-line (if has-code
                                 (save-excursion
                                   (forward-line 1) (point))))
                 (end (progn
                        (forward-paragraph)
                        (or (bolp) (newline 1))
                        (point)))
                 ;; If this comment starts on a line with code,
                 ;; include that line in the filling.

                 (beg (progn (backward-paragraph)
                             (if (eq (point) after-line)
                                 (forward-line -1))
                             (point))))
            (fill-region-as-paragraph beg end
                                      justify nil
                                      (save-excursion
                                        (goto-char beg)
                                        (if (looking-at fill-prefix)
                                            nil
                                          (re-search-forward comment-start-skip)
                                          (point)))))))
      t)))

;;}}}
;;{{{ Info

;; This must go elsewhere (in maplev-mode).
;; (put 'maplev 'info-file "maplev")

;;(info 'maplev)

(defun maplev-goto-info-node ()
  "Go to the info node for maplev."
  (interactive)
  (require 'info)
  (let ((where (save-window-excursion
		 (Info-find-emacs-command-nodes 'maplev))))
    (if (not where)
        (error "Could not find info file for maplev")
      (let (same-window-buffer-names)
	(info))
      (Info-find-node (car (car where)) (car (cdr (car where)))))))
;;}}}

;;{{{ MapleV mode
;;{{{   Release
(defsubst maplev--major-release ()
  "Integer variable assigned the selected release of Maple."
  (truncate (string-to-number maplev-release)))

(defun maplev-set-release (&optional release)
  "Assign the buffer local variable `maplev-release'.
RELEASE is a key in `maplev-executable-alist', if not supplied then
`maplev-default-release' is used. Set syntax table according to
RELEASE. If in `maplev-mode' also refontify the buffer."
  (interactive
   (list (completing-read "Use Maple release: "
                          (mapcar (lambda (item) (list (car item)))
                                  maplev-executable-alist)
                          nil t)))
  (setq release (or release maplev-default-release))
  ;; Invalid values of release are possible only due to an invalid value
  ;; of maplev-default-release.
  (unless (assoc release maplev-executable-alist)
    (error "Invalid Maple release: %S" release))
  (setq maplev-release release)
  (cond ((memq major-mode '(maplev-mode maplev-cmaple-mode maplev-proc-mode))
         (if (< (maplev--major-release) 5)
             (set-syntax-table maplev-mode-4-syntax-table)
           (set-syntax-table maplev-mode-syntax-table)))
        ;; for consistency also maplev-help-mode
        ((eq major-mode 'maplev-help-mode)
         (set-syntax-table maplev-help-mode-syntax-table)))
  (when (eq major-mode 'maplev-mode)
    (maplev-reset-font-lock)
    (maplev-mode-name)))
;;}}}
;;{{{   definition
(defun maplev-mode ()
  "Major mode for editing Maple code.

\\[maplev-electric-tab] indents the current line.
\\[maplev-indent-newline] indents the current line and inserts a new indented line.
\\[maplev-newline-and-comment] inserts a newline and begins a flush left comment.

\\[maplev-insert-assignment-operator] inserts `:=' with spaces at end of line.
\\[maplev-template-proc] inserts a procedure template after querying for options.

There are functions and keys for indenting code, syntax checking \(via mint\),
displaying Maple help pages and printing the source code of procedures from the
Maple libraries.

\\{maplev-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (use-local-map maplev-mode-map)
  (setq major-mode 'maplev-mode)

  ;; abbreviation
  (setq local-abbrev-table maplev-mode-abbrev-table)

  ;; paragraph filling
  ;;
  ;; The assignment to `paragraph-start' is copied from emacs-lisp.el.
  ;; Note that because `page-delimiter' is, by default, "^\f", that
  ;; is, `^L' anchored to the beginning of the line, the assignment to
  ;; `paragraph-start' violates the explicit warning in the docstring
  ;; about not anchoring this value.  Not a big deal.

  (set (make-local-variable 'paragraph-start)         (concat page-delimiter "\\|$"))
  (set (make-local-variable 'paragraph-separate)       paragraph-start)
  (set (make-local-variable 'fill-paragraph-function) 'maplev-fill-paragraph)
  (set (make-local-variable 'paragraph-ignore-fill-prefix) t)
  (set (make-local-variable 'adaptive-fill-mode)       nil)
  (set (make-local-variable 'auto-fill-inhibit-regexp) (concat "[ \t]*[^  \t#]"))

  (set (make-local-variable 'require-final-newline) t)

  (auto-fill-mode (if maplev-auto-fill-comment-flag 1 0))

  ;; indentation
  (set (make-local-variable 'indent-line-function)   'maplev-indent-line)
  (set (make-local-variable 'indent-region-function) 'maplev-indent-region)
  (set (make-local-variable 'tab-width)               maplev-indent-level)
  (set (make-local-variable 'indent-tabs-mode)        nil)

  ;; abbrev expansion
  (abbrev-mode (if maplev-initial-abbrev-mode-flag 1 0))

  ;; comments
  (set (make-local-variable 'comment-start)            maplev-comment-start)
  (set (make-local-variable 'block-comment-start)      maplev-block-comment-start)
  (set (make-local-variable 'comment-end)              "")
  (set (make-local-variable 'comment-start-skip)       "#+[ \t]*")
  (set (make-local-variable 'comment-column)           maplev-comment-column)
  (set (make-local-variable 'comment-indent-function) 'maplev-comment-indentation)

  ;; menubar (for Xemacs, GNU Emacs doesn't need this)
  (and maplev-menu (easy-menu-add maplev-menu))

  ;; imenu
  (set (make-local-variable 'imenu-default-create-index-function)
       'maplev--imenu-create-index-function)
  (set (make-local-variable 'imenu-default-goto-function)
       'maplev--imenu-goto-function)
  (set (make-local-variable 'imenu-generic-expression)
       maplev-imenu-generic-expression)
  (set (make-local-variable 'imenu-case-fold-search) nil)

  ;; aligning rules

  (setq align-mode-rules-list maplev-align-rules-list)
  (setq align-mode-exclude-rules-list maplev-align-exclude-rules-list)

  ;; completion

  (set (make-local-variable 'complete-symbol-function) 'maplev-complete-symbol)

  ;; Font lock support: make these variables buffer-local
  ;; so that we can change the decoration level
  (make-local-variable 'font-lock-defaults)
  (make-local-variable 'font-lock-maximum-decoration)

  ;; Mint support
  (make-local-variable 'maplev-mint--code-beginning)
  (make-local-variable 'maplev-mint--code-end)

  ;; Is this what one wants??
  (set (make-local-variable 'beginning-of-defun-function)
       #'(lambda () (maplev-proc-beginning 1 t)))
  (set (make-local-variable 'end-of-defun-function)
       #'(lambda () (maplev-proc-end 1 t)))
  ;; (set (make-local-variable 'add-log-current-defun-function)
  ;;      #'maplev-current-proc-name) ;; not yet available

  ;; Release support
  (maplev-set-release)
  ;; the file's local variables specs might change maplev-release
  ;; xemacs version of make-local-hook returns t, not the hook. (JR)
  ;; make-local-hook is obsolete in GNU emacs 21.1
  (make-local-hook 'hack-local-variables-hook)
  (add-hook 'hack-local-variables-hook 'maplev-mode-name nil t)

  ;; Set hooks
  (if maplev-clean-buffer-before-saving-flag
      (add-hook 'local-write-file-hooks 'maplev-remove-trailing-spaces))
  (make-local-hook 'before-change-functions)
  (add-hook 'before-change-functions 'maplev--before-change-function nil t)
  (run-hooks 'maplev-mode-hook))

(defun maplev-mode-name ()
  "Set `mode-name' in `maplev-mode' according to `maplev-release'."
  (setq mode-name (concat "Maple R" maplev-release)))
;;}}}
;;}}}

;;{{{ Electric functions

(defun maplev-indent-newline ()
  "Indent current line, insert a newline and indent the new line.
Current line is not indented if it is a comment.  Remove trailing
whitespace."
  (interactive "*")
  (or (maplev--comment-line-indentation) ; nil if a comment
      (maplev-indent-line))
  (delete-horizontal-space)              ; remove trailing whitespace
  (newline)
  (maplev-indent-line))

(defun maplev-insert-assignment-operator ()
  "Insert the Maple assignment operator after last nonwhite character."
  (interactive "*")
  (end-of-line)
  (skip-chars-backward " \t")
  (delete-region (point) (line-end-position))
  (insert maplev-assignment-operator))

(defun maplev-electric-tab ()
  "Indent the current line."
  (interactive "*")
  (maplev-indent-line))

(defun maplev-newline-and-comment ()
  "Insert a newline and start a new comment line.
If the current line is a code line, the comment is set flush left,
otherwise it is aligned with the previous code line."
  (interactive "*")
  (newline)                             ; should we indent?
  (let ((indent (maplev--comment-line-indentation -1)))
    (and indent (indent-to indent)))
  (insert block-comment-start))


(defun maplev--comment-line-indentation (&optional n)
  "Return the indentation of a Maple comment line, nil if not a comment line.
Optionally move N lines forward before testing.  Point is not affected."
  (save-excursion
    (forward-line (or n 0))
    (and (looking-at "^[ \t]*#") (current-indentation))))

(defun maplev-untab ()
  "Delete backwards to previous tab stop."
  (interactive "*")
  (backward-delete-char-untabify
   (let ((ind (% (current-column) maplev-indent-level)))
     (and (= ind 0) (setq ind maplev-indent-level))
     (if (> ind (current-column))
         (current-column)
       ind))))

;;}}}
;;{{{ Interactive functions

;; maplev-beginning-of-proc and maplev-end-of-proc are the low-level
;; functions that are doing all the work.
;; maplev-proc-beginning and maplev-proc-end are smarter high-level
;; functions that jump over nested procedures according to the value
;; of LEVEL.
;; Would it make sense to replace the keybindings for
;; maplev-beginning-of-proc and maplev-end-of-proc by keybindings
;; for maplev-proc-beginning and maplev-proc-end?

(defun maplev-beginning-of-proc (&optional top move n)
  "Character position of beginning of defun before point.
If optional arg TOP is non-nil search for beginning of top level defun.
If optional arg MOVE is non-nil move point \(t when called interactively\).
With argument N, search the Nth beginning of a defun before point.
Negative argument -N means beginning of defun after point.
Return point \(or nil if search failed\)."
  (interactive (list nil t (prefix-numeric-value current-prefix-arg)))
  (let ((regexp (if top maplev--top-defun-begin-re maplev--defun-re))
        pos case-fold-search)
    (setq n (or n 1))
    (save-excursion
      (if (or (< n 0)
              ;; If point possibly inside what we want to match, then
              ;; go forward to end of line. This is not always enough:
              ;; If the procedure name and the ``proc'' are on
              ;; separate lines and the point is between the two, it
              ;; will not find the beginning of this function.
              (not (looking-at regexp)))
          (end-of-line))
      (if (maplev--re-search-backward regexp nil t n)
          (setq pos (match-beginning 0))))
    (if (and move pos) (goto-char pos))
    pos))

(defun maplev-end-of-proc (&optional top move n)
  "Character position of end of defun before point.
If optional arg TOP is non-nil search for end of top level defun.
If optional arg MOVE is non-nil move point \(t when called interactively\).
With argument N, search the Nth end of a defun before point.
Negative argument -N means end of defun after point.
Return point \(or nil if search failed\)."
  ;; This function looks for one of two conditions:
  ;; (1) the end statement is flush left
  ;; (2) the end statement is on the same line as the proc statement
  ;;     that begins a procedure.
  (interactive (list nil t (prefix-numeric-value current-prefix-arg)))
  (let ((regexp (if top maplev--top-defun-end-re maplev--defun-end-re))
        pos)
    (setq n (or n 1))
    (save-excursion
      (if (or (< n 0)
              ;; If point possibly inside what we want to match,
              ;; then go backwards to beginning of line (not always enough).
              (not (or (bobp) (string-match "[:;]" (string (char-before))))))
          (beginning-of-line))
      (if (maplev--re-search-forward regexp nil t n)
          (setq pos (match-end 0))))
    (if (and move pos) (goto-char pos))
    pos))

(defun maplev-scan-proc (from count depth)
  "Scan maple procedures. FROM, COUNT, and DEPTH are like in `scan-lists'.
Returns the character number of the position thus found.
If no such position is found, it returns a list \(POSITION COUNT DEPTH\)
which corresponds to the last successful step before the search failed.
Return value is nil if there was not a single successful step."
  (let ((dir (if (> count 0) 1 -1))
        beg end pos err)
    (save-excursion
      (goto-char from)
      (while (progn (setq beg (maplev-beginning-of-proc nil nil (- dir))
                          end (maplev-end-of-proc nil nil dir))
                    (cond ((or (and beg (not end))
                               (and beg end (> (* dir (- end beg)) 0)))
                           (setq depth (+ depth dir))
                           (goto-char beg))
                          (end
                           (setq depth (- depth dir))
                           (goto-char end))
                          ((setq err t)))
                    (if (= 0 depth)
                        (setq count (- count dir)))
                    (if (not err) (setq pos (list (point) count depth)))
                    (and (not err) (/= 0 count))))
      (if (not err)
          (point)
        pos))))

(defun maplev-proc-beginning (&optional level move)
  "Character position of beginning of defun before point.
LEVEL defaults to 1. If LEVEL <= 0 find top level procedure.
If optional arg MOVE is non-nil move point (t when called interactively).
Return point \(or nil if search failed\)."
  (interactive (list (prefix-numeric-value current-prefix-arg) t))
  (maplev-proc-bounds -1 level move))

(defun maplev-proc-end (&optional level move)
  "Character position of end of defun at point.
LEVEL defaults to 1. If LEVEL <= 0 find top level procedure.
If optional arg MOVE is non-nil move point \(t when called interactively\).
Return point \(or nil if search failed\)."
  (interactive (list (prefix-numeric-value current-prefix-arg) t))
  (maplev-proc-bounds 1 level move))

(defun maplev-proc-bounds (dir &optional level move)
  "Character position of boundary of defun at point.
If DIR equals -1 search beginning, if DIR equals 1 search end.
LEVEL defaults to 1. If LEVEL <= 0 find top level procedure.
If optional arg MOVE is non-nil move point.
Return point \(or nil if search failed\)."
  (if (not level) (setq level 1))
  (let ((pnt (point)) beg)
    (if (< level 0)
        ;; Old algorithm.
        (if (< dir 0)
            (setq beg (maplev-beginning-of-proc t))
          (setq beg (maplev-end-of-proc t)))
      ;; Test the new algorithm.
      (if (= level 0) (setq level 10000))
      (setq beg (maplev-scan-proc pnt dir level))
      (when (and (listp beg) (nth 2 beg))
        ;; We try to calculate the top level
        (setq level (- level (nth 2 beg)))
        (if (> level 0)
            (setq beg (maplev-scan-proc pnt dir level))))
      (if (not (number-or-marker-p beg))
          (error "Current defun ill-defined"))
      (if (< dir 0)
          ;; If the current procedure is assigned to a variable,
          ;; the assignment is included in the `current procedure'.
          (save-excursion
            (goto-char beg)
            (if (re-search-backward (concat maplev--assignment-re "\\=") nil t)
                (setq beg (match-beginning 0))))))
    (if move (goto-char beg))
    beg))

(defun maplev-current-proc (&optional level)
  "The current Maple procedure.
LEVEL defaults to 1. If LEVEL <= 0 find top level procedure.
Return list with buffer positions of begin and end."
  ;; This is under development:
  ;; Do we always find the top level with maplev-scan-proc and
  ;; maplev-current-proc? So for testing this, we use the old
  ;; algorithm if level=-1 and the new algorithm if level=0.
  (if (not level) (setq level 1))
  (let ((pnt (point)) beg end)
    (if (< level 0)
        ;; Old algorithm.
        (setq beg (maplev-beginning-of-proc t)
              end (maplev-end-of-proc t))
      ;; Test the new algorithm.
      (if (= level 0) (setq level 10000))
      (setq beg (maplev-scan-proc pnt -1 level)
            end (maplev-scan-proc pnt 1 level))
      (when (and (listp beg) (listp end)
                 (nth 2 beg) (nth 2 end))
        ;; We try to calculate the top level
        (setq level (- level (min (nth 2 beg) (nth 2 end))))
        (if (> level 0)
            (setq beg (maplev-scan-proc pnt -1 level)
                  end (maplev-scan-proc pnt 1 level))))
      (if (or (not (number-or-marker-p beg))
              (not (number-or-marker-p end)))
          (error "Current defun ill-defined"))
      ;; If the current procedure is assigned to a variable,
      ;; the assignment is included in the `current procedure'.
      (save-excursion
        (goto-char beg)
        (if (re-search-backward (concat maplev--assignment-re "\\=") nil t)
            (setq beg (match-beginning 0)))))
    (list beg end)))

(defun maplev-mark-proc (&optional level)
  "Mark the current Maple procedure.
This puts the mark at the end, and point at the beginning.
LEVEL defaults to 1. If LEVEL <= 0 find top level procedure."
  (interactive "p")
  (let ((reg (maplev-current-proc level)))
    (push-mark (point) t)
    (goto-char (car reg))
    (set-mark (nth 1 reg))))

(defun maplev-narrow-to-proc (&optional level)
  "Make text outside current procedure invisible.
LEVEL defaults to 1. If LEVEL <= 0 find top level procedure."
  (interactive "p")
  (let ((reg (maplev-current-proc level)))
    (narrow-to-region (car reg) (nth 1 reg))))

;;; stuff used by mint

(defun maplev--re-search-forward (regexp &optional bound noerror count)
  "Search forward from point for regular expression REGEXP.
This function is like re-search-forward, but comments are ignored.
Optional arguments BOUND, NOERROR, and COUNT have the same meaning
like in re-search-forward."
  ;; This approach gets confused by a comment inside the match
  ;; (e.g., when REGEXP can match more than one line).
  ;; Therefore it's better to break complex REGEXP's apart
  ;; and handle the items seperately.
  (let (found case-fold-search)
    (setq found (re-search-forward regexp bound noerror count))
    (while (and (nth 4 (parse-partial-sexp (maplev-safe-position) (point)))
                (setq found (re-search-forward regexp bound noerror count))))
    found))

(defun maplev--re-search-backward (regexp &optional bound noerror count)
  "Search backward from point for regular expression REGEXP.
This function is like re-search-backward, but comments are ignored.
Optional arguments BOUND, NOERROR, and COUNT have the same meaning
like in re-search-backward."
  ;; See maplev--re-search-forward.
  (let (found case-fold-search)
    (setq found (re-search-backward regexp bound noerror count))
    (while (and (nth 4 (parse-partial-sexp (maplev-safe-position) (point)))
                (setq found (re-search-backward regexp bound noerror count))))
    found))

(defun maplev-safe-position (&optional to)
  "Search for safe buffer position before point \(a position not in a comment\).
Optional arg TO initializes the search. It defaults to point"
  (unless to (setq to (point)))
  (save-excursion
    (save-match-data
      (goto-char to)
      (while (and (= 0 (forward-line -1))
                  (looking-at "#")))
      (point))))

(defun maplev--scan-lists (count &optional from)
  "Scan COUNT lists. Optional arg FROM defaults to position of point.
Returns the character number of the position thus found."
  (if (not from) (setq from (point)))
  (let ((parse-sexp-ignore-comments t))
    (scan-lists from count 0)))

(defun maplev-delete-whitespace (&optional back)
  "Delete whitespace characters plus empty comments at point.
If optional arg BACK is non-nil, delete whitespace characters before point."
  ;; It would be nice to have a function looking-at-backward,
  ;; but there is nothing like that. (Guess why :-)
  (if back (let ((pos (point)))
             (skip-chars-backward " \t\n")
             (delete-region pos (point)))
    (save-match-data
      ;; Is this regexp too aggressive?
      (if (looking-at "\\([ \t\n]\\|\\(#[ \t]*$\\)\\)*")
          (delete-region (match-beginning 0) (match-end 0))))))

(defun maplev--statement-terminator ()
  "Buffer position immediately following next non-comment semicolon or
colon that is not part of a double colon."
  (save-excursion
    (maplev--re-search-forward "[^:]\\(;\\|:[^:]\\)" nil t)
    (+ 1 (match-beginning 1))))

(defun maplev--goto-declaration (keyword)
  "Move point to the start of the KEYWORD declaration in a Maple procedure.
Return nil if there no such statement.  Point must be to the right of
the closing parenthesis in the formal parameter list."
  (let ((bound (save-excursion
                 (maplev--re-search-forward maplev--defun-re
                                            (maplev-end-of-proc) 'move)
                 (point))))
    (if (save-excursion
          (maplev--re-search-forward
           (concat "\\<" keyword "\\>") bound t))
        (goto-char (match-beginning 0)))))

(defun maplev-add-declaration (keyword var)
  "To the current procedure's KEYWORD declaration add VAR.
If necessary, add a KEYWORD statement.  Point must be after the closing
parenthesis of the procedure's argument list."
  (save-excursion
    (if (maplev--goto-declaration keyword)
        (progn
          (goto-char (maplev--statement-terminator))
          (backward-char)
          (insert "," (make-string maplev-variable-spacing ?\ ) var))
      (let (stay)
        ;; Declarations are ordered: local, global, export
        (if (maplev--goto-declaration "local")
            (setq stay (goto-char (maplev--statement-terminator))))
        (if (maplev--goto-declaration "global")
            (setq stay (goto-char (maplev--statement-terminator))))
        ;; Position point and text in preparation for inserting a
        ;; declaration statement.
        (if (not (looking-at "[ \t]*\\(#.*\\)?$")) ; More code on line?
            (just-one-space)         ; Then insert declaration inbetween.
          (forward-line)             ; Else move to the next code line.
          (unless stay                     ; Keep moving if we not already
            (while (looking-at "[ \t]*#")  ; have a declaration.
              (forward-line)))))
      ;; Insert the declaration statement KEYWORD VAR ; at point.
      ;; If point is at beginning of line, insert a newline at end.
      ;; NOTE: It might be better to look whether there is any following text.
      (let ((new-line (bolp)))
        (insert keyword " " var "; ")
        (when new-line
          (newline)
          (forward-line -1)))
      (maplev-indent-line))))

(defun maplev-add-local-variable (var &optional level)
  "Add VAR to the current procedure's local statement.
Interactively, VAR defaults to identifier point is on.
LEVEL is the prefix arg."
  (interactive (list (maplev-ident-around-point-interactive
                      "Local variable")
                     (prefix-numeric-value current-prefix-arg)))
  (maplev-add-variable "local" var level))

(defun maplev-add-global-variable (var &optional level)
  "Add VAR to the current procedure's local statement.
Interactively, VAR defaults to identifier point is on.
LEVEL is the prefix arg."
  (interactive (list (maplev-ident-around-point-interactive
                      "Global variable")
                     (prefix-numeric-value current-prefix-arg)))
  (maplev-add-variable "global" var level))

(defun maplev-add-export-variable (var &optional level)
  "Add VAR to the current module's export statement.
Interactively, VAR defaults to identifier point is on.
LEVEL is the prefix arg."
  (interactive (list (maplev-ident-around-point-interactive
                      "Exported variable")
                     (prefix-numeric-value current-prefix-arg)))
  (maplev-add-variable "export" var level))

(defun maplev-add-variable (keyword var &optional level)
  "To the current procedure's KEYWORD declaration add VAR."
  (save-excursion
    (maplev-proc-beginning level t)
    (goto-char (maplev--scan-lists 1))
    (maplev-add-declaration keyword var)))

(defun maplev-delete-declaration (keyword vars &optional leave-one)
  "From the KEYWORD declaration delete occurrences of VARS.
VARS must be eiter a string or a list of strings. If optional
argument LEAVE-ONE is non-nil, then one occurrence of VARS is left.
The entire statement is deleted if it is left with no variables."
  (save-excursion
    (when (maplev--goto-declaration keyword)
      (maplev-delete-vars (point) (maplev--statement-terminator)
                          vars leave-one)
      ;; remove entire KEYWORD statement, if empty
      (let (case-fold-search)
        (when (looking-at (concat keyword "[ \t\n]*[;:]\\([ \t#]*$\\)?"))
          (delete-region (match-beginning 0) (match-end 0))
          (maplev-delete-whitespace t))))))

(defun maplev-delete-vars (start end vars &optional leave-one)
  "In region between START and END delete occurrences of VARS.
VARS must be eiter a string or a list of strings. If optional
argument LEAVE-ONE is non-nil, then one occurrence of VARS is left."
  (let (case-fold-search lo)
    (save-excursion
      (save-restriction
        (narrow-to-region start end)
        (if (stringp vars) (setq vars (list vars)))
        (while vars
          (setq lo leave-one)
          (goto-char (point-min))
          (while (maplev--re-search-forward
                  (concat "\\<" (car vars) "\\>")
                  nil t)
            (if lo
                (setq lo nil)
              (delete-region (match-beginning 0) (match-end 0))
              (maplev-delete-whitespace)
              (when (or (maplev--re-search-forward  "," nil t)
                        (maplev--re-search-backward "," nil t))
                (delete-region (match-beginning 0) (match-end 0))
                (maplev-delete-whitespace))))
          (setq vars (cdr vars)))))))

;;}}}
;;{{{ Templates

(defun maplev-template (function name args description)
  "Insert a template for a Maple FUNCTION \(\"proc\" or \"module\"\).
Use NAME, ARGUMENTS, and DESCRIPTION. Move point to body of FUNCTION.

If NAME equals \"\" then the function is anonymous,
no assignment operator is inserted and the closing
end statement is not terminated with a colon.

ARGS are inserted as formal arguments in the function statement.

If `maplev-insert-copyright-flag' is non-nil, then insert a copyright
as an option statement.  Confirmation is required for an anonymous
function.

Unless DESCRIPTION equals \"\" it is inserted as a description statement.

If `maplev-comment-end-flag' is non-nil, and the function is not
anonymous, then NAME is inserted as a comment following the closing
end statement.  Point is moved to the start of the function body."
  (let ((fname (not (string= name ""))))
    ;; Insert assignment if function has a name
    (when fname
      (setq name (maplev--string-to-name name))
      (insert name " := "))
    (insert function
            (make-string maplev-variable-spacing ?\ )
            "(" args ")")            ; Insert function, with formal args

    ;; Copyright notice
    (when (and maplev-insert-copyright-flag
               (or fname (y-or-n-p "Insert copyright? ")))
      (insert "\noption `Copyright (C) "
              (format-time-string "%Y" (current-time))
              " by " maplev-copyright-owner ". All rights reserved.`;"))

    ;; description
    (unless (string= description "")
      (insert "\ndescription " maplev-description-quote-char
              description maplev-description-quote-char ";"))

    (insert "\n\nend")
    (when fname
      (insert ":")
      (if maplev-comment-end-flag
          (insert maplev-template-end-comment name)))
    (forward-line -1)             ; Move point to start of body
    ;; bug in maplev-current-proc:
    ;; it doesn't work yet with anonymous procedures
    (when fname (maplev-indent-procedure))))

(defun maplev-template-proc (name args description)
  "Insert a template for a Maple procedure and move point to its body.
Prompt for the NAME, ARGS, and DESCRIPTION. See `maplev-template'."
  (interactive "*sName (return for anonymous) \nsArguments: \nsDescription: ")
  (maplev-template "proc" name args description))

(defun maplev-template-module (name args description)
  "Insert a template for a Maple module and move point to its body.
Prompt for the NAME, ARGUMENTS, and DESCRIPTION. See `maplev-template'."
  (interactive "*sName (return for anonymous) \nsArguments: \nsDescription: ")
  (maplev-template "module" name args description))

;;}}}

;;{{{ Font lock

(defvar maplev-preprocessor-face   'maplev-preprocessor-face
  "*Face name for Maple preprocessor directives.")

(defface maplev-preprocessor-face
  '((((class grayscale) (background light)) (:foreground "LightGray" :bold t))
    (((class grayscale) (background dark))  (:foreground "DimGray"   :bold t))
    (((class color)     (background light)) (:foreground "dark orange"))
    (((class color)     (background dark))  (:foreground "orange"))
    (t (:bold t)))
  "Font lock mode face used for Maple preprocessor directives."
  :group 'maplev-faces)


(defconst maplev--reserved-words-alist
  '((3 . ("and"  "by"   "do"        "done"   "elif"
          "else" "end"  "fi"        "for"    "from"
          "if"   "in"   "intersect" "local"  "minus"
          "mod"  "not"  "od"        "option" "options"
          "or"   "proc" "quit"      "read"   "save"
          "stop" "then" "to"        "union"  "while"
          "description" "local" "global"))
    (4 . ("and"  "by"   "do"        "done"   "elif"
          "else" "end"  "fi"        "for"    "from"
          "if"   "in"   "intersect" "local"  "minus"
          "mod"  "not"  "od"        "option" "options"
          "or"   "proc" "quit"      "read"   "save"
          "stop" "then" "to"        "union"  "while"
          "description" "local" "global"))
    (5 . ("and"  "by"   "do"        "done"   "elif"
          "else" "end"  "fi"        "for"    "from"
          "if"   "in"   "intersect" "local"  "minus"
          "mod"  "not"  "od"        "option" "options"
          "or"   "proc" "quit"      "read"   "save"
          "stop" "then" "to"        "union"  "while"
          "description" "local" "global"))
    (6 . ("and"     "break" "by"     "catch" "description" "do"        "done"
          "elif"    "else"  "end"    "error" "export"      "fi"        "finally"
          "for"     "from"  "global" "if"    "in"          "intersect" "local"
          "minus"   "mod"   "module" "next"  "not"         "od"        "option"
          "options" "or"    "proc"   "quit"  "read"        "return"    "save"
          "stop"    "then"  "to"     "try"   "union"       "use"       "while"))
    (7 . ("and"     "assuming"  "break"  "by"     "catch"
          "description" "do"    "done"   "elif"   "else"
          "end"     "error"     "export" "fi"     "finally"
          "for"     "from"      "global" "if"     "implies"
          "in"      "intersect" "local"  "minus"  "mod"
          "module"  "next"      "not"    "od"     "option"
          "options" "or"        "proc"   "quit"   "read"
          "return"  "save"      "stop"   "subset" "then"
          "to"     "try"        "union"  "use"    "while"
          "xor"))
    (8 . ("and"     "assuming"  "break"  "by"     "catch"
          "description" "do"    "done"   "elif"   "else"
          "end"     "error"     "export" "fi"     "finally"
          "for"     "from"      "global" "if"     "implies"
          "in"      "intersect" "local"  "minus"  "mod"
          "module"  "next"      "not"    "od"     "option"
          "options" "or"        "proc"   "quit"   "read"
          "return"  "save"      "stop"   "subset" "then"
          "to"     "try"        "union"  "use"    "while"
          "xor"))
    (9 . ("and"     "assuming"  "break"  "by"     "catch"
          "description" "do"    "done"   "elif"   "else"
          "end"     "error"     "export" "fi"     "finally"
          "for"     "from"      "global" "if"     "implies"
          "in"      "intersect" "local"  "minus"  "mod"
          "module"  "next"      "not"    "od"     "option"
          "options" "or"        "proc"   "quit"   "read"
          "return"  "save"      "stop"   "subset" "then"
          "to"     "try"        "union"  "use"    "while"
          "xor"))
    )
  "Alist of Maple reserved words.  The key is the major release.")

(defconst maplev--special-words-re
  (eval-when-compile
    (maplev--list-to-word-re
     (list "args" "nargs" "procname" "RootOf" "Float" "thismodule")))
  "Regex of special words in Maple.")

(defconst maplev--initial-variables-re
  (eval-when-compile
    (maplev--list-to-word-re
     (list "Catalan" "true" "false" "FAIL" "infinity" "Pi" "gamma"
           "integrate" "libname" "NULL" "Order" "printlevel" "lasterror"
           "`mod`" "Digits" "constants" "undefined" "I"
           "UseHardwareFloats"
           "Testzero" "Normalizer" "NumericEventHandlers"
           "Rounding" "`index/newtable`")))
  "Regexp of global, environmental variables and constants.")

(defconst maplev--preprocessor-directives-re
  (eval-when-compile
    (concat "^\\$\\(" 
            (regexp-opt (list "include" "define" "undef" "ifdef" "ifndef" "else" "endif" ))
            "\\)"))
  "Regex of preprocessor directives.")

;; Currently the backquoted builtin functions are font-locked as
;; quoted names rather than as builtin functions.  Fixing this
;; requires pulling them out.

(defconst maplev--builtin-types-alist
  '((8. ("`::`" "`..`" "`!`"
         "algebraic" "anyfunc" "anything" "atomic"
         "boolean"
         "complex" "constant" "cx_infinity" "cx_zero"
         "embedded_axis" "embedded_imaginary" "embedded_real"
         "equation" "even" "extended_numeric" "extended_rational"
         "finite" "float" "fraction" "function"
         "identical" "imaginary" "indexable" "indexed" "integer"
         "list" "literal"
         "module" "moduledefinition"
         "name" "neg_infinity" "negative" "negint" "negzero"
         "nonnegative" "nonnegint" "nonposint" "nonpositive"
         "nonreal" "numeric" "odd"
         "polynom" "pos_infinity" "posint" "positive" "poszero"
         "procedure" "protected"
         "radical" "range" "rational" "ratpoly" "real_infinity"
         "realcons" "relation"
         "sequential" "set" "sfloat" "specfunc" "string" "symbol"
         "tabular" "uneval" "zppoly")))
  "Alist of builtin Maple types.  Currently not used.")

(defconst maplev--builtin-functions-alist
  '((3 . ("`$`"   "ERROR"   "Im"   "RETURN"  "Re"   "SearchText"
          "abs" "addressof" "alias"  "anames" "appendto"
          "array" "assemble" "assigned" "callback" "cat"
          "coeff" "coeffs" "convert" "debugopts" "degree"
          "diff" "disassemble" "divide" "entries" "eval"
          "evalb" "evalf" "`evalf/hypergeom`" "evalhf" "evaln"
          "expand" "frontend" "gc" "genpoly" "goto"
          "has" "hastype" "icontent" "`if`" "igcd"
          "ilog10" "indets" "indices" "`int/series`" "intersect"
          "iquo" "irem" "isqrt" "lcoeff" "ldegree"
          "length" "lexorder" "lprint" "macro" "map"
          "max" "maxnorm" "member" "min" "minus"
          "modp" "modp1" "mods" "nops" "normal"
          "numboccur" "numer" "op" "order" "parse"
          "pointto" "print" "printf" "protect" "readlib"
          "readline" "searchtext" "select" "seq" "series"
          "sign" "sort" "sscanf" "ssystem" "subs"
          "subsop" "substring" "system" "table" "taylor"
          "tcoeff" "time" "traperror" "trunc" "type"
          "unames" "`union`" "unprotect" "userinfo" "words"
          "writeto" ))
    (4 . ("`$`" "`*`" "`+`" "ASSERT" "DEBUG"
          "ERROR" "Im" "MorrBrilCull" "RETURN" "Re"
          "SearchText" "abs" "add" "addressof" "alias"
          "anames" "appendto" "array" "assemble" "assigned"
          "attributes" "callback" "cat" "coeff" "coeffs"
          "convert" "debugopts" "degree" "denom" "diff"
          "disassemble" "divide" "entries" "eval" "evalb"
          "evalf" "`evalf/hypergeom`" "evalhf" "evaln" "expand"
          "frontend" "gc" "genpoly" "getuserinterface" "goto"
          "has" "hastype" "icontent" "`if`" "igcd"
          "ilog10" "indets" "indices" "inner" "`int/series`"
          "intersect" "iolib" "iquo" "irem" "isqrt"
          "`kernel/transpose`" "kernelopts" "lcoeff" "ldegree" "length"
          "lexorder" "lprint" "macro" "map" "map2"
          "max" "maxnorm" "member" "min" "minus"
          "modp" "modp1" "mods" "mul" "nops"
          "normal" "numboccur" "numer" "op" "order"
          "parse" "pointto" "print" "readlib" "searchtext"
          "select" "seq" "series" "setattribute" "setuserinterface"
          "sign" "sort" "ssystem" "subs" "subsop"
          "substring" "system" "table" "taylor" "tcoeff"
          "time" "traperror" "trunc" "type" "typematch"
          "unames" "`union`" "userinfo" "writeto"))
    (5 . ("`$`" "`*`" "`**`" "`+`" "`<`"
          "`<=`" "`<>`" "`=`" "`>`" "`>=`"
          "`^`" "ASSERT" "DEBUG" "ERROR" "Im"
          "MorrBrilCull" "RETURN" "Re" "SearchText" "abs"
          "add" "addressof" "alias" "anames" "appendto"
          "array" "assemble" "assigned" "attributes" "call"
          "callback" "cat" "coeff" "coeffs" "convert"
          "crinterp" "debugopts" "define" "degree" "denom"
          "diff" "disassemble" "divide" "entries" "eval"
          "evalb" "evalf" "`evalf/hypergeom/kernel`" "evalhf" "evaln"
          "expand" "frontend" "gc" "genpoly" "getuserinterface"
          "goto" "has" "hastype" "hfarray" "icontent"
          "`if`" "igcd" "ilog10" "indets" "indices"
          "inner" "`int/series`" "intersect" "iolib" "iquo"
          "irem" "isqrt" "`kernel/transpose`" "kernelopts" "lcoeff"
          "ldegree" "length" "lexorder" "lprint" "macro"
          "map" "map2" "max" "maxnorm" "member"
          "min" "minus" "modp" "modp1" "mods"
          "mul" "nops" "normal" "numboccur" "numer"
          "op" "order" "parse" "pointto" "print"
          "readlib" "searchtext" "select" "seq" "series"
          "setattribute" "setuserinterface" "sign" "sort" "ssystem"
          "subs" "subsop" "substring" "system" "table"
          "taylor" "tcoeff" "time" "timelimit" "traperror"
          "trunc" "type" "typematch" "unames" "`union`"
          "userinfo" "writeto"))
    (6 . ("`^`" "`||`" "`$`" "`*`" "`**`" "`+`"
          "`<`" "`<=`" "`<>`" "`=`" "`>`" "`>=`"
          "ASSERT" "Array" "ArrayOptions" "CopySign"
          "DEBUG" "Default0" "DefaultOverflow" "DefaultUnderflow"
          "ERROR" "EqualEntries" "EqualStructure" "FromInert"
          "Im" "MPFloat" "MorrBrilCull" "NextAfter"
          "NumericClass" "NumericEvent" "NumericEventHandler" "NumericStatus"
          "OrderedNE" "RETURN" "Re"
          "SFloatExponent" "SFloatMantissa" "Scale10" "Scale2" "SearchText"
          "TRACE" "ToInert" "Unordered"
          "abs" "add" "addressof" "alias" "anames" "and" "appendto" "array"
          "assemble" "assigned" "attributes" "bind"
          "call_external" "callback" "cat" "coeff" "coeffs" "conjugate" "convert"
          "crinterp" "debugopts" "define_external" "degree" "denom" "diff" "disassemble"
          "inner" "divide" "done" "entries" "eval" "evalb"
          "evalf" "evalgf1" "evalhf" "evaln" "expand" "exports"
          "frem" "frontend" "gc" "genpoly" "goto"
          "has" "hastype" "hfarray" "`evalf/hypergeom/kernel`"
          "icontent" "if" "igcd" "ilog10" "ilog2" "indets" "indices"
          "intersect" "`int/series`" "iolib" "iquo" "irem" "isqrt"
          "kernelopts" "lcoeff" "ldegree" "length" "lexorder" "lhs" "lprint"
          "macro" "map" "map2" "max" "maxnorm" "member" "min" "minus"
          "modp" "modp1" "modp2" "mods" "mul" "mvMultiply"
          "negate" "nops" "normal" "not" "numboccur" "numer"
          "op" "or" "order" "parse" "pointto" "print" "quit"
          "readlib" "remove" "rhs" "rtable" "rtableInfo" "rtable_indfns"
          "rtable_is_zero" "rtable_normalize_index" "rtable_num_dims"
          "rtable_num_elems" "rtable_options" "rtable_scanblock"
          "rtable_sort_indices" "searchtext" "select" "selectremove"
          "seq" "series" "setattribute" "sign" "sort" "ssystem" "stop"
          "streamcall" "subs" "subsop" "substring" "system"
          "table" "taylor" "tcoeff" "time" "timelimit"
          "`kernel/transpose`" "traperror" "trunc" "type" "typematch"
          "unames" "unbind" "union" "userinfo" "writeto"))
    (7 . ("`$`" "`*`" "`**`" "`+`" "`<`" "`<=`"
          "`<>`" "`=`" "`>`" "`>=`" "`^`" "`||`"
          "ASSERT" "Array" "ArrayOptions" "CopySign"
          "DEBUG" "Default0" "DefaultOverflow" "DefaultUnderflow"
          "ERROR" "EqualEntries" "EqualStructure" "FromInert" "Im"
          "MPFloat" "MorrBrilCull" "NextAfter" "NumericClass" "NumericEvent"
          "NumericEventHandler" "NumericStatus" "OrderedNE" "RETURN" "Re"
          "SFloatExponent" "SFloatMantissa" "Scale10" "Scale2" "SearchText"
          "TRACE" "ToInert" "Unordered" "_treeMatch" "_unify"
          "_xml" "`evalf/hypergeom/kernel`" "`int/series`" "`kernel/transpose`"
          "abs" "add" "addressof" "alias" "anames" "and"
          "appendto" "array" "assemble" "assigned" "attributes"
          "bind" "call_external" "callback" "cat" "coeff"
          "coeffs" "conjugate" "convert" "crinterp" "debugopts"
          "define_external" "degree" "denom" "diff" "disassemble"
          "divide" "dlclose" "done" "entries" "eval"
          "evalb" "evalf" "evalgf1" "evalhf" "evaln"
          "expand" "exports" "factorial" "frem" "frontend"
          "gc" "genpoly" "goto" "has" "hastype"
          "hfarray" "icontent" "if" "igcd" "ilog10"
          "ilog2" "implies" "indets" "indices" "inner"
          "intersect" "iolib" "iquo" "irem" "isqrt"
          "kernelopts" "lcoeff" "ldegree" "length" "lexorder"
          "lhs" "lprint" "macro" "map" "map2"
          "max" "maxnorm" "member" "min" "minus"
          "modp" "modp1" "modp2" "mods" "mul"
          "mvMultiply" "negate" "nops" "normal" "not"
          "numboccur" "numer" "op" "or" "order"
          "parse" "pointto" "print" "quit" "readlib"
          "remove" "rhs" "rtable" "rtableInfo" "rtable_indfns"
          "rtable_is_zero" "rtable_normalize_index" "rtable_num_dims"
          "rtable_num_elems" "rtable_options" "rtable_scanblock"
          "rtable_sort_indices" "searchtext" "select" "selectremove"
          "seq" "series" "setattribute" "sign" "sort" "ssystem"
          "stop" "streamcall" "subs" "subset" "subsop" "substring"
          "system" "table" "taylor" "tcoeff" "time" "timelimit"
          "traperror" "trunc" "type" "typematch" "unames" "unbind"
          "union" "userinfo" "writeto" "xor" ))
    (8 . ("`$`" "`*`" "`**`" "`+`" "`<`" "`<=`"
          "`<>`" "`=`" "`>`" "`>=`" "`^`" "`||`"
          "ASSERT" "Array" "ArrayOptions" "CopySign"
          "DEBUG" "Default0" "DefaultOverflow" "DefaultUnderflow"
          "ERROR" "EqualEntries" "EqualStructure" "FromInert" "Im"
          "MPFloat" "MorrBrilCull" "NextAfter" "NumericClass" "NumericEvent"
          "NumericEventHandler" "NumericStatus" "OrderedNE" "RETURN" "Re"
          "SFloatExponent" "SFloatMantissa" "Scale10" "Scale2" "SearchText"
          "TRACE" "ToInert" "Unordered" "_jvm" "_maplet" "_treeMatch" "_unify"
          "_xml" "`evalf/hypergeom/kernel`" "`int/series`" "`kernel/transpose`"
          "abs" "add" "addressof" "alias" "anames" "and" "andmap"
          "appendto" "array" "assemble" "assigned" "attributes"
          "bind" "call_external" "callback" "cat" "coeff"
          "coeffs" "conjugate" "convert" "crinterp" "debugopts"
          "define_external" "degree" "denom" "diff" "disassemble"
          "divide" "dlclose" "done" "entries" "eval"
          "evalb" "evalf" "evalgf1" "evalhf" "evaln"
          "expand" "exports" "factorial" "frem" "frontend"
          "gc" "genpoly" "goto" "has" "hastype"
          "hfarray" "icontent" "if" "igcd" "ilog10"
          "ilog2" "implies" "indets" "indices" "inner"
          "intersect" "iolib" "iquo" "irem" "isqrt"
          "kernelopts" "lcoeff" "ldegree" "length" "lexorder"
          "lhs" "lprint" "macro" "map" "map2"
          "max" "maxnorm" "member" "min" "minus"
          "modp" "modp1" "modp2" "mods" "mul"
          "mvMultiply" "negate" "nops" "normal" "not"
          "numboccur" "numer" "op" "or" "order" "ormap"
          "parse" "pointto" "print" "quit" "readlib"
          "remove" "rhs" "rtable" "rtableInfo" "rtable_indfns"
          "rtable_is_zero" "rtable_normalize_index" "rtable_num_dims"
          "rtable_num_elems" "rtable_options" "rtable_scanblock"
          "rtable_sort_indices" "searchtext" "select" "selectremove"
          "seq" "series" "setattribute" "sign" "sort" "ssystem"
          "stop" "streamcall" "subs" "subset" "subsop" "substring"
          "system" "table" "taylor" "tcoeff" "time" "timelimit"
          "traperror" "trunc" "type" "typematch" "unames" "unbind"
          "union" "userinfo" "writeto" "xor" ))
    (9 . ("`$`" "`*`" "`**`" "`+`" "`..`" "`<`" "`<=`" "`<>`" "`=`" "`>`" "`>=`" 
         "ASSERT" "Array" "ArrayOptions" "CopySign" 
         "DEBUG" "Default0" "DefaultOverflow" "DefaultUnderflow" 
         "ERROR" "EqualEntries" "EqualStructure" "FromInert" 
         "Im" "MPFloat" "MorrBrilCull" "NextAfter" "Normalizer" 
         "NumericClass" "NumericEvent" "NumericEventHandler" "NumericStatus" 
         "OrderedNE" "RETURN" "Re" "SFloatExponent" "SFloatMantissa" 
         "Scale10" "Scale2" "SearchText" "TRACE" "ToInert" 
         "Unordered" "UpdateSource" "^" 
         "_jvm" "_maplet" "_treeMatch" "_unify" "_xml" 
         "abs" "add" "addressof" "alias" "anames" "and" "andmap" 
         "appendto" "array" "assemble" "assigned" "attributes" 
         "bind" "call_external" "callback" "cat" "coeff" "coeffs" 
         "conjugate" "convert" "crinterp" "debugopts" "define_external" 
         "degree" "denom" "diff" "disassemble" "divide" "dlclose" "done" 
         "entries" "eval" "evalb" "evalf" "`evalf/hypergeom/kernel`" 
         "evalgf1" "evalhf" "evaln" "expand" "exports" "factorial" "frem" 
         "frontend" "gc" "genpoly" "gmp_isprime" "goto" "has" "hastype" "hfarray" 
         "icontent" "if" "igcd" "ilog10" "ilog2" "implies" "indets" 
         "indices" "inner" "`int/series`" "intersect" "iolib" "iquo" "irem" 
         "is_gmp" "isqrt" "`kernel/transpose`" "lcoeff" "ldegree" "length" 
         "lexorder" "lhs" "lprint" "macro" "map" "map2" "max" "maxnorm" 
         "member" "min" "minus" "mod" "modp" "modp1" "modp2" "mods" 
         "mul" "mvMultiply" "negate" "nops" "normal" "not" "numboccur" 
         "numer" "op" "or" "order" "ormap" "parse" "piecewise" 
         "pointto" "print" "quit" "readlib" "reduce_opr" "remove" 
         "rhs" "rtable" "rtableInfo" "rtable_eval" "rtable_indfns" 
         "rtable_is_zero" "rtable_normalize_index" "rtable_num_dims" 
         "rtable_num_elems" "rtable_options" "rtable_scanblock" 
         "rtable_sort_indices" "rtable_zip" "searchtext" "select" 
         "selectremove" "seq" "series" "setattribute" "sign" "sort" 
         "ssystem" "stop" "streamcall" "subs" "subset" "subsop" 
         "substring" "system" "table" "taylor" "tcoeff" "time" 
         "timelimit" "traperror" "trunc" "type" "typematch" 
         "unames" "unbind" "union" "userinfo" "writeto" "xor" "||"))
    )
  "Alist of Maple builtin funtions.  The key is the major release.")

(defun maplev--ditto-operators-re ()
  "Return a regexp that matches the ditto operators."
  (regexp-opt
   (if (< (maplev--major-release) 5)
       '("\"" "\"\"" "\"\"\"")
     '("%" "%%" "%%%"))))

(defun maplev-font-lock-keywords-1 ()
  "Compute the minimum decoration `font-lock-keywords' for MapleV mode.
Top level procedures, Maple reserved words, and preprocessor directives
are font locked."
  (list
   (list maplev--top-defun-begin-re '(1 font-lock-function-name-face t))
   (list maplev--preprocessor-directives-re '(0 maplev-preprocessor-face))
   (list (maplev--list-to-word-re
          (cdr (assoc (maplev--major-release)
                      maplev--reserved-words-alist)))
         '(0 font-lock-keyword-face))))


(defun maplev-font-lock-keywords-2 ()
  "Compute the medium decoration `font-lock-keywords' for MapleV mode.
Add special words, initial variables, and the ditto operators to the
minimum decoration keywords."
  (append
   (maplev-font-lock-keywords-1)
   (list
    (list maplev--special-words-re     '(0 font-lock-variable-name-face))
    (list maplev--initial-variables-re '(0 font-lock-reference-face))
    (list (maplev--ditto-operators-re) '(0 font-lock-variable-name-face)))))

(defun maplev-font-lock-keywords-3 ()
  "Compute the maximum decoration `font-lock-keywords' for MapleV mode.
Add builtin functions to the medium decoration keywords."
  (let ((max-specpdl-size 10000)) ; default 600 is too small
    (append (maplev-font-lock-keywords-2)
            (list (list (maplev--list-to-word-re
                         (cdr (assoc (maplev--major-release)
                                     maplev--builtin-functions-alist)))
                        ;; Xemacs doesn't have font-lock-builtin-face
                        '(0 font-lock-variable-name-face))))))
(defun maplev--font-lock-keywords ()
  "Return a list of symbols for font locking MapleV mode buffers."
  '(maplev-font-lock-keywords-3         ; default is maximum decoration
    maplev-font-lock-keywords-1
    maplev-font-lock-keywords-2
    maplev-font-lock-keywords-3))

(defun maplev--font-lock-syntax-alist ()
  "Return the syntax alist appropriate for font lock.
It depends on `maplev--major-release'."
  `((?_ . "w")          ; make `_' a word character
    ,(if (< (maplev--major-release) 5)
         '(?\" . "w")   ; make `"' a word character for R4 and down.
       '(?% . "w"))))   ; make `%' a word character for R5 and up.

(defun maplev--syntax-begin ()
  "Move backwards to start of a Maple procedure.
This is passed to `font-lock-defaults' as the SYNTAX-BEGIN argument."
  (re-search-backward maplev--top-defun-begin-re nil 'move))

(defun maplev-reset-font-lock (&optional decoration)
  "Reset the font lock patterns for MapleV mode.  Fontify the buffer.
The optional argument DECORATION selects the level of font lock.
If nil then `font-lock-maximum-decoration' selects the level."
  (interactive (list (completing-read "Decoration (1-3): "
                                      '(("1") ("2") ("3"))
                                      nil t)))
  (if decoration
      (setq font-lock-maximum-decoration decoration))
  (setq font-lock-defaults `(,(maplev--font-lock-keywords)
                             nil nil
                             ,(maplev--font-lock-syntax-alist)
                             maplev--syntax-begin))
  (font-lock-set-defaults)
  (font-lock-fontify-buffer))

;;}}}
;;{{{ Tags
;; I'm not sure about how tags should work.  Should it run on all
;; Maple files in the directory?  Running it on just one file makes
;; little sense.  The tags could be appended, but then the TAGS file
;; will have lots of redunancy following multiple executions.

;; (defcustom maplev-etags "etags"
;;   "Etag program."
;;   :type 'string
;;   :group 'maplev)

;; (defcustom maplev-tag-regexp
;;   (concat "'/\\([^# \t]+\\)[ \t]*:=[ \t]*proc(/\\1/'")
;;   "Regular expression used by etag."
;;   :type 'string
;;   :group 'maplev)

;; ;; where does the following store the tag table?
;; ;; Always in the same directory as the

;; (defun maplev-tag-file ()
;;   "Create a tags table for the existing buffer/file."
;;   (interactive)
;;   (shell-command
;;    (concat maplev-etags
;;         " --language=none --regex="
;;         maplev-tag-regexp
;;         " "
;;         (buffer-file-name))))
;;}}}

;;; Process Modes

;;{{{ Group definitions

(defgroup maplev-buffer nil
  "Maple buffer stuff \(mostly names\)."
  :group 'maplev)

(defgroup maplev-help nil
  "Maple help pages."
  :group 'maplev)

(defgroup maplev-mint nil
  "Mint setup."
  :group 'maplev
  :group 'maplev-executables)

;;}}}
;;{{{ Customizable variables

;;{{{   buffers

(defcustom maplev-pop-up-frames-flag nil
  "*Non-nil means help pages and procedure listings start in a separate frame."
  :type 'boolean
  :group 'maplev-misc)

(defcustom maplev-cmaple-end-notice "END_OF_OUTPUT"
  "*Message used to indicate the end of Maple output."
  :type 'string
  :group 'maplev-misc)

(defcustom maplev-cmaple-echoes-flag
  (not (string-match "windows-nt\\|ms-dos" (symbol-name system-type)))
  "*Non-nil means the process echoes."
  :type 'boolean
  :group 'maplev-buffer
  :group 'maplev-important)

;;}}}
;;{{{   maple setup
(defcustom maplev-start-options (list "-q")
  "*List of Maple command line options.  Each item is a string."
  :type 'list
  :group 'maplev-executables)

(defcustom maplev-startup-directory nil
  "If non-nil, change to this directory before running Maple.
Otherwise use the default directory of `maplev-cmaple-buffer'."
  :type '(choice string (const :tag "default" nil))
  :group 'maplev-executables)

(defcustom maplev-cmaple-prompt "> "
  "String inserted as prompt in Maple buffer."
  :type 'string
  :group 'maplev-executables
  :group 'maplev-buffer)
;;}}}

;;}}}
;;{{{ Internal variables

(defvar maplev--history-stack nil
  "Stack variable used for the history mechanism.
It is local to the `maplev-help-mode' and `maplev-proc-mode' buffers.")

(defvar maplev--process-item nil
  "The name of a function that processes items on `maplev--history-stack'.
It is local to the `maplev-help-mode' and `maplev-proc-mode' buffers.")

;;}}}
;;{{{ Release

(defun maplev--help-buffer ()
  "Return the name of the Maple help buffer."
  (concat "Maple R" maplev-release " help"))

(defun maplev--proc-buffer ()
  "Return the name of the Maple procedure listing buffer."
  (concat "Maple R" maplev-release " proc"))

(defun maplev--cmaple-buffer ()
  "Return the name of the Maple cmaple buffer."
  (concat "Maple R" maplev-release))

;;}}}
;;{{{ Maple
;;{{{   comm functions
;; Define the functions used for communicating with the command line
;; Maple process.

(defun maplev--cmaple-process ()
  "Return the cmaple process associated with the current buffer.
Start one, if necessary."
  (let ((process (get-buffer-process (maplev--cmaple-buffer))))
    (if (and process (eq (process-status process) 'run))
        process
      (maplev-cmaple--start-process))))

(defsubst maplev--short-delay ()
  "Pause for a brief duration."
  (sleep-for 0.1))

(defun maplev-cmaple--start-process ()
  "Start a cmaple process associated with the current buffer.
Return the process.  If such a process already exists, kill it and
restart it."
  (let* ((release maplev-release)
         (cmaple (nth 0 (cdr (assoc release maplev-executable-alist))))
         (inifile (nth 1 (cdr (assoc release maplev-executable-alist))))
         (buffer (get-buffer-create (maplev--cmaple-buffer)))
         (process (get-buffer-process buffer)))
    (with-current-buffer buffer
      (message "Starting Maple R%s..." release)
      (if process (delete-process process))
      (if maplev-startup-directory
          (cd (expand-file-name maplev-startup-directory)))
      (set-process-filter
       ;; `apply' is used because `maplev-start-options' is a list.
       (setq process (apply 'start-process
                            (concat "Maple R" release)
                            buffer
                            cmaple
                            (append (and inifile (list "-i" inifile))
                                    maplev-start-options;; add include path to argument list
                                    (and maplev-include-path
                                         (list (concat "-I " 
                                                       (mapconcat 'identity maplev-include-path ",")))))
                            ))
       'maplev--cmaple-filter)
      (maplev-cmaple-mode release)
      (maplev-cmaple--lock-access t)
      (comint-simple-send process
                          (cdr (assoc release maplev-init-string-alist)))
      (maplev-cmaple--send-end-notice process)
      ;; Wait until cmaple is unlocked, that is, it has responded.
      ;; The time step, 100 milliseconds, should be customizable, some OSs
      ;; do not support fractions of seconds.
      (while (maplev-cmaple--locked-p) (maplev--short-delay))
      (message "Maple R%s started" release)
      process)))

;; Access control

(defun maplev-cmaple--lock-access (&optional no-error)
  "Lock access to cmaple.
If access is already locked, generate an error
unless optional arg NO-ERROR is non-nil."
  (if (and (not no-error) (maplev-cmaple--locked-p))
      (error "Maple busy")
;hieida:
;    (put 'maplev-cmaple-state maplev-release 'locked)))
    (put 'maplev-cmaple-state 'maplev-release 'locked)))

(defun maplev-cmaple--unlock-access ()
  "Unlock access to cmaple.
Interactively use \\[maplev-cmaple-interrupt]."
;hieida:
;  (put 'maplev-cmaple-state maplev-release nil))
  (put 'maplev-cmaple-state 'maplev-release nil))

(defun maplev-cmaple--locked-p ()
  "Return non-nil if the Maple process is locked."
;hieida:
;  (eq (get 'maplev-cmaple-state maplev-release) 'locked))
  (eq (get 'maplev-cmaple-state 'maplev-release) 'locked))

(defun maplev-cmaple-status ()
  "Status of Maple process."
  (interactive)
;hieida:
;  (let ((status (get 'maplev-cmaple-state maplev-release)))
  (let ((status (get 'maplev-cmaple-state 'maplev-release)))
    (message "Maple R%s %s" maplev-release
             (cond ((eq status 'locked) "locked")
                   ((not status) "unlocked")
                   (status)))))

;; Functions that send stuff to cmaple

(defun maplev-cmaple-send ()
  "Send input to Maple."
  (interactive)
  (let ((pmark (process-mark (maplev--cmaple-process)))
        (maplev-mint-info-level maplev-mint-error-level)
        (comint-input-sender (function maplev-cmaple--send-string)))
    ;; Only _new_ input is checked for typos, see comint-send-input.
    ;; We might need something smarter for comint-get-old-input.
    ;; Why does comint-send-input use (line-end-position) instead of
    ;; (point-max)? To be consistent maplev-mint-region does the same.
    (if (or (< (point) (marker-position pmark))
            (equal 0 (maplev-mint-region pmark (line-end-position))))
        (comint-send-input))))

(defun maplev-cmaple--send-string (process string)
  "Send STRING to the cmaple process PROCESS."
  ;; handle Maple `restart' by adding the initialization according to
  ;; maplev-init-string-alist
  (let ((str "") case-fold-search)
    (while (string-match "\\<restart[ \t\n]*[:;]" string)
      (setq str (concat str (substring string 0 (match-end 0))
                        (cdr (assoc maplev-release maplev-init-string-alist)))
            string (if (> (length string) (match-end 0))
                       (substring string (match-end 0))
                     "")))
    (setq string (concat str string)))
  (maplev-cmaple--lock-access)
  (set-process-filter process 'maplev--cmaple-filter)
  (comint-simple-send process string)
  (maplev-cmaple--send-end-notice process))

(defun maplev-cmaple-send-region (beg end)
  "Send the region from BEG to END to cmaple.
If called interactively use the marked region.
If called with a prefix the cmaple buffer is first cleared."
  (interactive "r")
  (let ((maplev-mint-info-level maplev-mint-error-level))
    (when (equal 0 (maplev-mint-region beg end))
      (and current-prefix-arg (maplev-cmaple--clear-buffer))
      (maplev-cmaple--send-string (maplev--cmaple-process)
                                  (buffer-substring-no-properties beg end)))))

(defun maplev-cmaple-send-line ()
  "Send the current line to cmaple"
  (interactive)
  (maplev-cmaple-send-region (line-beginning-position) (line-end-position)))

(defun maplev-cmaple-send-buffer ()
  "Send the buffer to cmaple."
  (interactive)
  (maplev-cmaple-send-region (point-min) (point-max)))

(defun maplev-cmaple-send-procedure (&optional level)
  "Send the current procedure to cmaple."
  (interactive "p")
  (apply 'maplev-cmaple-send-region (maplev-current-proc level)))

(defun maplev-cmaple--send-end-notice (process)
  "Send a command to PROCESS \(cmaple\) to print `maplev-cmaple-end-notice'."
  (comint-simple-send process (concat "lprint(" maplev-cmaple-end-notice ");")))

(defun maplev-cmaple--ready (process)
  "Return t if PROCESS \(cmaple\) ready for new input, nil otherwise.
Remove `maplev-cmaple-end-notice' from the current buffer.
Reset the filter for PROCESS \(cmaple\) and unlock access."
  (let (case-fold-search)
    (save-excursion
      (when (re-search-backward
             (concat maplev-cmaple-end-notice "\n") nil t)
        (delete-region (match-beginning 0) (match-end 0))
        (when (and maplev-cmaple-echoes-flag
                   (re-search-backward
                    (concat "lprint(" maplev-cmaple-end-notice ");\n")
                    nil t))
          (delete-region (match-beginning 0) (match-end 0)))
        (maplev--cleanup-buffer)
        (set-process-filter process 'maplev--cmaple-filter)
        (maplev-cmaple--unlock-access)
        t))))

(defun maplev-cmaple-interrupt ()
  "Interrupt Maple."
  (interactive)
  (let ((process (get-buffer-process (maplev--cmaple-buffer))))
    (message "Interrupt process %s" (process-name process))
    (interrupt-process process)
    (maplev-cmaple--unlock-access)))

(defun maplev-cmaple-kill ()
  "Kill Maple."
  (interactive)
  (let ((process (get-buffer-process (maplev--cmaple-buffer))))
    (message "Kill process %s" (process-name process))
    (kill-process process)))

(defun maplev-cmaple--clear-buffer ()
  "Clear the contents of the cmaple buffer."
  (save-excursion
    (set-buffer (maplev--cmaple-buffer))
    (delete-region (point-min) (point-max))))
                 

(defun maplev-cmaple-pop-to-buffer (&optional release)
  "Pop up a buffer with command line Maple.  Start Maple, if necessary.
Optional arg RELEASE defaults to `maplev-release'."
  (interactive
   (list (if current-prefix-arg
             (completing-read "Maple release: "
                              (mapcar (lambda (item) (list (car item)))
                                      maplev-executable-alist)
                              nil t))))
  (unless release (setq release maplev-release))
  (let ((maplev-release release))
    (maplev--cmaple-process)
    (pop-to-buffer (maplev--cmaple-buffer))
    (goto-char (point-max))))

(defalias 'cmaple 'maplev-cmaple-pop-to-buffer)

(defun maplev--cmaple-filter (process string)
  "Send the Maple output to the Maple buffer.
PROCESS is the Maple process, STRING its output."
  (with-current-buffer (process-buffer process)
    (let ((pmark (process-mark process)))
      (save-excursion
        (save-restriction
          (goto-char pmark)
          (narrow-to-region (point) (point))
          (insert string)
          (maplev--cleanup-buffer)
          (goto-char (point-max))
          (set-marker pmark (point)))
        (when (maplev-cmaple--ready process)
          (insert maplev-cmaple-prompt)
          (set-marker pmark (point))))
      (goto-char pmark))))

(defun maplev--cleanup-buffer ()
  "Remove overstriking and underlining from the current buffer."
  (goto-char (point-min))
  (while (re-search-forward "\e\\[[0-9;]+m" nil t) (replace-match ""))
  (goto-char (point-min))
  (while (re-search-forward "\r+" nil t) (replace-match "\n")))

;;}}}
;;{{{   mode map
(defvar maplev-cmaple-map nil
  "Keymap used in Maple cmaple mode.")

(unless maplev-cmaple-map
  (let ((map (copy-keymap comint-mode-map)))
    (define-key map [(return)]                'maplev-cmaple-send)
    (define-key map [(control c) (control c)] 'maplev-cmaple-interrupt)
    (define-key map [?\?]                     'maplev-help-at-point)
    (define-key map [(control ?\?)]           'maplev-help-at-point)
    (define-key map [(meta ?\?)]              'maplev-proc-at-point)
    (define-key map [(meta tab)]              'maplev-complete-symbol)
    (define-key map [(control a)]             'comint-bol)

    ;; These two bindings are needed only under linux / unix
    (define-key map [(meta control y)]    'maplev-insert-cut-buffer)

    ;; mouse button bindings
    (define-key map (maplev--mouse-keymap '(control meta 2))  'maplev-mouse-yank-cut-buffer)
    (define-key map (maplev--mouse-keymap '(shift 2))         'maplev-help-follow-mouse)
    (define-key map (maplev--mouse-keymap '(control shift 2)) 'maplev-help-follow-mouse)
    (define-key map (maplev--mouse-keymap '(meta shift 2))    'maplev-proc-follow-mouse)

    ;; in comint-mode-map of emacs 21, `C-c C-s' is bound to comint-write-output.
    ;; Remove it so that it can be used as a prefix key to switch buffers.
    (define-key map [(control c) (control s)]     nil)
    (define-key map [(control c) (control s) ?h] 'maplev-switch-buffer-help)
    (define-key map [(control c) (control s) ?l] 'maplev-switch-buffer-proc)
    (define-key map [(shift return)]             'newline)
    (setq maplev-cmaple-map map)))

;;}}}
;;{{{   mode
(defconst maplev-input-line-keyword
  `((,(concat "^" maplev-cmaple-prompt ".*$") . maplev-input-face))
  "Keyword for font locking input lines in cmaple mode.")

(defun maplev-cmaple-mode (&optional release)
  "Major mode for interacting with cmaple.
RELEASE is the release of Maple that should be started, if nil the
`maplev-default-release' is used.  It has the same commands as
`comint-mode' plus some additional commands for interacting with
cmaple.

\\{maplev-cmaple-map}"
  (interactive)
  (comint-mode)
  (setq comint-prompt-regexp (concat "^\\(" maplev-cmaple-prompt "\\)+ *")
        ;; GNU Emacs 21
        comint-use-prompt-regexp-instead-of-fields t
        comint-eol-on-send t
        major-mode 'maplev-cmaple-mode
        mode-name "Maple")

  ;; Mint support
  (make-local-variable 'maplev-mint--code-beginning)
  (make-local-variable 'maplev-mint--code-end)

  (maplev-set-release release)
  (use-local-map maplev-cmaple-map)
  (set (make-local-variable 'font-lock-defaults)
       '(maplev-input-line-keyword))
  (set (make-local-variable 'comint-process-echoes)
       maplev-cmaple-echoes-flag)
  (make-local-variable 'maplev-cmaple-prompt)
  (font-lock-mode 1)
  (run-hooks 'maplev-cmaple-mode-hook))

;;}}}
;;}}}

;;{{{ Help mode

;;{{{   mode map

(defvar maplev-help-mode-map nil
  "Keymap used in `maplev-help-mode'.")

(unless maplev-help-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [(space>)]     'scroll-up)
    (define-key map [(backspace)]  'scroll-down)
    (define-key map [?q]                         'quit-window)
    (define-key map [?s]                         'isearch-forward)
    (define-key map [?r]                         'maplev-redo-item)
    (define-key map [?p]                         'maplev-prev-item)
    (define-key map [?n]                         'maplev-next-item)
    (define-key map [?d]                         'maplev-delete-item)
    (define-key map [?P]                         'maplev-help-parent)
    (define-key map [?\?]                        'maplev-help-at-point)
    (define-key map [(control ?\?)]              'maplev-help-at-point)
    (define-key map [(meta ?\?)]                 'maplev-proc-at-point)
    (define-key map [?f]                         'maplev-tear-off-window)
    (define-key map [(control c) (control s) ?h] 'maplev-switch-buffer-help)
    (define-key map [(control c) (control s) ?l] 'maplev-switch-buffer-proc)
    (define-key map [(control c) (control s) ?c] 'maplev-switch-buffer-cmaple)
    (define-key map [?h]                         'maplev-switch-buffer-help) ; short-cut
    (define-key map [?l]                         'maplev-switch-buffer-proc) ; short-cut
    (define-key map [?c]                         'maplev-switch-buffer-cmaple) ; short-cut
    (define-key map [(return)]                   'maplev-help-at-point)
    (define-key map [(meta return)]              'maplev-proc-at-point)

    ;; Bind mouse buttons
    (define-key map (maplev--mouse-keymap '(2))               'maplev-help-follow-mouse)
    (define-key map (maplev--mouse-keymap '(shift 2))         'maplev-help-follow-mouse)
    (define-key map (maplev--mouse-keymap '(control shift 2)) 'maplev-help-follow-mouse)

    (define-key map (maplev--mouse-keymap '(meta 2))          'maplev-proc-follow-mouse)
    (define-key map (maplev--mouse-keymap '(meta shift 2))    'maplev-proc-follow-mouse)

    (setq maplev-help-mode-map map)))

(defvar maplev-help-mode-menu nil)
(unless maplev-help-mode-menu
  (easy-menu-define
    maplev-help-mode-menu maplev-help-mode-map
    "Menu for Maple help and proc buffer."
    `("MapleV"
      ["Parent"         maplev-help-parent
       :included (eq major-mode 'maplev-help-mode)]
      ["Previous"       maplev-prev-item t]
      ["Next"           maplev-next-item t]
      ["Redraw"         maplev-redo-item t]
      ["Delete"         maplev-delete-item t]
      ["Goto help node" maplev-help-at-point t]
      ["Goto proc node" maplev-proc-at-point t]
      ["Clear history"  maplev-clear-history t]
      "---"
      ["Separate frame" maplev-tear-off-window
       :active (not (one-window-p t 'here))]
      "---"
      ("Decoration" :included (eq major-mode 'maplev-proc-mode)
       ,@maplev--menu-decoration))))

;;}}}
;;{{{   mode definition
(defun maplev-help-mode (&optional release)
  "Major mode for displaying Maple help pages.
RELEASE is the Maple release, if nil, `maplev-default-release' is used.

\\{maplev-help-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'maplev-help-mode) ;; needed by maplev-set-release
  (maplev-set-release release)
  (setq mode-name (concat "Maple-Help R" maplev-release))
  (use-local-map maplev-help-mode-map)
  (set (make-local-variable 'maplev--process-item)
       (function maplev--help-process))

  (make-local-variable 'maplev--history-stack)  ; set up the stack
  (maplev-clear-history)

  ;; for maplev--activate-hyperlinks
  (set (make-local-variable 'parse-sexp-lookup-properties) t)

  (maplev-help-fontify-node)
  (setq buffer-read-only t)
  (run-hooks 'maplev-help-mode-hook))

;;}}}
;;{{{   mode functions
(defun maplev-help-follow-mouse (click)
  "Display the Maple help page of the topic at the mouse CLICK."
  (interactive "e")
  (set-buffer (window-buffer (event-window click)))
  (goto-char (event-point click))
  (maplev-help-show-topic (maplev-ident-around-point)))

(defun maplev-ident-around-point (&optional default)
  "Return the identifier around the point as a string.
If it is empty use DEFAULT.
If choice is empty, an error is signaled, unless DEFAULT equals \"\" or t."
  ;; If point is in a string enclosed by backquotes,
  ;; we take the whole string including the backquotes.
  (let* ((state (parse-partial-sexp (maplev-safe-position)
                                    (point)))
         (choice (if (equal ?` (nth 3 state))
                     ;; inside a string
                     (buffer-substring-no-properties
                      (nth 8 state)
                      (save-excursion (goto-char (nth 8 state))
                                      (forward-sexp 1) (point)))
                   (current-word))))
    (if (string-equal choice "")
        (cond ((stringp default)
               default)
              (default "")
              ((error "Empty choice")))
      choice)))

(defun maplev-ident-around-point-interactive (prompt &optional default complete)
  "Request Maple identifier in minibuffer, using PROMPT.
Default is identifier around point. If it is empty use DEFAULT.
Minibuffer completion is used if COMPLETE is non-nil."
  ;; Suppress error message
  (if (not default) (setq default t))
  (let ((enable-recursive-minibuffers t)
        (ident (maplev-ident-around-point default))
        (maplev-completion-release maplev-release)
        choice)
    (setq prompt (concat prompt (unless (string-equal ident "")
                                  (concat " (default " ident ")"))
                         ": ")
          choice (if complete
                     (completing-read prompt 'maplev--completion
                                      nil nil nil maplev-history-list ident)
                   (read-string prompt nil maplev-history-list ident)))
    ;; Are there situations where we want to suppress the error message??
    (if (string-equal choice "")
        (error "Empty choice"))
    (maplev--string-to-name choice)))

(defun maplev--string-to-name (name)
  "Convert NAME to a valid Maple name. Add backquotes if needed."
  ;; Do we need something more general to match a string that might
  ;; require backquotes?
  (when (string-match "/" name)
    (if (not (string= "`" (substring name 0 1)))
        (setq name (concat "`" name)))
    (if (not (string= "`" (substring name -1)))
        (setq name (concat name "`"))))
  name)

(defun maplev-help-at-point (topic)
  "Display Maple help for TOPIC \(a string\).
Interactively, default is word point is on."
  (interactive (list (maplev-ident-around-point-interactive
                      "Maple help topic" "help" t)))
  (maplev-help-show-topic topic))

(defun maplev-help-show-topic (topic &optional hide)
  "Display Maple help for TOPIC \(a string\).
Push TOPIC onto the local stack, unless it is already on the top.
If optional arg HIDE is non-nil do not display buffer."
  (save-current-buffer       ; maybe should be deeper (NEW!!!!!)
    (let ((release maplev-release)) ;; we switch buffers!
      (set-buffer (get-buffer-create (maplev--help-buffer)))
      (unless (eq major-mode 'maplev-help-mode)
        (maplev-help-mode release))
      (maplev--history-stack-process topic hide))))

(defun maplev--help-process (topic)
  "Display Maple help for TOPIC in `maplev--help-buffer'."
  (let ((process (maplev--cmaple-process)))
    (maplev-cmaple--lock-access)
    (set-process-filter process 'maplev--help-filter)
    (set-buffer (maplev--help-buffer))
    (setq mode-line-buffer-identification (format "%-12s" topic))
    (let (buffer-read-only)
      (delete-region (point-min) (point-max)))
    (comint-simple-send process (concat "?" topic))
    (maplev-cmaple--send-end-notice process)))

(defun maplev--help-filter (process string)
  "Pipe the output of a help command into `maplev--help-buffer'.
PROCESS calls this filter.  STRING is the output."
  (with-current-buffer (maplev--help-buffer)
    (save-excursion
      (let (buffer-read-only)
        (save-restriction
          (goto-char (point-max))
          (narrow-to-region (point) (point))
          (insert string)
          (maplev--cleanup-buffer))
        (goto-char (point-max))
        (if (maplev-cmaple--ready process)
            (maplev-help--cleanup-buffer))))))

(defun maplev-help--cleanup-buffer ()
  "Cleanup Maple help pages."
  (if maplev-cmaple-echoes-flag
      (save-excursion
        (goto-char (point-min))
        (if (re-search-forward "\\`\\?.+\n" nil t)
            (delete-region (match-beginning 0) (match-end 0)))))
  (maplev-help-fontify-node)
  (set-buffer-modified-p nil))

(defun maplev--completion (word predicate mode)
  "Generate minibuffer completion using maple function names.
For the meaning of args see Info node `(elisp)Programmed Completion'."
  ;; Make sure we are using the correct value of maplev-release.
  ;; (Inside the minibuffer maplev-release equals maplev-default-release.)
  (let ((maplev-release maplev-completion-release))
    (unless (assoc maplev-release maplev-completion-alist)
      ;; processing node "index/function"
      (let (possibilities)
        (save-excursion
          (maplev-help-show-topic "index/function" t)
          (set-buffer (maplev--help-buffer))
          (while (maplev-cmaple--locked-p) (maplev--short-delay))
          (save-restriction
            ;; does this work with all releases?
            (narrow-to-region (save-excursion (goto-line 7) (point))
                              (save-excursion (goto-char (point-max))
                                              (forward-line -3) (point)))
            (goto-char (point-max))
            (while (forward-word -1)
              (setq possibilities
                    (cons (cons (buffer-substring-no-properties
                                 (point)
                                 (save-excursion (forward-word 1) (point)))
                                nil)
                        possibilities))))
          (maplev-delete-item))
        ;; processing node "index[package]" -- suggestions welcome!
        (setq maplev-completion-alist
              (cons (cons maplev-release (list possibilities))
                    maplev-completion-alist))))
    (let ((possibilities (cadr (assoc maplev-release maplev-completion-alist))))
      (cond ((eq mode t)
             (all-completions word possibilities predicate))
            ((not mode)
             (try-completion word possibilities predicate))
            ((eq mode 'lambda)
             (assoc word possibilities))))))

(defun maplev-complete-symbol (&optional prefix)
  "Perform completion on maple symbol preceding point.
Compare that symbol against `maplev-completion-alist'."
  ;; Code borrowed from lisp-complete-symbol.
  (interactive)
  (let* ((end (point))
         ;; The following probably can be improved
	 (beg (save-excursion
                (backward-sexp 1)
                (while (= (char-syntax (following-char)) ?\')
                  (forward-char 1))
                (point)))
	 (pattern (buffer-substring-no-properties beg end))
         (maplev-completion-release maplev-release)
	 (completion (try-completion pattern 'maplev--completion)))
    (cond ((eq completion t))
	  ((null completion)
	   (message "Can't find completion for \"%s\"" pattern)
	   (ding))
	  ((not (string= pattern completion))
	   (delete-region beg end)
	   (insert completion))
	  (t
	   (message "Making completion list...")
	   (let ((list (sort (all-completions pattern 'maplev--completion)
                             'string<)))
	     (with-output-to-temp-buffer "*Completions*"
	       (display-completion-list list)))
	   (message "Making completion list...%s" "done")))))

(defun maplev-switch-buffer-help ()
  "Switch to help buffer, if it exists."
  (interactive)
  (maplev-switch-buffer (maplev--help-buffer)))

(defun maplev-switch-buffer-proc ()
  "Switch to proc buffer, if it exists."
  (interactive)
  (maplev-switch-buffer (maplev--proc-buffer)))

(defun maplev-switch-buffer-cmaple ()
  "Switch to cmaple buffer, if it exists."
  (interactive)
  (maplev-switch-buffer (maplev--cmaple-buffer)))

(defun maplev-switch-buffer (buffer)
  "Switch to BUFFER, if it exists."
  (let ((buf (get-buffer buffer)))
    (if buf
        (switch-to-buffer buf)
      (message "No buffer \"%s\"." buffer))))
;;}}}
;;{{{   history mechanism
(defun maplev-help-parent ()
  "Display the parent node of the current help page.
The parent node is extracted from the context of the help page, not
from the parent defined in the Maple help system."
  (interactive)
  (goto-char (point-min))
  (if (looking-at "\\(Function: ?\\)?\\([a-zA-Z0-9]*\\)\\[")
      (maplev-help-show-topic (match-string 2))
    (maplev-help-show-topic "index")))
;;}}}
;;{{{   fontify
;;{{{     fonts

(defcustom maplev-help-function-face 'font-lock-function-name-face
  "Face name for functions in title lines of Maple help pages."
  :type 'face
  :group 'maplev-faces
  :group 'maplev-help)

(defvar maplev-help-title-face   'maplev-help-title-face
  "*Face name for subtitles in title lines of Maple help pages.")

(defvar maplev-help-section-face 'maplev-help-section-face
  "*Face name for section titles in Maple help pages.")

(defvar maplev-help-subsection-face 'maplev-help-section-face
  "*Face name for section titles in Maple help pages.")

(defvar maplev-input-face  'maplev-input-face
  "*Face name for Maple input in help pages and Maple buffer.")

(defface maplev-help-title-face
  '((((class grayscale) (background light)) (:foreground "LightGray" :bold t))
    (((class grayscale) (background dark))  (:foreground "DimGray"   :bold t))
    (((class color)     (background light)) (:foreground "Black"     :bold t))
    (((class color)     (background dark))  (:foreground "Green"     :bold t))
    (t (:bold t)))
  "Font lock mode face used to highlight subtitles in Maple help pages.
The title is the phrase following the function name."
  :group 'maplev-faces
  :group 'maplev-help)

(defface maplev-help-section-face
  '((((class grayscale) (background light)) (:foreground "LightGray" :bold t))
    (((class grayscale) (background dark))  (:foreground "DimGray"   :bold t))
    (((class color)     (background light)) (:foreground "Red"       :bold t))
    (((class color)     (background dark))  (:foreground "Red"       :bold t))
    (t (:bold t)))
  "Font lock mode face used to highlight section titles in Maple help pages."
  :group 'maplev-faces
  :group 'maplev-help)

(defface maplev-help-subsection-face
  '((((class grayscale) (background light)) (:foreground "LightGray" :bold t))
    (((class grayscale) (background dark))  (:foreground "DimGray"   :bold t))
    (((class color)     (background light)) (:foreground "orange"    :bold t))
    (((class color)     (background dark))  (:foreground "orange"    :bold t))
    (t (:bold t)))
  "Font lock mode face used to highlight section titles in Maple help pages."
  :group 'maplev-faces
  :group 'maplev-help)

(defface maplev-input-face
  '((((class grayscale) (background light)) (:foreground "LightGray" :bold t))
    (((class grayscale) (background dark))  (:foreground "DimGray"   :bold t))
    (((class color)     (background light)) (:foreground "dark green"))
    (((class color)     (background dark))  (:foreground "green"))
    (t (:bold t)))
  "Font lock mode face used to highlight Maple input."
  :group 'maplev-faces
  :group 'maplev-help)


;;}}}
;;{{{     regular expressions
; (defconst maplev--help-section-re
;   (concat "^[A-Z]"                      ; Must start with a capital.
;           "\\([^\n]*:\\|\\("            ; If it ends with a colon (and whitespace) it matches.
;           "\\([a-z]+ ?\\)?"             ; If it consists of no more than three alphabetic words,
;           "\\([A-Za-z][a-z]* ?\\)?"     ; possibly with capitals, then it matches.
;           "\\([A-Za-z][a-z]* ?\\)?\\)"
;           "\\)[ \t]*$")
;   "Regular expression for sections in a Maple help page.")

(defconst maplev--help-section-re
  (concat "^\\(Calling Sequences?"
          "\\|Parameters"
          "\\|Description"
          "\\|Examples"
          "\\|See Also"
          "\\):?")
  "Regular expression for sections in a Maple help page.")

(defconst maplev--help-subsection-re
  (concat "^\\([A-Z][a-z-0-9-]+ ?\\([A-Za-z0-9-][a-z]* ?\\)?"
          "\\([A-Za-z][a-z-]*\\)?:?[ \t]*$"
          "\\)")
  "Regular expression for subsections in a Maple help page.")
;;}}}
;;{{{     functions
(defun maplev-help-fontify-node ()
  "Fontify a Maple help page buffer. Does not use font-lock mode."
  (save-excursion
    (let (buffer-read-only
          (case-fold-search t))
      (if font-lock-mode (font-lock-mode)) ; turn-off font-lock.

      ;; Highlight the title.
      ;; The tricky part is handling multiple titles.

      (goto-char (point-min))
      ;; Move to the end of the title area.  Stop at first section or bullet.
      (if (re-search-forward (concat maplev--help-section-re "\\|^- ")
                             nil 'move)
          ;; Move backward to top of buffer, checking each line.
          (while (= 0 (forward-line -1))
            (if (looking-at "\\(Function:\\)?\\([^-\n]*\\)[ \t]+-[ \t]+\\(.*\\)$") ; regexp for function name(sort of)
                (progn (and (match-beginning 1)
                            (put-text-property (match-beginning 1) (match-end 1)
                                               'face 'maplev-help-section-face))
                       (and (match-beginning 3)
                            (put-text-property (match-beginning 3) (match-end 3)
                                               'face 'maplev-help-title-face))
                       (and (match-beginning 2)
                            (maplev--activate-hyperlinks (match-beginning 2) (match-end 2))))
              (put-text-property (point) (progn (end-of-line) (point))
                                 'face 'maplev-help-title-face)))
        (goto-char (point-min))
        (end-of-line)
        (put-text-property (point-min) (point)
                           'face 'maplev-help-title-face))

      ;; Highlight subsection titles
      (goto-char (point-min))
      (while (re-search-forward maplev--help-subsection-re nil t)
        (put-text-property (match-beginning 0) (match-end 0)
                           'face 'maplev-help-subsection-face))

      ;; Highlight section titles
      (goto-char (point-min))
      (while (re-search-forward maplev--help-section-re nil t)
        (put-text-property (match-beginning 0) (match-end 0)
                           'face 'maplev-help-section-face))

      ;; Highlight functions in a package. This usually works.  It
      ;; searches for `- The functions [arbitrary text] are:' and
      ;; highlights everything from the colon to the next line that
      ;; starts with a character that is not whitespace.
      (goto-char (point-min))
      (when (re-search-forward
             "^- The\\( available\\)? \\(functions\\|routines\\)[^\n]* are\\( the following\\)?: *$"
             nil 'move)
        (maplev--activate-hyperlinks
         (point) (progn (re-search-forward "^[^ \t\n]" nil 'move)
                        (line-end-position -1))))

      ;; Highlight Maple input
      (goto-char (point-min))
      (while (re-search-forward "^> .*$" nil t)
        (put-text-property (match-beginning 0) (match-end 0)
                           'face 'maplev-input-face))

      ;; Activate hyperlinks following "See Also:".  R4 does not
      ;; insert a carriage return so font-lock the section title,
      ;; which would not have matched `maplev-help-section-face'.
      (goto-char (point-max))
      (and (re-search-backward "^See Also:?" nil 'move)
           (maplev--activate-hyperlinks (match-end 0) (point-max)))

      ;; Activate hyperlinks forllowing "Multiple matches:".
      (goto-char (point-min))
      (and (re-search-forward "^Multiple matches found:" nil 'move)
           (maplev--activate-hyperlinks (match-end 0) (point-max))))))

(defun maplev--activate-hyperlinks (beg end)
  "Font lock and activate Maple keywords in the region from BEG to END."
  (goto-char beg)
  (while (re-search-forward
          (concat  maplev--name-re
                   "\\([,/]" maplev--name-re "\\)*")
          end 'move)
    (let ((beg (match-beginning 0))
          (end (match-end 0)))
      ;; Treat everything between beg and end as word constituents.
      ;; In particular, ignore the syntactic meaning of, e.g., `[',
      ;; `]', and `,'. Thus we can use current-word to pick up
      ;; these Maple keywords.
      (put-text-property beg end 'syntax-table '(2 . nil))
      (put-text-property beg end 'mouse-face 'highlight)
      (put-text-property beg end 'face maplev-help-function-face))))

;;}}}

;;}}}

;;}}}
;;{{{ Proc mode

;;{{{   mode map

(defvar maplev-proc-mode-map nil
  "Keymap used in `maplev-proc-mode'.")

(unless maplev-proc-mode-map
  (setq maplev-proc-mode-map (copy-keymap maplev-help-mode-map))
  (define-key maplev-proc-mode-map [?P] 'self-insert-command))

;;}}}
;;{{{   mode definition

(defun maplev-proc-mode (&optional release)
  "Major mode for displaying the source code of Maple procedures.
RELEASE is the Maple release, if nil, `maplev-default-release' is used.

\\{maplev-proc-mode-map}"
  (interactive)
  (kill-all-local-variables)

  (setq major-mode 'maplev-proc-mode) ;; needed by maplev-set-release
  (maplev-set-release release)
  (setq mode-name (concat "Maple-Proc R" maplev-release))
  (use-local-map maplev-proc-mode-map)

  (set (make-local-variable 'maplev--process-item)
       (function maplev--proc-process))

  (make-local-variable 'maplev--history-stack)  ; set up the stack
  (maplev-clear-history)

  ;; Mint support
  (make-local-variable 'maplev-mint--code-beginning)
  (make-local-variable 'maplev-mint--code-end)

  ;; font-lock support
  (make-local-variable 'font-lock-defaults)
  (make-local-variable 'font-lock-maximum-decoration)
  (maplev-reset-font-lock)

  (setq buffer-read-only t)
  (run-hooks 'maplev-proc-mode-hook))

;;}}}
;;{{{   functions
;;; Define functions for displaying a Maple procedure from the Maple
;;; library in a buffer.

(defun maplev-proc-follow-mouse (click)
  "Display the Maple procedure at the mouse CLICK."
  (interactive "e")
  (set-buffer (window-buffer (event-window click)))
  (goto-char (event-point click))
  (maplev--proc-show-topic (maplev-ident-around-point)))

(defun maplev-proc-at-point (proc)
  "Display the Maple procedure PROC.
Request procedure name in minibuffer, using identifier at point as default."
  (interactive (list (maplev-ident-around-point-interactive
                      "Maple procedure" nil t)))
  (maplev--proc-show-topic proc))

(defun maplev--proc-show-topic (proc &optional hide)
  "Display the Maple procedure PROC \(a string\).
Push PROC onto the local stack, unless it is already on the top.
If optional arg HIDE is non-nil do not display buffer."
  ;; Do not try to display builtin procedures.
  (if (assoc proc (mapcar 'list
                          (cdr (assoc (maplev--major-release) 
                                      maplev--builtin-functions-alist))))
      (message "Procedure \`%s\' builtin." proc)
    (save-current-buffer
      (let ((release maplev-release)) ;; we switch buffers!
        (set-buffer (get-buffer-create (maplev--proc-buffer)))
        (unless (eq major-mode 'maplev-proc-mode)
          (maplev-proc-mode release))
        (maplev--history-stack-process proc hide)))))

(defun maplev--proc-process (proc)
  "Display the Maple procedure PROC \(a string\) in `maplev--proc-buffer'."
  (let ((process (maplev--cmaple-process)))
    (maplev-cmaple--lock-access)
    (set-process-filter process 'maplev-proc-filter)
    (set-buffer (maplev--proc-buffer))
    (setq mode-line-buffer-identification (format "%-12s" proc))
    (let (buffer-read-only)
      (delete-region (point-min) (point-max))
      (goto-char (point-min))
      (insert proc " := "))
    (comint-simple-send process (concat "maplev_print(" proc ");"))
    (maplev-cmaple--send-end-notice process)))

(defun maplev-proc-filter (process string)
  "Pipe a Maple procedure listing into `maplev--proc-buffer'.
PROCESS calls this filter.  STRING is the Maple procedure."
  (with-current-buffer (maplev--proc-buffer)
    (save-excursion
      (let (buffer-read-only)
        (save-restriction
          (goto-char (point-max))
          (narrow-to-region (point) (point))
          (insert string)
          (maplev--cleanup-buffer))
        (goto-char (point-max))
        (if (maplev-cmaple--ready process)
            (maplev-proc-cleanup-buffer))))))

(defun maplev-proc-cleanup-buffer ()
  "Cleanup Maple procedure listings."
  (save-excursion
    (when maplev-cmaple-echoes-flag
      (goto-char (point-min))
      (if (re-search-forward "maplev_print(.+);\n" nil t)
          (delete-region (match-beginning 0) (match-end 0))))
    ;; Delete multiple spaces.
    (goto-char (point-min))
    (while (re-search-forward "[ \t][ \t]+" nil t)
      (replace-match " "))
    ;; terminate with `;'
    (goto-char (point-max))
    (skip-chars-backward " \t\n")
    (insert ";"))
  (maplev-indent-buffer)
  (set-buffer-modified-p nil)
  (font-lock-fontify-buffer))

;;}}}

;;}}}
;;{{{ Mint mode

;;{{{   customizable variables

(defcustom maplev-mint-coding-system 'undecided-dos
  "*Coding system used by Mint.  See `coding-system-for-read' for details."
  :type '(choice (const undecided-dos) (const raw-text-unix) (symbol :tag "other"))
  :group 'maplev-mint)

(defcustom maplev-mint-query t
  "*Non-nil means query before correcting."
  :type 'boolean
  :group 'maplev-mint)

(defcustom maplev-mint-process-all-vars nil
  "*Non-nil means process all variables in one step."
  :type 'boolean
  :group 'maplev-mint)

;;}}}
;;{{{   syntax table

(defvar maplev-mint-mode-syntax-table nil
  "Syntax table used in Maple mint buffer.")
(unless maplev-mint-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?[  "w"  table)
    (modify-syntax-entry ?]  "w"  table)
    (modify-syntax-entry ?_  "w"  table)
    (modify-syntax-entry ?/  "w"  table)
    (modify-syntax-entry ?\` "\"" table) ; string quotes
    (setq maplev-mint-mode-syntax-table table)))

;;}}}
;;{{{   mode map

(defvar maplev-mint-mode-map nil
  "Keymap used in Mint mode.")

(unless maplev-mint-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [(space)]                     'scroll-up)
    (define-key map [(backspace)]                 'scroll-down)
    (define-key map [(return)]                    'maplev-mint-rerun)
    (define-key map [(control c) (return) return] 'maplev-mint-rerun)
    (define-key map [?q]                          'quit-window)
    (define-key map [?s]                          'isearch-forward)
    (define-key map [?r]                          'isearch-backward)
    (define-key map (maplev--mouse-keymap '(2))   'maplev-mint-click)
    
    (define-key map [(control c) (control c)]     'maplev-mint-handler)
    (setq maplev-mint-mode-map map)))

;;}}}
;;{{{   menu

(easy-menu-define maplev-mint-mode-menu maplev-mint-mode-map
  "Menu for Mint buffer."
  '("Mint"
    ["Fix errors" maplev-mint-fix-errors :visible nil]  ; not yet defined
    ["Rerun mint" maplev-mint-rerun t]
    ["Quit"       quit-window t]))

;;}}}
;;{{{   mode definition

(defun maplev-mint-mode (code-buffer)
  "Major mode for displaying Mint output.
CODE-BUFFER is the buffer that contains the source code.
\\{maplev-mint-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (use-local-map maplev-mint-mode-map)
  (setq major-mode 'maplev-mint-mode
        mode-name "Mint")
  (set-syntax-table maplev-mint-mode-syntax-table)
  (set (make-local-variable 'maplev-mint--code-buffer) code-buffer)
  (maplev-mint-fontify-buffer)
  (setq buffer-read-only t)
  (run-hooks 'maplev-mint-mode-hook))

;;}}}
;;{{{   mode functions

(defun maplev-mint-goto-source-pos (l c)
  "Move to position in `maplev-mint--code-buffer' relative to `maplev-mint--code-beginning'.
The source code buffer is popped up and point is moved L lines forward
and then C columns forward from the origin. Return position of point."
  (pop-to-buffer maplev-mint--code-buffer)
  (goto-char maplev-mint--code-beginning)
  (if (> l 0) (forward-line l))
  (forward-char c)
  (point))

(defun maplev-mint-goto-error (pos)
  "Go to error in Maple source according to Mint message at position POS.
Return position of error in Maple source."
  (let (line col)
    (save-excursion
      (goto-char pos)
      ;; The location of the error is indicated by the caret
      ;; in the Mint output.
      (when (re-search-backward "\\^" (line-beginning-position) t)
        (setq col (current-column))
        (forward-line -1)
        (re-search-forward  "[0-9]+")
        (setq line (1- (string-to-number (match-string 0)))
              col  (- col (current-column) 2))))
    (maplev-mint-goto-source-pos line col)))

(defun maplev-mint--goto-source-proc (pos)
  "According to Mint buffer position POS, move point after closing
parenthesis of argument list of a source procedure. Return non-nil
if this is a procedure, nil if an operator.

THIS NEEDS WORK TO HANDLE OPERATORS."
  ;; This function uses a fairly complicated regexp in an attempt to
  ;; match the appropriate procedure assignment.  In one sense this is
  ;; overkill; Mint indicates the line number of the start of the
  ;; procedure, so we should be able to go directly to the procedure on
  ;; that line.  It is possible, however, to have a nested procedure on
  ;; the same line as another procedure.  More to the point, a nested
  ;; anonymous procedure inside an anonymous procedure. In that case the
  ;; only distinction is the argument list.  Does this happen enough to
  ;; justify this code?   If we merely desire to move point to the
  ;; correct place in the source, getting to the right line is
  ;; sufficient.  But if there is some automated work to do, the exact
  ;; point is required.  One way to avoid this complexity is to not
  ;; offer the user the option of automatically adding or deleting
  ;; variables from an anonymous procedure.  The sticking point is that
  ;; Mint, alas, considers indexed names to be anonymous procedures so
  ;; their frequency is greater than should be.

  (let (name args-re line case-fold-search)
    (save-excursion
      (goto-char pos)
      (re-search-backward "^\\(Nested \\)?\\(Anonymous \\)?\\(Procedure\\|Operator\\|Module\\)")
      ;; Get the procedure name
      (setq name (if (nth 4 (match-data)) ; t if anonymous procedure
                     ""
                   (save-excursion
                     ;; Use `(' to terminate proc-name
                     (re-search-forward "\\(Procedure\\|Module\\)[ \t]*\\([^(]*\\)")
                     (concat "`?" (match-string-no-properties 2) "[ \t\n]*:=[ \t\n]*")))
            ;; Return a regular expression that matches the argument
            ;; list in the source. The generated regexp does not
            ;; match an argument list with duplicate arguments; this
            ;; because Mint does not print the duplicate arguments.
            ;; This can be improved, made more robust.
            ;; Allow comments before commas, too.
            args-re (save-excursion
                      (re-search-forward "(\\([^)]*\\))")
                      (maplev--replace-string
                       (match-string-no-properties 1)
                       `(("::" . " :: ")
                         ("[ \t\n]+" . "[ \t\n]*")
                         ("," . ,(concat "\\([ \t]*\\(#.*\\)?\n\\)*[ \t]*"
                                         ","
                                         "\\([ \t]*\\(#.*\\)?\n\\)*[ \t]*"))))))

      (re-search-forward "on\\s-*lines?\\s-*\\([0-9]+\\)")
      (setq line (1- (string-to-number (match-string 1)))))

    (maplev-mint-goto-source-pos line 0)
    (unless (re-search-forward (concat name
                                       "\\(proc\\|module\\)[ \t\n*]*"
                                       "(\\([ \t]*\\(#.*\\)?\n\\)*"
                                       args-re
                                       "\\([ \t\n]*#.*$\\)*[ \t\n]*)")
                               nil t)
      ;; If search failed (possibly because of duplicate arguments,
      ;; try again with out explicitly specifying the argument list.
      (goto-char (maplev--scan-lists 1)))))

(defun maplev--replace-string (string replace)
  "In STRING replace as specified by REPLACE.
REPLACE is an alist with elements \(OLD . NEW\)."
  (while replace
    (let ((pos 0)
          (old (caar replace))
          (new (cdar replace)))
      (while (and (< pos (length string))
                  (setq pos (string-match old string pos)))
        (setq string (replace-match new t t string)
              pos (+ pos (length new)))))
    (setq replace (cdr replace)))
  string)

;;}}}
;;{{{   fontify

(defcustom maplev-mint-proc-face 'font-lock-function-name-face
  "Face name for procedure names in a Mint buffer."
  :type 'face
  :group 'maplev-faces
  :group 'maplev-mint)

(defcustom maplev-mint-warning-face 'font-lock-warning-face
  "Face name for warnings in a Mint buffer."
  :type 'face
  :group 'maplev-faces
  :group 'maplev-mint)

(defcustom maplev-mint-error-face 'font-lock-warning-face
  "Face name for error messages in a Mint buffer."
  :type 'face
  :group 'maplev-faces
  :group 'maplev-mint)

(defcustom maplev-mint-note-face 'font-lock-warning-face
  "Face name for notes in a Mint buffer."
  :type 'face
  :group 'maplev-faces
  :group 'maplev-mint)

(defconst maplev-mint-variables-re
  "[ \t\n]*\\(\\(.*,[ \t]*\n\\)*.*\\)[ \t]*$"
  "Regexp used to match the argument list of procedures in Mint output.")
(defconst maplev-mint-fontify-alist
  '(("^on line[ \t]*[0-9]+:" 0 maplev-mint-note-face)
    ("^[ \t]*\\(\\^.*$\\)"
     1 maplev-mint-error-face 'error)
    ("^\\(Nested \\)?\\(Procedure\\|Operator\\)[ ]*\\([^(]*\\)"
     3 maplev-mint-proc-face 'proc)
    ("^\\(Nested \\)?Anonymous \\(Procedure\\|Operator\\)[ ]*\\(proc([^)]*)\\)"
     3 maplev-mint-proc-face 'proc)
    ("These parameters were never used\\( explicitly\\)?:"
     2 maplev-mint-warning-face 'unused-arg t)
    ("These names appeared more than once in the parameter list:"
     1 maplev-mint-warning-face 'repeat-arg t)
    ("These local variables were not declared explicitly:"
     1 maplev-mint-warning-face 'undecl-local t)
    ("These local variables were never used:"
     1 maplev-mint-warning-face 'unused-local t)
    ("These names were declared more than once as a local variable:"
     1 maplev-mint-warning-face 'repeat-local t)
    ("These names were used as global names but were not declared:"
     1 maplev-mint-warning-face 'undecl-global t)
    ;; Could we make the following optional?
    ;; ("Global names used in this procedure:"
    ;;  1 maplev-mint-warning-face 'undecl-global t)
    )
  "Alist for fontification in a Mint buffer. Each element is a list
of the form \(REGEXP SUBEXP FACE PROP VAR\), where REGEXP is to be
matched, SUBEXP, a number, specifies which parenthesized expression
in REGEXP is picked up, and FACE is a face. Optional fourth element
PROP is a symbol used for marking the category of SUBEXP. Optional
fifth element VAR is non-nil if REGEXP is concatenated with
`maplev-mint-variables-re'.")

(defun maplev-mint-fontify-buffer ()
  "Fontify the mint buffer. Does not use font-lock mode."
  (let ((mlist maplev-mint-fontify-alist)
        regexp mel buffer-read-only case-fold-search)
    (if font-lock-mode (font-lock-mode)) ; turn-off font-lock
    ;; Process elements of maplev-mint-fontify-alist
    (while (setq mel (car mlist))
      (goto-char (point-min))
      (setq regexp (concat (nth 0 mel)
                           (if (nth 4 mel) maplev-mint-variables-re)))
      (while (re-search-forward regexp nil t)
        (let ((beg (match-beginning (nth 1 mel)))
              (end (match-end (nth 1 mel))))
          ;; Here we are working with variables whose values are symbols
          ;; with a face property.
          (put-text-property beg end 'face (eval (nth 2 mel)))
          (when (nth 3 mel)
            ;; We use a text property `maplev-mint' to store in the text
            ;; what kind of info we have from Mint.
            (put-text-property beg end 'maplev-mint (eval (nth 3 mel)))
            (if (and (nth 4 mel)
                     (not maplev-mint-process-all-vars)) ; then we do highlighting word-wise
                (save-excursion
                  (goto-char beg)
                  ;; Slightly simpler algorithm than the one used by
                  ;; maplev-ident-around-point to pick up the word
                  ;; where point is. Does it matter for highlighting?
                  (while (re-search-forward "\\<\\w+\\>" end t)
                    (put-text-property (match-beginning 0) (match-end 0)
                                       'mouse-face 'highlight)))
              (put-text-property beg end 'mouse-face 'highlight)))))
      (setq mlist (cdr mlist)))
    (set-buffer-modified-p nil)))

;;}}}
;;{{{   interactive functions

(defun maplev-mint-click (click)
  "Move point to CLICK."
  (interactive "e")
  (set-buffer (window-buffer (event-window click)))
  (maplev-mint-handler (event-point click)))

(defun maplev-mint-handler (pos)
  "Handle mint output at position POS.
When called interactively, POS is position where point is."
  (interactive "d")
  (let ((prop (get-text-property pos 'maplev-mint)))
    (if prop
        (let (string vars)
          (if maplev-mint-process-all-vars
              (let ((str (buffer-substring-no-properties
                          (next-single-property-change pos 'maplev-mint)
                          (previous-single-property-change (1+ pos) 'maplev-mint))))
                ;; string is like str, but with maplev-variable-spacing
                ;; vars is a comma separated list of names extracted from str
                (while (and (not (string= str ""))
                            (string-match "\\<\\w+\\>" str))
                  (setq vars (cons (match-string 0 str) vars)
                        string (if string
                                   (concat string ","
                                           (make-string maplev-variable-spacing ?\ )
                                           (match-string 0 str))
                                 (match-string 0 str))
                        str (substring str (match-end 0)))))
            (setq string (save-excursion
                           (goto-char pos)
                           (maplev-ident-around-point))
                  vars (list string)))
          ;;
          (cond
           ;; Jump to the start of a procedure in the source.
           ((equal prop 'proc)
            (maplev-mint--goto-source-proc pos))
           ;;
           ;; Jump to the location of an error in the source code.
           ((equal prop 'error)
            (maplev-mint-goto-error pos))
           ;;
           ;; Remove unused args from argument list.
           ((equal prop 'unused-arg)
            (when (maplev-mint-query "Delete `%s' from argument list? " string)
              (maplev-mint--goto-source-proc pos)
              (maplev-delete-vars (maplev--scan-lists -1) (point) vars)))
           ;;
           ;; Remove unused local variables from local declaration.
           ((equal prop 'unused-local)
            (when (maplev-mint-query "Delete `%s' from local statement? " string)
              (maplev-mint--goto-source-proc pos)
              (maplev-delete-declaration "local" vars)))
           ;;
           ;; Remove repeated args from argument list.
           ((equal prop 'repeat-arg)
            (when (maplev-mint-query "Remove duplicate `%s' from parameters? " string)
              (maplev-mint--goto-source-proc pos)
              (maplev-delete-vars (maplev--scan-lists -1) (point) vars 1)))
           ;;
           ;; Remove repeated local variables from local declaration.
           ((equal prop 'repeat-local)
            (when (maplev-mint-query "Remove duplicate `%s' from local statement? " string)
              (maplev-mint--goto-source-proc pos)
              (maplev-delete-declaration "local" vars 1)))
           ;;
           ;; Declaration of undeclared locals variables.
           ((equal prop 'undecl-local)
            (when (maplev-mint-query "Add `%s' to local statement? " string)
              (maplev-mint--goto-source-proc pos)
              (maplev-add-declaration "local" string)))
           ;;
           ;; Declaration of undeclared global variables.
           ((equal prop 'undecl-global)
            (when (maplev-mint-query "Add `%s' to global statement? " string)
              (maplev-mint--goto-source-proc pos)
              (maplev-add-declaration "global" string)))
           )))))

(defun maplev-mint-query (form &rest vars)
  "Return t if correction suggested by mint should be made.
FORM and VARS are used for y-or-n-p query."
  (or (not maplev-mint-query)
      (y-or-n-p (apply 'format form vars))))

;;}}}
;;{{{   regions

(defun maplev-mint-region (beg end)
  "Run Mint on the current region \(from BEG to END\).
Return exit code of mint."
  (interactive "r")
  (let ((code-buffer (current-buffer))
        (code-window (get-buffer-window (current-buffer)))
        (coding-system-for-read maplev-mint-coding-system)
        (mint-buffer (concat "*Mint " maplev-release "*"))
        (mint (nth 2 (cdr (assoc maplev-release maplev-executable-alist))))
        status eoi lines errpos)
    ;; Allocate markers, unless they exist
    (unless maplev-mint--code-beginning
      (setq maplev-mint--code-beginning (make-marker)
            maplev-mint--code-end (make-marker)))
    (set-marker maplev-mint--code-beginning beg)
    (set-marker maplev-mint--code-end end)
    (save-excursion
      (set-buffer (get-buffer-create mint-buffer))
      (setq buffer-read-only nil))
    (copy-to-buffer mint-buffer beg end)
    (save-excursion
      (set-buffer mint-buffer)
      (goto-char (point-max))
      ;; Add a blank line to the end of the buffer, unless there is
      ;; one already.  This is needed for mint to work properly.
      ;; (That's why mint-buffer is used as a temp buffer for mint input.)
      (if (not (bolp)) (newline))
      ;; remember end-of-input
      (setq eoi (point-max))
      ;; Run Mint
      (setq status (apply 'call-process-region
                          (point-min) (point-max)
                          mint nil mint-buffer nil
                          (concat "-i" (number-to-string maplev-mint-info-level)
                                  ;; Add include path to argument list.
                                  ;; Use commas to separate directories (see ?mint)
                                  (and maplev-include-path
                                       (concat " -I " 
                                               (mapconcat 'identity maplev-include-path ","))))
                          maplev-mint-start-options))
      (delete-region (point-min) eoi)
      ;; Display Mint output
      (maplev-mint-mode code-buffer)
      (setq lines (if (= (buffer-size) 0)
                      0
                    (count-lines (point-min) (point-max))))
      (cond ((= lines 0)
             ;; let's assume: no mint output means no "real" error
             ;; This happens with maplev-mint-info-level set to 1
             (setq status 0))
            ((= lines 1)
             (goto-char (point-min))
             (message "%s" (buffer-substring-no-properties
                            (point) (line-end-position))))
            ((> lines 1)
             (display-buffer (current-buffer))))
      ;; If error in maple source (should be identical to status > 0)
      ;; locate position of error
      (goto-char (point-min))
      (if (re-search-forward "^[ \t]*\\^" nil t)
          (setq errpos (maplev-mint-goto-error (point)))))
    ;; If there is an error in the maple source and a window displays it,
    ;; move point in this window
    (if (and code-window errpos)
        (set-window-point code-window errpos))
    status))

(defun maplev-mint-buffer ()
  "Run Mint on the current buffer."
  (interactive)
  (save-restriction
    (widen)
    (maplev-mint-region (point-min) (point-max))))

(defun maplev-mint-procedure (&optional level)
  "Run Mint on the current procedure."
  (interactive "p")
  (apply 'maplev-mint-region (maplev-current-proc level)))

(defun maplev-mint-rerun ()
  "Rerun Mint on the previously executed region.
If no region has been selected, run Mint on the buffer."
  (interactive)
  (save-current-buffer
    (if maplev-mint--code-buffer ; we are in mint buffer
        (set-buffer maplev-mint--code-buffer))
    (if (not maplev-mint--code-beginning)
        (maplev-mint-buffer)
      (maplev-mint-region (marker-position maplev-mint--code-beginning)
                          (marker-position maplev-mint--code-end)))))

;;}}}

;;}}}
;;{{{ History mechanism

;; History of history.
;;
;; Originally this structure was implemented as a browsable stack.
;; New entries were always inserted on the top.  The usage,
;; however, seemed confusing.  Bringing up a new node while browsing
;; the stack would move you to the top of the stack, away from where
;; you were.
;;
;; The new design inserts entries where you are at.  An interesting
;; modification, not implemented (yet) would be to make this a
;; rolodex, that is, a ring rather than a stack.

;;{{{   Module

;; Implement a stack-like structure for providing a history mechanism
;; for the Help and Proc modes.  The stack is a list.  The car of the
;; list is an integer that indexes a particular element in the list;
;; it is used when scrolling through the stack.

(defvar maplev--history-stack nil
  "List containing history of previous `commands'.
The car of the list is an integer that indexes a particular element in
the list, it is used to scroll through the stack.")

(defun maplev--history-stack-insert (item)
  "Put ITEM into `maplev--history-stack'."
  (let ((pos (car maplev--history-stack)))
    (setcdr (nthcdr pos maplev--history-stack)
            (cons item (nthcdr (1+ pos) maplev--history-stack)))))

(defun maplev--history-stack-prev ()
  "Return the item on `maplev--history-stack' preceding the one last accessed.
If at the bottom of the stack return nil, otherwise increment the pointer."
  (let* ((pos (1+ (car maplev--history-stack)))
         (item (nth pos (cdr maplev--history-stack))))
    (when item
      (setcar maplev--history-stack pos)
      item)))

(defun maplev--history-stack-next ()
  "Return the item on `maplev--history-stack' following the one last accessed.
If at the top of the stack, return nil, otherwise decrement the pointer."
  (let ((pos (1- (car maplev--history-stack))))
    (when (>= pos 0)
      (setcar maplev--history-stack pos)
      (nth pos (cdr maplev--history-stack)))))

(defun maplev--history-stack-top ()
  "Return the top item of `maplev--history-stack'.
Do not change the pointer."
  (nth 1 maplev--history-stack))

(defun maplev--history-stack-current ()
  "Return the currently accessed element of `maplev--history-stack'."
  (nth (car maplev--history-stack) (cdr maplev--history-stack)))

;;}}}
;;{{{   Commands

;;; The following commands process the history items.  The symbol
;;; `maplev--process-item' should be buffer local and assigned the
;;; name of the function that process the items.

(defsubst maplev--process-item-func (item)
  "Apply the function symbol `maplev--process-item' to ITEM."
  (if (stringp item)
      (funcall maplev--process-item item)
    (message "End of stack")))

(defun maplev-next-item ()
  "Process the next item on `maplev--history-stack'."
  (interactive)
  (maplev--process-item-func (maplev--history-stack-next)))

(defun maplev-prev-item ()
  "Process the previous item on `maplev--history-stack'."
  (interactive)
  (maplev--process-item-func (maplev--history-stack-prev)))

(defun maplev-redo-item ()
  "Process the current item on `maplev--history-stack'."
  (interactive)
  (maplev--process-item-func (maplev--history-stack-current)))

(defun maplev-delete-item ()
  "Delete current item from `maplev--history-stack'."
  (interactive)
  (let ((pos (car maplev--history-stack)))
    (setcdr (nthcdr pos maplev--history-stack)
            (nthcdr (+ 2 pos) maplev--history-stack))
    (unless (nth pos (cdr maplev--history-stack))
      (setcar maplev--history-stack (setq pos (1- pos))))
    (if (>= pos 0)
        (maplev--process-item-func (maplev--history-stack-current))
      (kill-buffer nil))))

(defun maplev-clear-history ()
  "Assign `maplev--history-stack' an empty stack."
  (interactive)
  (setq maplev--history-stack (list 0)))

(defun maplev--history-stack-process (item &optional hide)
  "Insert ITEM into `maplev--history-stack' and process it.
Do not insert ITEM into the stack if it is already at the current
or following position.
If optional arg HIDE is non-nil do not display buffer."
  (let ((pos (car maplev--history-stack)))
    (unless (or (string= item (maplev--history-stack-current))
                (and (/= pos 0)
                     (string= item (nth pos maplev--history-stack))))
      (maplev--history-stack-insert item))
    (maplev--process-item-func item)
    (unless hide
      (let ((pop-up-frames maplev-pop-up-frames-flag))
        (display-buffer (current-buffer) nil (not maplev-xemacsp))))))

;;}}}

;;}}}
;;{{{ Frames

;; The following is a slightly modified version of
;; `mouse-tear-off-window' from mouse.el.

(defun maplev-tear-off-window ()
  "Delete the current window and create a new frame displaying its buffer."
  (interactive)
  (if (one-window-p t 'here)
      (message "Only one window in frame.")
    (let* ((window (selected-window))
           (buf (window-buffer window))
           (frame (make-frame)))
      (select-frame frame)
      (switch-to-buffer buf)
      (delete-window window))))

;;}}}

(provide 'maplev)

;;; maplev.el ends here
