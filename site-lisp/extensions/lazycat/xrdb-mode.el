;;; xrdb-mode.el --- mode for editing X resource database files

;; Copyright (C) 1998,1999,2000 Free Software Foundation, Inc.

;; Author:        1994-2003 Barry A. Warsaw
;; Maintainer:    barry@python.org
;; Created:       May 1994
;; Keywords:      data languages

(defconst xrdb-version "2.31"
  "`xrdb-mode' version number.")

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;; This file provides a major mode for editing X resource database
;; files.  It includes font-lock definitions and commands for
;; controlling indentation, re-indenting by subdivisions, and loading
;; and merging into the the resource database.
;;
;; To use, put the following in your .emacs:
;;
;; (autoload 'xrdb-mode "xrdb-mode" "Mode for editing X resource files" t)
;;
;; You may also want something like:
;;
;; (setq auto-mode-alist
;;       (append '(("\\.Xdefaults$"    . xrdb-mode)
;;                 ("\\.Xenvironment$" . xrdb-mode)
;;                 ("\\.Xresources$"   . xrdb-mode)
;;                 ("*.\\.ad$"         . xrdb-mode)
;;                 )
;;               auto-mode-alist))

;;; Credits:
;;
;; The database merge feature was inspired by Joel N. Weber II.
;;
;; The canonical Web site for xrdb-mode is
;; <http://www.python.org/emacs/>

;;; Code:
(require 'custom)



(defgroup xrdb nil
  "Support for editing X resource database files"
  :group 'languages)

(defcustom xrdb-mode-hook nil
  "*Hook to be run when `xrdb-mode' is entered."
  :type 'hook
  :group 'xrdb)

(defcustom xrdb-subdivide-by 'paragraph
  "*Default alignment subdivision when re-indenting a region or buffer.
This variable controls how much of the buffer is searched to find a
goal column on which to align.  Every non-comment line in the region
defined by this variable is scanned for the first `:' character on the
line, and this character's column is the line's goal column.  The
rightmost line goal column in the region is taken as the region's goal
column.

This variable can take one of the following symbol values:

 `buffer'    - All lines in the buffer are scanned.  This is the
               slowest option.

 `paragraph' - All lines in the paragraph are scanned.  Paragraphs
               are delimited by blank lines, comment lines, and page
               delimiters.

 `page'      - All lines in the page are scanned.  Pages are delimited
               with `page-delimiter', usually ^L (control-L).

 `line'      - Only the previous non-comment line is scanned.  This is
               the fastest method.

This variable is used by the various indentation commands, and can be
overridden in those commands by using \\[universal-argument]."
  :type '(radio (const :tag "Do not subdivide buffer" buffer)
                (const :tag "Subdivide by paragraphs" paragraph)
                (const :tag "Subdivide by pages" page)
                (const :tag "Each line is independent" line))
  :group 'xrdb)

(defcustom xrdb-compress-whitespace nil
  "*Collapse all whitespace to a single space after insertion of `:'."
  :type 'boolean
  :group 'xrdb)

(defcustom xrdb-program "xrdb"
  "*Program to run to load or merge resources in the X resource database."
  :type 'string
  :group 'xrdb)

(defcustom xrdb-program-args '("-merge")
  "*List of string arguments to pass to `xrdb-program'."
  :type '(repeat string)
  :group 'xrdb)

(defvar xrdb-master-file nil
  "If non-nil, merge in the named file instead of the buffer's file.
The intent is to allow you to set this variable in the file's local
variable section, e.g.:

    ! Local Variables:
    ! xrdb-master-file: \"Xdefaults\"
    ! End:

so that typing \\[xrdb-database-merge-buffer-or-region] in that buffer
merges the named master file instead of the buffer's file.  Note that
if the file name has a relative path, the `default-directory' for the
buffer is prepended to come up with a file name.

You may also want to set `xrdb-program-args' in the local variables
section as well.")
(make-variable-buffer-local 'xrdb-master-file)


;; Non-user customizable
(defconst xrdb-comment-re "^[ \t]*[!]"
  "Regular expression describing the beginning of a comment line.")



;; utilities
(defun xrdb-point (position)
  ;; Returns the value of point at certain commonly referenced POSITIONs.
  ;; POSITION can be one of the following symbols:
  ;;
  ;; bol  -- beginning of line
  ;; eol  -- end of line
  ;; bod  -- beginning of defun
  ;; boi  -- back to indentation
  ;; ionl -- indentation of next line
  ;; iopl -- indentation of previous line
  ;; bonl -- beginning of next line
  ;; bopl -- beginning of previous line
  ;; bop  -- beginning of paragraph
  ;; eop  -- end of paragraph
  ;; bopg -- beginning of page
  ;; eopg -- end of page
  ;;
  ;; This function does not modify point or mark.
  (let ((here (point)))
    (cond
     ((eq position 'bod)  (beginning-of-defun))
     ((eq position 'bol)  (beginning-of-line))
     ((eq position 'eol)  (end-of-line))
     ((eq position 'boi)  (back-to-indentation))
     ((eq position 'bonl) (forward-line 1))
     ((eq position 'bopl) (forward-line -1))
     ((eq position 'bop)  (forward-paragraph -1))
     ((eq position 'eop)  (forward-paragraph 1))
     ((eq position 'bopg)  (forward-page -1))
     ((eq position 'eopg)  (forward-page 1))
     (t
      (error "unknown buffer position requested: %s" position)))
    (prog1
        (point)
      (goto-char here))
    ))

(defmacro xrdb-safe (&rest body)
  ;; safely execute BODY, return nil if an error occurred
  (` (condition-case nil
         (progn (,@ body))
       (error nil))))

(defsubst xrdb-skip-to-separator ()
  ;; skip forward from the beginning of the line to the separator
  ;; character as given by xrdb-separator-char. Returns t if the
  ;; char was found, otherwise, nil.
  (beginning-of-line)
  (skip-chars-forward "^:" (xrdb-point 'eol))
  (and (eq (char-after) ?:)
       (current-column)))

(defsubst xrdb-in-comment-p (&optional lim)
  (let* ((lim (or lim (xrdb-point 'bod)))
         (state (parse-partial-sexp lim (point))))
    (nth 4 state)))

(defsubst xrdb-boi-col ()
  (let ((here (point)))
    (goto-char (xrdb-point 'boi))
    (prog1
        (current-column)
      (goto-char here))))

(defvar xrdb-prompt-history nil)

(defun xrdb-prompt-for-subdivision ()
  (let ((options '(("buffer" . buffer)
                   ("paragraphs" . paragraph)
                   ("pages" . page)
                   ("lines" . line)))
        (completion-ignore-case t))
    (cdr (assoc
          (completing-read "Subdivide alignment by? " options nil t
                           (cons (format "%s" xrdb-subdivide-by) 0)
                           'xrdb-prompt-history)
          options))))


;; commands
(defun xrdb-electric-separator (arg)
  "Insert a colon, and possibly indent line.
Numeric argument inserts that many separators.  If the numeric
argument is not given, or is 1, and the separator is not inserted in a
comment, then the line is indented according to `xrdb-subdivide-by'."
  (interactive "P")
  (self-insert-command (prefix-numeric-value arg))
  ;; only do electric behavior if arg is not given
  (or arg
      (xrdb-in-comment-p)
      (xrdb-indent-line))
  ;; compress whitespace
  (and xrdb-compress-whitespace
       (just-one-space)))

(defun xrdb-electric-bang (arg)
  "Insert an exclamation point to start a comment.
Numeric argument inserts that many exclamation characters.  If the
numeric argument is not given, or is 1, and the bang character is the
first character on a line, the line is indented to column zero."
  (interactive "P")
  (let ((how-many (prefix-numeric-value arg)))
    (self-insert-command how-many)
    (save-excursion
      (if (and (= how-many 1)
               (xrdb-in-comment-p)
               (memq (char-before (xrdb-point 'boi)) '(?\n nil)))
          (indent-line-to 0)))
    ))


(defun xrdb-indent-line (&optional arg)
  "Align the current line according to `xrdb-subdivide-by'.
With optional \\[universal-argument], prompt for subdivision."
  (interactive "P")
  (xrdb-align-to-column
   (xrdb-guess-goal-column (if arg
                               (xrdb-prompt-for-subdivision)
                             xrdb-subdivide-by))
   (xrdb-point 'bol)
   (xrdb-point 'bonl)))

(defun xrdb-indent-region (start end &optional arg)
  "Indent all lines in the region according to `xrdb-subdivide-by'.
With optional \\[universal-argument], prompt for subdivision."
  (interactive "r\nP")
  (xrdb-align-to-column
   (xrdb-guess-goal-column (if arg
                               (xrdb-prompt-for-subdivision)
                             xrdb-subdivide-by))
   start end))

(defun xrdb-indent-page (&optional arg)
  "Indent all lines in the page according to `xrdb-subdivide-by'.
With optional \\[universal-argument], prompt for subdivision."
  (interactive "P")
  (xrdb-align-to-column
   (xrdb-guess-goal-column (if arg
                               (xrdb-prompt-for-subdivision)
                             xrdb-subdivide-by))
   (xrdb-point 'bopg)
   (xrdb-point 'eopg)))

(defun xrdb-indent-paragraph (&optional arg)
  "Indent all lines in the paragraph according to `xrdb-subdivide-by'.
With optional \\[universal-argument], prompt for subdivision."
  (interactive "P")
  (xrdb-align-to-column
   (xrdb-guess-goal-column (if arg
                               (xrdb-prompt-for-subdivision)
                             xrdb-subdivide-by))
   (xrdb-point 'bop)
   (xrdb-point 'eop)))

(defun xrdb-indent-buffer (&optional arg)
  "Indent all lines in the buffer according to `xrdb-subdivide-by'.
With optional \\[universal-argument], prompt for subdivision."
  (interactive "P")
  (let ((subdivide-by (if arg
                          (xrdb-prompt-for-subdivision)
                        xrdb-subdivide-by)))
    (save-excursion
      (beginning-of-buffer)
      (if (eq subdivide-by 'buffer)
          (xrdb-align-to-column (xrdb-guess-goal-column 'buffer)
                                (point-min) (point-max))
        (let (mvfwdfunc indentfunc)
          (cond
           ((eq subdivide-by 'paragraph)
            (setq mvfwdfunc 'forward-paragraph
                  indentfunc 'xrdb-indent-paragraph))
           ((eq subdivide-by 'page)
            (setq mvfwdfunc 'forward-page
                  indentfunc 'xrdb-indent-page))
           ((eq subdivide-by 'line)
            (setq mvfwdfunc 'forward-line
                  indentfunc 'xrdb-indent-page))
           (t (error "Illegal alignment subdivision: %s" subdivide-by))
           )
          (while (< (point) (point-max))
            (funcall indentfunc)
            (funcall mvfwdfunc 1))
          )))))


;; internal alignment functions
(defun xrdb-align-to-column (goalcol &optional start end)
  (let ((start (or start (xrdb-point 'bol)))
        (end (or end (xrdb-point 'bonl))))
    (save-excursion
      (save-restriction
        (narrow-to-region start end)
        (beginning-of-buffer)
        (while (< (point) (point-max))
          (if (and (not (looking-at xrdb-comment-re))
                   (xrdb-skip-to-separator))
              (indent-line-to (max 0 (+ goalcol
                                        (- (current-column))
                                        (xrdb-boi-col))
                                   )))
          (forward-line 1))
        ))))

(defun xrdb-guess-goal-column (subdivide-by)
  ;; Returns the goal column of the current line based on SUBDIVIDE-BY,
  ;; which can be any value allowed by `xrdb-subdivide-by'.
  (let ((here (point))
        (goalcol 0))
    (save-restriction
      (cond
       ((eq subdivide-by 'line)
        (while (and (zerop (forward-line -1))
                    (or (looking-at xrdb-comment-re)
                        (not (xrdb-skip-to-separator)))))
        ;; maybe we didn't find one
        (if (not (xrdb-skip-to-separator))
            (goto-char here))
        (narrow-to-region (xrdb-point 'bol) (xrdb-point 'bonl)))
       ((eq subdivide-by 'page)
        (narrow-to-page))
       ((eq subdivide-by 'paragraph)
        (narrow-to-region (xrdb-point 'bop) (xrdb-point 'eop)))
       ((eq subdivide-by 'buffer))
       (t (error "Illegal alignment subdivision: %s" subdivide-by)))
      (goto-char (point-min))
      (while (< (point) (point-max))
        (if (and (not (looking-at xrdb-comment-re))
                 (xrdb-skip-to-separator))
            (setq goalcol (max goalcol (- (current-column) (xrdb-boi-col)))))
        (forward-line 1)))
    (goto-char here)
    goalcol))



;; major-mode stuff
(defvar xrdb-mode-abbrev-table nil
  "Abbreviation table used in `xrdb-mode' buffers.")
(define-abbrev-table 'xrdb-mode-abbrev-table ())


(defvar xrdb-mode-syntax-table nil
  "Syntax table used in `xrdb-mode' buffers.")
(if xrdb-mode-syntax-table
    nil
  (setq xrdb-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?!  "<"    xrdb-mode-syntax-table)
  (modify-syntax-entry ?\\ "\\"   xrdb-mode-syntax-table)
  (modify-syntax-entry ?\n ">"    xrdb-mode-syntax-table)
  (modify-syntax-entry ?/  ". 14" xrdb-mode-syntax-table)
  (modify-syntax-entry ?*  "_ 23" xrdb-mode-syntax-table)
  (modify-syntax-entry ?.  "_"    xrdb-mode-syntax-table)
  (modify-syntax-entry ?#  "_"    xrdb-mode-syntax-table)
  (modify-syntax-entry ??  "_"    xrdb-mode-syntax-table)
  (modify-syntax-entry ?<  "("    xrdb-mode-syntax-table)
  (modify-syntax-entry ?>  ")"    xrdb-mode-syntax-table)
  )


(defvar xrdb-mode-map ()
  "Keymap used in `xrdb-mode' buffers.")
(if xrdb-mode-map
    ()
  (setq xrdb-mode-map (make-sparse-keymap))
  ;; make the separator key electric
  (define-key xrdb-mode-map ":"        'xrdb-electric-separator)
  (define-key xrdb-mode-map "!"        'xrdb-electric-bang)
  (define-key xrdb-mode-map "\t"       'xrdb-indent-line)
  (define-key xrdb-mode-map "\C-c\C-a" 'xrdb-indent-buffer)
  (define-key xrdb-mode-map "\C-c\C-b" 'xrdb-submit-bug-report)
  (define-key xrdb-mode-map "\C-c\C-c" 'xrdb-database-merge-buffer-or-region)
  (define-key xrdb-mode-map "\C-c\C-p" 'xrdb-indent-paragraph)
  (define-key xrdb-mode-map "\C-c\["   'xrdb-indent-page)
  (define-key xrdb-mode-map "\C-c\C-r" 'xrdb-indent-region)
  )

;;;###autoload
(defun xrdb-mode ()
  "Major mode for editing xrdb config files"
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table xrdb-mode-syntax-table)
  (setq major-mode 'xrdb-mode
        mode-name "xrdb"
        local-abbrev-table xrdb-mode-abbrev-table)
  (use-local-map xrdb-mode-map)
  (setq font-lock-defaults '(xrdb-font-lock-keywords))
  ;; local variables
  (make-local-variable 'parse-sexp-ignore-comments)
  (make-local-variable 'comment-start-skip)
  (make-local-variable 'comment-start)
  (make-local-variable 'comment-end)
  (make-local-variable 'paragraph-start)
  (make-local-variable 'paragraph-separate)
  (make-local-variable 'paragraph-ignore-fill-prefix)
  (make-local-variable 'indent-region-function)
  ;; now set their values
  (setq parse-sexp-ignore-comments t
        comment-start-skip "![ \t]*"
        comment-start "! "
        comment-end "")
  (setq indent-region-function 'xrdb-indent-region
        paragraph-ignore-fill-prefix t
        paragraph-start (concat "^[ \t]*$\\|^[ \t]*[!]\\|" page-delimiter)
        paragraph-separate paragraph-start)
  (run-hooks 'xrdb-mode-hook))



;; faces and font-locking
(defvar xrdb-option-name-face 'xrdb-option-name-face
  "Face for option name on a line in an X resource db file")

(defvar xrdb-option-value-face 'xrdb-option-value-face
  "Face for option value on a line in an X resource db file")

(make-face 'xrdb-option-name-face)
(make-face 'xrdb-option-value-face)

(defun xrdb-font-lock-mode-hook ()
  (or (face-differs-from-default-p 'xrdb-option-name-face)
      (copy-face 'font-lock-keyword-face 'xrdb-option-name-face))
  (or (face-differs-from-default-p 'xrdb-option-value-face)
      (copy-face 'font-lock-string-face 'xrdb-option-value-face))
  (remove-hook 'font-lock-mode-hook 'xrdb-font-lock-mode-hook))
(add-hook 'font-lock-mode-hook 'xrdb-font-lock-mode-hook)

(defvar xrdb-font-lock-keywords
  (list '("^[ \t]*\\([^\n:]*:\\)[ \t]*\\(.*\\)$"
          (1 xrdb-option-name-face)
          (2 xrdb-option-value-face)))
  "Additional expressions to highlight in X resource db mode.")
(put 'xrdb-mode 'font-lock-defaults '(xrdb-font-lock-keywords))



;; merging and manipulating the X resource database
(defun xrdb-database-merge-buffer-or-region (start end)
  "Merge the current buffer's resources into the X resource database.

`xrdb-program' is the program to actually call, with the arguments
specified in `xrdb-program-args'.  This latter can be set to do either
a merge or a load, etc.  Also, if the file local variable
`xrdb-master-file' is non-nil, then it is merged instead of the
buffer's file.

If the current region is active, it is merged instead of the buffer,
and this overrides any use of `xrdb-master-file'."
  (interactive
   ;; the idea here is that if the region is inactive, start and end
   ;; will be nil, if not passed in programmatically
   (list (xrdb-safe (and (mark) (region-beginning)))
         (xrdb-safe (and (mark) (region-end)))))
  (message "Merging with args: %s..." xrdb-program-args)
  (let ((outbuf (get-buffer-create "*Shell Command Output*")))
    ;; I prefer the XEmacs way of doing this, but this is the easiest
    ;; way to work in both XEmacs and Emacs.
    (with-current-buffer outbuf (erase-buffer))
    (cond
     ((and start end)
      (apply 'call-process-region start end xrdb-program nil outbuf t
             xrdb-program-args))
     (xrdb-master-file
      (apply 'call-process xrdb-program xrdb-master-file outbuf t
             xrdb-program-args))
     (t
      (apply 'call-process-region (point-min) (point-max) xrdb-program
             nil outbuf t xrdb-program-args)))
    (if (not (zerop (with-current-buffer outbuf (buffer-size))))
        (pop-to-buffer outbuf)))
  (message "Merging... done"))



;; submitting bug reports

(defconst xrdb-mode-help-address "tools-help@python.org"
  "Address for xrdb-mode bug reports.")

(defun xrdb-submit-bug-report ()
  "Submit via mail a bug report on xrdb-mode."
  (interactive)
  ;; load in reporter
  (require 'reporter)
  (let ((reporter-prompt-for-summary-p t)
        (varlist '(xrdb-subdivide-by
                   xrdb-mode-hook
                   xrdb-compress-whitespace
                   )))
    (and (if (y-or-n-p "Do you want to submit a report on xrdb-mode? ")
             t
           (message "")
           nil)
         (require 'reporter)
         (reporter-submit-bug-report
          xrdb-mode-help-address
          (format "xrdb-mode %s" xrdb-version)
          varlist nil nil "Dear Barry,")
         )))


(provide 'xrdb-mode)
;;; xrdb-mode.el ends here
