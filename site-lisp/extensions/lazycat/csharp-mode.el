;;; csharp-mode.el --- csharp-mode

;; Copyright (C) 2002-2003  Free Software Foundation, Inc.

;; Authors: 2002-2003 Dennis Haney <davh@davh.dk>
;;          2000 Brad Merrill <zbrad@cybercom.net>
;; Keywords: c, languages, oop
;; Version: 1.12
;; $Id: csharp-mode.el,v 1.23 2003/05/05 19:10:49 davh Exp $

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Version one was ripped from version 3.0 from
;; http://www.cybercom.net/~zbrad/DotNet/Emacs/

;; Newest version of this can be found here:
;; http://davh.dk/script/

;; To activate put this in your .emacs:

;; (autoload 'csharp-mode "csharp-mode" 
;;   "Major mode for editing C# code." t)
;; (setq auto-mode-alist (cons '( "\\.cs\\'" . csharp-mode ) auto-mode-alist ))

;; BUGS:

;; A lot, most has to do with cc-mode hardcoding things like eg. how enum looks like.

;; TODO:

;; Make own font-lock-match-csharp-style-declaration-item-and-skip-to-next

;; $Log: csharp-mode.el,v $
;; Revision 1.23  2003/05/05 19:10:49  davh
;; Hmm. Apparently "get" and "set" are not keywords?!?
;;
;; Revision 1.22  2003/05/04 21:13:32  davh
;; fix minor speliing error causing try/catch to indent wrong
;;
;; Revision 1.21  2003/05/04 21:08:15  davh
;; # is now a comment starter
;;
;; Revision 1.20  2003/05/04 20:00:23  davh
;; Forgot to actually remove usage of the var i removed
;;
;; Revision 1.19  2003/05/04 19:52:43  davh
;; correct builtin types
;;
;; Revision 1.18  2003/05/04 19:45:02  davh
;; Updated keywords et al.
;;
;; Revision 1.17  2003/05/04 18:46:38  davh
;; ARGG, never set c-offset-style properly. I do now. This means all
;; default has changed since I just copied what mine set it to (and
;; changed some of the csharp specific)
;; get and set are now indented properly and recognized as keywords
;;
;; Revision 1.16  2003/05/03 01:01:46  davh
;; Forgot to change version number
;;
;; Revision 1.15  2003/05/03 01:01:29  davh
;; updated syntactics. It should now detect namespace and classes correctly
;;
;; Revision 1.14  2003/05/01 19:24:35  davh
;; new keywords: "as" and "is"
;;
;; Revision 1.13  2003/01/08 21:25:06  davh
;; patch from Ryan Sammartino. Better preprocesser highlight and xemacs
;; font-lock fix for it too.
;;
;; Revision 1.12  2002/11/08 16:34:47  davh
;; faces are not vars, they are faces. repeat and cleanse
;; Added compilation regexp
;; Thanks to Martin Jul and Jarl Friis (I should have guessed, XEmacs is involved) for the above
;; c++ regexp is better suited than the c regexp for skipping, make one for csharp when I have time
;; Fixed typo
;;
;; Revision 1.11  2002/11/08 11:00:21  davh
;; Fix compile warning in Xemacs and fix another ancient font-lock problem with xemacs
;;
;; Revision 1.10  2002/11/07 16:01:21  davh
;; Fix byte-compile plus forgot to update version last time.
;;
;; Revision 1.9  2002/11/07 15:54:37  davh
;; Fix problem with ancient (ie. the current) Xemacs font-lock mode
;;
;; Revision 1.8  2002/09/23 20:48:27  davh
;; A few details. After checking what cc-mode really thinks it looks like
;; a lot is actually broken, but few ever notices.
;;
;; Revision 1.7  2002/07/06 13:58:55  davh
;; fixing problem with:
;;
;; namespace Foobar {
;;   using System;
;;
;; public class Foobar {
;;   public static void Main() {
;;     Console.WriteLine ("Damn indention!");
;;   }
;; }
;; }
;;
;; versus
;;
;; namespace Foobar {
;;   using System;
;;
;;   class Foobar {
;;     public static void Main() {
;;       Console.WriteLine ("Damn indention!");
;;     }
;;   }
;; }
;;
;; Revision 1.6  2002/06/09 18:06:01  davh
;; "params" added as keyword
;;
;; Revision 1.5  2002/05/31 18:03:45  davh
;; Didnt get Merrills name with :(
;;
;; Revision 1.4  2002/05/31 17:48:00  davh
;; I moved a comment
;;
;; Revision 1.3  2002/05/31 17:45:31  davh
;; Added website 'n log
;;
;; 

;;; Code:

(provide 'csharp-mode)

(require 'compile)
(require 'cc-mode)
(require 'font-lock)

(eval-when-compile
  (require 'compile)
  (require 'cc-mode)
  (require 'font-lock)
)

;;xemacs fix
(if (not (facep 'font-lock-constant-face))
    (copy-face 'font-lock-reference-face 'font-lock-constant-face))
(if (not (facep 'font-lock-builtin-face))
    (copy-face 'font-lock-keyword-face 'font-lock-builtin-face))

;; Primitive type keywords.
(defconst c-Csharp-primitive-type-kwds
  (eval-when-compile
    (regexp-opt '(
                  "bool" "byte" "char" "decimal"
                  "double" "float" "int" "long"
                  "object" "sbyte" "short" "string"
                  "uint" "ulong" "ushort"
                  ))))

;; Declaration specifier keywords.
(defconst c-Csharp-specifier-kwds
  ;;stole from c and c++, may be completly bogus
  (eval-when-compile
    (regexp-opt '(
                  "const" "extern" "static" "override" "namespace"
                  "friend" "inline" "virtual"
                  ))))

;; Class/struct declaration keywords.
(defconst c-Csharp-class-kwds "class\\|struct")

;; Keywords introducing other declaration-level blocks.
(defconst c-Csharp-extra-toplevel-kwds "extern\\|namespace")

;; Keywords introducing other declaration-level constructs.
(defconst c-Csharp-other-decl-kwds
  ;;stole from c and c++, may be completly bogus
  (eval-when-compile
    (regexp-opt '(
                  "enum" "typedef" "template"
                  ))))

;; Protection label keywords in classes.
(defconst c-Csharp-protection-kwds
  (eval-when-compile
    (regexp-opt '(
                  "new" "public" "protected" "internal" "private"
                  "static" "virtual" "override" "abstract" "sealed"
                  "extern" "unsafe"
                  ))))

;; Statement keywords followed directly by a block.
(defconst c-Csharp-block-stmt-1-kwds
  (eval-when-compile
    (regexp-opt '(
                  "do" "else" "try" "finally" "get" "set"
                  ))))

;; Statement keywords followed by a paren sexp and then by a block.
(defconst c-Csharp-block-stmt-2-kwds
  (eval-when-compile
    (regexp-opt '(
                  "foreach" "for" "if"  "while" "switch"
                  "catch" 
                  ))))

;; Statement keywords followed by an expression or nothing.
(defconst c-Csharp-simple-stmt-kwds
  ;;stole from c and java, may be completly bogus
  "break\\|continue\\|goto\\|return\\|throw")

;; Keywords introducing labels in blocks.
(defconst c-Csharp-label-kwds
  ;;stole from c, may be completly bogus
  "case\\|default")

;; Keywords that can occur anywhere in expressions.
(defconst c-Csharp-expr-kwds
  ;;may be completly bogus and missing a lot
  (eval-when-compile
    (regexp-opt '(
                  "sizeof" "typeof" "new" "this" "throw"
                  ))))

;; All keywords.
(defconst c-Csharp-keywords
  (concat c-Csharp-primitive-type-kwds "\\|"
          c-Csharp-specifier-kwds "\\|"
          c-Csharp-class-kwds "\\|"
          c-Csharp-extra-toplevel-kwds "\\|"
          c-Csharp-other-decl-kwds "\\|"
          c-Csharp-protection-kwds "\\|"
          c-Csharp-block-stmt-1-kwds "\\|"
          c-Csharp-block-stmt-2-kwds "\\|"
          c-Csharp-simple-stmt-kwds "\\|"
          c-Csharp-label-kwds "\\|"
          c-Csharp-expr-kwds
          ))

(defconst c-Csharp-attrib-key (concat "\["
                                      "\\s *" c-symbol-key "\\(\\s *([^)]*)\\)?\\s *"
                                      "\\(?:,"
                                      "\\s *" c-symbol-key "\\(\\s *([^)]*)\\)?\\s *"
                                      "\\)*\]"))
(defconst c-Csharp-protection-key
  (concat "\\<" (c-paren-re c-Csharp-protection-kwds) "\\>"))

;; Regexps introducing class definitions.
(defconst c-Csharp-class-key
  (concat
   "\\(" c-Csharp-attrib-key "\\s +\\)*"
   "\\(" c-Csharp-protection-key "\\s +\\)*"
   "\\(" c-Csharp-class-kwds "\\)\\s +"
   c-symbol-key                       ;name of the class
   "\\(\\s *:\\s *" c-symbol-key	;maybe followed by parent(s)
   "\\(\\s *,\\s *" c-symbol-key "\\)*"
   "\\)?"
   ))

(defconst c-Csharp-extra-toplevel-key (c-paren-re c-Csharp-extra-toplevel-kwds))

;; regexp describing access protection clauses.  language specific
(defconst c-Csharp-access-key 
  (concat "\\<\\(" c-Csharp-protection-kwds "\\)\\>[ \t]*:"
          ))

;; keywords introducing conditional blocks
(defconst c-Csharp-conditional-key 
  (c-identifier-re
   (concat c-Csharp-block-stmt-1-kwds "\\|" c-Csharp-block-stmt-2-kwds)
   ))

;; keywords describing method definition introductions
(defconst c-Csharp-method-key
  (concat
   "\\(" c-Csharp-attrib-key "\\s *\\)*"
   "\\(\\<" (c-paren-re c-Csharp-specifier-kwds) "\\>\\s +\\)*"
   c-symbol-key "\\s *"               ;return type
   c-symbol-key                       ;name
   "\\([.]" c-symbol-key "\\)*"       ;or class.class.name
   "\\s *\\(([^)]*)\\)"               ;params
   ))

(defconst c-Csharp-comment-start-regexp "\\(?:#\\(?:end\\)?region\\>\\|/[/*]\\)")

(defconst c-Csharp-xmldoc-paragraph-start
  "@\\(remarks\\|summary\\|exception\\|param\\|returns\\|see\\|c\\)")

(defconst c-Csharp-inexpr-class-key "\\<new\\>")

;; Support for Csharp

(defvar csharp-mode-abbrev-table nil
  "Abbreviation table used in csharp-mode buffers.")
(define-abbrev-table 'csharp-mode-abbrev-table ())

(defvar csharp-mode-map ()
  "Keymap used in csharp-mode buffers.")
(if csharp-mode-map
    nil
  (setq csharp-mode-map (c-make-inherited-keymap))
  ;; add bindings which are only useful for Csharp
  )

(defvar csharp-mode-syntax-table nil
  "Syntax table used in csharp-mode buffers.")

(if csharp-mode-syntax-table
    ()
  (setq csharp-mode-syntax-table (make-syntax-table))
  (c-populate-syntax-table csharp-mode-syntax-table)
;; @keyword can be used
  (modify-syntax-entry ?@ "_" csharp-mode-syntax-table)
;; # start comment
  (modify-syntax-entry ?\# "< b" csharp-mode-syntax-table)

;; try to treat "." as part of the sexp
;(modify-syntax-entry ?. "_" csharp-mode-syntax-table)
;; try to treat "." as part of the word
;(modify-syntax-entry ?. "w" csharp-mode-syntax-table)
  )

(easy-menu-define c-csharp-menu csharp-mode-map "C# Mode Commands"
		  (c-mode-menu "C#"))

(defcustom csharp-mode-hook nil
  "*Hook called by `csharp-mode'."
  :type 'hook
  :group 'c)

(defcustom csharp-font-lock-extra-types
  '()
  "*List of extra types to fontify in C# mode.
Each list item should be a regexp not containing word-delimiters.
For example, a value of (\"System\") means the word string is treated as a type
name.

The value of this variable is used when Font Lock mode is turned on."
  :type 'font-lock-extra-types-widget
  :group 'font-lock-extra-types)

(defconst csharp-font-lock-keywords-1 nil
  "Subdued level highlighting for C# mode.")

(defconst csharp-font-lock-keywords-2 nil
  "Medium level highlighting for C# mode.
See also `csharp-font-lock-extra-types'.")

(defconst csharp-font-lock-keywords-3 nil
  "Gaudy level highlighting for C# mode.
See also `csharp-font-lock-extra-types'.")

(let* ((csharp-keywords
	(eval-when-compile
	  (regexp-opt
	   '(
             "abstract" "as" "base" "bool" "break"
             "byte" "case" "catch" "char" "checked"
             "class" "const" "continue" "decimal" "default"
             "delegate" "do" "double" "else" "enum"
             "event" "explicit" "extern" "false" "finally"
             "fixed" "float" "for" "foreach" "goto"
             "if" "implicit" "in" "int" "interface"
             "internal" "is" "lock" "long" "namespace"
             "new" "null" "object" "operator" "out"
             "override" "params" "private" "protected" "public"
             "readonly" "ref" "return" "sbyte" "sealed"
             "short" "sizeof" "stackalloc" "static" "string"
             "struct" "switch" "this" "throw" "true"
             "try" "typeof" "uint" "ulong" "unchecked"
             "unsafe" "ushort" "using" "virtual" "void"
             "volatile" "while"
	     ) t)))
       (csharp-operators
	(eval-when-compile
	  (regexp-opt
	   '(
             ;;unary
             "+" "-" "!" "~" "++" "--" "true" "false"
             ;binary
             "+" "-" "*" "/" "%" "&" "|" "^" "<<" ">>" "==" "!=" ">"
             "<" ">=" "<="
             ))))
       ;;
       ;; Classes immediately followed by an object name.
       (csharp-type-names
	`(mapconcat 'identity
	  (cons 
	   (,@ c-Csharp-primitive-type-kwds)
	   csharp-font-lock-extra-types)
	  "\\|"))
       (csharp-type-names-depth `(regexp-opt-depth (,@ csharp-type-names)))
       )
 (setq csharp-font-lock-keywords-1
  (list
   ;;
   ;; Fontify symbol names in #elif or #if ... defined preprocessor directives.
   '("^[ \t]*\\(#[ \t]*\\(?:elif\\|if\\|endif\\|define\\)\\)[ \t]*\\(\\sw+\\)?"
     (1 font-lock-preprocessor-face) (2 font-lock-variable-name-face prepend t)) 

   ;; #region comment
   '("^[ \t]*#\\(\\(?:end\\)?region[ \t]*.*\\)" 1 font-lock-warning-face prepend)

   ;; Fontify class names.
   '("\\<\\(class\\|struct\\|enum\\|interface\\)\\>[ \t]*\\(\\sw+\\)?"
     (1 font-lock-keyword-face) (2 font-lock-type-face nil t))
   )
  )

 (setq csharp-font-lock-keywords-2
  (append csharp-font-lock-keywords-1
   (list
    ;;
    ;; Fontify class names.
    `(eval .
      (cons (concat "\\<\\(" (,@ csharp-type-names) "\\)\\>")
	    '(1 font-lock-type-face)
	    ))
    ;;
    ;; Fontify get/set operators.
    (list (concat "\\(\\<get\\|set\\>\\)")
	  '(1 font-lock-function-name-face)
          )
    ;;
    ;; Fontify operator overloading.
    (list (concat "\\<"
		  "\\(\\(?:implicit\\|explicit\\)?\\)"
		  "[ \t]*\\(operator\\)"
		  "\\>[ \t]*"
		  "\\(" csharp-operators "\\)?")
	  '(1 font-lock-keyword-face)
	  '(2 font-lock-function-name-face nil t)
	  '(3 font-lock-builtin-face nil t)
          )
    ;;
    ;; Fontify all builtin keywords (except below).
    (concat "\\<\\(" csharp-keywords "\\)\\>")
    ;;
    ;; Fontify keywords and targets, and case default/goto tags.
    (list "\\<\\(break\\|case\\|continue\\|goto\\)\\>[ \t]*\\(-?\\sw+\\)?"
	  '(1 font-lock-keyword-face)
	  '(2 font-lock-constant-face nil t)
	  )
    ;;
    ;; This must come after the one for keywords and targets.
    '(":" ("^[ \t]*\\(\\sw+\\)[ \t]*:[ \t]*$"
	   (beginning-of-line) (end-of-line)
	   (1 font-lock-constant-face)
	   ))
    ;;
    ;; Fontify all constants.
    '("\\<\\(false\\|null\\|true\\)\\>" . font-lock-constant-face)
    )))

 (setq csharp-font-lock-keywords-3
  (append csharp-font-lock-keywords-2
   ;;
   ;; More complicated regexps for more complete highlighting for types.
   ;; (Note: Need to revisit the styling here.)
   (list
    ;;
    ;; Fontify random types immediately followed by an item or items.
    `(eval .
      (list (concat "\\<\\(" (,@ csharp-type-names) "\\)\\>"
		    "\\([ \t]*\\[[ \t]*\\]\\)*"
		    "\\([ \t]*\\sw\\)")
	    ;; Fontify each declaration item.
	    (list 'font-lock-match-c++-style-declaration-item-and-skip-to-next
		  ;; Start with point after all type specifiers.
		  (list 'goto-char (list 'or
					 (list 'match-beginning
					       (+ (,@ csharp-type-names-depth) 2))
					 '(match-end 1)))
		  ;; Finish with point after first type specifier.
		  '(goto-char (match-end 1))
		  ;; Fontify as a variable or function name.
		  '(1 (if (match-beginning 2)
			  font-lock-function-name-face
			font-lock-variable-name-face)))))
    ;;
    ;; Fontify structures, or typedef names, plus their items.
    '("\\(}\\)[ \t*]*\\sw"
      (font-lock-match-c++-style-declaration-item-and-skip-to-next
       (goto-char (match-end 1)) nil
       (1 font-lock-type-face)))
    ;;
    ;; Fontify anything at beginning of line as a declaration or definition.
    '("^\\(\\sw+\\)\\>\\([ \t*]+\\sw+\\>\\)*"
      (1 font-lock-type-face)
      (font-lock-match-c++-style-declaration-item-and-skip-to-next
       (goto-char (or (match-beginning 2) (match-end 1))) nil
       (1 (if (match-beginning 2)
	      font-lock-function-name-face
	    font-lock-variable-name-face))))
    )))
 )

(defvar csharp-font-lock-keywords csharp-font-lock-keywords-3
  "Default expressions to highlight in C# mode.
See also `csharp-font-lock-extra-types'.")

(defvar cc-imenu-csharp-generic-expression
  cc-imenu-c++-generic-expression
  "Imenu generic expression for C# mode.  See `imenu-generic-expression'.")

(c-add-style "C#"
 '("Java"
   (c-basic-offset . 2)
   (c-comment-only-line-offset . (0 . 0))
   (c-offsets-alist 
    . (
       (access-label . -)
       (arglist-close . c-lineup-arglist)
       (arglist-cont . 0)
       (arglist-cont-nonempty . c-lineup-arglist)
       (arglist-intro . c-lineup-arglist-intro-after-paren)
       (block-close . 0)
       (block-open . 0)
       (brace-entry-open . 0)
       (brace-list-close . 0)
       (brace-list-entry . 0)
       (brace-list-intro . +)
       (brace-list-open . +)
       (c . c-lineup-C-comments)
       (case-label . 0)
       (catch-clause . 0)
       (class-close . 0)
       (class-open . 0)
       (comment-intro . c-lineup-comment)
       (cpp-macro . 0)
       (cpp-macro-cont . c-lineup-dont-change)
       (defun-block-intro . +)
       (defun-close . 0)
       (defun-open . 0)
       (do-while-closure . 0)
       (else-clause . 0)
       (extern-lang-close . 0)
       (extern-lang-open . 0)
       (friend . 0)
       (func-decl-cont . +)
       (inclass . +)
       (inexpr-class . +)
       (inexpr-statement . 0)
       (inextern-lang . +)
       (inher-cont . c-lineup-multi-inher)
       (inher-intro . +)
       (inlambda . c-lineup-inexpr-block)
       (inline-close . 0)
       (inline-open . 0)
       (innamespace . +)
       (knr-argdecl . 0)
       (knr-argdecl-intro . 5)
       (label . 0)
       (lambda-intro-cont . +)
       (member-init-cont . c-lineup-multi-inher)
       (member-init-intro . +)
       (namespace-close . 0)
       (namespace-open . 0)
       (objc-method-args-cont . c-lineup-ObjC-method-args)
       (objc-method-call-cont . c-lineup-ObjC-method-call)
       (objc-method-intro . [0])
       (statement . 0)
       (statement-block-intro . +)
       (statement-case-intro . +)
       (statement-case-open . +)
       (statement-cont . +)
       (stream-op . c-lineup-streamop)
       (string . c-lineup-dont-change)
       (substatement . +)
       (substatement-open . 0)
       (template-args-cont c-lineup-template-args +)
       (topmost-intro . 0)
       (topmost-intro-cont . 0)
       ))
   ))

;;;###autoload
(defun csharp-mode ()
  "Major mode for editing C# code.
To submit a problem report, enter `\\[c-submit-bug-report]' from a
csharp-mode buffer.  This automatically sets up a mail buffer with
version information already added.  You just need to add a description
of the problem, including a reproducible test case and send the
message.

To see what version of CC Mode you are running, enter `\\[c-version]'.

The hook variable `csharp-mode-hook' is run with no args, if that value
is bound and has a non-nil value.  Also the common hook
`c-mode-common-hook' is run first.  Note that this mode automatically
sets the \"C#\" style before calling any hooks so be careful if you
set styles in `c-mode-common-hook'.

Key bindings:
\\{csharp-mode-map}"
  (interactive)
  (c-initialize-cc-mode)
  (kill-all-local-variables)
  (set-syntax-table csharp-mode-syntax-table)
  (setq major-mode 'csharp-mode
 	mode-name "C#"
 	local-abbrev-table csharp-mode-abbrev-table)
  (use-local-map csharp-mode-map)
  (if (not (assq 'csharp-mode c-default-style))
      (setq c-default-style (cons '(csharp-mode . "C#") c-default-style)))
  (c-common-init)
  (make-local-variable 'comment-start)
  (make-local-variable 'comment-end)
  (setq comment-start "// "
 	comment-end   ""
 	c-conditional-key c-Csharp-conditional-key
 	c-comment-start-regexp c-Csharp-comment-start-regexp
  	c-class-key c-Csharp-class-key
	c-extra-toplevel-key c-Csharp-extra-toplevel-key
;	c-method-key c-Csharp-method-key
 	c-baseclass-key nil
	c-recognize-knr-p nil
 	c-access-key  c-Csharp-access-key
	c-inexpr-class-key nil
	imenu-generic-expression cc-imenu-csharp-generic-expression
	imenu-case-fold-search nil
	)
  ;; Font lock
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults
	'((csharp-font-lock-keywords csharp-font-lock-keywords-1
				     csharp-font-lock-keywords-2 csharp-font-lock-keywords-3)
	  nil nil ((?_ . "w") (?$ . "w")) nil
	  (font-lock-mark-block-function . mark-defun)))
  ;; Compile
  (make-local-variable 'compilation-error-regexp-alist)
  (setq compilation-error-regexp-alist
	(append 
	 '(
	   ;;C# Compiler
	   ;;t.cs(6,18): error SC1006: Name of constructor must match name of class
	   ("\\(\\([_a-zA-Z]:\\)?[^:(\t\n]+\\)(\\([0-9]+\\)[,]\\([0-9]+\\)): \\(error\\|warning\\) CS[0-9]+:" 1 3 4))
	 compilation-error-regexp-alist))
  ;; hocks
  (run-hooks 'c-mode-common-hook)
  (run-hooks 'csharp-mode-hook)
  (c-update-modeline))

;;; csharp-mode.el ends here
