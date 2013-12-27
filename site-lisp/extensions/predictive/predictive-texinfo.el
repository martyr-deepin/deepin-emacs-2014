
;;; predictive-texinfo.el --- predictive mode Texinfo setup function


;; Copyright (C) 2008 Toby Cubitt

;; Author: Toby Cubitt <toby-predictive@dr-qubit.org>
;; Version: 0.1
;; Keywords: predictive, setup function, texinfo
;; URL: http://www.dr-qubit.org/emacs.php


;; This file is part of the Emacs Predictive Completion package.
;;
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
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
;; MA 02110-1301, USA.


;;; Change Log:
;;
;; Version 0,1
;; * initial release, borrowing heavily from predictive-latex.el



;;; Code:

(require 'predictive)
(require 'auto-overlay-word)
(require 'auto-overlay-line)
(require 'auto-overlay-self)
(require 'auto-overlay-nested)
(require 'predictive-latex)

(provide 'predictive-texinfo)
(add-to-list 'predictive-major-mode-alist
	     '(Texinfo-mode . predictive-setup-texinfo))
(add-to-list 'predictive-major-mode-alist
	     '(texinfo-mode . predictive-setup-texinfo))



;;;============================================================
;;;                  Customization Options

(defgroup predictive-texinfo nil
  "Predictive completion mode LaTeX support."
  :group 'predictive)


(defcustom predictive-texinfo-electric-environments nil
  "*When enabled, environment names are automatically synchronized
between \\begin{...} and \\end{...} commands."
  :group 'predictive-latex
  :type 'boolean)




;;;============================================================
;;;                       Variables

;; set up 'predictive-texinfo-word to be a `thing-at-point' symbol
(put 'predictive-texinfo-word 'forward-op 'predictive-texinfo-forward-word)
;; set up 'predictive-texinfo-node-word to be a `thing-at-point' symbol
(put 'predictive-texinfo-node-word 'forward-op
     'predictive-texinfo-node-forward-word)


;; variables holding dictionaries
(defvar predictive-texinfo-node-dict nil)
(make-variable-buffer-local 'predictive-texinfo-node-dict)
(defvar predictive-texinfo-local-texinfo-dict nil)
(make-variable-buffer-local 'predictive-texinfo-local-texinfo-dict)
(defvar predictive-texinfo-local-flag-dict nil)
(make-variable-buffer-local 'predictive-texinfo-local-flag-dict)


;; variables used to restore local settings of variables when predictive mode
;; is disabled in a Texinfo buffer
(defvar predictive-restore-main-dict nil)
(make-variable-buffer-local 'predictive-restore-main-dict)
(defvar predictive-restore-override-syntax-alist nil)
(make-variable-buffer-local 'predictive-restore-override-syntax-alist)


;; background color for certain auto-overlays to aid debugging
(defvar predictive-overlay-debug-colour nil)
(defvaralias 'predictive-overlay-debug-color 'predictive-overlay-debug-colour)


;; prevent bogus compiler warnings
(eval-when-compile
  (defvar dict-texinfo)
  (defvar dict-texinfo-env)
  (defvar dict-texinfo-indicating))


;; background color for certain auto-overlays to aid debugging
(defvar predictive-overlay-debug-colour nil)
(defvaralias 'predictive-overlay-debug-color 'predictive-overlay-debug-colour)




;;;=========================================================
;;;                  Setup function

(defun predictive-setup-texinfo (arg)
  "With a positive ARG, set up predictive mode for use with Texinfo major modes.
With a negative ARG, undo these changes. Called when predictive
mode is enabled via entry in `predictive-major-mode-alist'."

  (cond
   ;; ----- enabling Texinfo setup -----
   ((> arg 0)
    (catch 'load-fail
      ;; save overlays and unload regexp definitions along with buffer
      (add-hook 'after-save-hook
		(lambda ()
		  (auto-overlay-save-overlays
		   'predictive nil
		   predictive-auxiliary-file-location))
		nil t)
      (add-hook 'kill-buffer-hook
		(lambda ()
		  (when (not (buffer-modified-p))
		    (auto-overlay-save-overlays
		     'predictive nil
		     predictive-auxiliary-file-location)))
		nil t)

      ;; use Texinfo browser menu if first character of prefix is "\"
      (make-local-variable 'completion-menu)
      (setq completion-menu
	    (lambda (prefix completions)
	      (if (string= (substring prefix 0 1) "@")
		  (predictive-texinfo-construct-browser-menu
		   prefix completions)
		(completion-construct-menu prefix completions))
	      ))
      ;; save predictive-main-dict; restored when predictive mode is disabled
      (setq predictive-restore-main-dict predictive-main-dict)

      ;; load the Texinfo dictionaries
      (mapc (lambda (dic)
	      (unless (predictive-load-dict dic)
		(message "Failed to load %s" dic)
		(throw 'load-fail nil)))
	    ;; FIXME: should create and use separate dict-tex-math instead of
	    ;;        using dict-latex-math
	    '(dict-texinfo dict-texinfo-env dict-texinfo-indicating
			   dict-texinfo-math dict-latex-math))
      ;; load/create the node and local Texinfo dictionaries
      (predictive-load-auto-dict "texinfo-node")
      (predictive-load-auto-dict "texinfo-local-texinfo")
      (predictive-load-auto-dict "texinfo-local-flag")

      ;; add Texinfo dictionaries to main dictionary list
      (make-local-variable 'predictive-main-dict)
      (when (atom predictive-main-dict)
	(setq predictive-main-dict (list predictive-main-dict)))
      (setq predictive-main-dict
	    (append predictive-main-dict
		    '(dict-texinfo predictive-texinfo-local-texinfo-dict)))

      ;; delete any existing predictive auto-overlay regexps and load Texinfo
      ;; auto-overlay regexps
      (auto-overlay-unload-set 'predictive)
      (predictive-texinfo-load-regexps)

      ;; start the auto-overlays
      (auto-overlay-start 'predictive nil
			  predictive-auxiliary-file-location)

      ;; load the keybindings and related settings
      (predictive-texinfo-load-keybindings)
      ;; consider @ as start of a word
      (setq completion-word-thing 'predictive-texinfo-word)

      t))  ; indicate successful setup


   ((< arg 0)
    ;; stop predictive auto overlays
    (auto-overlay-stop 'predictive nil predictive-auxiliary-file-location)
    (auto-overlay-unload-set 'predictive)
    ;; restore predictive-main-dict to saved setting
    (kill-local-variable 'predictive-main-dict)
    (setq predictive-main-dict predictive-restore-main-dict)
    (kill-local-variable 'predictive-restore-main-dict)
    ;; restore `auto-completion-override-syntax-alist' to saved setting
    (kill-local-variable 'auto-completion-override-syntax-alist)
    (setq auto-completion-override-syntax-alist
	  predictive-restore-override-syntax-alist)
    (kill-local-variable 'predictive-restore-override-syntax-alist)
    ;; remove other local variable settings
    (kill-local-variable 'predictive-texinfo-dict)
    (kill-local-variable 'predictive-texinfo-local-texinfo-dict)
    (kill-local-variable 'completion-menu)
    ;; remove hook function that saves overlays
    (remove-hook 'after-save-hook
		 (lambda ()
		   (auto-overlay-save-overlays
		    'predictive nil
		    predictive-auxiliary-file-location))
		 t)
    (remove-hook 'kill-buffer-hook
		 (lambda ()
		   (when (not (buffer-modified-p))
		     (auto-overlay-save-overlays
		      'predictive nil
		      predictive-auxiliary-file-location)))
		 t)

    t))  ; indicate successful reversion of changes
)



(defun predictive-texinfo-load-regexps ()
  "Load the predictive mode Texinfo auto-overlay regexp definitions."

  (let* ((word-behaviour (completion-lookup-behaviour nil ?w))
	 (word-complete (completion-get-completion-behaviour word-behaviour))
	 (word-resolve (completion-get-resolve-behaviour word-behaviour))
	 (punct-behaviour (completion-lookup-behaviour nil ?.))
	 (punct-complete (completion-get-completion-behaviour punct-behaviour))
	 (punct-resolve (completion-get-resolve-behaviour punct-behaviour))
	 (whitesp-behaviour (completion-lookup-behaviour nil ? ))
	 (whitesp-complete (completion-get-completion-behaviour
			    whitesp-behaviour))
	 (whitesp-resolve (completion-get-resolve-behaviour
			   whitesp-behaviour)))

    ;; @c starts comments that last till end of line
    (auto-overlay-load-definition
     'predictive
     `(line :id short-comment
	    ("@c \\|@comment "
	     (dict . predictive-main-dict)
	     (priority . 50)
	     (exclusive . t)
	     (face . (background-color . ,predictive-overlay-debug-colour)))))

    ;; "@ignore ... @end ignore" defines an extended comment
    (auto-overlay-load-definition
     'predictive
     `(flat :id long-comment
	      ("@ignore[[:blank:]]*$"
	       :edge start
	       (dict . predictive-main-dict)
	       (priority . 50)
	       (exclusive . t)
	       (face . (background-color . ,predictive-overlay-debug-colour)))
	      ("@end ignore$"
	       :edge end
	       (dict . predictive-main-dict)
	       (priority . 50)
	       (exclusive . t)
	       (face . (background-color . ,predictive-overlay-debug-colour)))
	      ))


    ;; \<command>{'s do various things in Texinfo. All are ended by } but not
    ;; by \}. The { is included to ensure all { and } match, but @{ and @} are
    ;; excluded
    (auto-overlay-load-definition
     'predictive
     `(nested :id brace
	      ("[^@]\\(@@\\)*@\\(x\\|px\\|info\\)?ref{"
	       :edge start
	       (dict . predictive-texinfo-node-dict)
	       (priority . 40)
	       (completion-menu
		. predictive-texinfo-construct-browser-menu)
	       (completion-word-thing . predictive-texinfo-node-word)
	       (auto-completion-syntax-alist . ((?w . (add ,word-complete))
						(?_ . (add ,word-complete))
						(?  . (,whitesp-resolve none))
						(?. . (add ,word-complete))
						(t  . (reject none))))
	       (auto-completion-override-syntax-alist
		. ((?} . (,punct-resolve none))))
	       (face . (background-color . ,predictive-overlay-debug-colour)))
	      ("^@\\(x\\|px\\|info\\)?ref{"
	       :edge start
	       (dict . predictive-texinfo-node-dict)
	       (priority . 40)
	       (completion-menu
		. predictive-texinfo-construct-browser-menu)
	       (completion-word-thing . predictive-texinfo-node-word)
	       (auto-completion-syntax-alist . ((?w . (add ,word-complete))
						(?_ . (add ,word-complete))
						(?  . (,whitesp-resolve none))
						(?. . (add ,word-complete))
						(t  . (reject none))))
	       (auto-completion-override-syntax-alist
		. ((?} . (,punct-resolve none))))
	       (face . (background-color . ,predictive-overlay-debug-colour)))

	      ("[^@]\\(@@\\)*@math{"
	       :edge start
	       (dict . (dict-texinfo-math dict-latex-math))
	       (priority . 40)
	       (completion-word-thing . predictive-latex-word)
	       (auto-completion-override-syntax-alist
		. ((?\\ . ((lambda ()
			     (if (and (char-before) (= (char-before) ?\\)
				      (or (not (char-before (1- (point))))
					  (not (= (char-before (1- (point)))
						  ?\\))))
				 'add ',punct-resolve))
			   ,word-complete))
		   (?_ . (,punct-resolve none))
		   (?^ . (,punct-resolve none))
		   (?{ . ((lambda ()
			    (if (and (char-before) (= (char-before) ?\\))
				'add ',punct-resolve))
			  (lambda ()
			    (if (and (char-before) (= (char-before) ?\\))
				',punct-complete 'none))))
		   (?} . ((lambda ()
			    (if (and (char-before) (= (char-before) ?\\))
				'add ',punct-resolve))
			  (lambda ()
			    (if (and (char-before) (= (char-before) ?\\))
				',punct-complete 'none))))
		   (?\; . ((lambda ()
			     (if (and (char-before) (= (char-before) ?\\))
				 'add ',punct-resolve))
			   (lambda ()
			     (if (and (char-before (1- (point)))
				      (= (char-before (1- (point))) ?\\))
				 ',punct-complete 'none))))
		   (?! . ((lambda ()
			    (if (and (char-before) (= (char-before) ?\\))
				'add ',punct-resolve))
			  (lambda ()
			    (if (and (char-before (1- (point)))
				     (= (char-before (1- (point))) ?\\))
				',punct-complete 'none))))))
	       (face . (background-color . ,predictive-overlay-debug-colour)))

	      ("^@math{"
	       :edge start
	       (dict . (dict-texinfo-math dict-latex-math))
	       (priority . 40)
	       (completion-word-thing . predictive-latex-word)
	       (auto-completion-override-syntax-alist
		. ((?\\ . ((lambda ()
			     (if (and (char-before) (= (char-before) ?\\)
				      (or (not (char-before (1- (point))))
					  (not (= (char-before (1- (point)))
						  ?\\))))
				 'add ',punct-resolve))
			   ,word-complete))
		   (?_ . (,punct-resolve none))
		   (?^ . (,punct-resolve none))
		   (?{ . ((lambda ()
			    (if (and (char-before) (= (char-before) ?\\))
				'add ',punct-resolve))
			  (lambda ()
			    (if (and (char-before) (= (char-before) ?\\))
				',punct-complete 'none))))
		   (?} . ((lambda ()
			    (if (and (char-before) (= (char-before) ?\\))
				'add ',punct-resolve))
			  (lambda ()
			    (if (and (char-before) (= (char-before) ?\\))
				',punct-complete 'none))))
		   (?\; . ((lambda ()
			     (if (and (char-before) (= (char-before) ?\\))
				 'add ',punct-resolve))
			   (lambda ()
			     (if (and (char-before (1- (point)))
				      (= (char-before (1- (point))) ?\\))
				 ',punct-complete 'none))))
		   (?! . ((lambda ()
			    (if (and (char-before) (= (char-before) ?\\))
				'add ',punct-resolve))
			  (lambda ()
			    (if (and (char-before (1- (point)))
				     (= (char-before (1- (point))) ?\\))
				',punct-complete 'none))))))
	       (face . (background-color . ,predictive-overlay-debug-colour)))

	      ("[^@]\\(@@\\)*@value{"
	       :edge start
	       (dict . dict-texinfo-flag) (priority . 40)
	       (face . (background-color . ,predictive-overlay-debug-colour)))
	      ("^@value{"
	       :edge start
	       (dict . dict-texinfo-flag) (priority . 40)
	       (face . (background-color . ,predictive-overlay-debug-colour)))

	      ;; Note: the following regexps are complicated because they have
	      ;; to check whether number of @'s in front of { is even or
	      ;; odd. Also, since auto-overlay regexps aren't allowed to match
	      ;; across lines, we have to deal with the case of { or } at the
	      ;; start of a line separately.
	      (("^\\({\\)" . 1)
	       :edge start
	       (priority . 40)
	       (face . (background-color . ,predictive-overlay-debug-colour)))
	      (("[^@]\\(@@\\)*\\({\\)" . 2)
	       :edge start
	       (priority . 40)
	       (face . (background-color . ,predictive-overlay-debug-colour)))
	      (("^\\(}\\)" . 1)
	       :edge end
	       (priority . 40)
	       (face . (background-color . ,predictive-overlay-debug-colour)))
	      (("[^@]\\(@@\\)*\\(}\\)" . 2)
	       :edge end
	       (priority . 40)
	       (face . (background-color . ,predictive-overlay-debug-colour)))
	      ))


    ;; @end ends a Texinfo environment (the second definition deals with
    ;; overlays that have to extend to the end of the line)
    (auto-overlay-load-definition
     'predictive
     `(word
       :id environment
       (("@end \\([[:alpha:]]*\\)[^[:alpha:]]" . 1)
	(priority . 10)
	(dict . dict-texinfo-env)
	(face . (background-color . ,predictive-overlay-debug-colour)))))
    (auto-overlay-load-definition
     'predictive
     `(word
       :id environment-eol
       (("@end \\([[:alpha:]]*\\)$" . 1)
	(priority . 10)
	(dict . dict-texinfo-env)
	(face . (background-color . ,predictive-overlay-debug-colour)))))


    ;; @table, @vtable and @ftable define two-column tables, and should be
    ;; followed by a Texinfo "indicating" command (the second definition deals
    ;; with overlays that have to extend to the end of the line)
    (auto-overlay-load-definition
     'predictive
     `(word
       :id table
       (("@[vf]?table \\([[:alpha:]]*\\)[^[:alpha:]]" . 1)
	(priority . 10)
	(dict . dict-texinfo-indicating)
	(face . (background-color . ,predictive-overlay-debug-colour)))))
    (auto-overlay-load-definition
     'predictive
     `(word
       :id table-eol
       (("@[vf]?table \\([[:alpha:]]*\\)$" . 1)
	(priority . 10)
	(dict . dict-texinfo-indicating)
	(face . (background-color . ,predictive-overlay-debug-colour)))))


    ;; @node and @anchor create cross-reference labels. Through the use of a
    ;; special "auto-dict" regexp class defined below, this automagically adds
    ;; the label to the label dictionary.
    (auto-overlay-load-definition
     'predictive
     '(predictive-auto-dict
       :id node
       (("@node \\(.*?\\)\\(,.*\\|$\\)" . 1)
	(auto-dict . predictive-texinfo-node-dict))))
    (auto-overlay-load-definition
     'predictive
     '(predictive-auto-dict
       :id anchor
       (("@anchor{\\(.*?\\)}" . 1)
	(auto-dict . predictive-texinfo-node-dict))))


    ;; the other optional arguments of @node are references to other nodes
    (auto-overlay-load-definition
     'predictive
     '(word
       :id node-args1
       (("@node [^,]*,\\([^,]*\\)\\($\\|,\\)" . 1)
	(dict . predictive-texinfo-node-dict))))
    (auto-overlay-load-definition
     'predictive
     '(word
       :id node-args2
       (("@node \\([^,]*,\\)\\{2\\}\\([^,]*\\)\\($\\|,\\)" . 2)
	(dict . predictive-texinfo-node-dict))))
    (auto-overlay-load-definition
     'predictive
     '(word
       :id node-args3
       (("@node \\([^,]*,\\)\\{3\\}\\([^,]*\\)\\($\\|,\\)" . 2)
	(dict . predictive-texinfo-node-dict))))


    ;; @macro and @rmacro define new Texinfo macros. Through the use of a
    ;; special "auto-dict" regexp class defined below, this automagically adds
    ;; the command to the Texinfo dictionary.
    (auto-overlay-load-definition
     'predictive
     '(predictive-auto-dict
       :id macro
       (("@r?macro \\(.*?\\)[[:blank:]]*\\({\\|$\\)" . 1)
	(auto-dict . predictive-texinfo-local-texinfo-dict))))

    ;; @alias defines a new alias to a Texinfo macro
    (auto-overlay-load-definition
     'predictive
     '(predictive-auto-dict
       :id alias
       (("@alias \\(.*?\\)[[:blank:]]*=" . 1)
	(auto-dict . predictive-texinfo-local-texinfo-dict))))


    ;; @set defines and/or sets a flag. Through the use of a special "auto-dict"
    ;; regexp class defined below, this automagically adds the command to the
    ;; flags dictionary.
    (auto-overlay-load-definition
     'predictive
     '(predictive-auto-dict
       :id set
       (("@set \\(.*\\)\\([^ ]\\|$\\)" . 1)
	(dict . predictive-texinfo-flag-dict))))


    ;; @clear, @ifset and @ifclear refer to flags defined by @set
    (auto-overlay-load-definition
     'predictive
     '(word
       :id clear
       (("@clear \\(.*\\)\\([^ ]\\|$\\)" . 1)
	(dict . predictive-texinfo-flag-dict))))
    (auto-overlay-load-definition
     'predictive
     '(word
       :id ifset
       (("@ifset \\(.*\\)\\([^ ]\\|$\\)" . 1)
	(dict . predictive-texinfo-flag-dict))))
    (auto-overlay-load-definition
     'predictive
     '(word
       :id ifclear
       (("@ifclear \\(.*\\)\\([^ ]\\|$\\)" . 1)
	(dict . predictive-texinfo-flag-dict))))
    )
)



(defun predictive-texinfo-load-keybindings ()
  "Load the predictive mode Texinfo key bindings."

  ;; override AUCTeX bindings so completion works
  (make-local-variable 'predictive-map)
  (define-key predictive-map [?$]  'completion-self-insert)
  (define-key predictive-map [?\"] 'completion-self-insert)
  (define-key predictive-map [?_]  'completion-self-insert)
  (define-key predictive-map [?^]  'completion-self-insert)
  (define-key predictive-map [?\\] 'completion-self-insert)
  (define-key predictive-map [?-]  'completion-self-insert)

  (setq predictive-restore-override-syntax-alist
	auto-completion-override-syntax-alist)
  (make-local-variable 'auto-completion-override-syntax-alist)
  ;; get behaviours defined in `auto-completion-syntax-alist'
  (let* ((word-behaviour (completion-lookup-behaviour nil ?w))
	 (word-complete (completion-get-completion-behaviour word-behaviour))
	 (word-resolve (completion-get-resolve-behaviour word-behaviour))
	 (punct-behaviour (completion-lookup-behaviour nil ?.))
	 (punct-complete (completion-get-completion-behaviour punct-behaviour))
	 (punct-resolve (completion-get-resolve-behaviour punct-behaviour))
	 (whitesp-behaviour (completion-lookup-behaviour nil ? ))
	 (whitesp-complete (completion-get-completion-behaviour
			    whitesp-behaviour))
	 (whitesp-resolve (completion-get-resolve-behaviour
			   whitesp-behaviour)))
    ;; make "\", "$", "{" and "}" do the right thing
    (setq auto-completion-override-syntax-alist
	  (append
	   `((?@ . ((lambda ()
		      (if (and (char-before) (= (char-before) ?@)
			       (or (not (char-before (1- (point))))
				   (not (= (char-before (1- (point)))
					   ?@))))
			  'add ',punct-resolve))
		    ,word-complete))
	     (?  . ((lambda ()
		      (if (and (char-before) (= (char-before) ?\\))
			  'add ',whitesp-resolve))
		    (lambda ()
		      (cond
		       ((auto-overlays-at-point
			 nil '((lambda (dic) (eq dic 'dict-texinfo-env))
			       dict))
			(complete-in-buffer "" 'auto)
			'none)
		       ((and (char-before) (= (char-before) ?\\))
			',word-complete)
		       (t 'none)))))
	     (?{ . ((lambda ()
		      (if (and (char-before) (= (char-before) ?@))
			  'add ',punct-resolve))
		    (lambda ()
		      (if (and (char-before) (= (char-before) ?@))
			  ',punct-complete 'none))))
	     (?} . ((lambda ()
		      (if (and (char-before) (= (char-before) ?@))
			  'add ',punct-resolve))
		    (lambda ()
		      (if (and (char-before) (= (char-before) ?@))
			  ',punct-complete 'none))))
	     (?! . ((lambda ()
		      (if (and (char-before) (= (char-before) ?@))
			  'add ',punct-resolve))
		    (lambda ()
		      (if (and (char-before (1- (point)))
			       (= (char-before (1- (point))) ?@))
			  ',punct-complete 'none))))
	     (?\" . ((lambda ()
		       (if (and (char-before) (= (char-before) ?@))
			   'add ',punct-resolve))
		     (lambda ()
		       (if (and (char-before (1- (point)))
				(= (char-before (1- (point))) ?@))
			   ',punct-complete 'none))
		     (lambda ()
		       (if (or (and (char-before) (= (char-before) ?@))
			       (not (fboundp 'TeX-insert-quote)))
			   t
			 (TeX-insert-quote nil)
			 nil))))
	     (?' . ((lambda ()
		      (if (and (char-before) (= (char-before) ?@))
			  'add ',punct-resolve))
		    (lambda ()
		      (if (and (char-before (1- (point)))
			       (= (char-before (1- (point))) ?@))
			  ',punct-complete 'none))))
	     (?* . ((lambda ()
		      (if (and (char-before) (= (char-before) ?@))
			  'add ',punct-resolve))
		    (lambda ()
		      (if (and (char-before (1- (point)))
			       (= (char-before (1- (point))) ?@))
			  ',punct-complete 'none))))
	     (?, . ((lambda ()
		      (if (and (char-before) (= (char-before) ?@))
			  'add ',punct-resolve))
		    (lambda ()
		      (if (and (char-before (1- (point)))
			       (= (char-before (1- (point))) ?@))
			  ',punct-complete 'none))))
	     (?- . ((lambda ()
		      (if (and (char-before) (= (char-before) ?@))
			  'add ',punct-resolve))
		    (lambda ()
		      (if (and (char-before (1- (point)))
			       (= (char-before (1- (point))) ?@))
			  ',punct-complete 'none))))
	     (?. . ((lambda ()
		      (if (and (char-before) (= (char-before) ?@))
			  'add ',punct-resolve))
		    (lambda ()
		      (if (and (char-before (1- (point)))
			       (= (char-before (1- (point))) ?@))
			  ',punct-complete 'none))))
	     (?/ . ((lambda ()
		      (if (and (char-before) (= (char-before) ?@))
			  'add ',punct-resolve))
		    (lambda ()
		      (if (and (char-before (1- (point)))
			       (= (char-before (1- (point))) ?@))
			  ',punct-complete 'none))))
	     (?: . ((lambda ()
		      (if (and (char-before) (= (char-before) ?@))
			  'add ',punct-resolve))
		    (lambda ()
		      (if (and (char-before (1- (point)))
			       (= (char-before (1- (point))) ?@))
			  ',punct-complete 'none))))
	     (?= . ((lambda ()
		      (if (and (char-before) (= (char-before) ?@))
			  'add ',punct-resolve))
		    (lambda ()
		      (if (and (char-before (1- (point)))
			       (= (char-before (1- (point))) ?@))
			  ',punct-complete 'none))))
	     (?? . ((lambda ()
		      (if (and (char-before) (= (char-before) ?@))
			  'add ',punct-resolve))
		    (lambda ()
		      (if (and (char-before (1- (point)))
			       (= (char-before (1- (point))) ?@))
			  ',punct-complete 'none))))
	     (?^ . ((lambda ()
		      (if (and (char-before) (= (char-before) ?@))
			  'add ',punct-resolve))
		    (lambda ()
		      (if (and (char-before (1- (point)))
			       (= (char-before (1- (point))) ?@))
			  ',punct-complete 'none))))
	     (?` . ((lambda ()
		      (if (and (char-before) (= (char-before) ?@))
			  'add ',punct-resolve))
		    (lambda ()
		      (if (and (char-before (1- (point)))
			       (= (char-before (1- (point))) ?@))
			  ',punct-complete 'none))))
 	     )
	   auto-completion-override-syntax-alist) ; append
	  )
    )
)


;; FIXME: these probably need to go in a math auto-overlay
;;        'completion-override-syntax-alist property
;; (?\\ . ((lambda ()
;; 	  (if (and (char-before) (= (char-before) ?\\)
;; 		   (or (not (char-before (1- (point))))
;; 		       (not (= (char-before (1- (point)))
;; 			       ?\\))))
;; 	      'add ',punct-resolve))
;; 	,word-complete))
;; (?$ . (,punct-resolve none))
;; (?^ . (,punct-resolve none))




;;;=============================================================
;;;                Completion-browser functions

(defun predictive-texinfo-construct-browser-menu (prefix completions)
  "Construct the Texinfo browser menu keymap."

  ;; construct menu, dropping the last two entries which are a separator and a
  ;; link back to the basic completion menu (would just redisplay this menu,
  ;; since we're using the browser as the default menu)
  (let ((menu (completion-construct-browser-menu
	       prefix completions 'predictive-texinfo-browser-menu-item)))
    (setq menu (butlast menu 2)))
)



(defun predictive-texinfo-browser-menu-item (prefix completion &rest ignore)
  "Construct predictive Texinfo completion browser menu item."

  ;; if entry is @end, create sub-menu containing environment completions
  (if (string= (concat prefix completion) "@end")
      ;; find all Texinfo environments
      (let ((envs (dictree-complete dict-texinfo-env ""))
	    (menu (make-sparse-keymap)))
	(setq envs (mapcar (lambda (e) (concat completion " " (car e))) envs))
	;; create sub-menu keymap
	(setq menu (completion-browser-sub-menu
		    prefix envs 'predictive-texinfo-browser-menu-item
		    'completion-browser-sub-menu))
	;; add completion itself (@end) to the menu
	(define-key menu [separator-item-sub-menu] '(menu-item "--"))
	(define-key menu [completion-insert-root]
	  (list 'menu-item (concat prefix completion)
		`(lambda () (insert ,completion))))
	;; return the menu keymap
	menu)

    ;; otherwise, create a selectable completion item
    `(lambda () (insert ,completion)))
)




;;;=============================================================
;;;               Miscelaneous utility functions

(defun predictive-texinfo-forward-word (&optional n)
  ;; going backwards...
  (if (and n (< n 0))
      (unless (bobp)
	(dotimes (i (- n))
	  ;; make sure we're at the end of a word
	  (when (re-search-backward "@\\|\\w" nil t)
	    (forward-char))
	  ;; if point is within or just after a sequence of @'s, go
	  ;; backwards for the correct number of @'s
	  (if (= (char-before) ?@)
	      (let ((pos (point)))
		(save-excursion
		  (while (= (char-before) ?@) (backward-char))
		  (setq pos (- pos (point))))
		(if (= (mod pos 2) 1) (backward-char) (backward-char 2)))
	    ;; otherwise, go back one word, plus one @ if there's an odd
	    ;; number of them before it
	    (backward-word 1)  ; argument not optional in Emacs 21
	    (when (and (not (bobp)) (= ?@ (char-before)))
	      (let ((pos (point)))
		(save-excursion
		  (while (= (char-before) ?@) (backward-char))
		  (setq pos (- pos (point))))
		(when (= (mod pos 2) 1) (backward-char))))
	    )))

    ;; going forwards...
    (unless (eobp)
      ;; deal with point within sequence of @'s
      (when (= (char-after) ?@)
	(let ((pos (point)))
	  (save-excursion
	    (while (= (char-before) ?@) (backward-char))
	    (setq pos (- pos (point))))
	  (when (= (mod pos 2) 1) (backward-char))))
      ;; go forward, counting @ as part of word, @@ as entire word
      (dotimes (i (or n 1))
	(when (re-search-forward "@\\|\\w" nil t)
	  (backward-char))
	(re-search-forward "@\\W\\|@\\w+\\|\\w+" nil t)
	(when (= (char-before) ?\n) (backward-char))))
    )
)



(defun predictive-texinfo-node-forward-word (&optional n)
  ;; going backwards...
  (if (and n (< n 0))
      (unless (bobp)
	(setq n (- n))
	(when (= ?\\ (char-before))
	  (while (= ?\\ (char-before)) (backward-char))
	  (setq n (1- n)))
	(dotimes (i n)
	  (when (and (char-before) (= (char-syntax (char-before)) ?w))
	    (backward-word 1))  ; argument not optional in Emacs 21
	  (while (and (char-before) (/= (char-before) ?{))
	    (backward-char))))
    ;; going forwards...
    (unless (eobp)
      (setq n (if n n 1))
      (dotimes (i n)
	(when (and (char-after) (= (char-syntax (char-after)) ?w))
	  (forward-word 1))  ; argument not optional in Emacs 21
	(while (and (char-after) (/= (char-after) ?}) (/= (char-after) ?\n))
	  (forward-char))))
    )
)



;;; predictive-texinfo.el ends here
