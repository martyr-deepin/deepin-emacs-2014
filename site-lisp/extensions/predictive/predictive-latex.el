
;;; predictive-latex.el --- predictive mode LaTeX setup function
;;;                         (assumes AMSmath)


;; Copyright (C) 2004-2008 Toby Cubitt

;; Author: Toby Cubitt <toby-predictive@dr-qubit.org>
;; Version: 0.10.1
;; Keywords: predictive, setup function, latex
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
;; Version 0.10.1
;; * moved auto-dict utility functions to predictive-auto-overlay-auto-dict.el
;; * fixed bug in `predictive-latex-forward-word'
;; * fixed bug in `completion-override-syntax-alist' "\" definition
;; * honour user's choices in `auto-completion-syntax-alist' and
;;   `auto-completion-override-syntax-alist' in
;;   `predictive-latex-load-regexps'
;; * improved regexp definitions that start with "\"
;; * save overlays when unmodified buffers are killed
;;
;; Version 0.10
;; * separated off predictive-latex-auto-dict overlays into stand-alone
;;   auto overlay class, in predictive-auto-overlay-auto-dict.el
;;
;; Version 0.9.3
;; * save overlay and local dictionary files to a directory specified by new
;;   `predictive-auxiliary-file-location' customization option
;;
;; Version 0.9.2
;; * improved `predictive-latex-label-forward-word'
;; * added "!" to `auto-completion-override-syntax-alist'
;; * made `predictive-setup-latex' fail gracefully when a required dictionary
;;   can't be found
;; * auto-complete immediately after "\documentclass{", just like "\begin{"
;;
;; Version 0.9.1
;; * fixed bug in loading of main dictionary and latex dictionary list
;; * fixed potential bug in `predictive-latex-forward-word'
;;
;; Version 0.9
;; * generalised predictive-latex-label overlay class into
;;   predictive-latex-auto-dict class, that can be used to auto-add words to
;;   any dictionary
;; * added new automatically maintained dictionaries of locally declared
;;   environments and commands, using predictive-latex-auto-dict overlay class
;; * bug-fix to `predictive-latex-reparse-buffer'
;;
;; Version 0.8.1
;; * minor bug fixes to `predictive-latex-load-keybindings'
;; * switched ordering of `auto-completion[-override]-syntax-alist' entries to
;;   conform to new completion-ui
;;
;; Version 0.8
;; * updated for new auto-overlay regexp definition interface
;;
;; Version 0.7.5
;; * updated to conform to completion-ui.el version 0.7
;; * renamed `predictive-latex-completion-add-to-regexp' to
;;   `predictive-latex-completion-add-till-regexp'
;;
;; Version 0.7.4
;; * modified `predictive-latex-reparse-buffer' to kill local values of
;;   dictionary lists, to avoid duplicate dictionaries being added to them
;;   by usepackage overlays when buffer is reparsed
;;
;; Version 0.7.3
;; * updated `predictive-latex-setup' to conform to new way major-mode setup
;;   functions are called by `predictive-mode', and deleted now redundant
;;   `predictive-latex-disable'
;; * avoid clobbering `completion-dynamic-syntax-override-alist' settings
;;
;; Version 0.7.2
;; * added `predictive-latex-reparse-buffer' command to allow easy reparsing
;;   of auto-overlays if they get in a mess due to bugs in the auto-overlays
;;   code :-(
;; * restore buffer's modified flag after enabling predictive mode, since
;;   "electric" synchronization of LaTeX envionments can modify buffer without
;;   actually changing buffer text
;; * renamed "nest" regexps to "nested" regexps
;; * prevent scheduling of "electric" environment syncronization when blocked
;;   by `predictive-latex-disable-env-synchronize'
;; * don't bother synchronizing if environments are already synchronized
;;
;; Version 0.7.1
;; * fixed regexps for {, }, \[ and \] so that they correctly deal with having
;;   an even number of \'s in front of them
;;
;; Version 0.7
;; * added automatic synchronization of environment names
;; * added interactive commands for navigating by LaTeX environments
;; * added automatic switching of main dictionary based on document class
;;
;; Version 0.6.8
;; * fixed small bug in `predictive-latex-unload-package'
;;
;; Version 0.6.7
;; * updated to work with new versions of the completion-UI and auto-overlays
;;   packages
;; * predictive mode can once again be enabled and disabled independently in
;;   different LaTeX buffers that share the same TeX-master, but disabling it
;;   in the TeX-master buffer will now disable it in all those related LaTeX
;;   buffers
;;
;; Version 0.6.6
;; * added the appropriate checks for changed regexp definitions when starting
;;   the auto-overlay set
;;
;; Version 0.6.5
;; * removed `auto-overlay-functions' and changed to use new interface
;; * renamed "stack" regexps to "nest" regexps
;;
;; Version 0.6.4
;; * overlays are no longer deleted unnecessarily when a buffer is killed
;;
;; Version 0.6.3
;; * generalised label-specific function for adding part of a completion
;;   candidate and re-completing to `predictive-completion-add-to-regexp'
;;
;; Version 0.6.2
;; * rewrote `predictive-latex-forward-word'
;; * prevented label dictionary being clobbered if it's already loaded
;;
;; Version 0.6.1
;; * added missing overlay-local `completion-override-syntax-alist' binding
;;
;; Version 0.6
;; * \usepackage commands now cause dictionaries and regexps to be
;;   automatically loaded and unloaded as necessary
;; * updated to reflect changes in `auto-overlays.el'
;; * \label commands now add (and delete) entries to a special label
;;   dictionary, used to complete cross-reference commands, e.g. \ref.
;; * made the latex completion browser come up as the first menu for LaTeX
;;   commands and labels, and dropped link back to basic completion menu
;; * added eval-when-compile to prevent bogus compilation errors
;;
;; Version 0.5.6
;; * fixed some variables that were set globally instead of locally by the
;;   setup function, and added kill-local-variable lines to the hook function
;;   to restore the default values when predictive mode is disabled
;;
;; Version 0.5.5
;; * minor enhancements and bug fixes
;;
;; Version 0.5.4
;; * updated to reflect changes in completion-ui.el
;;
;; Version 0.5.3
;; * updated to reflect naming changes in dict-tree.el
;;
;; Version 0.5.2
;; * bug fixes
;; * set `predictive-completion-browser-menu' so that LaTeX browser is called
;;   when completing a LaTeX command (can't use overlay-local binding anymore
;;   since LaTeX commands no longer use auto-overlays)
;;
;; Version 0.5.1
;; * renamed to `predictive-latex' and released as only latex setup package
;;
;; Version 0.5
;; * added support for completion browser
;; * modified latex enviroments to use new stack-sync class
;; * stopped using auto-overlays for LaTeX commands, and just set `dict-latex'
;;   as second main dictionary instead
;;
;; Version 0.4
;; * modified to work with new auto-overlays package
;;
;; Version 0.3
;; * changed LaTeX commands back to (new) 'word regexps
;; * added comments as 'line regexps
;; * modified priorities and ordering so things work with new switch-dict code
;;
;; Version 0.2
;; * changed 'word regexps to 'start and 'end regexps so that
;;   predictive-learn-from- functions can learn LaTeX commands
;;
;; Version 0.1
;; * initial release



;;; Code:

(require 'predictive)
(require 'auto-overlay-word)
(require 'auto-overlay-line)
(require 'auto-overlay-self)
(require 'auto-overlay-nested)
(require 'predictive-auto-overlay-auto-dict)

(provide 'predictive-latex)
(add-to-list 'predictive-major-mode-alist
	     '(LaTeX-mode . predictive-setup-latex))
(add-to-list 'predictive-major-mode-alist
	     '(latex-mode . predictive-setup-latex))



;;;============================================================
;;;                  Customization Options

(defgroup predictive-latex nil
  "Predictive completion mode LaTeX support."
  :group 'predictive)


(defcustom predictive-latex-electric-environments nil
  "*When enabled, environment names are automatically synchronized
between \\begin{...} and \\end{...} commands."
  :group 'predictive-latex
  :type 'boolean)


(defcustom predictive-latex-docclass-alist nil
  "*Alist associating LaTeX document classes with dictionaries.
When a document class is in the list, "
  :group 'predictive-latex
  :type '(repeat (cons string symbol)))




;;;============================================================
;;;                       Variables

;; variables holding dictionaries for different LaTeX contexts
(defvar predictive-latex-dict '(dict-latex))
(make-variable-buffer-local 'predictive-latex-dict)
(defvar predictive-latex-math-dict '(dict-latex-math))
(make-variable-buffer-local 'predictive-latex-math-dict)
(defvar predictive-latex-preamble-dict '(dict-latex-preamble))
(make-variable-buffer-local 'predictive-latex-preamble-dict)
(defvar predictive-latex-env-dict '(dict-latex-env))
(make-variable-buffer-local 'predictive-latex-env-dict)
(defvar predictive-latex-bibstyle-dict '(dict-latex-bibstyle))
(make-variable-buffer-local 'predictive-latex-bibstyle-dict)
(defvar predictive-latex-label-dict nil)
(make-variable-buffer-local 'predictive-latex-label-dict)
(defvar predictive-latex-local-latex-dict nil)
(make-variable-buffer-local 'predictive-latex-local-latex-dict)
(defvar predictive-latex-local-math-dict nil)
(make-variable-buffer-local 'predictive-latex-local-math-dict)
(defvar predictive-latex-local-env-dict nil)
(make-variable-buffer-local 'predictive-latex-local-env-dict)


;; alist holding functions called when loading and unloading latex packages
(defvar predictive-latex-usepackage-functions nil)


;; set up 'predictive-latex-word to be a `thing-at-point' symbol
(put 'predictive-latex-word 'forward-op 'predictive-latex-forward-word)
;; set up 'predictive-latex-label-word to be a `thing-at-point' symbol
(put 'predictive-latex-label-word 'forward-op
     'predictive-latex-label-forward-word)


;; convenience const holding alist associating latex dictionary variables and
;; corresponding dictionary name prefices
(defconst predictive-latex-dict-classes
  '((predictive-latex-dict . "dict-latex-")
    (predictive-latex-math-dict . "dict-latex-math-")
    (predictive-latex-preamble-dict . "dict-latex-preamble-")
    (predictive-latex-env-dict . "dict-latex-env-")))


;; variables used to restore local settings of variables when predictive mode
;; is disabled in a LaTeX buffer
(defvar predictive-restore-main-dict nil)
(make-variable-buffer-local 'predictive-restore-main-dict)
(defvar predictive-restore-override-syntax-alist nil)
(make-variable-buffer-local 'predictive-restore-override-syntax-alist)


;; prevent bogus compiler warnings
(eval-when-compile
  (defvar dict-latex-docclass)
  (defvar dict-latex-bibstyle)
  (defvar TeX-master))


;; background color for certain auto-overlays to aid debugging
(defvar predictive-overlay-debug-colour nil)
(defvaralias 'predictive-overlay-debug-color 'predictive-overlay-debug-colour)



;;;=========================================================
;;;                  Setup function

(defun predictive-setup-latex (arg)
  "With a positive ARG, set up predictive mode for use with LaTeX major modes.
With a negative ARG, undo these changes. Called when predictive
mode is enabled via entry in `predictive-major-mode-alist'."

  (cond
   ;; ----- enabling LaTeX setup -----
   ((> arg 0)
    (catch 'load-fail

      ;; save overlays and unload regexp definitions along with buffer
      (add-hook 'after-save-hook
		(lambda () (auto-overlay-save-overlays
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

      ;; use latex browser menu if first character of prefix is "\"
      (make-local-variable 'completion-menu)
      (setq completion-menu
	    (lambda (prefix completions)
	      (if (string= (substring prefix 0 1) "\\")
		  (predictive-latex-construct-browser-menu prefix completions)
		(completion-construct-menu prefix completions))
	      ))
      ;; save predictive-main-dict; restored when predictive mode is disabled
      (setq predictive-restore-main-dict predictive-main-dict)


      (cond
       ;; if we're not the TeX master, visit the TeX master buffer, enable
       ;; predictive mode in it, and share buffer-local settings with it
       ((and (boundp 'TeX-master) (stringp TeX-master))
	(let (filename buff used-dicts main-dict latex-dict math-dict
		       preamble-dict env-dict label-dict local-env-dict)
	  (setq filename (expand-file-name TeX-master))
	  (unless (string= (substring filename -4) ".tex")
	    (setq filename (concat filename ".tex")))
	  (save-window-excursion
	    (find-file filename)
	    (turn-on-predictive-mode)
	    (setq buff (current-buffer))
	    (setq used-dicts predictive-used-dict-list)
	    (setq main-dict predictive-main-dict)
	    (setq latex-dict predictive-latex-dict)
	    (setq math-dict predictive-latex-math-dict)
	    (setq preamble-dict predictive-latex-preamble-dict)
	    (setq env-dict predictive-latex-env-dict)
	    (setq label-dict predictive-latex-label-dict)
	    (setq local-env-dict predictive-latex-local-env-dict))
	  (auto-overlay-share-regexp-set 'predictive buff)
	  (setq predictive-used-dict-list used-dicts)
	  (setq predictive-main-dict main-dict)
	  (setq predictive-latex-dict latex-dict)
	  (setq predictive-latex-math-dict math-dict)
	  (setq predictive-latex-preamble-dict preamble-dict)
	  (setq predictive-latex-env-dict env-dict)
	  (setq predictive-latex-label-dict label-dict)
	  (setq predictive-latex-local-env-dict local-env-dict)
	  ;; start the auto-overlays, restoring buffer's modified flag
	  ;; afterwards, since automatic synchronization of LaTeX envionments
	  ;; can modify buffer without actually changing buffer text
	  (let ((restore-modified (buffer-modified-p)))
	    (auto-overlay-start 'predictive nil
				predictive-auxiliary-file-location)
	    (set-buffer-modified-p restore-modified))
	  ))


       ;; if we're the TeX master file, set up LaTeX auto-overlay regexps
       ;; FIXME: probably need to handle null TeX-master case differently
       (t
	;; load the latex dictionaries
	(unless (predictive-load-dict 'dict-latex-docclass)
	  (message "Failed to load dict-latex-docclass")
	  (throw 'load-fail nil))
	(mapc (lambda (dic)
		(unless (predictive-load-dict dic)
		  (message "Failed to load %s" dic)
		  (throw 'load-fail nil)))
	      (append predictive-latex-dict
		      predictive-latex-math-dict
		      predictive-latex-preamble-dict
		      predictive-latex-env-dict
		      predictive-latex-bibstyle-dict))
	;; load/create the label and local latex dictionaries
	(predictive-load-auto-dict "latex-label")
	(predictive-load-auto-dict "latex-local-latex")
	(predictive-load-auto-dict "latex-local-math")
	(predictive-load-auto-dict "latex-local-env")

	;; add local environment, maths and text-mode dictionaries to
	;; appropriate dictionary lists
	;; Note: we add the local text-mode command dictionary to the maths
	;; dictionary list too, because there's no way to tell whether
	;; \newcommand's are text- or math-mode commands.
	(setq predictive-latex-dict
	      (append predictive-latex-dict
		      (if (dictree-p predictive-latex-local-latex-dict)
			  (list predictive-latex-local-latex-dict)
			predictive-latex-local-latex-dict)))
	(setq predictive-latex-math-dict
	      (append predictive-latex-math-dict
		      (if (dictree-p predictive-latex-local-math-dict)
			  (list predictive-latex-local-math-dict)
			predictive-latex-local-math-dict)
		      (if (dictree-p predictive-latex-local-latex-dict)
			  (list predictive-latex-local-latex-dict)
			predictive-latex-local-latex-dict)))
	(setq predictive-latex-env-dict
	      (append predictive-latex-env-dict
		      (if (dictree-p predictive-latex-local-env-dict)
			  (list predictive-latex-local-env-dict)
			predictive-latex-local-env-dict)))

	;; add latex dictionaries to main dictionary list
	(make-local-variable 'predictive-main-dict)
	(when (atom predictive-main-dict)
	  (setq predictive-main-dict (list predictive-main-dict)))
	(setq predictive-main-dict
	      (append predictive-main-dict predictive-latex-dict))


	;; delete any existing predictive auto-overlay regexps and load latex
	;; auto-overlay regexps
	(auto-overlay-unload-set 'predictive)
	(predictive-latex-load-regexps)

	;; start the auto-overlays, skipping the check that regexp definitions
	;; haven't changed if there's a file of saved overlay data to use, and
	;; restoring buffer's modified flag afterwards (if used, automatic
	;; synchronization of LaTeX envionments can modify buffer without
	;; actually changing buffer text)
	(let ((restore-modified (buffer-modified-p)))
	  (auto-overlay-start 'predictive nil
			      predictive-auxiliary-file-location
			      'no-regexp-check)
	  (set-buffer-modified-p restore-modified))
	))


      ;; load the keybindings and related settings
      (predictive-latex-load-keybindings)
      ;; consider \ as start of a word
      (setq completion-word-thing 'predictive-latex-word)
      (set (make-local-variable 'words-include-escapes) nil)

      t))  ; indicate successful setup



   ;; ----- Disabling LaTeX setup -----
   ((< arg 0)
    ;; if we're the TeX-master, first disable predictive mode in all related
    ;; LaTeX buffers, which we find by looking for buffers that share the
    ;; auto-overlays 'predictive regexp set
    (when (eq TeX-master t)
      (dolist (buff (auto-o-get-buffer-list 'predictive))
	;; TeX-master itself will be in list of buffers sharing regexp set, so
	;; need to filter it out
	(unless (eq buff (current-buffer))
	  (save-excursion
	    (set-buffer buff)
	    (predictive-mode -1)))))

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
    (kill-local-variable 'completion-menu)
    (kill-local-variable 'words-include-escapes)
    (kill-local-variable 'predictive-latex-dict)
    (kill-local-variable 'predictive-latex-math-dict)
    (kill-local-variable 'predictive-latex-env-dict)
    (kill-local-variable 'predictive-map)
    (kill-local-variable 'predictive-latex-label-dict)
    (kill-local-variable 'predictive-latex-local-latex-dict)
    (kill-local-variable 'predictive-latex-local-math-dict)
    (kill-local-variable 'predictive-latex-local-env-dict)
    ;; remove hook function that saves overlays
    (remove-hook 'after-save-hook
		 (lambda () (auto-overlay-save-overlays
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




(defun predictive-latex-load-regexps ()
  "Load the predictive mode LaTeX auto-overlay regexp definitions."

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

    ;; %'s start comments that last till end of line
    (auto-overlay-load-definition
     'predictive
     `(line :id comment
	    ("%" (dict . predictive-main-dict) (priority . 50) (exclusive . t)
	     (completion-menu . predictive-latex-construct-browser-menu)
	     (face . (background-color . ,predictive-overlay-debug-colour)))))

    ;; \begin{ and \end{ start and end LaTeX environments. Other \<command>{'s
    ;; do various other things. All are ended by } but not by \}. The { is
    ;; included to ensure all { and } match, but \{ is excluded
    (auto-overlay-load-definition
     'predictive
     `(nested :id brace
	      ("\\([^\\]\\|^\\)\\(\\\\\\\\\\)*\\\\usepackage{"
	       :edge start
	       (dict . t)
	       (priority . 40)
	       (face . (background-color . ,predictive-overlay-debug-colour)))
;;; 	      ("^\\\\usepackage{"
;;; 	       :edge start
;;; 	       (dict . t)
;;; 	       (priority . 40)
;;; 	       (face . (background-color . ,predictive-overlay-debug-colour)))
	      ("\\([^\\]\\|^\\)\\(\\\\\\\\\\)*\\\\label{"
	       :edge start
	       :id label
	       (dict . t)
	       (priority . 40)
	       (face . (background-color . ,predictive-overlay-debug-colour)))
;;; 	      ("^\\\\label{"
;;; 	       :edge start
;;; 	       :id label-bol
;;; 	       (dict . t)
;;; 	       (priority . 40)
;;; 	       (face . (background-color . ,predictive-overlay-debug-colour)))

	      ("\\([^\\]\\|^\\)\\(\\\\\\\\\\)*\\\\\\(eq\\)?ref{"
	       :edge start
	       (dict . predictive-latex-label-dict)
	       (priority . 40)
	       (completion-menu
		. predictive-latex-construct-browser-menu)
	       (completion-word-thing . predictive-latex-label-word)
	       (auto-completion-syntax-alist . ((?w . (add ,word-complete))
						(?_ . (add ,word-complete))
						(?  . (,whitesp-resolve none))
						(?. . (add ,word-complete))
						(t  . (reject none))))
	       (auto-completion-override-syntax-alist
		. ((?:
		    . ((lambda ()
			 (predictive-latex-completion-add-till-regexp ":")
			 nil)
		       ,word-complete))
		   (?_
		    . ((lambda ()
			 (predictive-latex-completion-add-till-regexp "\\W")
			 nil)
		       ,word-complete))
		   (?} . (,punct-resolve none))))
	       (face . (background-color . ,predictive-overlay-debug-colour)))

;;; 	      ("^\\\\\\(eq\\)?ref{"
;;; 	       :edge start
;;; 	       (dict . predictive-latex-label-dict)
;;; 	       (priority . 40)
;;; 	       (completion-menu
;;; 		. predictive-latex-construct-browser-menu)
;;; 	       (completion-word-thing . predictive-latex-label-word)
;;; 	       (auto-completion-syntax-alist . ((?w . (add ,word-complete))
;;; 						(?_ . (add ,word-complete))
;;; 						(?  . (,whitesp-resolve none))
;;; 						(?. . (add ,word-complete))
;;; 						(t  . (reject none))))
;;; 	       (auto-completion-override-syntax-alist
;;; 		. ((?:
;;; 		    . ((lambda ()
;;; 			 (predictive-latex-completion-add-till-regexp ":")
;;; 			 nil)
;;; 		       ,word-complete))
;;; 		   (?_
;;; 		    . ((lambda ()
;;; 			 (predictive-latex-completion-add-till-regexp "\\W")
;;; 			 nil)
;;; 		       ,word-complete))
;;; 		   (?} . (,punct-resolve none))))
;;; 	       (face . (background-color . ,predictive-overlay-debug-colour)))

	      ("\\([^\\]\\|^\\)\\(\\\\\\\\\\)*\\\\cite{"
	       :edge start
	       (dict . t) (priority . 40)
	       (face . (background-color . ,predictive-overlay-debug-colour)))
;;; 	      (("^\\(\\\\cite{\\)" . 1)
;;; 	       :edge start
;;; 	       (dict . t) (priority . 40)
;;; 	       (face . (background-color . ,predictive-overlay-debug-colour)))
	      ("\\([^\\]\\|^\\)\\(\\\\\\\\\\)*\\\\begin{"
	       :edge start
	       (dict . predictive-latex-env-dict) (priority . 40)
	       (completion-menu
		. predictive-latex-construct-browser-menu)
	       (face . (background-color . ,predictive-overlay-debug-colour)))
;;; 	      (("^\\(\\\\begin{\\)" . 1)
;;; 	       :edge start
;;; 	       (dict . predictive-latex-env-dict) (priority . 40)
;;; 	       (completion-menu
;;; 		. predictive-latex-construct-browser-menu)
;;; 	       (face . (background-color . ,predictive-overlay-debug-colour)))
	      ("\\([^\\]\\|^\\)\\(\\\\\\\\\\)*\\\\end{"
	       :edge start
	       (dict . predictive-latex-env-dict) (priority . 40)
	       (completion-menu
		. predictive-latex-construct-browser-menu)
	       (face . (background-color . ,predictive-overlay-debug-colour)))
;;; 	      (("^\\(\\\\end{\\)" . 1)
;;; 	       :edge start
;;; 	       (dict . predictive-latex-env-dict) (priority . 40)
;;; 	       (completion-menu
;;; 		. predictive-latex-construct-browser-menu)
;;; 	       (face . (background-color . ,predictive-overlay-debug-colour)))
	      ("\\([^\\]\\|^\\)\\(\\\\\\\\\\)*\\\\text{"
	       :edge start
	       (dict . predictive-main-dict) (priority . 40)
	       (completion-menu
		. predictive-latex-construct-browser-menu)
	       (face . (background-color . ,predictive-overlay-debug-colour)))
;;; 	      (("^\\(\\\\text{\\)" . 1)
;;; 	       :edge start
;;; 	       (dict . predictive-main-dict) (priority . 40)
;;; 	       (completion-menu
;;; 		. predictive-latex-construct-browser-menu)
;;; 	       (face . (background-color . ,predictive-overlay-debug-colour)))
	      ("\\([^\\]\\|^\\)\\(\\\\\\\\\\)*\\\\documentclass\\(\\[.*\\]\\)?{"
	       :edge start
	       (dict . dict-latex-docclass) (priority . 40)
	       (completion-menu
		. predictive-latex-construct-browser-menu)
	       (face . (background-color . ,predictive-overlay-debug-colour)))
;;; 	      (("^\\(\\\\documentclass\\(\\[.*\\]\\)?{\\)" . 1)
;;; 	       :edge start
;;; 	       (dict . dict-latex-docclass) (priority . 40)
;;; 	       (completion-menu
;;; 		. predictive-latex-construct-browser-menu)
;;; 	       (face . (background-color . ,predictive-overlay-debug-colour)))
	      ("\\([^\\]\\|^\\)\\(\\\\\\\\\\)*\\\\bibliographystyle\\(\\[.*\\]\\)?{"
	       :edge start
	       (dict . dict-latex-bibstyle)
	       (priority . 40)
	       (completion-menu
		. predictive-latex-construct-browser-menu)
	       (face . (background-color . ,predictive-overlay-debug-colour)))
;;; 	      (("^\\(\\\\bibliographystyle\\(\\[.*\\]\\)?{\\)" . 1)
;;; 	       :edge start
;;; 	       (dict . dict-latex-bibstyle)
;;; 	       (priority . 40)
;;; 	       (completion-menu
;;; 		. predictive-latex-construct-browser-menu)
;;; 	       (face . (background-color . ,predictive-overlay-debug-colour)))

	      ;; Note: the following regexps contain a lot of \'s because they
	      ;; have to check whether number of \'s in front of { is even or
	      ;; odd. Also, since auto-overlay regexps aren't allowed to match
	      ;; across lines, we have to deal with the case of { or } at the
	      ;; start of a line separately.
;;; 	      (("^\\({\\)" . 1)
;;; 	       :edge start
;;; 	       (priority . 40)
;;; 	       (face . (background-color . ,predictive-overlay-debug-colour)))
	      ("\\([^\\]\\|^\\)\\(\\\\\\\\\\)*{"
	       :edge start
	       (priority . 40)
	       (face . (background-color . ,predictive-overlay-debug-colour)))
;;; 	      (("^\\(}\\)" . 1)
;;; 	       :edge end
;;; 	       (priority . 40)
;;; 	       (face . (background-color . ,predictive-overlay-debug-colour)))
	      (("\\([^\\]\\|^\\)\\(\\\\\\\\\\)*\\(}\\)" . 3)
	       :edge end
	       (priority . 40)
	       (face . (background-color . ,predictive-overlay-debug-colour)))
	      ))


    ;; $'s delimit the start and end of maths regions...
    (auto-overlay-load-definition
     'predictive
     `(self :id inline-math
	    ("\\$" (dict . predictive-latex-math-dict) (priority . 30)
	     (completion-menu . predictive-latex-construct-browser-menu)
	     (face . (background-color . ,predictive-overlay-debug-colour)))))

    ;; ...as do \[ and \], but not \\[ and \\] etc.
    ;; Note: regexps contain a lot of \'s because it has to check whether number
    ;; of \'s in front of { is even or odd
    (auto-overlay-load-definition
     'predictive
     `(nested :id display-math
	      (("\\([^\\]\\|^\\)\\(\\\\\\\\\\)*\\(\\\\\\[\\)" . 3)
	       :edge start
	       (dict . predictive-latex-math-dict) (priority . 30)
	       (completion-menu
		. predictive-latex-construct-browser-menu)
	       (face . (background-color . ,predictive-overlay-debug-colour)))
;;; 	      (("^\\(\\\\\\[\\)" . 1)
;;; 	       :edge start
;;; 	       (dict . predictive-latex-math-dict) (priority . 30)
;;; 	       (completion-menu
;;; 		. predictive-latex-construct-browser-menu)
;;; 	       (face . (background-color . ,predictive-overlay-debug-colour)))
	      (("\\([^\\]\\|^\\)\\(\\\\\\\\\\)*\\(\\\\\\]\\)" . 3)
	       :edge end
	       (dict . predictive-latex-math-dict) (priority . 30)
	       (completion-menu . predictive-latex-construct-browser-menu)
	       (face . (background-color . ,predictive-overlay-debug-colour)))
;;; 	      (("^\\(\\\\\\]\\)" . 1)
;;; 	       :edge end
;;; 	       (dict . predictive-latex-math-dict) (priority . 30)
;;; 	       (completion-menu . predictive-latex-construct-browser-menu)
;;; 	       (face . (background-color . ,predictive-overlay-debug-colour)))
	      ))


    ;; preamble lives between \documentclass{...} and \begin{document}
    (auto-overlay-load-definition
     'predictive
     `(nested :id preamble
	      (("\\([^\\]\\|^\\)\\(\\\\\\\\\\)*\\(\\\\documentclass\\(\\[.*?\\]\\)?{.*?}\\)"
		. 3)
	       :edge start
	       (dict . predictive-latex-preamble-dict)
	       (priority . 20)
	       (completion-menu . predictive-latex-construct-browser-menu))
;;; 	      (("^\\(\\\\documentclass\\(\\[.*?\\]\\)?{.*?}\\)" . 1)
;;; 	       :edge start
;;; 	       (dict . predictive-latex-preamble-dict)
;;; 	       (priority . 20)
;;; 	       (completion-menu . predictive-latex-construct-browser-menu))
	      (("\\([^\\]\\|^\\)\\(\\\\\\\\\\)*\\(\\\\begin{document}\\)" . 3)
	       :edge end
	       (dict . predictive-latex-preamble-dict)
	       (priority . 20)
	       (completion-menu . predictive-latex-construct-browser-menu))
;;; 	      (("^\\(\\\\begin{document}\\)" . 1)
;;; 	       :edge end
;;; 	       (dict . predictive-latex-preamble-dict)
;;; 	       (priority . 20)
;;; 	       (completion-menu . predictive-latex-construct-browser-menu))
	      ))


    ;; \begin{...} and \end{...} start and end LaTeX environments, which we make
    ;; "electric" by using a special env overlay class (defined below) if the
    ;; corresponding customization option is enabled
    (if predictive-latex-electric-environments
	(auto-overlay-load-definition
	 'predictive
	 '(predictive-latex-env :id environment))
      (auto-overlay-load-definition
       'predictive
       '(nested :id environment)))
    ;; load the regexps into the list
    (auto-overlay-load-regexp
     'predictive 'environment
     `(("\\([^\\]\\|^\\)\\(\\\\\\\\\\)*\\\\begin{\\(equation\\*?\\|align\\(at\\)?\\*?\\|flalign\\*?\\|gather\\*?\\|multline\\*?\\)}"
	0 3)
       :edge start
       (dict . predictive-latex-math-dict)
       (priority . 10)
       (completion-menu . predictive-latex-construct-browser-menu)
       (face . (background-color . ,predictive-overlay-debug-colour))))
;;;     (auto-overlay-load-regexp
;;;      'predictive 'environment
;;;      `(("^\\\\begin{\\(equation\\*?\\|align\\(at\\)?\\*?\\|flalign\\*?\\|gather\\*?\\|multline\\*?\\)}" 0 1)
;;;        :edge start
;;;        (dict . predictive-latex-math-dict)
;;;        (priority . 10)
;;;        (completion-menu . predictive-latex-construct-browser-menu)
;;;        (face . (background-color . ,predictive-overlay-debug-colour))))
    (auto-overlay-load-regexp
     'predictive 'environment
     `(("\\([^\\]\\|^\\)\\(\\\\\\\\\\)*\\\\end{\\(equation\\*?\\|align\\(at\\)?\\*?\\|flalign\\*?\\|gather\\*?\\|multline\\*?\\)}"
	0 3)
       :edge end
       (dict . predictive-latex-math-dict)
       (priority . 10)
       (completion-menu . predictive-latex-construct-browser-menu)
       (face . (background-color . ,predictive-overlay-debug-colour))))
;;;     (auto-overlay-load-regexp
;;;      'predictive 'environment
;;;      `(("^\\\\end{\\(equation\\*?\\|align\\(at\\)?\\*?\\|flalign\\*?\\|gather\\*?\\|multline\\*?\\)}" 0 1)
;;;        :edge end
;;;        (dict . predictive-latex-math-dict)
;;;        (priority . 10)
;;;        (completion-menu . predictive-latex-construct-browser-menu)
;;;        (face . (background-color . ,predictive-overlay-debug-colour))))
    (auto-overlay-load-regexp
     'predictive 'environment
     '(("\\([^\\]\\|^\\)\\(\\\\\\\\\\)*\\\\begin{\\(.*?\\)}" 0 3)
       :edge start
       (priority . 10)
       (dict . nil)
       (face . nil)))
;;;     (auto-overlay-load-regexp
;;;      'predictive 'environment
;;;      '(("^\\\\begin{\\(.*?\\)}" 0 1)
;;;        :edge start
;;;        (priority . 10)
;;;        (dict . nil)
;;;        (face . nil)))
    (auto-overlay-load-regexp
     'predictive 'environment
     `(("\\([^\\]\\|^\\)\\(\\\\\\\\\\)*\\\\end{\\(.*?\\)}" 0 3)
       :edge end
       (priority . 10)
       (dict . nil)
       (face . nil)))
;;;     (auto-overlay-load-regexp
;;;      'predictive 'environment
;;;      `(("^\\\\end{\\(.*?\\)}" 0 1)
;;;        :edge end
;;;        (priority . 10)
;;;        (dict . nil)
;;;        (face . nil)))


    ;; \documentclass defines the document type. Through the use of a special
    ;; "docclass" regexp class defined below, this automagically changes the
    ;; main dictionary if one is defined for the docclass in
    ;; `predictive-latex-docclass-alist'
    (auto-overlay-load-definition
     'predictive
     '(predictive-latex-docclass
       (("\\([^\\]\\|^\\)\\(\\\\\\\\\\)*\\\\documentclass\\(\\[.*?\\]\\)?{\\(.*?\\)}"
	 . 4))))
;;;     (auto-overlay-load-definition
;;;      'predictive
;;;      '(predictive-latex-docclass
;;;        (("^\\\\documentclass\\(\\[.*?\\]\\)?{\\(.*?\\)}" . 2))))


    ;; \usepackage loads a latex package. Through the use of a special
    ;; "usepackage" regexp class defined below, this automagically loads new
    ;; dictionaries and auto-overlay regexps.
    (auto-overlay-load-definition
     'predictive
     '(predictive-latex-usepackage
       (("\\([^\\]\\|^\\)\\(\\\\\\\\\\)*\\\\usepackage\\(\\[.*?\\]\\)?{\\(.*?\\)}"
	 . 4))))
;;;     (auto-overlay-load-definition
;;;      'predictive
;;;      '(predictive-latex-usepackage
;;;        (("^\\\\usepackage\\(\\[.*?\\]\\)?{\\(.*?\\)}" . 2))))


    ;; \label creates a cross-reference label. Through the use of a special
    ;; "auto-dict" regexp class defined below, this automagically adds the label
    ;; to the label dictionary.
    (auto-overlay-load-definition
     'predictive
     '(predictive-auto-dict
       :id label
       (("\\([^\\]\\|^\\)\\(\\\\\\\\\\)*\\\\label{\\(.*?\\)}" . 3)
	(auto-dict . predictive-latex-label-dict))))
;;;     (auto-overlay-load-definition
;;;      'predictive
;;;      '(predictive-auto-dict
;;;        :id label-bol
;;;        (("^\\\\label{\\(.*?\\)}" . 1)
;;; 	(auto-dict . predictive-latex-label-dict))))

    ;; \newcommand defines a new command. Through the use of a special
    ;; "auto-dict" regexp class defined below, this automagically adds the
    ;; command to the LaTeX and math dictionaries (there's no way to tell
    ;; whether the new command is a text-mode or math-mode command, so we add it
    ;; to both).
    (auto-overlay-load-definition
     'predictive
     '(predictive-auto-dict
       :id newcommand
       (("\\([^\\]\\|^\\)\\(\\\\\\\\\\)*\\\\newcommand\\*?{\\(.*?\\)}" . 3)
	(auto-dict . predictive-latex-local-latex-dict))))
;;;     (auto-overlay-load-definition
;;;      'predictive
;;;      '(predictive-auto-dict
;;;        :id newcommand-bol
;;;        (("^\\\\newcommand\\*?{\\(.*?\\)}" . 1)
;;; 	(auto-dict . predictive-latex-local-latex-dict))))

    ;; \newenvironment and \newtheorem define new environments. Through the use
    ;; of a special "auto-dict" regexp class defined below, this automagically
    ;; adds the environment to the local environment dictionary.
    (auto-overlay-load-definition
     'predictive
     '(predictive-auto-dict
       :id newenvironment
       (("\\([^\\]\\|^\\)\\(\\\\\\\\\\)*\\\\newenvironment{\\(.*?\\)}" . 3)
	(auto-dict . predictive-latex-local-env-dict))))
;;;     (auto-overlay-load-definition
;;;      'predictive
;;;      '(predictive-auto-dict
;;;        :id newenvironment-bol
;;;        (("^\\\\newenvironment{\\(.*?\\)}" . 1)
;;; 	(auto-dict . predictive-latex-local-env-dict))))
    (auto-overlay-load-definition
     'predictive
     '(predictive-auto-dict
       :id newtheorem
       (("\\([^\\]\\|^\\)\\(\\\\\\\\\\)*\\\\newtheorem{\\(.*?\\)}" . 3)
	(auto-dict . predictive-latex-local-env-dict))))
;;;     (auto-overlay-load-definition
;;;      'predictive
;;;      '(predictive-auto-dict
;;;        :id newtheorem-bol
;;;        (("^\\\\newtheorem{\\(.*?\\)}" . 1)
;;; 	(auto-dict . predictive-latex-local-env-dict))))

    ;; \DeclareMathOperator defines a new math-mode command. Through the use of
    ;; a special "auto-dict" regexp class defined below, this automagically adds
    ;; the environment to the local maths dictionary.
    (auto-overlay-load-definition
     'predictive
     '(predictive-auto-dict
       :id DeclareMathOperator
       (("\\([^\\]\\|^\\)\\(\\\\\\\\\\)*\\\\DeclareMathOperator\\*?{\\(.*?\\)}"
	 . 3)
	(auto-dict . predictive-latex-local-math-dict))))
;;;     (auto-overlay-load-definition
;;;      'predictive
;;;      '(predictive-auto-dict
;;;        :id DeclareMathOperator-bol
;;;        (("^\\\\DeclareMathOperator\\*?{\\(.*?\\)}" . 1)
;;; 	(auto-dict . predictive-latex-local-math-dict))))
    )
)



(defun predictive-latex-load-keybindings ()
  "Load the predictive mode LaTeX key bindings."

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
	 (punct-resolve (completion-get-resolve-behaviour punct-behaviour)))
    ;; make "\", "$", "{" and "}" do the right thing
    (setq auto-completion-override-syntax-alist
	  (append
	   `((?\\ . ((lambda ()
		       (if (and (char-before) (= (char-before) ?\\)
				(or (not (char-before (1- (point))))
				    (not (= (char-before (1- (point)))
					    ?\\))))
			   'add ',punct-resolve))
		     ,word-complete))
	     (?$ . (,punct-resolve none))
	     (?_ . (,punct-resolve none))
	     (?^ . (,punct-resolve none))
	     (?{ . ((lambda ()
		      (if (and (char-before) (= (char-before) ?\\))
			  'add ',punct-resolve))
		    (lambda ()
		      (cond
		       ((auto-overlays-at-point
			 nil '((lambda (dic)
				 (or (eq dic 'predictive-latex-env-dict)
				     (eq dic 'dict-latex-docclass)))
			       dict))
			(complete-in-buffer "" 'auto) 'none)
		       ((and (char-before) (= (char-before) ?\\))
			',word-complete)
		       (t 'none)))))
	     (?} . ((lambda ()
		      (if (and (char-before) (= (char-before) ?\\))
			  'add ',punct-resolve))
		    (lambda ()
		      (if (and (char-before) (= (char-before) ?\\))
			  ',punct-complete 'none))))
	     (?\" . ((lambda ()
		       (if (and (char-before) (= (char-before) ?\\))
			   'add ',punct-resolve))
		     (lambda ()
		       (if (and (char-before (1- (point)))
				(= (char-before (1- (point))) ?\\))
			   ',punct-complete 'none))
		     (lambda ()
		       (if (or (and (char-before) (= (char-before) ?\\))
			       (not (fboundp 'TeX-insert-quote)))
			   t
			 (TeX-insert-quote nil)
			 nil))))
	     (?' . ((lambda ()
		      (if (and (char-before) (= (char-before) ?\\))
			  'add ',punct-resolve))
		    (lambda ()
		      (if (and (char-before (1- (point)))
			       (= (char-before (1- (point))) ?\\))
			  ',punct-complete 'none))))
	     (?\( . ((lambda ()
		       (if (and (char-before) (= (char-before) ?\\))
			   'add ',punct-resolve))
		     (lambda ()
		       (if (and (char-before (1- (point)))
				(= (char-before (1- (point))) ?\\))
			   ',punct-complete 'none))))
	     (?\) . ((lambda ()
		       (if (and (char-before) (= (char-before) ?\\))
			   'add ',punct-resolve))
		     (lambda ()
		       (if (and (char-before (1- (point)))
				(= (char-before (1- (point))) ?\\))
			   ',punct-complete 'none))))
	     (?+ . ((lambda ()
		      (if (and (char-before) (= (char-before) ?\\))
			  'add ',punct-resolve))
		    (lambda ()
		      (if (and (char-before (1- (point)))
			       (= (char-before (1- (point))) ?\\))
			  ',punct-complete 'none))))
	     (?, . ((lambda ()
		      (if (and (char-before) (= (char-before) ?\\))
			  'add ',punct-resolve))
		    (lambda ()
		      (if (and (char-before (1- (point)))
			       (= (char-before (1- (point))) ?\\))
			  ',punct-complete 'none))))
	     (?- . ((lambda ()
		      (if (and (char-before) (= (char-before) ?\\))
			  'add ',punct-resolve))
		    (lambda ()
		      (if (and (char-before (1- (point)))
			       (= (char-before (1- (point))) ?\\))
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
			  ',punct-complete 'none))))
	     (?< . ((lambda ()
		      (if (and (char-before) (= (char-before) ?\\))
			  'add ',punct-resolve))
		    (lambda ()
		      (if (and (char-before (1- (point)))
			       (= (char-before (1- (point))) ?\\))
			  ',punct-complete 'none))))
	     (?= . ((lambda ()
		      (if (and (char-before) (= (char-before) ?\\))
			  'add ',punct-resolve))
		    (lambda ()
		      (if (and (char-before (1- (point)))
			       (= (char-before (1- (point))) ?\\))
			  ',punct-complete 'none))))
	     (?> . ((lambda ()
		      (if (and (char-before) (= (char-before) ?\\))
			  'add ',punct-resolve))
		    (lambda ()
		      (if (and (char-before (1- (point)))
			       (= (char-before (1- (point))) ?\\))
			  ',punct-complete 'none))))
	     (?\[ . ((lambda ()
		       (if (and (char-before) (= (char-before) ?\\))
			   'add ',punct-resolve))
		     (lambda ()
		       (if (and (char-before (1- (point)))
				(= (char-before (1- (point))) ?\\))
			   ',punct-complete 'none))))
	     (?\] . ((lambda ()
		       (if (and (char-before) (= (char-before) ?\\))
			   'add ',punct-resolve))
		     (lambda ()
		       (if (and (char-before (1- (point)))
				(= (char-before (1- (point))) ?\\))
			   ',punct-complete 'none))))
	     (?` . ((lambda ()
		      (if (and (char-before) (= (char-before) ?\\))
			  'add ',punct-resolve))
		    (lambda ()
		      (if (and (char-before (1- (point)))
			       (= (char-before (1- (point))) ?\\))
			  ',punct-complete 'none))))
 	     )
	   auto-completion-override-syntax-alist) ; append
	  )
    )
)



;;;=======================================================================
;;;                 Miscelaneous interactive commands

(defun predictive-latex-reparse-buffer ()
  "Clear all auto-overlays, then reparse buffer from scratch."
  (interactive)

  ;; stop the predictive auto-overlays without saving to file
  (auto-overlay-stop 'predictive)
  ;; kill local modifications to dict lists (dictionaries are added again when
  ;; usepackage overlays are reparsed)
  (kill-local-variable 'predictive-latex-dict)
  (make-local-variable 'predictive-latex-dict)
  (kill-local-variable 'predictive-latex-math-dict)
  (make-local-variable 'predictive-latex-math-dict)
  (kill-local-variable 'predictive-latex-preamble-dict)
  (make-local-variable 'predictive-latex-preamble-dict)
  (kill-local-variable 'predictive-latex-env-dict)
  (make-local-variable 'predictive-latex-env-dict)
  (kill-local-variable 'predictive-latex-bibstyle-dict)
  (make-local-variable 'predictive-latex-bibstyle-dict)
  ;; clear and reload the overlay definitions (have to do this, otherwise some
  ;; auto-overlays try to add duplicate regexp definitions when reparsed)
  (setq auto-overlay-regexps nil)
  (predictive-latex-load-regexps)
  ;; restart the predictive auto-overlays
  (auto-overlay-start 'predictive nil 'ignore-save-file 'no-regexp-check)
)



(defun predictive-latex-goto-matching-delim ()
  "If the point is currently on some kind of LaTeX delimeter
\(\\begin{...}, \\end{...}, \\[, \\] or $\), move to its matching
delimeter."
  (interactive)

  ;; get innermost LaTeX environment match overlay
  (let ((o-match
	 (car (sort (auto-overlays-at-point
		     nil `((eq auto-overlay-match t)
			   (eq set-id predictive)
			   (,(lambda (entry-id)
			       (or (eq entry-id 'environment)
				   (eq entry-id 'inline-math)
				   (eq entry-id 'display-math)))
			    entry-id)))
		    (lambda (a b)
		      (and (<= (- (overlay-end a) (overlay-start a))
			       (- (overlay-end b) (overlay-start b)))))
		    )))
	o-parent o-other)

    ;; if we haven't found a match overlay, display a message
    (if (not o-match)
	(message "Not at a LaTeX delimeter")
      ;; otherwise, if other edge of its parent is not matched, display
      ;; message
      (if (not (and (setq o-parent (overlay-get o-match 'parent))
		    (setq o-other
			  (overlay-get o-parent
				       (if (eq o-match
					       (overlay-get o-parent 'start))
					   'end 'start)))))
	  (message "Unmatched LaTeX delimeter")
	;; otherwise, move point to the other edge
	(goto-char (overlay-get o-other
				(if (eq o-other (overlay-get o-parent 'start))
				    'delim-end 'delim-start)))))
    )
)



(defun predictive-latex-goto-delim-start ()
  "If the point is currently within a LaTeX environment or
math-mode, move to the start of it."
  (interactive)

  ;; get innermost LaTeX environment overlay
  (let ((overlay
	 (car (sort (auto-overlays-at-point
		     nil `((identity auto-overlay)
			   (eq set-id predictive)
			   (,(lambda (entry-id)
			       (or (eq entry-id 'environment)
				   (eq entry-id 'inline-math)
				   (eq entry-id 'display-math)))
			    entry-id)))
		    (lambda (a b)
		      (and (<= (- (overlay-end a) (overlay-start a))
			       (- (overlay-end b) (overlay-start b)))))
		    )))
	o-match)

    ;; if we've found an overlay, and it's start-matched, move to its start
    ;; match
    (if (and overlay (setq o-match (overlay-get overlay 'start)))
	(goto-char (overlay-get o-match 'delim-end))
      (message "Not within a LaTeX environment")))
)



(defun predictive-latex-goto-delim-end ()
  "If the point is currently within a LaTeX environment or
math-mode, move to the end of it."
  (interactive)

  ;; get innermost LaTeX environment overlay
  (let ((overlay
	 (car (sort (auto-overlays-at-point
		     nil `((identity auto-overlay)
			   (eq set-id predictive)
			   (,(lambda (entry-id)
			       (or (eq entry-id 'environment)
				   (eq entry-id 'inline-math)
				   (eq entry-id 'display-math)))
			    entry-id)))
		    (lambda (a b)
		      (and (<= (- (overlay-end a) (overlay-start a))
			       (- (overlay-end b) (overlay-start b)))))
		    )))
	o-match)

    ;; if we've found an overlay, and it's start-matched, move to its start
    ;; match, otherwise display message
    (if (and overlay (setq o-match (overlay-get overlay 'end)))
	(goto-char (overlay-get o-match 'delim-start))
      (message "Not within a LaTeX environment")))
)



;;; FIXME: generalise label searching to allow cycling through all label
;;;        definitions and references

(defun predictive-latex-goto-label-def ()
  "If the point is on a reference command, move to the label it
refers to."
  (interactive)

  (let ((current-dict (predictive-current-dict))
	label overlay-list filtered-list)
    (when (dictree-p current-dict) (setq current-dict (list current-dict)))

    ;; when we're not within a referencing command (which we check by checking
    ;; if current dictionary is the label dictionary), display a message,
    ;; otherwise...
    (if (null (member (eval (predictive-auto-dict-name "latex-label"))
		      current-dict))
	(message "Not on LaTeX cross-reference")

      ;; get label name and list of label overlays
      (setq label (thing-at-point 'predictive-latex-label-word))
      (setq overlay-list (auto-overlays-in (point-min) (point-max)
					   '((identity auto-overlay)
					     (eq set-id predictive)
					     (eq entry-id label))))
      ;; filter overlay list for matching label name (filtered list should
      ;; only contain one element, unless label is multiply defined)
      (mapc (lambda (o)
	      (when (string= (buffer-substring-no-properties
			      (overlay-start o) (overlay-end o))
			     label)
		(push o filtered-list)))
	    overlay-list)
      ;; goto matching label definition, displaying warning if the label is
      ;; multiply defined
      (when filtered-list
	(goto-char (overlay-start (car filtered-list)))
	(when (> (length filtered-list) 1)
	  (message "LaTeX label \"%s\" is multiply defined" label)))
      ))
)




;;;=======================================================================
;;;    Automatic main dictionary switching based on document class

(put 'predictive-latex-docclass 'auto-overlay-parse-function
     'predictive-latex-parse-docclass-match)
(put 'predictive-latex-docclass 'auto-overlay-suicide-function
     'predictive-latex-docclass-suicide)


(defun predictive-latex-parse-docclass-match (o-match)
  ;; Create a new word overlay for a docclass command, and load and set the
  ;; appropriate dictionaries

  ;; create new word overlay and extract docclass name
  (let ((o-new (auto-o-parse-word-match o-match))
	(docclass (buffer-substring-no-properties
		   (overlay-get o-match 'delim-start)
		   (overlay-get o-match 'delim-end)))
	dict)
    ;; save the docclass in an overlay property
    (overlay-put o-match 'docclass-name docclass)
    ;; update the main dict
    (predictive-latex-docclass-change-main-dict docclass)
    ;; return the new overlay
    o-new)
)



(defun predictive-latex-docclass-suicide (o-match)
  ;; Delete the word overlay for a docclass command, and unload the
  ;; appropriate dictionaries

  (let ((docclass (overlay-get o-match 'docclass-name)))
    ;; delete the overlay
    (auto-o-delete-overlay (overlay-get o-match 'parent))
    ;; unload the dictionary and restore the default main dictionary
    (predictive-latex-docclass-restore-main-dict docclass))
)



(defun predictive-latex-schedule-docclass-update
  (o-self modified &rest unused)
  ;; All docclass overlay modification hooks are set to this function, which
  ;; schedules `predictive-latex-docclass-update' to run after any suicide
  ;; functions have been called
  (unless modified
    (add-to-list 'auto-o-pending-post-suicide
		 (list 'predictive-latex-docclass-update o-self)))
)



(defun predictive-latex-docclass-update (o-self)
  ;; Update the main dictionary dictionary after a modification, in case
  ;; docclass has changed

  (let* ((o-match (overlay-get o-self 'start))
	 (docclass (buffer-substring-no-properties
		    (overlay-get o-match 'delim-start)
		    (overlay-get o-match 'delim-end))))
    ;; if we haven't been deleted by a suicide function, and docclass has
    ;; changed, restore main dict and then change to new one
    (when (and (overlay-buffer o-self)
	       (not (string= docclass (overlay-get o-match 'docclass))))
      (predictive-latex-docclass-restore-main-dict
       (overlay-get o-match 'docclass))
      (predictive-latex-docclass-change-main-dict docclass)))
)



(defun predictive-latex-docclass-change-main-dict (docclass)
  ;; If there is a dictionary associated with the docclass matched by OVERLAY,
  ;; load it and change the main dict

  ;; look for a dictionary associated with the docclass
  (let ((dict-list (assoc docclass predictive-latex-docclass-alist)))
    (when dict-list
      (setq dict-list (cdr dict-list))
      (when (atom dict-list) (setq dict-list (list dict-list)))
      ;; if loading of any of the dictionaries in the list fails, unload those
      ;; which succeeded and don't change dictionary
      (if (not (catch 'failed
		 (mapc (lambda (dic)
			 (unless (predictive-load-dict dic)
			   (throw 'failed nil)))
		       dict-list)))
	  (mapc 'predictive-unload-dict dict-list)
	;; otherwise, unload the old main dictionary and change to the new one
	(let ((old-dict predictive-restore-main-dict))
	  (when (atom old-dict) (setq old-dict (list old-dict)))
	  (mapc 'predictive-unload-dict old-dict))
	(setq predictive-main-dict (append dict-list predictive-latex-dict))
	)))
)



(defun predictive-latex-docclass-restore-main-dict (docclass)
  ;; Unload any dictionary that has been loaded for DOCCLASS, and restore the
  ;; default main dict
  (let ((dict-list (assoc docclass predictive-latex-docclass-alist)))
    (when dict-list
      (setq dict-list (cdr dict-list))
      (when (atom dict-list) (setq dict-list (list dict-list)))
      (mapc 'predictive-unload-dict dict-list)
      (setq dict-list predictive-restore-main-dict)
      (when (atom dict-list) (setq dict-list (list dict-list)))
      (setq predictive-main-dict
	    (append dict-list predictive-latex-dict
		    (if (dictree-p predictive-latex-local-latex-dict)
			(list predictive-latex-local-latex-dict)
		      predictive-latex-local-latex-dict)))
      ))
)




;;;=======================================================================
;;;  Automatic loading and unloading of LaTeX package dictionaries etc.

(put 'predictive-latex-usepackage 'auto-overlay-parse-function
     'predictive-latex-parse-usepackage-match)
(put 'predictive-latex-usepackage 'auto-overlay-suicide-function
     'predictive-latex-usepackage-suicide)


(defun predictive-latex-parse-usepackage-match (o-match)
  ;; Create a new word overlay for a usepackage command, and load the
  ;; appropriate dictionaries

  ;; create new word overlay
  (let ((o-new (auto-o-parse-word-match o-match))
	package)
    ;; extract package name
    (setq package (buffer-substring-no-properties
		   (overlay-get o-match 'delim-start)
		   (overlay-get o-match 'delim-end)))

    ;; save package name in overlay property
    (overlay-put o-match 'package-name package)
    ;; load package dictionaries and run the load function
    (predictive-latex-load-package package)
    ;; add change function to overlay modification hooks
    (overlay-put o-new 'modification-hooks
		 (cons 'predictive-latex-schedule-usepackage-update
		       (overlay-get o-new 'modification-hooks)))
    (overlay-put o-new 'insert-in-front-hooks
		 (cons 'predictive-latex-schedule-usepackage-update
		       (overlay-get o-new 'insert-in-front-hooks)))
    (overlay-put o-new 'insert-behind-hooks
		 (cons 'predictive-latex-schedule-usepackage-update
		       (overlay-get o-new 'insert-behind-hooks)))
    ;; return the new overlay
    o-new)
)



(defun predictive-latex-usepackage-suicide (o-match)
  ;; Delete the word overlay for a usepackage command, and unload the
  ;; appropriate dictionaries

  (let ((package (overlay-get o-match 'package-name)))
    ;; delete the overlay
    (auto-o-delete-overlay (overlay-get o-match 'parent))
    ;; unload package dictionaries and run the unload function
    (predictive-latex-unload-package package))
)



(defun predictive-latex-schedule-usepackage-update
  (o-self modified &rest unused)
  ;; All usepackage overlay modification hooks are set to this function, which
  ;; schedules `predictive-latex-usepackage-update' to run after any suicide
  ;; functions have been called
  (unless modified
    (add-to-list 'auto-o-pending-post-suicide
		 (list 'predictive-latex-usepackage-update o-self)))
)



(defun predictive-latex-usepackage-update (o-self)
  ;; Update the package dictionaries and re-run the load function after a
  ;; modification, in case package name has changed

  (let (package)
    ;; if we haven't been deleted by a suicide function...
    (when (overlay-buffer o-self)
      ;; unload old package dictionaries and run unload function
      (predictive-latex-unload-package
       (overlay-get (overlay-get o-self 'start) 'package-name))
      ;; extract package name
      (setq package (buffer-substring-no-properties
		     (overlay-start o-self)
		     (overlay-end o-self)))
      ;; load new package dictionaries and run load function
      (overlay-put (overlay-get o-self 'start) 'package-name package)
      (predictive-latex-load-package package)))
)



(defun predictive-latex-load-package (package)
  "Load a LaTeX PACKAGE into the current buffer.
This loads the package dictionary and runs the load functions for
the package, if they exist."
  (interactive "sPackage name: ")

  (let (dict func loaded)
    ;; try to load package dictionaries and add them to the appropriate lists
    ;; if they exists
    (dolist (dic predictive-latex-dict-classes)
      (setq dict (concat (cdr dic) package))
      (when (predictive-load-dict dict)
	(setq loaded t)
	(setq dict (intern-soft dict))
	(if (and (listp (eval (car dic)))
		 (not (dictree-p (eval (car dic)))))
	    (nconc (eval (car dic)) (list dict))
	  (set (car dic) (list (eval (car dic)) dict)))))
    
    ;; try to load lisp library for the package
    (require (intern (concat "predictive-latex-" package)) nil t)
    ;; run load function for package, if one is defined
    (setq func (nth 1 (assoc package predictive-latex-usepackage-functions)))
    (when func
      (funcall func)
      (setq loaded t))
    ;; display message if we've loaded something
    (when loaded
      (message (format "LaTeX package \"%s\" loaded" package))))
)



(defun predictive-latex-unload-package (package)
  "Unload a LaTeX PACKAGE from the current buffer.
This unloads the dictionary and runs the unload functions, if
they exist."
  (interactive "sPackage name: ")
  ;; FIXME: ought to complete on loaded package names when called
  ;;        interactively

  (let (dict)
    ;; unload any package dictionaries
    (dolist (dic predictive-latex-dict-classes)
      (when (setq dict (intern-soft (concat (cdr dic) package)))
	(predictive-unload-dict (eval dict))
	(when (and (listp (eval (car dic)))
		   (not (dictree-p (eval (car dic)))))
	  ;; we don't use "(set ... (delq ..." here because other variables
	  ;; may share structure with the dictionary list variables, and the
	  ;; element we want to delete can not be the first one, as that is
	  ;; always the standard dictionary
	  (delq dict (eval (car dic)))))))

  ;; try to load lisp library for the package
  (require (intern (concat "predictive-latex-" package)) nil t)
  ;; run unload function for package, if one is defined
  (let ((func (nth 2 (assoc package predictive-latex-usepackage-functions))))
    (when func (funcall func)))
  ;; display informative message
  (message (format "LaTeX package \"%s\" unloaded" package))
)




;;;=======================================================================
;;;  Automatic synchronization of LaTeX \begin{...} \end{...} environments

;;; FIXME: The features provided by this new auto-overlay class should be
;;;        integrated into the standard nested class once they work reliably

(put 'predictive-latex-env 'auto-overlay-parse-function
     'predictive-latex-parse-env-match)
(put 'predictive-latex-env 'auto-overlay-suicide-function
     'predictive-latex-env-suicide)
(put 'predictive-latex-env 'auto-overlay-complex-class t)



(defun predictive-latex-parse-env-match (o-match)
  ;; Perform any necessary updates of auto overlays due to a match for a
  ;; nested regexp.

  ;; add synchronization function to match overlay's modification hooks
  (overlay-put o-match 'modification-hooks
	       (append (overlay-get o-match 'modification-hooks)
		       '(predictive-latex-schedule-env-synchronize)))


  ;; update auto overlays as necessary
  (let* ((overlay-stack (auto-o-nested-stack o-match))
	 (o (car overlay-stack)))
    (cond
     ;; if the stack is empty, just create and return a new unmatched overlay
     ((null overlay-stack)
      (auto-o-make-nested o-match 'unmatched))

     ;; if appropriate edge of innermost overlay is unmatched, just match it
     ((or (and (eq (auto-o-edge o-match) 'start)
	       (not (auto-o-start-matched-p o)))
	  (and (eq (auto-o-edge o-match) 'end)
	       (not (auto-o-end-matched-p o))))
      (predictive-latex-match-env-overlay o o-match)
      ;; return nil since haven't created any new overlays
      nil)

     ;; otherwise...
     (t
      ;; create new innermost overlay and add it to the overlay stack
      (push (auto-o-make-nested o-match) overlay-stack)
      ;; sort out the overlay stack
      (predictive-latex-env-stack-cascade overlay-stack)
      ;; return newly created overlay
      (car overlay-stack)))
    )
)



(defun predictive-latex-env-suicide (o-self)
  ;; Called when match no longer matches. Unmatch the match overlay O-SELF, if
  ;; necessary deleting its parent overlay or cascading the stack.

  (let* ((overlay-stack (auto-o-nested-stack o-self))
	(o-parent (car overlay-stack)))

    (cond
     ;; if other end of parent is unmatched, just delete parent
     ((not (auto-o-edge-matched-p
	    o-parent
	    (if (eq (auto-o-edge o-self) 'start) 'end 'start)))
      (auto-o-delete-overlay o-parent))

     ;; if parent is the only overlay in the stack...
     ((= (length overlay-stack) 1)
      ;; if we're a start match, make parent start-unmatched
      (if (eq (auto-o-edge o-self) 'start)
	  (predictive-latex-match-env-overlay o-parent 'unmatched nil)
	    ;; if we're an end match, make parent end-unmatched
	(predictive-latex-match-env-overlay o-parent nil 'unmatched)))

      ;; otherwise, unmatch ourselves from parent and cascade the stack
     (t
      (overlay-put o-parent (auto-o-edge o-self) nil)
      (overlay-put o-self 'parent nil)
      (predictive-latex-env-stack-cascade overlay-stack))
     ))
)



;; Variable used to temporarily disable \begin{...} \end{...} synchronization
;; when the text within a match overlay is being modified
(defvar predictive-latex-disable-env-synchronize nil)


(defun predictive-latex-schedule-env-synchronize
  (o-self &optional modified &rest unused)
  ;; Schedule synchronization of \begin{...} and \end{...} environment names
  (unless (or modified predictive-latex-disable-env-synchronize)
    (add-to-list 'auto-o-pending-post-suicide
		 (list 'predictive-latex-env-synchronize o-self))))


(defun predictive-latex-env-synchronize (o-self)
  ;; Synchronize the corresponding start/end match after any modification

  ;; if synchronization has not been disabled, and we haven't been deleted by
  ;; the suicide function...
  (when (and (not predictive-latex-disable-env-synchronize)
	     (overlay-buffer o-self))
    (let ((o-other (overlay-get (overlay-get o-self 'parent)
				(if (eq (auto-o-edge o-self) 'start)
				    'end 'start)))
	  env)

      ;; if other end of parent overlay is matched...
      (when o-other
	(save-excursion
	  ;; get environment name from self
	  (goto-char (overlay-start o-self))
	  (when (search-forward-regexp "{\\(.*?\\)}" (overlay-end o-self) t)
	    (setq env (match-string-no-properties 1))
	    ;; replace environment name in other edge
	    (goto-char (overlay-start o-other))
	    (when (and (search-forward-regexp "{\\(.*?\\)}"
					      (overlay-end o-other) t)
		       (not (string= env (match-string-no-properties 1))))
	      (let ((predictive-latex-disable-env-synchronize t))
		;; Have to force `auto-o-run-after-update-functions' to
		;; (recursively) call itself a second time, since doing the
		;; replace-match will schedule some suicides and updates.
		(auto-o-schedule-update (line-number-at-pos (point)))
		;; Note: the replace-match will *not* in fact cause a
		;; synchronization to be scheduled for the other match
		;; overlay, since it is impossible for a function called by
		;; `auto-o-run-after-change-functions' to schedule something
		;; else in the same pending list as itself. Therefore, the
		;; `predictive-latex-disable-env-synchronization' mechanism to
		;; protect against recursion is probably redundant.
		(replace-match env t t nil 1)))
	    )))
      ))
)



(defun predictive-latex-env-stack-cascade (overlay-stack)
  ;; Cascade the ends of the overlays in OVERLAY-STACK up or down the stack,
  ;; so as to re-establish a valid stack. It assumes that only the innermost
  ;; is incorrect.

  (let ((o (car overlay-stack)) o1)
    (cond

     ;; if innermost overlay is start-matched (and presumably
     ;; end-unmatched)...
     ((auto-o-start-matched-p o)
      ;; cascade overlay end matches up through stack until one is left
      (dotimes (i (- (length overlay-stack) 1))
	(setq o (nth i overlay-stack))
	(setq o1 (nth (+ i 1) overlay-stack))
	(predictive-latex-match-env-overlay o nil
			      (if (overlay-get o1 'end)
				    (overlay-get o1 'end)
				'unmatched)
			      nil nil 'protect-match))
      ;; if final overlay is start-matched, make it end-unmatched, otherwise
      ;; delete it
      (if (auto-o-start-matched-p o1)
	  ;; FIXME: could postpone re-parsing here in case it can be avoided
	  (predictive-latex-match-env-overlay
	   o1 nil 'unmatch nil nil 'protect-match)
	(auto-o-delete-overlay o1 nil 'protect-match)))


     ;; if innermost overlay is end-matched (and presumably
     ;; start-unmatched)...
     ((auto-o-end-matched-p o)
      ;; cascade overlay start matches up through stack until one is left
      (dotimes (i (- (length overlay-stack) 1))
	(setq o (nth i overlay-stack))
	(setq o1 (nth (+ i 1) overlay-stack))
	(predictive-latex-match-env-overlay o (if (overlay-get o1 'start)
						   (overlay-get o1 'start)
						 'unmatched)
					     nil nil nil 'protect-match))
      ;; if final overlay is end-matched, make it start-unmatched, otherwise
      ;; delete it
      (if (auto-o-end-matched-p o1)
	  ;; FIXME: could postpone re-parsing here in case it can be avoided
	  (predictive-latex-match-env-overlay
	   o1 'unmatch nil nil nil 'protect-match)
	(auto-o-delete-overlay o1 nil 'protect-match))))
    )
)



(defun predictive-latex-match-env-overlay
  (overlay start &optional end no-props no-parse protect-match)

  ;; match the overlay
  (auto-o-match-overlay overlay start end no-props no-parse protect-match)

  ;; if overlay is now both start and end matched, synchronize end with start
  (setq start (overlay-get overlay 'start))
  (setq end (overlay-get overlay 'end))
  (when (and start end)
    (save-excursion
      (let (env)
	;; get environment name from start
	(goto-char (overlay-get start 'delim-start))
	(when (search-forward-regexp "{\\(.*?\\)}" (overlay-end start) t)
	  (setq env (match-string-no-properties 1))
	  ;; replace environment name in end
	  (goto-char (overlay-start end))
	  (when (and (search-forward-regexp "{\\(.*?\\)}" (overlay-end end) t)
		     (not (string= env (match-string-no-properties 1))))
	    (let ((predictive-latex-disable-env-synchronize t))
	      (replace-match env t t nil 1)))
	  ))))
)




;;;=============================================================
;;;                Completion-browser functions

(defun predictive-latex-construct-browser-menu (prefix completions)
  "Construct the LaTeX browser menu keymap."

  ;; construct menu, dropping the last two entries which are a separator and a
  ;; link back to the basic completion menu (would just redisplay this menu,
  ;; since we're using the browser as the default menu)
  (let ((menu (completion-construct-browser-menu
	       prefix completions 'predictive-latex-browser-menu-item)))
    (setq menu (butlast menu 2)))
)



(defun predictive-latex-browser-menu-item (prefix completion &rest ignore)
  "Construct predictive LaTeX completion browser menu item."

  (cond
   ;; if entry is \begin or \end, create sub-menu containing environment
   ;; completions
   ((or (string= (concat prefix completion) "\\begin")
	(string= (concat prefix completion) "\\end"))
    ;; find all latex environments
    (let ((envs (dictree-complete
		 (mapcar (lambda (dic) (if (dictree-p dic) dic (eval dic)))
			 predictive-latex-env-dict)
		 ""))
	  (menu (make-sparse-keymap)))
      (setq envs (mapcar (lambda (e) (concat completion "{" (car e) "}"))
			 envs))
      ;; create sub-menu keymap
      (setq menu (completion-browser-sub-menu
		  prefix envs 'predictive-latex-browser-menu-item
		  'completion-browser-sub-menu))
      ;; add completion itself (\begin or \end) to the menu
      (define-key menu [separator-item-sub-menu] '(menu-item "--"))
      (define-key menu [completion-insert-root]
	(list 'menu-item (concat prefix completion)
	      `(lambda () (insert ,completion))))
      ;; return the menu keymap
      menu))


   ;; if entry is \documentclass, create sub-menu containing environment
   ;; completions
   ((string= (concat prefix completion) "\\documentclass")
    ;; find all latex docclasses
    (let ((classes
	   (dictree-mapcar (lambda (word entry) word) dict-latex-docclass))
	  (menu (make-sparse-keymap)))
      (setq classes
	    (mapcar (lambda (e) (concat completion "{" e "}")) classes))
      ;; create sub-menu keymap
      (setq menu (completion-browser-sub-menu
		  prefix classes 'predictive-latex-browser-menu-item
		  'completion-browser-sub-menu))
      ;; add completion itself (i.e. \documentclass) to the menu
      (define-key menu [separator-item-sub-menu] '(menu-item "--"))
      (define-key menu [completion-insert-root]
	(list 'menu-item (concat prefix completion)
	      `(lambda () (insert ,completion))))
      ;; return the menu keymap
      menu))


   ;; if entry is \bibliographystyle, create sub-menu containing bib styles
   ((string= (concat prefix completion) "\\bibliographystyle")
    ;; find all bib styles
    (let ((styles
	   (dictree-mapcar (lambda (word entry) word) dict-latex-bibstyle))
	  (menu (make-sparse-keymap)))
      (setq styles
	    (mapcar (lambda (e) (concat completion "{" e "}")) styles))
      ;; create sub-menu keymap
      (setq menu (completion-browser-sub-menu
		  prefix styles 'predictive-latex-browser-menu-item
		  'completion-browser-sub-menu))
      ;; add completion itself (i.e. \bibliographystyle) to the menu
      (define-key menu [separator-item-sub-menu] '(menu-item "--"))
      (define-key menu [completion-insert-root]
	(list 'menu-item (concat prefix completion)
	      `(lambda () (insert ,completion))))
      ;; return the menu keymap
      menu))


   ;; otherwise, create a selectable completion item
   (t `(lambda () (insert ,completion))))
)




;;;=============================================================
;;;               Miscelaneous utility functions

(defun predictive-latex-completion-add-till-regexp (regexp)
  "Add characters up to REGEXP from a completion candidate,
then cause `completion-self-insert' to add the last typed
character and re-complete.

Intended to be used as the \"resolve\" entry in
`completion-dynamic-syntax-alist' or
`completion-dynamic-override-syntax-alist'."

  (let (overlay completion)
    ;; if completion characters contain REGEXP, insert characters up to first
    ;; regexp match, and add them to the completion overlay prefix
    (when (and (setq overlay (completion-overlay-at-point))
	       (setq completion (buffer-substring-no-properties
				 (overlay-start overlay)
				 (overlay-end overlay)))
	       (string-match regexp completion))

      (insert (setq completion (substring completion 0 (match-beginning 0))))
      (move-overlay overlay (point) (overlay-end overlay))
      (overlay-put overlay 'prefix
		   (concat (overlay-get overlay 'prefix) completion)))

    ;; return 'add, causing `completion-self-insert' to add last typed
    ;; character to the prefix
    'add)
)



(defun predictive-latex-forward-word (&optional n)
  ;; going backwards...
  (if (and n (< n 0))
      (unless (bobp)
	(dotimes (i (- n))
	  ;; make sure we're at the end of a word
	  (when (re-search-backward "\\\\\\|\\w" nil t)
	    (forward-char))
	  ;; if point is within or just after a sequence of \'s, go
	  ;; backwards for the correct number of \'s
	  (if (= (char-before) ?\\)
	      (let ((pos (point)))
		(save-excursion
		  (while (= (char-before) ?\\) (backward-char))
		  (setq pos (- pos (point))))
		(if (= (mod pos 2) 1) (backward-char) (backward-char 2)))
	    ;; otherwise, go back one word, plus one \ if there's an odd
	    ;; number of them before it
	    (backward-word 1)  ; argument not optional in Emacs 21
	    (when (and (not (bobp)) (= ?\\ (char-before)))
	      (let ((pos (point)))
		(save-excursion
		  (while (= (char-before) ?\\) (backward-char))
		  (setq pos (- pos (point))))
		(when (= (mod pos 2) 1) (backward-char))))
	    )))

    ;; going forwards...
    (unless (eobp)
      ;; deal with point within sequence of \'s
      (when (= (char-after) ?\\)
	(let ((pos (point)))
	  (save-excursion
	    (while (= (char-before) ?\\) (backward-char))
	    (setq pos (- pos (point))))
	  (when (= (mod pos 2) 1) (backward-char))))
      ;; go forward, counting \ as part of word, \\ as entire word
      (dotimes (i (or n 1))
	(when (re-search-forward "\\\\\\|\\w" nil t)
	  (backward-char))
	(re-search-forward "\\\\\\W\\|\\\\\\w+\\|\\w+" nil t)
	(when (= (char-before) ?\n) (backward-char))))
    )
)



(defun predictive-latex-label-forward-word (&optional n)
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
	  (while (and (char-before)
		      (or (= (char-syntax (char-before)) ?w)
			  (= (char-syntax (char-before)) ?_)
			  (and (= (char-syntax (char-before)) ?.)
			       (/= (char-before) ?{))))
	    (backward-char))))
    ;; going forwards...
    (unless (eobp)
      (setq n (if n n 1))
      (dotimes (i n)
	(when (and (char-after) (= (char-syntax (char-after)) ?w))
	  (forward-word 1))  ; argument not optional in Emacs 21
	(while (and (char-after)
		    (or (= (char-syntax (char-after)) ?w)
			(= (char-syntax (char-after)) ?_)
			(and (= (char-syntax (char-after)) ?.)
			     (/= (char-after) ?}))))
	  (forward-char))))
    )
)



;;; predictive-latex.el ends here
