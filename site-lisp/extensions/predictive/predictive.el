
;;; predictive.el --- predictive completion minor mode for Emacs


;; Copyright (C) 2004-2008 Toby Cubitt

;; Author: Toby Cubitt <toby-predictive@dr-qubit.org>
;; Version: 0.17.6
;; Keywords: predictive, completion
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


;;; Install:
;;
;; Put the Predictive Completion package files in your load-path, and add the
;; following to your .emacs:
;;
;;     (require 'predictive)
;;
;; Alternatively, you can use autoload instead to save memory:
;;
;;     (autoload 'predictive-mode "/path/to/predictive.elc" t)
;;
;; Full instructions are given in the INSTALL file that should come with the
;; Predictive Completion package.



;;; Change Log:
;;
;; Version 0.17.6
;; * set new `completion-includes-prefix' variable to nil in predictive-mode
;;
;; Version 0.17.5
;; * added `predictive-undefine-all-prefixes' function
;; * modified `predictive-define-all-prefixes' and `predictive-reset-weight'
;;   to avoid doing redundant computations when not called interactively
;; * modified `predictive-define-all-prefixes' to define all prefix
;;   relationships for a single prefix if one is supplied, reverting to
;;   defining all possible prefixes as before if argument is empty
;;
;; Version 0.17.4
;; * added `predictive-auxiliary-file-location' customization option
;; * created corresponding `predictive-create-auxiliary-file-location' macro,
;;   which should be called whenever a predictive-mode auxiliary file needs to
;;   be saved, so that the directory is only created when first used
;;
;; Version 0.17.3
;; * made `predictive-remove-from-dict' display informative message
;; * made `predictive-mode' cope more gracefully with failed major-mode setup
;;   function
;; * renamed `predictive-dict-compilation-mode' to
;;   `predictive-dict-compilation' to avoid confusion with compilation-mode
;; * added `predictive-which-dict-delay' customization option
;; * made `predictive-undefine-prefix' display informative message
;;
;; Version 0.17.2
;; * minor change to `predictive-reset-weight'
;;
;; Version 0.17.1
;; * fixed bug in `predictive-flush-auto-learn-caches', which failed to check
;;   that cached words were longer than `predictive-auto-add-min-chars' and
;;   passed the filter before adding them to the dictionary
;; * fixed bug in `predictive-fast-learn-from-buffer'
;; * remove text properties from string returned by `thing-at-point'
;; * fixed bug in `predictive-add-to-dict'
;; * modified which-dict-mode so that, if current dictionary is a list, it
;;   displays name of first one in mode-line, rest in help-echo text
;;
;; Version 0.17
;; * added `predictive-dict-compilation-mode' option which determines whether
;;   dictionaries are saved in compiled or uncompiled form, or both
;; * added `predictive-save-dict', `predictive-write-dict' and
;;   `predictive-save-modified-dicts' commands; these are wrappers around the
;;   corresponding dict-tree.el functions which are no longer interactive
;; * auto-add filter functions now passed two arguments: prefix and dict
;;
;; Version 0.16.4
;; * modified `predictive-fast-learn-from-buffer' to honour
;;   `predictive-auto-add-to-dict' setting
;;
;; Version 0.16.3
;; * simplified `predictive-define-all-prefixes'
;; * added `predictive-auto-add-min-chars' option to filter auto-added words
;;   that are shorter than this value
;; * added var aliases for completion-UI variables
;;
;; Version 0.16.2
;; * changed how `predictive-major-mode-alist' functions are called: now
;;   called with one argument, positive if enabling, negative if disabling
;; * added `predictive-auto-add-filter' to allow unwanted auto-added words to
;;   be filtered out before they're added to the dictionary
;;
;; Version 0.16.1
;; * added `predictive-reset-weight' command to facilitate resetting word
;;   weights
;; * remove `predictive-max-completions' customization option, long since
;;   superseded by completion-ui
;;
;; Version 0.16
;; * `predictive-add-to-dict' can once again be set to 'buffer, to auto-add
;;   words to the buffer-local dictionary
;;
;; Version 0.15
;; * auto-completion is now a separate minor-mode provided by completion-UI,
;;   and controlled by `predictive-auto-complete'
;;
;; Version 0.14.2
;; * fixed auto-define-prefixes functionality...again
;;
;; Version 0.14.1
;; * fixeds bugs in new auto-define-prefixes functionality, most significantly
;;   in `predictive-add-to-dict'
;;
;; Version 0.14
;; * enhanced the prefix definition functions
;; * changed `predictive-auto-learn' to work with new completion-UI
;;   accept/reject hooks
;; * a word can now be learned even if its completion was rejected, by
;;   supplying a prefix argument when calling `completion-reject'
;;   interactively (thanks to Mathias Dahl for related discussions)
;; * added `predictive-auto-define-prefixes' feature, which automatically
;;   defines all prefixes for any word added to a dictionary
;;
;; Version 0.13.3
;; * fixed bug in `predictive-which-dict-mode' that assumed dict names were at
;;   least 10 chars long
;; * fixed bug in `predictive-auto-learn' that prevented capitalized words
;;   being learned
;;
;; Version 0.13.2
;; * `predictive-define-prefix' now ensures prefix weight is at least as big
;;   as the word it's a prefix of
;; * added `predictive-dict-autosave-on-kill-buffer/disable-mode'
;;   customization options to allow more control over when modified
;;   dictionaries are saved
;; * `predictive-unload-dict' now features completion on names of dictionaries
;;   associated with current buffer
;;
;; Version 0.13.1
;; * Fixed minor bug in `predictive-define-prefix'
;;
;; Version 0.13
;; * finally wrote a `predictive-remove-from-dict' function!
;;
;; Version 0.12.2
;; * added `predictive-dump-dict-to-buffer/file' functions, since dict-tree.el
;;   equivalents are no longer interactive
;; * updated `dictree-create' calls to reflect change in argument list
;;
;; Version 0.12.1
;; * minor bug fixes
;;
;; Version 0.12
;; * changed buffer-local dictionary functionality to use new meta-dictionary
;;   features of `dict-tree.el'
;;
;; Version 0.11.1
;; * bug fixes in completion-ui and predictive-latex.el
;; * fixed bug in `predictive-define-prefix'
;;
;; Version 0.11
;; * moved a lot of functions to completion-ui
;;
;; Version 0.10.3
;; * bug fixes to `predictive-load-dict'
;;
;; Version 0.10.2
;; * bug fixes
;; * updated to reflect naming changes in dict-tree.el
;; * interactive definition for `predictive-create-dict' now passes empty
;;   strings for filenames by default
;;
;; Version 0.10.1
;; * bug fixes in `predictive-latex' and `dict-tree'
;; * added recommendation to create own dictionaries to mode function
;;   docstring
;;
;; Version 0.10
;; * added back `predictive-scoot-ahead' function
;; * documented keys in mode function's docstring (thanks to Mark Zonzon for
;;   patch)
;; * `predictive-completion' package renamed to `completion-ui'
;; * dictionaries can now store list of prefices for each word, whose weights
;;   are automatically kept at least as large as word's
;; * modified `predictive-add-to-dict' to take prefices into acount, and added
;;   `predictive-define-prefix' and `predictive-undefine-prefix' functions
;;
;; Version 0.9.1
;; * moved defmacros before their first use so byte-compilation works (thanks
;;   to Dan Nicolaescu for pointing out this problem)
;;
;; Version 0.9
;; * modified to use new `predictive-completion' package
;; * tweaked auto-learn caching (again)
;; * now uses command remapping for main keymap if available
;;
;; Version 0.8.2
;; * minor bug fixes
;;
;; Version 0.8.1
;; * minor bug fixes
;;
;; Version 0.8
;; * bug fixes
;; * performance tweaks to auto-learn and auto-add caching
;; * added `predictive-boost-prefix-weights' function
;; * interactive commands that read a dictionary name now provide completion
;;
;; Version 0.7
;; * switch-dictionary code moved to separate, more general, more efficient,
;;   completely re-written, and stand-alone `auto-overlays' package
;;
;; Version 0.6.1
;; * minor bug fixes
;;
;; Version 0.6
;; * predictive-auto-add-to-dict no longer buffer-local
;; * minor bug fixes
;; (major version bump because dict.el provides new `dict-dump-words-to-file'
;; function)
;;
;; Version 0.5.1
;; * fixed bugs in 'word, 'start and 'end regexp parsing code
;; * fixed bug in predictive-overlay-suicide
;;
;; Version 0.5
;; * added auto-learn and auto-add caching
;; * overhauled switch-dictionary code (again!), changed the way 'word regexps
;;   work and added new 'line regexps
;;
;; Version 0.4
;; * tidied and fixed bugs in switch-dictionary code
;; * added option to display active dictionary in mode line
;; * added functions to learn from buffers and files
;; * cleaned up auto-learning and auto-adding code
;;
;; Version 0.3.1
;; * fixed bugs in switch-dictionary regions
;;
;; Version 0.3
;; * added significantly more powerful dictionary switching features
;; * removed redundant predictive-scoot-and-insert function (same effect can
;;   easily be achieved with predictive-scoot-ahead and predictive-accept)
;;
;; Version 0.2.1:
;; * repackaging
;; * removed c setup function (should be provided in separate package)
;;
;; Version 0.2:
;; * added options for dictionary autosaving
;; * other changes required for compatibility with dict.el version 0.2
;; * fixed auto-learn bugs
;; * explicitly require cl.el
;;
;; Version 0.1:
;; * initial release



;;; Code:

(provide 'predictive)
(require 'completion-ui)
(require 'dict-tree)
(require 'auto-overlays)
(require 'timerfunctions)


;; use dynamic byte compilation to save memory
;;(eval-when-compile (setq byte-compile-dynamic t))




;;; ================================================================
;;;          Customization variables controling predictive mode 

(defgroup predictive '((completion-ui custom-group)
		       (predictive-latex custom-group))
  "Predictive completion."
  :group 'convenience)


(defcustom predictive-main-dict 'dict-english
  "*Main dictionary to use in a predictive mode buffer.

It should be the symbol of a loaded dictionary. It can also be a
list of such symbols, in which case predictive mode searches for
completions in all of them, as though they were one large
dictionary.

Note that using lists of dictionaries can lead to unexpected effets when
auto-learn or auto-add-to-dict are used. If auto-learn is enabled, weights
will be updated in the first dictionary in the list that contains the word
being updated \(see `predictive-auto-learn'\). Similarly, if auto-add-to-dict
is set to t, words will be added to the first dictionary in the list \(see
`predictive-auto-add-to-dict'\)."
  :group 'predictive
  :type 'symbol)


(defcustom predictive-completion-speed 0.1
  "*Default completion speed for new predictive mode dictionaries
created using `predictive-create-dict'.

The completion speed is a desired upper limit on the time it
takes to find completions, in seconds. However, there is no
guarantee it will be achieved!  Lower values result in faster
completion, at the expense of dictionaries taking up more
memory."
  :group 'predictive
  :type 'number)


(defcustom predictive-dict-autosave t
  "*Default autosave flag for new predictive mode dictionaries.

A value of t means modified dictionaries will be saved
automatically when unloaded (or when quitting Emacs). The symbol
'ask' means you will be prompted to save modified dictionaries. A
value of nil means dictionaries will not be saved automatically,
and unless you save the dictionary manually all changes will be
lost when the dictionary is unloaded. See also
`predictive-save-dict'."
  :group 'predictive
  :type 'boolean)


(defcustom predictive-dict-autosave-on-kill-buffer t
  "*Whether to save dictionaries when a buffer is killed.

If non-nil, modifications to dictionaries that are used by a
buffer will automatically be saved when the buffer is killed, for
dictionaries that have their autosave flag set (see
`predictive-dict-autosave')."
  :group 'predictive
  :type 'boolean)


(defcustom predictive-dict-autosave-on-mode-disable t
  "*Whether to save dictionaries when predictive mode is disabled.

If non-nil, modifications to dictionaries that are used by a
buffer will automatically be saved when predictive mode is
disabled in that buffer, for dictionaries that have their
autosave flag set (see `predictive-dict-autosave')."
  :group 'predictive
  :type 'boolean)


(defcustom predictive-dict-compilation nil
  "*Whether to save dictionaries in compiled or uncompiled form.

The default is to save both compiled and uncompiled forms. If set
to 'compiled, only the compiled form is saved. If set to
'uncompiled, only the uncompiled form is saved. The compiled form
loads faster, but is not portable across different Emacs
versions."
  :group 'predictive
  :type '(choice (const :tag "both" nil)
		 (const compiled)
		 (const uncompiled)))


(defcustom predictive-ignore-initial-caps t
  "*Whether to ignore initial capital letters when completing
words. If non-nil, completions for the uncapitalized string are
also found.

Note that only the *first* capital letter of a string is
ignored. Thus typing \"A\" would find \"and\", \"Alaska\" and
\"ANSI\", but typing \"AN\" would only find \"ANSI\", whilst
typing \"a\" would only find \"and\"."
  :group 'predictive
  :type 'boolean)


(defcustom predictive-auto-complete t
  "*Enable and disable auto-completion-mode along with predictive mode."
  :group 'predictive
  :type 'boolean)


(defcustom predictive-auto-learn nil
  "*Enables predictive mode's automatic word frequency learning.

When non-nil, the frequency count for that word is incremented
each time a completion is accepted, making the word more likely
to be offered higher up the list of completions in the future."
  :group 'predictive
  :type 'boolean)


(defcustom predictive-auto-add-to-dict nil
  "*Controls automatic adding of new words to dictionaries.

If nil, words are never automatically added to a dictionary. If
t, new words \(i.e. words that are not in the dictionary\) are
automatically added to the active dictionary. If set to a
dictionary name (a symbol), new words are automatically added to
that dictionary instead of the active one. If set to the symbol
'buffer, words will be added to the buffer-local dictionary
\(`predictive-use-buffer-local-dict' must be non-nil in this
case\). Note that this is subtly different from adding to the
active dictionary when `predictive-use-buffer-local-dict' is
enabled, which will add words to *both* the buffer-local and main
dictionaries.

If `predctive-add-to-dict-ask' is enabled, predictive mode will
ask before adding any word."
  :group 'predictive
  :type '(choice (const :tag "off" nil)
		 (const :tag "active" t)
		 (symbol :tag "dictionary")))
(make-variable-buffer-local 'predictive-auto-add-to-dict)


(defcustom predictive-add-to-dict-ask t
  "*If non-nil, predictive mode will ask before auto-adding a word
to a dictionary. Enabled by default. This has no effect unless
`predictive-auto-add-to-dict' is also enabled."
  :group 'predictive
  :type 'boolean)


(defcustom predictive-auto-add-min-chars nil
  "*Minimum length of auto-added words.
Words shorter than this will not be automatically added to the
  dictionary when `predictive-auto-add-to-dict' is enabled."
  :group 'predictive
  :type '(choice (const :tag "Off" nil)
		 (integer :tag "On")))
  

(defcustom predictive-auto-define-prefixes t
  "*Controls automatic prefix definitions in predictive mode.

If non-nil, whenever a word is added to a dictionary, it is
automatically defined to be a prefix for all words for which it
*is* a prefix. Predictive mode will then automatically ensure
that its weight is always at least as great as the weight of any
of those words, so that it takes precedence over them when
completing."
  :group 'predictive
  :type 'boolean)


(defcustom predictive-auxiliary-file-location ".predictive/"
  "*Directory to which predictive mode auxiliary files are saved.

If this is a relative path, it is relative to the current
directory of a buffer using predictive mode. This means that
files located in different directories will use separate
auxiliary file subdirectories.

Setting an absolute path is possible, but discouraged. All
auxiliary files will be created in the same directory, and there
are no safe-guards to prevent two different auxiliary files that
happen to have the same name from clobbering one another. That
said, auxiliary filenames incorporate the buffer filename, so
only identically named files in different directories pose a
risk."
  :group 'predictive
  :type 'directory
  ;; ensure trailing directory separator
  :set (lambda (var val)
	 (unless (string= (file-name-directory val) val)
	   (setq val (concat val "/")))
	 (set-default var val))
)


(defcustom predictive-use-buffer-local-dict nil
  "*If non-nil, a buffer-local dictionary will be used
in conjunction with `predictive-main-dict'. Results from both
dictionaries are combined, as though they were one large
dictionary.

The buffer-local dictionary is saved to a file in the same
directory as the buffer's associated file, and is loaded from
there the next time predictive mode is enabled in the same
buffer.

The dictionary is initially empty, if `predictive-auto-learn' or
`predictive-auto-add-to-dict' are enabled, words will be added to
it as you type. The learning rate for the word weights is
`predictive-local-learn-multiplier' times higher than that for
`predictive-main-dict', so the buffer-local dictionary will
quickly adapt to the vocabulary used in specific buffers.

Note that all the words from `predictive-main-dict' will still be
available as completions, but their weights will be modified by
the buffer-local dictionary."
  :group 'predictive
  :type 'boolean)


(defcustom predictive-buffer-local-learn-multiplier 50
  "*Multiplier for buffer-local learning rate.
When words are learnt or added to a buffer-local dictionary, the
weight increment is multiplied by this number. See also
`predictive-use-buffer-local-dict'."
  :group 'predictive
  :type 'integer)


(defcustom predictive-use-auto-learn-cache t
  "*If non-nil, auto-learned and auto-added words will be cached
and only added to the dictionary when Emacs is idle. This makes
predictive mode more responsive, since learning or adding words
can otherwise cause a small but noticeable delay when typing.

This has no effect unless `predictive-auto-learn' or
`predictive-auto-add' are enabled. See also
`predictive-flush-auto-learn-delay'."
  :group 'predictive
  :type 'boolean)


(defcustom predictive-flush-auto-learn-delay 10
  "*Time to wait before flushing auto-learn/add caches.
The caches will only be flushed after Emacs has been idle for
this many seconds. To take effect, this variable must be set
before predictive mode is enabled.

This has no effect unless `predictive-use-auto-learn-cache' is enabled."
  :group 'predictive
  :type 'number)


(defcustom predictive-which-dict nil
  "*If non-nil, display the predictive mode dictionary in the mode line."
  :group 'predictive
  :type 'boolean)


(defcustom predictive-which-dict-delay 1
  "*Number of seconds of idle time before which-dict display is updated.

See also `predictive-which-dict-mode' and `predictive-which-dict'."
  :group 'predictive
  :type 'integer)


(defcustom predictive-guess-prefix-suffixes
  '("ability" "ibility" "ity" "ables" "able" "ibles" "ible" "ally" "ingly"
    "ings" "ing" "ately" "ates" "ate" "ly" "ations" "ation" "tions" "tion"
    "ions" "ion" "atives" "ative" "ives" "ive" "ments" "ment" "ances" "ance"
    "ancy" "ish" "ists" "ist" "isms" "ism" "eous" "ous" "ians" "ian" "als"
    "al" "ed" "es" "en" "ic" "ns" "s" "d" "n" "y")
  "List of possible suffixes. Earlier entries take precedence."
  :group 'predictive
  :type '(repeat string))



;;; ==================================================================
;;;          Aliases for completion-UI customization options
;;;

(when (fboundp 'defvaralias)
  (defvaralias 'predictive-completion-max-candidates
    'completion-max-candidates)
  (defalias 'predictive-completion-resolve-old-method
    'completion-resolve-old-method)
  (defvaralias 'predictive-auto-completion-min-chars
    'auto-completion-min-chars)
  (defvaralias 'predictive-auto-completion-delay
    'auto-completion-delay)
  (defvaralias 'predictive-auto-completion-backward-delete-delay
    'auto-completion-backward-delete-delay)
  (defvaralias 'predictive-completion-use-dynamic
    'completion-use-dynamic)
  (defvaralias 'predictive-completion-dynamic-syntax-alist
    'completion-dynamic-syntax-alist)
  (defvaralias 'predictive-completion-dynamic-override-syntax-alist
    'completion-dynamic-override-syntax-alist)
  (defvaralias 'predictive-completion-use-hotkeys
    'completion-use-hotkeys)
  (defvaralias 'predictive-completion-hotkey-list
    'completion-hotkey-list)
  (defvaralias 'predictive-completion-use-tooltip
    'completion-use-tooltip)
  (defvaralias 'predictive-completion-tooltip-delay
    'completion-tooltip-delay)
  (defvaralias 'predictive-completion-tooltip-timeout
    'completion-tooltip-timeout)
  (defvaralias 'predictive-completion-tooltip-offset
    'completion-tooltip-offset)
  (defvaralias 'predictive-completion-tooltip-face
    'completion-tooltip-face)
  (defvaralias 'predictive-completion-auto-show-menu
    'completion-auto-show-menu)
  (defvaralias 'predictive-completion-browser-max-items
    'completion-browser-max-items)
  (defvaralias 'predictive-completion-browser-buckets
    'completion-browser-buckets)
  (defvaralias 'predictive-completion-use-echo
    'completion-use-echo)
)




;;; ==================================================================
;;;     Non-customization variables controlling predictive mode
;;;

;; These variables can be set in major-mode setup functions, hooks, or init
;; files. They are not customization definitions since it makes no sense for a
;; user to customize them.
;;
;; Note: default values for some are set by code at the end of this file


(defvar predictive-mode-hook nil
  "Hook run after predictive mode is enabled.")

(defvar predictive-mode-disable-hook nil
  "Hook run after predictive mode is disabled.")


;; FIXME: should this be a customization option?
(defvar predictive-major-mode-alist nil
  "Alist associating major mode symols with functions.
The functions should take one argument. The alist is checked
whenever predictive mode is enabled or disabled in a buffer. If
the buffer's major made matches an entry in the alist, the
associated function is called, with a positive argument if
predictive mode is being enabled or a negative one if it is being
disabled. This makes it easier to customize predictive mode for
different major modes.")


(defvar predictive-completion-filter nil
  "Function that returns a filter function for completions.
When set, this function is called with one argument: the prefix
that is being completed (a string). The function it returns
should take two arguments: a word from a dictionary and the value
stored for that word.

Note: this can be overridden by an \"overlay local\" binding (see
`auto-overlay-local-binding').")


(defvar predictive-auto-add-filter nil
  "Function called to decide whether to auto-add a word to a dictionary.
When set, this function is called with two arguments: the word
potentially being added (a string), and the dictionary it would
be added to. It should return non-nil if the word should be added
to the dictionary, nil if it should not. Only used when
`predictive-auto-add-to-dict' is enabled.")


(defvar predictive-map (make-sparse-keymap)
  "Keymap used in predictive mode.")





;;; ================================================================
;;;             Internal variables to do with completion

;; variables storing auto-learn and auto-add caches
(defvar predictive-auto-learn-cache nil)
(make-variable-buffer-local 'predictive-auto-learn-cache)

(defvar predictive-auto-add-cache nil)
(make-variable-buffer-local 'predictive-auto-add-cache)

;; permanent timer for flushing auto-learn and auto-add caches
(defvar predictive-flush-auto-learn-timer nil)
(make-variable-buffer-local 'predictive-flush-auto-learn-timer)

;; flag used to indicate failed major-mode setup function
(defvar predictive-disable-major-mode-setup nil)
(make-variable-buffer-local 'predictive-disable-major-mode-setup)



;;; ==============================================================
;;;          Internal variables to do with dictionaries

;; when set, overrides predictive-main-dict in a buffer
(defvar predictive-buffer-dict nil)
(make-variable-buffer-local 'predictive-buffer-dict)


;; stores list of dictionaries used by buffer
(defvar predictive-used-dict-list nil)
(make-variable-buffer-local 'predictive-used-dict-list)


;; Stores current dictionary names for display in mode line
(defvar predictive-which-dict-name nil)
(defvar predictive-which-dict-list nil)
(make-variable-buffer-local 'predictive-which-dict-name)
(make-variable-buffer-local 'predictive-which-dict-list)


;; Store buffer and point for which last dictionary name update was performed
(defvar predictive-which-dict-last-update nil)


;; Stores idle-timer that updates the current dictionary name
(defvar predictive-which-dict-timer nil)
(make-variable-buffer-local 'predictive-which-dict-timer)




;;; ================================================================
;;;                       Convenience macros

(defun predictive-capitalized-p (string)
  ;; Return t if string is capitalized (only first letter upper case), nil
  ;; otherwise.
  (and (> (length string) 0)
       (= (aref string 0) (upcase (aref string 0)))
       (not (= (aref string 0) (downcase (aref string 0))))
       (or (= 1 (length string))
	   (string= (substring string 1) (downcase (substring string 1)))))
)



(defmacro predictive-create-auxiliary-file-location ()
  ;; Create directory specified by `predictive-auxiliary-file-locaion' for
  ;; current buffer, if necessary.
  '(make-directory predictive-auxiliary-file-location t))



(defmacro predictive-buffer-local-dict-name ()
  ;; Return the buffer-local dictionary name
  '(intern
    (concat "dict-"
	    (replace-regexp-in-string
	     "\\." "-"
	     (file-name-nondirectory
	      (or (buffer-file-name) (buffer-name)))))))



(defmacro predictive-buffer-local-meta-dict-name ()
  ;; Return the buffer-local meta-dictionary name
  '(intern
    (concat "dict-meta-"
	    (replace-regexp-in-string
	     "\\." "-"
	     (file-name-nondirectory
	      (or (buffer-file-name)
		  (buffer-name)))))))




;;; ===============================================================
;;;                   The minor mode definition

;; the mode variable
(defcustom predictive-mode nil
  "Non-nil if Predictive Completion mode is enabled.
Setting this variable directly will have no effect. Use \\[customize] or
`predictive-mode' command instead."
  :group 'predictive
  :type 'boolean
  :set (lambda (symbol value) (predictive-mode (or value 0)))
  :initialize 'custom-initialize-default)

(make-variable-buffer-local 'predictive-mode)


;; setup the mode-line indicator
(add-to-list 'minor-mode-alist
	     '(predictive-mode
	       (" Predict" (predictive-which-dict-mode
			    ("["
			     (:eval
			      (let ((str predictive-which-dict-name))
				(add-text-properties
				 0 (length str)
				 `(help-echo ,predictive-which-dict-list)
				 str)
				str))
			     "]")))))


;; add the minor mode keymap to the list
(let ((existing (assq 'predictive-mode minor-mode-map-alist))
      (mode-alist minor-mode-map-alist)
      (new (cons 'predictive-mode predictive-map)))
  
  ;; if it's already there, just update the keymap part
  (if existing
      (setcdr existing predictive-map)
    
    ;; otherwise, we have to make sure predictive mode's keymap comes after
    ;; `completion-hotkey-map', so search the list for its enabling
    ;; variable: `completion-use-hotkeys'
    (while (and mode-alist
		(not (eq (car (nth 0 mode-alist)) 'completion-use-hotkeys)))
      (setq mode-alist (cdr mode-alist)))
    ;; if it was found in the list, add `predictive-map' after it
    (if mode-alist
	(setcdr mode-alist (cons new (cdr mode-alist)))
    ;; otherwise, just add `predictive-map' to the front (though this should
    ;; never happen if `predictive-selection' package loaded successfully
      (push new minor-mode-map-alist))
    ))



(defun predictive-mode (&optional arg)
  "Toggle Predictive Completion mode.
With no argument, this command toggles the mode.
A positive prefix argument turns the mode on.
A negative prefix argument turns it off.

Note that simply setting the minor-mode variable
`predictive-mode' is not sufficient to enable predictive
mode. Use the command `predictive-mode' instead.

Predictive Completion mode implements predictive text completion,
in an attempt to save on typing. It looks up completions for the
word currently being typed in a dictionary. See the `predictive'
and `completion-ui' customization groups for documentation on the
various configuration options, and the Predictive Completion mode
manual for fuller information.

If the `completion-use-dynamic' customization option is enabled,
typing a character in predictive mode will either add to, accept
or reject the current dynamic completion, depending on the
character's syntax.

You can also use the following keys when completing a word:
\\<completion-dynamic-map>
\\[completion-accept] \t\t Accept current dynamic completion.
\\[completion-reject] \t\t Reject current dynamic completion.
\\[completion-cycle] \t\t Cycle through available completion candidates.
\\[completion-tab-complete] \t\t Insert longest common prefix.
\\[completion-show-menu] \t Show the completion menu below the point.
\\<completion-menu-map>
If `completion-use-menu' is enabled, you can also display the
completion menu with M-down.

If the `completion-use-hotkeys' customization option is enabled,
you can select from a list of completions (displayed in the echo
area if `completion-use-echo' is enabled) by typing a single key.
Enabling the `completion-use-tooltip' customization option will
cause completions to be displayed in a tooltip below the point.

Although the English dictionary supplied with the Predictive
Completion Mode package gives quite good results \"out of the
box\", for best results you're strongly encouraged to create your
own dictionaries and train them on your own text (the recommended
setup is to create one dictionary for each type of writing you
do: emails, academic research articles, letters...)"
  
  (interactive "P")
  
  (cond
   ;; do nothing if enabling/disabling predictive mode and it is already
   ;; enabled/disabled
   ((and arg (eq predictive-mode (> (prefix-numeric-value arg) 0))))

   
   ;; ----- enabling predictive mode -----
   ((not predictive-mode)
    ;; set the completion function
    (setq completion-function 'predictive-complete)
    (setq completion-includes-prefix nil)
    ;; make sure main dictionary is loaded
    (when predictive-main-dict
      (if (atom predictive-main-dict)
	  (predictive-load-dict predictive-main-dict)
	(mapc 'predictive-load-dict predictive-main-dict)))
    ;; make sure modified dictionaries used in the buffer are saved when the
    ;; bufer is killed
    (when predictive-dict-autosave-on-kill-buffer
      (add-hook 'kill-buffer-hook
		(lambda ()
		  (dictree-save-modified predictive-used-dict-list
					 predictive-dict-compilation))
		nil 'local))
    ;; load/create the buffer-local dictionary if using it, and make sure it's
    ;; saved and unloaded when buffer is killed
    (when predictive-use-buffer-local-dict
      (predictive-load-buffer-local-dict)
      (add-hook 'kill-buffer-hook 'predictive-unload-buffer-local-dict
		nil 'local))
    ;; make sure auto-learn/add caches are flushed if buffer is killed, and
    ;; add auto-learn function to completion and rejection hooks (rejection
    ;; hook only causes word to be auto-learned/added if prefix arg was
    ;; supplied)
    (add-hook 'kill-buffer-hook 'predictive-flush-auto-learn-caches
	      nil 'local)
    (add-hook 'completion-accept-functions 'predictive-auto-learn nil 'local)
    (add-hook 'completion-reject-functions
	      (lambda (prefix word arg)
		(when arg (predictive-auto-learn prefix prefix)))
	      nil 'local)
    
    ;; look up major mode in major-mode-alist and call any matching function
    ;; with a positive argument to indicate enabling
    (let ((modefunc (assq major-mode predictive-major-mode-alist)))
      (when modefunc
	(if (functionp (cdr modefunc))
	    (unless (funcall (cdr modefunc) 1)
	      (message "Predictive major-mode setup function failed;\
 %s support disabled"
		       major-mode)
	      (setq predictive-disable-major-mode-setup t)
	      (sit-for 2))
	  (error "Wrong type in `predictive-major-mode-alist': functionp, %s"
		 (prin1-to-string (cdr modefunc))))))

    ;; turn on auto-completion mode if necessary
    (when predictive-auto-complete (auto-completion-mode 1))
    
    ;; turn on which-dict mode if necessary
    (when predictive-which-dict (predictive-which-dict-mode t))
    ;; setup idle-timer to flush auto-learn and auto-add caches
    (setq predictive-flush-auto-learn-timer
	  (tf-run-with-idle-timer predictive-flush-auto-learn-delay t
				  0.1 t nil
				  'predictive-flush-auto-learn-caches 'idle))
    ;; set the mode variable and run the hook
    (setq predictive-mode t)
    (run-hooks 'predictive-mode-hook)
    (message "Predictive mode enabled"))
   
   
   ;; ----- disabling predictive mode -----
   (predictive-mode
    ;; unset the completion function
    (setq completion-function nil)
    ;; turn off auto-completion mode if necessary
    (when predictive-auto-complete (auto-completion-mode -1))
    ;; turn off which-dict mode
    (predictive-which-dict-mode -1)
    ;; cancel auto-learn timer and flush the caches
    (cancel-timer predictive-flush-auto-learn-timer)
    (predictive-flush-auto-learn-caches)
    
    ;; save the dictionaries
    (when predictive-dict-autosave-on-mode-disable
      (dictree-save-modified predictive-used-dict-list
			     predictive-dict-compilation))
    (when predictive-use-buffer-local-dict
      (predictive-unload-buffer-local-dict))

    ;; if major-mode setup function failed to load, just reset the flag
    (if predictive-disable-major-mode-setup
	(setq predictive-disable-major-mode-setup nil)
      ;; otherwise, look up major mode in major-mode-alist and call any
      ;; matching function with a negative argument to indicate disabling
      (let ((modefunc (assq major-mode predictive-major-mode-alist)))
	(when modefunc
	  (if (functionp (cdr modefunc))
	      (funcall (cdr modefunc) -1)
	    (error "Wrong type in `predictive-major-mode-alist': functionp, %s"
		   (prin1-to-string (cdr modefunc)))))))
    
    ;; remove hooks
    (remove-hook 'kill-buffer-hook 'predictive-flush-auto-learn-caches 'local)
    (remove-hook 'kill-buffer-hook
		 (lambda ()
		   (dictree-save-modified predictive-used-dict-list
					  predictive-dict-compilation))
		 'local)
    (remove-hook 'kill-buffer-hook 'predictive-unload-buffer-local-dict 'local)
    (remove-hook 'completion-accept-functions 'predictive-auto-learn 'local)
    (remove-hook 'completion-reject-functions
		 (lambda (prefix word arg)
		   (when arg (predictive-auto-learn prefix word)))
		 'local)
    
    ;; delete local variable bindings
    (kill-local-variable 'predictive-used-dict-list)
    (kill-local-variable 'completion-menu)
    ;; reset the mode variable and run the hook
    (setq predictive-mode nil)
    (run-hooks 'predictive-mode-disable-hook)
    (message "Predictive mode disabled"))
   )
)




;;; ================================================================
;;;       Public functions for completion in predictive mode

(defun turn-on-predictive-mode ()
  "Turn on predictive mode. Useful for adding to hooks."
  (unless predictive-mode (predictive-mode))
)



(defun predictive-auto-learn (ignored1 word &optional ignored2)
  "Function to deal with auto-learning WORD.
Usually called after a completion is accepted."
  
  (let ((dict (predictive-current-dict))
	found dic)
    
    ;; if there is a current dict...
    (unless (eq dict t)     
      (let ((dictlist dict) wordlist)
	(when (dictree-p dict) (setq dictlist (list dict)))
	;; if ignoring initial caps, look for uncapitalized word too
	(if (and predictive-ignore-initial-caps
		 (predictive-capitalized-p word))
	    (setq wordlist (list (downcase word) word))
	  (setq wordlist (list word)))
	;; look for word in all dictionaries in list
	(setq found
	      (catch 'found
		(while dictlist
		  (setq dic (pop dictlist))
		  (dolist (wrd wordlist)
		    (when (dictree-member-p dic wrd) (throw 'found wrd)))))))
      
      
      ;; if the completion was not in the dictionary,
      ;; `predictive-auto-add-to-dict' is enabled, and either
      ;; `predictive-add-to-dict-ask' is disabled or user responded "y" when
      ;; asked, then add the new word to the appropriate dictionary
      (if (null found)
	  (when (and predictive-auto-add-to-dict
		     (or (not predictive-add-to-dict-ask)
			 (y-or-n-p
			  (format "Add word \"%s\" to dictionary? " word))))
	    ;; if adding to the currently active dictionary, then do just that,
	    ;; adding to the first in the list if there are a list of
	    ;; dictionaries
	    (cond
	     ((eq predictive-auto-add-to-dict t)
	      ;; if caching auto-added words, do so
	      (if predictive-use-auto-learn-cache
		  (push (cons word (car dict)) predictive-auto-add-cache)
		;; otherwise, check it pases the filter (if there is one),
		;; then add it to the dictionary
		(when (and (or (null predictive-auto-add-min-chars)
			       (>= (length word)
				   predictive-auto-add-min-chars))
			   (or (null predictive-auto-add-filter)
			       (funcall predictive-auto-add-filter
					word (car dict))))
		  (predictive-add-to-dict (car dict) word))))
	     
	     ;; if adding to the buffer-local dictionary...
	     ((eq predictive-auto-add-to-dict 'buffer)
	      ;; if buffer-local dictionaries are not enabled, display an
	      ;; error message
	      (if (null predictive-use-buffer-local-dict)
		  (message "The setting of `predictive-auto-add-to-dict'\
 specifies adding to the buffer-local dictionary, but buffer-local\
 dictionaries are not enabled by `predictive-use-buffer-local-dict'")
		;; if caching auto-added words, do so
		(if predictive-use-auto-learn-cache
		    (push (cons word
				(eval (predictive-buffer-local-dict-name)))
			  predictive-auto-add-cache)
		  ;; otherwise, check it passes the filter (if there is one),
		  ;; then add it to the dictionary
		  (when (and (or (null predictive-auto-add-min-chars)
				 (>= (length word)
				     predictive-auto-add-min-chars))
			     (or (null predictive-auto-add-filter)
				 (funcall
				  predictive-auto-add-filter
				  word
				  (eval (predictive-buffer-local-dict-name)))
				 ))
		    (predictive-add-to-dict
		     (eval (predictive-buffer-local-dict-name))
		     word)))))
	     
	     ;; anything else specifies an explicit dictionary to add to
	     (t
	      (setq dict (eval predictive-auto-add-to-dict))
	      ;; check `predictive-auto-add-to-dict' is a dictionary
	      (if (dictree-p dict)
		  ;; if caching auto-added words, do so
		  (if predictive-use-auto-learn-cache
		      (push (cons word dict) predictive-auto-add-cache)
		    ;; otherwise, check is passes the filter (if there is
		    ;; one), then add it to the dictionary
		    (when (and (or (null predictive-auto-add-min-chars)
				   (>= (length word)
				       predictive-auto-add-min-chars))
			       (or (null predictive-auto-add-filter)
				   (funcall predictive-auto-add-filter
					    word dict)))
		      (predictive-add-to-dict dict word)))
		;; display error message if not a dictionary
		(beep)
		(message
		 "Wrong type in `predictive-auto-add-to-dict': dictp")))
	     ))
	
	
	;; if the completion was in the dictionary and auto-learn is set...
	(when predictive-auto-learn
	  ;; if caching auto-learned words, do so
	  (if predictive-use-auto-learn-cache
	      (push (cons found dic) predictive-auto-learn-cache)
	    ;; if not caching, increment its weight in the dictionary it was
	    ;; found in
	    (predictive-add-to-dict dic found)))
	)))
)





;;; ================================================================
;;;           Internal functions to do with completion


(defun predictive-complete (prefix &optional maxnum)
  "Try to complete string PREFIX, usually the string before the point,
returning at most MAXNUM completion candidates, ordered by
their weighting.

If MAXNUM is null, all possible completion candidates are
returned in alphabetical order, rather than by weight.

If `predictive-ignore-initial-caps' is enabled and first
character of string is capitalized, also search for completions
for uncapitalized version."
  
  (let ((str prefix)
	(dict (predictive-current-dict))
	filter completions)
    
    ;; construct the completion filter
    (let ((completion-filter predictive-completion-filter))
      (setq filter (auto-overlay-local-binding 'completion-filter)))
    (when filter
      (unless (functionp filter)
	(error "Wrong type in completion-filter: functionp %s"
	       (prin1-to-string filter)))
      (setq filter (funcall filter prefix)))
    
    ;; if there is a current dictionary...
    (when dict
      ;; sort out capitalisation
      (when (and predictive-ignore-initial-caps
		 (predictive-capitalized-p prefix))
	(setq str (list prefix (downcase prefix))))
      ;; complete the prefix using the current dictionary
      (setq completions
	    (if (null maxnum)
		(dictree-complete dict str)
	      (dictree-complete-ordered dict str maxnum nil filter)))
      (when completions (setq completions (mapcar 'car completions)))
      
      ;; return the completions
      completions))
)



(defun predictive-flush-auto-learn-caches (&optional idle)
  ;; Flush entries from the auto-learn and auto-add caches, adding them to the
  ;; appropriate dictionary. If optional argument IDLE is supplied, no
  ;; informative messages are displayed, and flushing will only continue
  ;; whilst emacs is idle
  
  (let ((learn-count (length predictive-auto-learn-cache))
	(add-count (length predictive-auto-add-cache))
	entry word dict count)
    
    (unless idle
      ;; set variables used in messages
      (setq count (+ learn-count add-count))
      (message "Flushing predictive mode auto-learn caches...(word 1 of %d)"
	       count))
    
    ;; flush words from auto-learn cache
    (dotimes (i (if idle (min 1 learn-count) learn-count))
      (setq entry (pop predictive-auto-learn-cache))
      (setq word (car entry))
      (setq dict (cdr entry))
      (unless idle
	(message
	 "Flushing predictive mode auto-learn caches...(word %d of %d)"
	 i count))
      ;; add word to whichever dictionary it is found in
      (when (dictree-p dict) (setq dict (list dict)))
      (catch 'learned
	(dolist (dic dict)
	  (when (dictree-member-p dic word)
	    (predictive-add-to-dict dic word 1)
	    (throw 'learned t)))))
    
    ;; flush words from auto-add cache
    (dotimes (i (if idle (min 1 add-count) add-count))
      (setq entry (pop predictive-auto-add-cache))
      (setq word (car entry))
      (setq dict (cdr entry))
      (unless idle
	(message
	 "Flushing predictive mode auto-learn caches...(word %d of %d)"
	 i count))
      (when (and (or (null predictive-auto-add-min-chars)
		     (>= (length word) predictive-auto-add-min-chars))
		 (or (null predictive-auto-add-filter)
		     (funcall predictive-auto-add-filter word dict)))
	(predictive-add-to-dict dict word)))
    )
  
  (unless idle (message "Flushing predictive mode auto-learn caches...done"))
)





;;; ================================================================
;;;       Public functions for predictive mode dictionaries

(defun predictive-set-main-dict (dict)
  "Set the main dictionary for the current buffer.
To set it permanently, you should customize
`predictive-main-dict' instead."
  (interactive (list (read-dict "Dictionary: ")))

  ;; sort out arguments
  (cond
   ((stringp dict) (setq dict (intern-soft dict)))
   ((dictree-p dict) (setq dict (intern-soft (dictree-name dict)))))
  ;; set main dictionary in current buffer
  (make-local-variable 'predictive-main-dict)
  (setq predictive-main-dict dict)
  ;; clear predictive-which-dict-last-update so mode-line gets updated
  (setq predictive-which-dict-last-update nil)
)



(defun predictive-load-dict (dict)
  "Load the dictionary DICT into the current buffer.

DICT must be the name of a dictionary to be found somewhere in
the load path. Returns nil if dictionary fails to
load. Interactively, it is read from the mini-buffer."
  (interactive "sDictionary to load: \n")
  (unless (stringp dict) (setq dict (symbol-name dict)))

  ;; load dictionary if not already loaded
  (if (not (or (dictree-p (condition-case
			      error (eval (intern-soft dict))
			    (void-variable nil)))
	       (and (load dict t)
		    (dictree-p (condition-case
				   error (eval (intern-soft dict))
				 (void-variable nil))))))
      ;; if we failed to load dictionary, throw an error if called
      ;; interactively, otherwise just return nil
      (if (interactive-p)
	  (error "Could not load dictionary %s" (prin1-to-string dict))
	nil)
    
    ;; if we successfully loaded the dictionary, add it to buffer's used
    ;; dictionary list (note: can't use add-to-list because we want comparison
    ;; with eq, not equal)
    (setq dict (eval (intern-soft dict)))
    (unless (memq dict predictive-used-dict-list)
      (setq predictive-used-dict-list
	    (cons dict predictive-used-dict-list)))
    
    ;; indicate successful loading
    (message "Dictionary %s loaded in buffer %s"
	     (dictree--name dict) (buffer-name (current-buffer)))
    t)
)



(defun predictive-unload-dict (dict)
  "Remove DICT from the list of dictionaries used by the current buffer.
Interactively, DICT is read from the mini-buffer.

Note that this does not unload the dictionary from Emacs (see
`dictree-unload'), nor does it prevent the dictionary being used
in the buffer. It only affects which dictionaries are included
when learning from the buffer (see `predictive-learn-from-buffer'
and `predictive-fast-learn-from-buffer'), and which dictionaries
are auto-saved when the buffer is killed or predictive mode is
disabled (see `predictive-dict-autosave-on-kill-buffer' and
`predictive-dict-autosave-on-disable-mode')."
  
  (interactive (list (read-dict "Dictionary: "
				nil predictive-used-dict-list)))
  ;; sort out argument
  (when (symbolp dict) (setq dict (eval dict)))
  
  ;; remove dictionary from buffer's used dictionary list
  (setq predictive-used-dict-list (delq dict predictive-used-dict-list))
  (message "Dictionary %s unloaded from buffer %s"
	   (dictree--name dict) (buffer-name (current-buffer)))
)



(defun predictive-save-dict (dict)
  "Save dictionary DICT to its associated file.
Use `predictive-write-dict' to save to a different file.

See also `predictive-dict-compilation'."
  (interactive (list (read-dict "Dictionary to save: ")))
  (dictree-save dict predictive-dict-compilation)
)



(defun predictive-write-dict (dict filename &optional overwrite)
  "Write dictionary DICT to file FILENAME.

If optional argument OVERWRITE is non-nil, no confirmation will
be asked for before overwriting an existing file.

See also `predictive-dict-compilation'."
  (interactive (list (read-dict "Dictionary to write: ")
		     (read-file-name "File to write to: ")
		     current-prefix-arg))
  
  (dictree-write dict filename overwrite
		 predictive-dict-compilation)
)



(defun predictive-create-dict (&optional dictname file
					 populate autosave speed)
  "Create a new predictive mode dictionary called DICTNAME.

The optional argument FILE specifies a file to associate with the
dictionary. The dictionary will be saved to this file by default
\(similar to the way a file is associated with a buffer).

If POPULATE is not specified, create an empty dictionary. If
POPULATE is specified, populate the dictionary from that file
\(see `dict-populate-from-file').

If the optional argument AUTOSAVE is t, the dictionary will
automatically be saved when it is unloaded. If nil, all unsaved
changes are lost when it is unloaded. Defaults to
`predictive-dict-autosave'.

The optional argument SPEED sets the desired speed with which
string should be completed using the dictionary, in seconds. It
defaults to `predictive-completion-speed'.

Interactively, DICTNAME and FILE are read from the
minibuffer. SPEED and AUTOSAVE use the defaults provided by
`predictive-completion-speed' and `predictive-dict-autosave'
respectively."
  
  (interactive (list
		(read-string "Dictionary name: ")
		(read-file-name "File to save to \(optional): " nil "")
		(read-file-name
		 "File to populate from \(leave blank for empty dictionary\): "
		 nil "")))
  
  ;; sort out arguments
  (when (and (stringp populate) (string= populate ""))
    (setq populate nil))
  (unless (or (null populate) (file-regular-p populate))
    (setq populate nil)
    (message "File %s does not exist; creating blank dictionary" populate))
  (when (and dictname (symbolp dictname))
    (setq dictname (symbol-name dictname)))
  
  ;; confirm if overwriting existing dict, then unload existing one
  ;; (Note: we need the condition-case to work around bug in intern-soft. It
  ;;        should return nil when the symbol isn't interned, but seems to
  ;;        return the symbol instead)
  (when (or (null dictname)
	    (and (null (dictree-p (condition-case
				      error (eval (intern-soft dictname))
				    (void-variable nil))))
		 (setq dictname (intern dictname)))
	    (and (or (null (interactive-p))
		     (and (y-or-n-p
			   (format "Dictionary %s already exists. Replace it? "
			      dictname))
			  (dictree-unload (eval (intern-soft dictname)))))
		 (setq dictname (intern dictname))))
    
    (let (dict
	  (complete-speed (if speed speed predictive-completion-speed))
	  (autosave (if autosave autosave predictive-dict-autosave))
	  ;; the insertion function inserts a weight if none already exists,
	  ;; otherwise it adds the new weight to the existing one, or if
	  ;; supplied weight is nil, incremenets existing weight
	  (insfun '(lambda (weight data)
		     (cond ((not (or weight data)) 0)
			   ((null weight) (1+ data))
			   ((null data) weight)
			   (t (+ weight data)))))
	  ;; the rank function compares by weight (larger is "better"), failing
	  ;; that by string length (smaller is "better"), and failing that it
	  ;; compares the strings alphabetically
	  (rankfun '(lambda (a b)
		      (if (= (cdr a) (cdr b))
			  (if (= (length (car a)) (length (car b)))
			      (string< (car a) (car b))
			    (< (length (car a)) (length (car b))))
			(> (cdr a) (cdr b))))))
    
      ;; create the new dictionary
      (setq dict (dictree-create dictname file autosave
				 nil nil complete-speed nil
				 nil insfun rankfun))
      ;; populate it
      (if (null populate)
	  (when (interactive-p) (message "Created dictionary %s" dictname))
	(dictree-populate-from-file dict populate)
	(when (interactive-p)
	  (message "Created dictionary %s and populated it from file %s"
		   dictname populate)))
    
      ;; return the new dictionary
      dict))
)



(defun predictive-create-meta-dict
  (dictname dictlist &optional file autosave speed)
  "Create a new predictive mode meta-dictionary called DICTNAME,
based on the dictionaries in DICTLIST.

The other arguments are as for `predictive-create-dict'."
  
  (interactive
   (list (read-string "Dictionary name: ")
	 (let ((dic (read-dict
		     "Constituent dictionary (blank to end): " nil))
	       diclist)
	   (while dic
	     (setq diclist (append diclist (list dic)))
	     (setq dic
		   (read-dict "Constituent dictionary (blank to end): " nil)))
	   diclist)
	 (read-file-name "File to save to \(optional): " nil "")))
  
  ;; sort out arguments
  (when (< (length dictlist) 2)
    (error "Can't see the point in creating a meta-dictionary based on less\
 than two dictionaries"))
  (when (symbolp dictname) (setq dictname (symbol-name dictname)))
  
  ;; confirm if overwriting existing dict, then unload existing one
  ;; (Note: we need the condition-case to work around bug in intern-soft. It
  ;;        should return nil when the symbol isn't interned, but seems to
  ;;        return the symbol instead)
  (when (or (and (null (dictree-p (condition-case
				      error (eval (intern-soft dictname))
				    (void-variable nil))))
		 (setq dictname (intern dictname)))
	    (or (null (interactive-p))
		(and (y-or-n-p
		      (format "Dictionary %s already exists. Replace it? "
			      dictname))
		     (dictree-unload (eval (intern-soft dictname)))
		     (setq dictname (intern dictname)))))
    
    (let (dict
	  (complete-speed (if speed speed predictive-completion-speed))
	  (autosave (if autosave autosave predictive-dict-autosave))
	  ;; the combine function sums word weights and takes the union of any
	  ;; lists of prefices
	  (combfun '(lambda (a b)
		      ;; (need nil at end of append so both cdr's are copied)
		      (cons (cond ((null a) b) ((null b) a) (+ a b))
			    (delete-dups (append (cdr a) (cdr b) nil)))))
	  ;; the rank function compares by weight (larger is "better"), failing
	  ;; that by string length (smaller is "better"), and failing that it
	  ;; compares the strings alphabetically
	  (rankfun '(lambda (a b)
		      (if (= (cdr a) (cdr b))
			  (if (= (length (car a)) (length (car b)))
			      (string< (car a) (car b))
			    (< (length (car a)) (length (car b))))
			(> (cdr a) (cdr b))))))
    
      ;; create the new dictionary
      (setq dict (dictree-create-meta-dict dictname dictlist file autosave
					   nil nil complete-speed nil
					   combfun rankfun))
      ;; return the new dictionary
      dict))
)



(defun predictive-dump-dict-to-buffer (dict &optional buffer)
  "Dump words and their associated weights
from dictionary DICT to BUFFER. If BUFFER exists, data will be
appended to the end of it. Otherwise, a new buffer will be
created. If BUFFER is omitted, the current buffer is used.

If saved to a file, the dumped data can be used to populate a
dictionary when creating it using `predictive-create-dict'. See
also `predictive-dump-dict-to-file'."
  
  (interactive (list (read-dict "Dictionary to dump: ")
		     (read-buffer "Buffer to dump to: "
				  (buffer-name (current-buffer)))))
  (dictree-dump-to-buffer dict buffer 'string)
)



(defun predictive-dump-dict-to-file (dict &optional filename overwrite)
  "Dump words and their associated weights
from dictionary DICT to a text file FILENAME. If BUFFER exists,
data will be appended to the end of it. Otherwise, a new buffer
will be created.

If OVERWRITE is non-nil, FILENAME will be overwritten *without*
prompting if it already exists. Interactively, OVERWRITE is set
by supplying a prefix arg.

The dumped data can be used to populate a dictionary when
creating it using `predictive-create-dict'. See also
`predictive-dump-dict-to-buffer'."
  
  (interactive (list (read-dict "Dictionary to dump: ")
		     (read-file-name "File to dump to: ")
		     current-prefix-arg))
  (dictree-dump-to-file dict filename 'string overwrite)
)



(defun predictive-add-to-dict (dict word &optional weight)
  "Insert WORD into predictive mode dictionary DICT.

Optional argument WEIGHT sets the weight. If the word is not in the
dictionary, it will be added to the dictionary with initial weight WEIGHT \(or
0 if none is supplied\). If the word is already in the dictionary, its weight
will be incremented by WEIGHT \(or by 1 if WEIGHT is not supplied).

Interactively, WORD and DICT are read from the minibuffer, and WEIGHT is
specified by the prefix argument."
  (interactive (list (read-dict "Dictionary to add to: ")
		     (read-from-minibuffer
		      (concat "Word to add"
			      (let ((str (thing-at-point 'word)))
				(when str (concat " (default \"" str "\")")))
			      ": "))
		     current-prefix-arg))

  ;; if called interactively, sort out arguments
  (when (interactive-p)
    ;; throw error if no dict supplied
    (unless dict (error "No dictionary supplied"))
    ;; sort out word argument
    (when (string= word "")
      (let ((str (thing-at-point 'word)))
	(if (null str)
	    (error "No word supplied")
	  (set-text-properties 0 (length str) nil str)
	  (setq word str))))
    ;; sort out weight argument
    (unless (null weight) (setq weight (prefix-numeric-value weight))))
  
  ;; insert word
  (let* ((defpref (and predictive-auto-define-prefixes
		       (not (dictree-member-p dict word))))
	 (newweight (dictree-insert dict word weight))
	 pweight)
    
    ;; if adding a new word, and we're automatically defining prefixes...
    (when defpref
      ;; define new word to be a prefix of all its completions
      (dolist (cmpl (cdr (dictree-complete dict word)))
	(predictive-define-prefix dict (concat word (car cmpl)) word))
      ;; define all prefixes of new word (note: `predictive-define-prefix'
      ;; does nothing if prefix isn't in dict, so no need to check that here)
      (dotimes (i (1- (length word)))
	(predictive-define-prefix dict word (substring word 0 (1+ i)))))
    
    ;; if word has associated prefixes, make sure weight of each prefix is at
    ;; least as great as word's new weight
    (dolist (prefix (dictree-lookup-meta-data dict word))
      (setq pweight (dictree-lookup dict prefix))
      (when (and pweight (< pweight newweight))
	(dictree-insert dict prefix newweight (lambda (a b) a)))))

  (when (interactive-p)
    (message "\"%s\" added to dictionary %s" word (dictree-name dict)))
)




(defun predictive-remove-from-dict (dict word)
  "Delete WORD from predictive mode dictionary DICT.
Interactively, WORD and DICT are read from the minibuffer."
  (interactive (list (read-dict "Dictionary to delete from: ")
		     (read-from-minibuffer
		      (concat "Word to delete"
			      (let ((str (thing-at-point 'word)))
				(when str (concat " (default \"" str "\")")))
			      ": "))))
  
  ;; if called interactively, sort out arguments
  (when (interactive-p)
    ;; throw error if no dict supplied
    (unless dict (error "No dictionary supplied"))
    ;; sort out word argument
    (when (string= word "")
      (let ((str (thing-at-point 'word)))
	(if (null str)
	    (error "No word supplied")
	  (set-text-properties 0 (length str) nil str)
	  (setq word str)))))
  
  ;; delete word
  (if (dictree-delete dict word)
      (when (interactive-p)
	(message "\"%s\" deleted from dictionary %s" word
		 (dictree--name dict)))
    (when (interactive-p)
      (message "\"%s\" not found in dictionary %s" word
	       (dictree--name dict))))
)



(defun predictive-learn-from-buffer (&optional buffer dict all)
  "Learn word weights from BUFFER (defaults to the current buffer).

The word weight of each word in dictionary DICT is incremented by the number
of occurences of that word in the buffer. DICT can either be a dictionary, or
a list of dictionaries. If DICT is not supplied, it defaults to all
dictionaries used by BUFFER. However, DICT must be supplied if ALL is
specified (see below).

By default, only occurences of a word that occur in a region where the
dictionary is active are taken into account. If optional argument ALL is
non-nil, all occurences are taken into account. In this case, a dictionary
must be sprecified.

Interactively, BUFFER and DICT are read from the mini-buffer, and ALL is
specified by the presence of a prefix argument.

See also `predictive-fast-learn-from-buffer'."

  (interactive (list (read-buffer "Buffer to learn from: "
				  (buffer-name (current-buffer)) t)
		     (read-dict
		      "Dictionary to update (defaults to all in use): "
		      nil)
		     current-prefix-arg))

  ;; sanity check arguments
  (when (and all (null dict))
    (error "Argument ALL supplied but no dictionary specified"))
  

  (let ((d 0) i dict-list numdicts dictsize dictname
	restore-mode regexp currdict)
    (save-excursion      
      ;; switch on predictive mode in the buffer if necessary
      (when buffer (set-buffer buffer))
      (unless all
	(if predictive-mode (setq restore-mode t) (predictive-mode 1)))
      
      ;; either use list of dictionaries used in buffer, or bundle single
      ;; dictionary inside list so dolist can handle it
      (if (null dict)
	  (setq dict-list predictive-used-dict-list)
	(setq dict-list (list dict)))
      (setq numdicts (length dict-list))
      
      ;; loop over all dictionaries in dictionary list
      (dolist (dict dict-list)
	(message "Learning words for dictionary %s...(dict %d of %d)"
		 dictname d numdicts)
	;; initialise counters etc. for messages
	(setq dictname (dictree-name dict))
	(setq dictsize (dictree-size dict))
	(setq d (1+ d))  ; counts dictionaries
	(setq i 0)       ; counts words
	(if (> numdicts 1)
	    (message "Learning words for dictionary %s...(dict %d of %d,\
 word 1 of %d)" dictname d numdicts dictsize)
	  (message "Learning words for dictionary %s...(word 1 of %d)"
		   dictname dictsize))
	
	;; map over all words in dictionary
	(dictree-map
	 (lambda (word weight)   ; (value passed to weight is ignored)
	   ;; construct regexp for word
	   (setq regexp (regexp-quote word))
	   (when (= ?w (char-syntax (aref word 0)))
	     (setq regexp (concat "\\b" regexp)))
	   (when (= ?w (char-syntax (aref word (1- (length word)))))
	     (setq regexp (concat regexp "\\b")))
	   ;; count occurences of current word
	   (setq weight 0)
	   (goto-char (point-min))
	   (while (re-search-forward regexp nil t)
	     (if all
		 (setq weight (1+ weight))
	       ;; if ALL is nil, only count occurence if the active
	       ;; dictionary at that location matches the dictionary we're
	       ;; working on
	       (setq currdict (predictive-current-dict))
	       (when (or (and (listp currdict) (memq dict currdict))
			 (eq dict currdict))
		 (setq weight (1+ weight)))))
	   ;; increment word's weight
	   (predictive-add-to-dict dict word weight)
	   (when (= 0 (mod (setq i (1+ i)) 10))
	     (if (> numdicts 1)
		 (message "Learning words for dictionary %s...(dict %d of %d,\
 word %d of %d)..." dictname d numdicts i dictsize)
	       (message "Learning words for dictionary %s...(word %d of %d)"
			dictname i dictsize))))
	 dict 'string)   ; map over all words in dictionary
	
	(message "Learning words for dictionary %s...done" dictname))
      
      ;; restore predictive-mode state
      (unless (or all restore-mode) (predictive-mode -1))
      ))
)




(defun predictive-learn-from-file (file &optional dict all)
  "Learn word weights from FILE.

The word weight of each word in dictionary DICT is incremented by the number
of occurences of that word in the file. DICT can either be a dictionary, or a
list of dictionaries. If DICT is not supplied, it defaults to all dictionaries
used by FILE. However, DICT must be supplied if ALL is specified, see below.

By default, only occurences of a word that occur in a region where the
dictionary is active are taken into account. If optional argument ALL is
non-nil, all occurences are taken into account. In this case, a dictionary
must be specified.

Interactively, FILE and DICT are read from the mini-buffer, and ALL is
specified by the presence of a prefix argument."

  (interactive (list (read-file-name "File to learn from: " nil nil t)
		     (read-dict
		      "Dictionary to update (defaults to all in use): "
		      nil)
		     current-prefix-arg))
  
  (save-excursion
    ;; open file in a buffer
    (let (visiting buff)
      (if (setq buff (get-file-buffer file))
	  (setq visiting t)
	(find-file file))
      ;; learn from the buffer
      (predictive-learn-from-buffer buff dict all)
      (unless visiting (kill-buffer buff))))
)




(defun predictive-fast-learn-from-buffer (&optional buffer dict all)
  "Learn word weights from BUFFER (defaults to the current buffer).

The word weight of each word in dictionary DICT is incremented by
the number of occurences of that word in the buffer. DICT can
either be a dictionary, or a list of dictionaries. If DICT is not
supplied, it defaults to all dictionaries used by
BUFFER. However, DICT must be supplied if ALL is specified, see
below.

By default, only occurences of a word that occur in a region
where the dictionary is active are taken into account. If
optional argument ALL is non-nil, all occurences are taken into
account. In this case, a dictionary must be sprecified.

Note that this function takes the setting of
`predictive-auto-add-to-dict' and related options into
account. If an explicit dictionary is supplied, new words will be
added to that dictionary if `predictive-auto-add-to-dict' has any
non-nil value. If DICT is not supplied, the
`predictive-auto-add-to-dict' setting has the usual effect
\(which see\). If `predictive-add-to-dict-ask' is non-nil, you
will be prompted to confirm each and every word before it is
added \(so you may well wish to temporarily set
`predictive-add-to-dict-ask to nil before using this
function\). The `predictive-auto-add-min-chars' and
`predictive-auto-add-filter' variables also have their usual
effect.

Interactively, BUFFER and DICT are read from the mini-buffer, and
ALL is specified by the presence of a prefix argument.

This function is faster then `predictive-learn-from-buffer' for
large dictionaries, but will miss any words not consisting
entirely of word- or symbol-constituent characters according to
the buffer's syntax table."
  
  (interactive (list (read-buffer "Buffer to learn from: "
				  (buffer-name (current-buffer)) t)
		     (read-dict
		      "Dictionary to update (defaults to all in use): "
		      nil)
		     current-prefix-arg))
  
  ;; sanity check arguments
  (when (and all (null dict))
    (error "Argument ALL supplied but no dictionary specified"))
  
  
  (let (restore-mode currdict word percent)
    (save-excursion
      ;; switch on predictive mode in the buffer if necessary
      (when buffer (set-buffer buffer))
      (unless all
	(if predictive-mode (setq restore-mode t) (predictive-mode 1)))
      
      ;; step through each word in buffer...
      (goto-char (point-min))
      (setq percent 0)
      (message "Learning words for dictionary %s...(0%%)" (dictree-name dict))
      (while (re-search-forward "\\b\\(\\sw\\|\\s_\\)+\\b" nil t)
	(setq word (match-string-no-properties 0))
	(when (and predictive-ignore-initial-caps
		   (predictive-capitalized-p word))
	  (setq word (downcase word)))
	(cond
	 
	 ;; if ALL was specified, learn current word
	 (all
	  (when (or (dictree-member-p dict word)
		    (and predictive-auto-add-to-dict
			 (or (not predictive-add-to-dict-ask)
			     (y-or-n-p
			      (format "Add word \"%s\" to dictionary? " word))
			     )))
	    (predictive-add-to-dict dict word)))
	 
	 ;; if ALL was not specified and a dictionary has been specified, only
	 ;; increment the current word's weight if dictionary is active there
	 (dict
	  (setq currdict (predictive-current-dict))
	  (when (and (or (and (listp currdict) (memq dict currdict))
			 (eq dict currdict))
		     (or (dictree-member-p dict word)
			 (and predictive-auto-add-to-dict
			      (or (not predictive-add-to-dict-ask)
				  (y-or-n-p
				   (format "Add word \"%s\" to dictionary? "
					   word))
				  ))))
	    (predictive-add-to-dict dict word)))
	 
	 
	 ;; if ALL is not specified and no dictionary was specified, increment
	 ;; its weight in first dictionary active there that contains the word
	 (t
	  (setq currdict (predictive-current-dict))
	  (when currdict
	    (when (dictree-p currdict) (setq currdict (list currdict)))
	    (unless (catch 'learned
		      (dotimes (i (length currdict))
			(when (dictree-member-p (nth i currdict) word)
			  (predictive-add-to-dict (nth i currdict) word)
			  (throw 'learned t))))
	      
	      ;; if word wasn't in any dictionary but auto-add is enabled, add
	      ;; the word to the appropriate dictionary
	      (when (and predictive-auto-add-to-dict
			 (or (not predictive-add-to-dict-ask)
			     (y-or-n-p
			      (format "Add word \"%s\" to dictionary? "
				      word)))
			 (or (null predictive-auto-add-min-chars)
			     (>= (length word)
				 predictive-auto-add-min-chars)))
		(cond
		 
		 ;; if adding to the current dictionary, or first dictionary
		 ;; in the list if former is a list of dictionaries, then do
		 ;; so if the word passes the filter (if any)
		 ((eq predictive-auto-add-to-dict t)
		  (when (or (null predictive-auto-add-filter)
			    (funcall predictive-auto-add-filter
				     word (car currdict)))
		    (predictive-add-to-dict (car currdict) word)))
		 
		 ;; if adding to the buffer-local dictionary, do so
		 ((eq predictive-auto-add-to-dict 'buffer)
		  ;; if buffer-local dictionaries are not enabled, display an
		  ;; error message
		  (if (null predictive-use-buffer-local-dict)
		      (message "The setting of `predictive-auto-add-to-dict'\
 specifies adding to the buffer-local dictionary, but buffer-local\
 dictionaries are not enabled by `predictive-use-buffer-local-dict'")
		    ;; if word passes the filter (if any), add it to the
		    ;; buffer-local dictionary
		    (when (or (null predictive-auto-add-filter)
			      (funcall
			       predictive-auto-add-filter
			       word
			       (eval (predictive-buffer-local-dict-name))))
		      (predictive-add-to-dict
		       (eval (predictive-buffer-local-dict-name)) word))))
		 
		 ;; anything else specifies an explicit dictionary to add to
		 (t
		  (setq currdict (eval predictive-auto-add-to-dict))
		  ;; check `predictive-auto-add-to-dict' is a dictionary
		  (if (dictree-p dict)
		      (when (or (null predictive-auto-add-filter)
				(funcall predictive-auto-add-filter
					 word dict))
			(predictive-add-to-dict dict word))
		    ;; display error message if not a dictionary
		    (beep)
		    (message
		     "Wrong type in `predictive-auto-add-to-dict': dictp")))
		 ))))))
	
	
	(when (> (- (/ (float (point)) (point-max)) percent) 0.0001)
	  (setq percent (/ (float (point)) (point-max)))
	  (message "Learning words for dictionary %s...(%s%%)"
		   (dictree-name dict)
		   (progn
		     (string-match ".*\\..?.?"
				   (prin1-to-string (* 100 percent)))
		     (match-string 0 (prin1-to-string (* 100 percent))))
		   ))
	)  ; end while loop
      
      (unless (or all restore-mode) (predictive-mode -1))
      (message "Learning words for dictionary %s...done" (dictree-name dict))
      ))
)




(defun predictive-fast-learn-from-file (file &optional dict all)
  "Learn word weights from FILE.

The word weight of each word in dictionary DICT is incremented by
the number of occurences of that word in the file. DICT can
either be a dictionary, or a list of dictionaries. If DICT is not
supplied, it defaults to all dictionaries used by FILE. However,
DICT must be supplied if ALL is specified, see below.

By default, only occurences of a word that occur in a region
where the dictionary is active are taken into account. If
optional argument ALL is non-nil, all occurences are taken into
account. In this case, a dictionary must be specified.

Note that this function takes the setting of
`predictive-auto-add-to-dict' and related options into
account. If an explicit dictionary is supplied, new words will be
added to that dictionary if `predictive-auto-add-to-dict' has any
non-nil value. If DICT is not supplied, the
`predictive-auto-add-to-dict' setting has the usual effect
\(which see\). If `predictive-add-to-dict-ask' is non-nil, you
will be prompted to confirm each and every word before it is
added \(so you may well wish to temporarily set
`predictive-add-to-dict-ask to nil before using this
function\). The `predictive-auto-add-min-chars' and
`predictive-auto-add-filter' variables also have their usual
effect.

Interactively, FILE and DICT are read from the mini-buffer, and
ALL is specified by the presence of a prefix argument.

This function is faster then `predictive-learn-from-file' for
large dictionaries, but will miss any words not consisting
entirely of word- or symbol-constituent characters."

  (interactive (list (read-file-name "File to learn from: " nil nil t)
		     (read-dict
		      "Dictionary to update (defaults to all in use): "
		      nil)
		     current-prefix-arg))
  
  (save-excursion
    ;; open file in a buffer
    (let (visiting buff)
      (if (setq buff (get-file-buffer file))
	  (setq visiting t)
	(find-file file))
      ;; learn from the buffer
      (predictive-fast-learn-from-buffer buff dict all)
      (unless visiting (kill-buffer buff))))
)



(defun predictive-reset-weight (dict word &optional weight)
  "Reset the weight of WORD in dictionary DICT to 0.

If WORD is null, reset weights of all words in the
dictionary (prompting for confirmation first if this is called
interactively).

If WEIGHT is supplied, reset to that value instead of
0. Interactively, WEIGHT is the numerical prefix argument."

  (interactive (list (read-dict "Dictionary: " nil)
		     (read-string
		      "Word to reset (leave blank to reset all words): ")
		     current-prefix-arg))
  
  ;; sort out arguments
  (when (and (stringp word) (string= word "")) (setq word nil))
  (cond
   ((null weight) (setq weight 0))
   ((interactive-p) (setq weight (prefix-numeric-value weight))))
  
  ;; confirm interactive reset of all weights
  (when (or word
	    (not (interactive-p))
	    (yes-or-no-p
	     (format "Really reset weights of all words in dictionary %s? "
		     (dictree-name dict))))
    ;; if a word was specified, reset its weight to 0
    (if word
	(and (dictree-insert dict word weight (lambda (a b) a))
	     (interactive-p)
	     (message "Weight of \"%s\" in %s reset to 0"
		      word (dictree-name dict)))
      ;; if no word was specified, reset all weights to 0
      (let ((i 0) (count (when (interactive-p) (dictree-size dict))))
	(when (interactive-p)
	  (message "Resetting word weights in %s...(word 1 of %d)"
		   (dictree-name dict) count))
	(dictree-map
	 (lambda (word ignored)
	   (when (and (interactive-p) (setq i (1+ i)) (= (mod i 10) 0))
	     (message "Resetting word weights in %s...(word %d of %d)"
		      (dictree-name dict) i count))
	   (dictree-insert dict word weight (lambda (a b) a)))
	 dict)
	(when (interactive-p)
	  (message "Resetting word weights in %s...done" (dictree-name dict)))
	)))
)



(defun predictive-guess-prefix (word)
  "Guess a likely prefix for WORD.
See `predictive-define-prefix' and 'predictive-guess-prefix-suffixes'."

  (let ((suffix
	 (catch 'found
	   (dolist (sfx predictive-guess-prefix-suffixes)
	     (when (and (> (length word) (length sfx))
			(string= (substring word (- (length word) (length sfx))
					    (length word))
				 sfx))
	       (throw 'found sfx))))))
    (when suffix (substring word 0 (- (length word) (length suffix)))))
)



(defun predictive-define-prefix (dict word prefix)
  "Add PREFIX to the list of prefixes for WORD in dictionary DICT.
The weight of PREFIX will automatically be kept at least as large
as the weight of WORD."
  (interactive (list (read-dict "Dictionary: ")
		     (setq word (read-string
				 (format "Word (default \"%s\"): "
					 (thing-at-point 'word))))
		     (let ((wrd (if (or (null word) (string= word ""))
				    (thing-at-point 'word)
				  word)))
		       (read-string
			(format "Prefix for \"%s\" (default \"%s\"): "
				wrd (or (predictive-guess-prefix wrd) ""))))
		     ))
  
  ;; when called interactively, sort out arguments
  (when (interactive-p)
    ;; default to word at point
    (when (or (null word) (string= word ""))
      (let ((str (thing-at-point 'word)))
	(if (null str)
	    (error "No word supplied")
	  (set-text-properties 0 (length str) nil str)
	  (setq word str))))
    ;; default to guessed prefix, throwing error if there is no guess
    (when (or (null prefix) (string= prefix ""))
      (setq prefix (predictive-guess-prefix word))
      (unless prefix (error "Prefix must be supplied for \"%s\"" word)))
    ;; throw error if word not in dict
    (when (not (dictree-member-p dict word))
      (error "\"%s\" not found in dictionary %s" word (dictree-name dict))))
  

    ;; prompt for confirmation if prefix isn't really a prefix for word
    (when (and (dictree-member-p dict word)
	       (dictree-member-p dict prefix)
	       (or (not (interactive-p))
		   (and (> (length word) (length prefix))
			(string= (substring word 0 (length prefix)) prefix))
		   (y-or-n-p
		    (format
		     "\"%s\" is not a prefix of \"%s\". Continue anyway? "
		     prefix word))))
      
      (let ((prefixes (dictree-lookup-meta-data dict word)))
	;; unless prefix is already defined, define it
	(unless (member prefix prefixes)
	  (dictree-set-meta-data dict word (cons prefix prefixes))))
      
      ;; make sure prefix's weight is at least as large as word's
      (let ((weight (dictree-lookup dict word))
	    (pweight (dictree-lookup dict prefix)))
	(when (and weight (or (null pweight) (< pweight weight)))
	  (dictree-insert dict prefix weight (lambda (a b) a))))
      )
)



(defun predictive-undefine-prefix (dict word prefix)
  "Remove PREFIX from list of prefixes for WORD in dictionary DICT.
The weight of PREFIX will no longer automatically be kept at
least as large as the weight of WORD."
  (interactive (list (read-dict "Dictionary: ")
		     (setq word (read-string
				 (format "Word (default \"%s\"): "
					 (thing-at-point 'word))))
		     (let ((wrd (if (or (null word) (string= word ""))
				    (thing-at-point 'word)
				  word)))
		       (read-string
			(format "Remove prefix of \"%s\" (default \"%s\"): "
				wrd (or (predictive-guess-prefix wrd) ""))))
		     ))
  
  ;; when called interactively, sort out arguments
  (when (interactive-p)
    (when (or (null word) (string= word ""))
      (let ((str (thing-at-point 'word)))
	(if (null str)
	    (error "No word supplied")
	  (set-text-properties 0 (length str) nil str)
	  (setq word str))))
    (when (or (null prefix) (string= prefix ""))
      (setq prefix (predictive-guess-prefix word))))

  ;; delete prefix, displaying message if called interactively
  (let ((prefixes (dictree-lookup-meta-data dict word)))
    (if (and (interactive-p) (not (member prefix prefixes)))
	(message "\"%s\" is not defined as a prefix of \"%s\"" prefix word)
      (dictree-set-meta-data dict word (delete prefix prefixes))
      (when (interactive-p)
	(message "Prefix \"%s\" of \"%s\" removed" prefix word))))
)



(defun predictive-define-all-prefixes (dict &optional prefix length)
  "Define prefix relationships for PREFIX in dictionary DICT.
PREFIX will be added to the prefix list of any word for which it
is a prefix. Predictive mode will then automatically ensure that
the weight of PREFIX is always at least as great as the weight of
those words.

If PREFIX is null, it defines prefix relationships for *all*
words in DICT that are prefixes of other words. In this case,
optional argument LENGTH specifies a minimum length for a prefix
word\; prefix words shorter than this minimum will be ignored. If
it is zero or negative, all prefix words will be included.

Interactively, DICT and PREFIX are read from the minibuffer, and
LENGTH is the integer prefix argument."
  (interactive (list (read-dict "Dictionary: ")
		     (read-string "Prefix (leave blank for all): ")
		     (prefix-numeric-value current-prefix-arg)))
  
  ;; sort out arguments
  (and (stringp prefix) (string= prefix "") (setq prefix nil))
  
  
  (let (prefix-fun)  
    ;; create function for defining prefixes
    (setq prefix-fun
	  (lambda (word)
	    (let (completion-list string)
	      ;; deal with capitalisation
	      (if (and predictive-ignore-initial-caps
		       (predictive-capitalized-p word))
		  (setq string (list word (downcase word)))
		(setq string word))
	      ;; find completions of word, dropping first which is always the
	      ;; word itself
	      (setq completion-list
		    (mapcar (lambda (entry) (concat word (car entry)))
			    (dictree-complete dict string)))
	      (setq completion-list
		    (last completion-list
			  (1- (length completion-list))))
	      ;; define the word to  be a prefix for all its completions
	      (dolist (cmpl completion-list)
		(predictive-define-prefix dict cmpl word))
	      )))
    
    
    ;; display informative messages if called interactively
    (when (interactive-p)
      (if prefix
	  (message "Defining prefix \"%s\" in %s..."
		   prefix (dictree-name dict))
      (message "Defining prefixes in %s..." (dictree-name dict))))
    (let ((i 0) (count (when (interactive-p) (dictree-size dict))))
      (when (and (interactive-p) prefix)
	(message "Defining prefixes in %s...(word 1 of %d)"
		 (dictree-name dict) count))

      ;; define one prefix
      (if prefix (funcall prefix-fun prefix)

	;; define all prefixes
	(dictree-map
	 (lambda (word weight)
	   (when (and (interactive-p) (setq i (1+ i)) (= 0 (mod i 50)))
	     (message "Defining prefixes in %s...(word %d of %d)"
		      (dictree-name dict) i count))
	   ;; ignore word if it's too short
	   (unless (< (length word) length) (funcall prefix-fun word)))
	 dict 'string))
      
      (when (interactive-p)
	(if prefix
	    (message "Defining prefix \"%s\" in %s...done"
		     prefix (dictree-name dict))
	  (message "Defining prefixes in %s...done" (dictree-name dict))))
      ))
)



(defun predictive-undefine-all-prefixes (dict &optional prefix)
  "Remove PREFIX from all prefix relationships in DICT.
Predictive mode will no longer ensure that the weight of PREFIX
is greater than that of other words.

If PREFIX is null, remove all prefix relationships (prompting for
confirmation first if called interactively)."
  (interactive (list (read-dict "Dictionary: ")
		     (setq prefix (read-string "Prefix: "))))
  
  ;; sort out arguments
  (and (stringp prefix) (string= prefix "") (setq prefix nil))
  ;; prompt for confirmation if called interactively to remove all prefixes
  (when (or (not (interactive-p))
	    prefix
	    (y-or-n-p
	     (format
	      "Really remove all prefix relationships from dictionary %s? "
	      (dictree-name dict))))

    ;; display informative message so people have something to look at whilst
    ;; calculating dictionary size
    (if prefix
	(message "Undefining prefix \"%s\" in %s..."
		 prefix (dictree-name dict))
      (message "Undefining prefixes in %s..." (dictree-name dict)))
    
    (let ((count (when (interactive-p) (dictree-size dict)))
	  (interactive (interactive-p))
	  (i 0) prefix-fun prefix-list)
      
      ;; define function to be mapped over dictionary words, for removing
      ;; single prefix or all prefixes, as appropriate
      (if prefix
	  (setq prefix-fun
		(lambda (word dummy)
		  (when (and interactive (setq i (1+ i)) (= 0 (mod i 50)))
		    (message
		     "Undefining prefix \"%s\" in %s...(word %d of %d)"
		     prefix (dictree-name dict) i count))
		  ;; remove PREFIX if it appears in word's prefix list
		  (when (member prefix
				(setq prefix-list
				      (dictree-lookup-meta-data dict word)))
		    (dictree-set-meta-data dict word
					   (delete prefix prefix-list)))))
	(setq prefix-fun
	      (lambda (word dummy)
		(when (and interactive (setq i (1+ i)) (= 0 (mod i 50)))
		  (message "Undefining prefixes in %s...(word %d of %d)"
			   (dictree-name dict) i count))
		;; clear word's prefix list
		(dictree-set-meta-data dict word nil))))

      
      ;; do actual work...
      (when (interactive-p)
	(if prefix
	    (message "Undefining prefix \"%s\" in %s...(word 1 of %d)"
		     prefix (dictree-name dict) count)
	  (message "Undefining prefixes in %s...(word 1 of %d)"
		   (dictree-name dict) count)))
      (dictree-map prefix-fun dict 'string)
      (when (interactive-p)
	(if prefix
	    (message "Undefining prefix \"%s\" in %s...done"
		     prefix (dictree-name dict))
	  (message "Undefining prefixes in %s...done" (dictree-name dict))))
      ))
)




;;; ===================================================================
;;;    Internal functions and variables to do with predictive mode
;;;    dictionaries


(defun predictive-current-dict (&optional point)
  "Return the currently active dictionary(ies) at POINT
\(defaults to the point\). Always returns a list of dictionaries, even if
there's only one."
  (when (null point) (setq point (point)))
  
  ;; get the active dictionary and the overlay that sets it, if any
  ;; note: can't use `auto-overlay-local-binding' here because we want the
  ;; overlay as well as the binding
  (let ((overlay (auto-overlay-highest-priority-at-point
		  point '(identity dict)))
	dict generate)
    (if (null overlay)
	(setq dict (or predictive-buffer-dict predictive-main-dict))
      (setq dict (overlay-get overlay 'dict))
      (when (symbolp dict) (setq dict (eval dict))))
    
    ;; t indicates no active dictionary, so return nil
    (if (eq dict t) nil
      ;; otherwise bundle the dictionary inside a list for mapcar
      (unless (and (listp dict) (not (dictree-p dict))) (setq dict (list dict)))
      
      (mapcar
       (lambda (dic)
	 ;; if element is a function or symbol, evaluate it
	 (cond
	  ((functionp dic) (setq dic (funcall dic)))
	  ((symbolp dic) (setq dic (eval dic))))

	 (cond
	  ;; if element is a dictionary, return it
	  ((dictree-p dic) dic)
	  
	  ;; if element is a plist with a :generate property...
	  ((and (listp dic) (setq generate (plist-get dic :generate)))
	   (unless (functionp generate)
	     (error "Wrong type in dictionary's :generate property:\
 functionp %s" (prin1-to-string generate)))
	   ;; if plist has a :dict property, and it's :refresh function
	   ;; returns nil, use existing :dict property
	   (if (and (plist-get dict :dict)
		    (or (not (functionp (plist-get dict :refresh)))
			(not (funcall (plist-get dict :refresh) overlay))))
	       (plist-get dict :dict)
	     ;; otherwise, generate and return the dictionary, saving it in
	     ;; the :dict propery
	     (overlay-put overlay 'dict
			  (plist-put dict :dict (funcall generate overlay)))
	     (plist-get dict :dict)))
	  
	  ;; throw error on anything else
	  (t (error "Wrong type in element of dictionary list: functionp,\
 symbolp, dict-p, plist (with :generate) or t at %d %s"
		    point (prin1-to-string dic)))
	  ))
       
       dict)  ; map over dict
      ))
)



(defun predictive-load-buffer-local-dict ()
  "Load/create the buffer-local dictionary."

  (let (filename buffer-dict meta-dict insfun rankfun combfun)
    ;; The rank function compares by weight (larger is "better"), failing that
    ;; by string length (smaller is "better"), and failing that it compares
    ;; the strings alphabetically.
    (setq rankfun
	  (lambda (a b)
	    (if (= (cdr a) (cdr b))
		(if (= (length (car a)) (length (car b)))
		    (string< (car a) (car b))
		  (< (length (car a)) (length (car b))))
	      (> (cdr a) (cdr b)))))


    ;; ----- buffer-local dictionary -----
    (when (buffer-file-name)
      (setq filename
	    (concat (file-name-directory (buffer-file-name))
		    predictive-auxiliary-file-location
		    (symbol-name (predictive-buffer-local-dict-name))
		    ".elc"))
      ;; create directory if necessary
      (predictive-create-auxiliary-file-location))
    ;; if the buffer-local dictionary exists, load it, otherwise create it
    (if (and filename (file-exists-p filename))
	(progn
	  (load filename)
	  ;; FIXME: probably shouldn't be using an internal dict-tree.el
	  ;; function
	  (dictree--set-filename (eval (predictive-buffer-local-dict-name))
				 filename)
	  (setq buffer-dict (eval (predictive-buffer-local-dict-name))))
      ;; The insertion function inserts a weight multiplied by the multiplier
      ;; if none already exists, otherwise it adds the new weight times the
      ;; multiplier to the existing one, or if supplied weight is nil,
      ;; incremenets existing weight by the multiplier.
      (setq insfun
	    (lambda (weight data)
	      (cond
	       ((not (or weight data)) 0)
	       ((null weight)
		(+ data predictive-buffer-local-learn-multiplier))
	       ((null data)
		(* weight predictive-buffer-local-learn-multiplier))
	       (t (+ data (* weight
			     predictive-buffer-local-learn-multiplier))))))
      ;; create the buffer-local dictionary
      (setq buffer-dict
	    (dictree-create (predictive-buffer-local-dict-name)
			    filename (when filename t) nil nil
			    predictive-completion-speed nil
			    nil insfun rankfun)))
    
    
    ;; ----- meta-dictionary -----
    (when (buffer-file-name)
      (setq filename
	    (concat (file-name-directory (buffer-file-name))
		    (symbol-name (predictive-buffer-local-meta-dict-name))
		    ".elc")))
    ;; if the buffer meta-dictionary exists, load it
    (when (and filename (file-exists-p filename))
      (load filename)
      ;; FIXME: probably shouldn't be using an internal dict-tree.el function
      (dictree--set-filename (eval (predictive-buffer-local-meta-dict-name))
			     filename)
      ;; if the meta-dictionary is not based on the current main dictionary,
      ;; prompt user to update it
      (when (and
	     (not (memq (eval predictive-main-dict)
			(dictree--dict-list
			 (eval (predictive-buffer-local-meta-dict-name)))))
	     (y-or-n-p "Existing buffer-local dictionary is not based on the\
 current main dictionary. Update it? "))
	(unintern (predictive-buffer-local-meta-dict-name))))

    ;; if the buffer meta-dictionary doesn't exist or is being updated...
    (unless (boundp (predictive-buffer-local-meta-dict-name))
      ;; the combine function adds the weights from the two constituent
      ;; dictionaries
      (setq combfun
	    (lambda (a b)
	      (cond ((null b) a)
		    ((null a) b)
		    (t (cons (+ (car a) (car b)) (cdr b))))))
      ;; create the buffer-local meta-dictionary
      (setq meta-dict
	    (dictree-create-meta-dict
	     (predictive-buffer-local-meta-dict-name)
	     (list buffer-dict (eval predictive-main-dict)) filename
	     (when filename t) nil nil predictive-completion-speed
	     nil combfun rankfun)))
    
    ;; set buffer's dictionary to the meta-dictionary
    (setq predictive-buffer-dict (predictive-buffer-local-meta-dict-name))
    ;; add meta-dictionary to the list of dictionaries used by buffer
    (unless (memq meta-dict predictive-used-dict-list)
      (setq predictive-used-dict-list
	    (cons meta-dict predictive-used-dict-list))))
)



(defun predictive-unload-buffer-local-dict ()
  "Unload the buffer-local dictionary."
    (let ((buffer-dict (predictive-buffer-local-dict-name))
	  (meta-dict (predictive-buffer-local-meta-dict-name)))
      (when (boundp buffer-dict)
	(if (dictree-p (eval (predictive-buffer-local-dict-name)))
	    (dictree-unload (eval (predictive-buffer-local-dict-name)))
	  (unintern buffer-dict)))
      (when (boundp meta-dict)
	(if (dictree-p (eval (predictive-buffer-local-meta-dict-name)))
	    (dictree-unload (eval (predictive-buffer-local-meta-dict-name)))
	  (unintern meta-dict))))
)




;;; ==================================================================
;;;       Functions and variables to do with which-dict mode

(define-minor-mode predictive-which-dict-mode
    "Toggle predictive mode's which dictionary mode.
With no argument, this command toggles the mode.
A positive prefix argument turns the mode on.
A negative prefix argument turns it off.

Note that simply setting the minor-mode variable
`predictive-which-dict-mode' is not sufficient to enable
predictive mode."

    ;; initial value, mode-line indicator, and keymap
    nil nil nil

    ;; if which-dict mode has been turned on, setup the timer to update the
    ;; mode-line indicator
    (if predictive-which-dict-mode
	(progn
	  (when (timerp predictive-which-dict-timer)
	    (cancel-timer predictive-which-dict-timer))
	  (setq predictive-which-dict-timer
		(run-with-idle-timer predictive-which-dict-delay
				     t 'predictive-update-which-dict)))
      
      ;; if which-dict mode has been turned off, cancel the timer and reset
      ;; variables
      (when predictive-which-dict-timer
	(cancel-timer predictive-which-dict-timer)
	(setq predictive-which-dict-timer nil)
	(setq predictive-which-dict-last-update nil)
	(setq predictive-which-dict-name nil)
	(setq predictive-which-dict-list nil)))
)

      

(defun predictive-update-which-dict ()
  ;; Updates the `predictive-which-dict-name' variable used in the mode
  ;; line. Runs automatically from an idle timer setup by the minor mode
  ;; function.

  ;; only run if predictive mode is enabled and point has moved since last run
  (unless (or ;;(null predictive-mode)
	      ;;(null predictive-which-dict-mode)
	      (and (eq (current-buffer)
		       (car predictive-which-dict-last-update))
		   (eq (point) (cdr predictive-which-dict-last-update))))
    
    ;; store buffer and point at which update is being performed
    (setq predictive-which-dict-last-update (cons (current-buffer) (point)))
    
    (let ((dict (predictive-current-dict)) name list dic)
      ;; get current dictionary name(s)
      (if (null dict) (setq name "" list nil)

	;; get name of first dictionary in list
	(if (dictree-p dict) (setq dic dict) (setq dic (car dict)))
	;; if dict is the buffer-local meta-dictioary, display name of main
	;; dictionary it's based on instead
	(if (and (>= (length (dictree-name dic)) 10)
		 (string= (substring (dictree-name dic) 0 10) "dict-meta-")
		 (dictree--meta-dict-p dic))
	    (setq name (dictree-name (nth 1 (dictree--dict-list dic))))
	  (setq name (dictree-name dic)))
;;; 	;; truncate to 15 characters
;;; 	(when (> (length name) 15) (setq name (substring name 0 15)))
	  ;; filter list to remove "-dict-" and "-predictive-" prefixes
	(when (string-match "-*dict-*\\|-*predictive-*" name)
	  (setq name (replace-match "" nil nil name)))
	
	;; if current dictionary is a list, add "..." to end of name, and
	;; construct list of all dictionary names for help-echo text
	(if (or (dictree-p dict) (= (length dict) 1))
	    (setq list nil)
	  (setq name (concat name "..."))
	  (setq list (mapconcat
		      (lambda (dic)
			(if (and (>= (length (dictree-name dic)) 10)
				 (string= (substring (dictree-name dic) 0 10)
					  "dict-meta-")
				 (dictree--meta-dict-p dic))
			    (dictree-name (nth 1 (dictree--dict-list dic)))
			  (dictree-name dic)))
		      dict "\n"))
	  ;; filter list to remove "-dict-" and "-predictive-" prefixes
	  (while (string-match "-*dict-*\\|-*predictive-*" list)
	    (setq list (replace-match "" nil nil list)))))
      
      
      ;; if dictionary name has changed, update the mode line
      (unless (and (string= name predictive-which-dict-name)
		   (or (and (null list) (null predictive-which-dict-list))
		       (string= list predictive-which-dict-list)))
	(setq predictive-which-dict-name name)
	(setq predictive-which-dict-list list)
	(force-mode-line-update))
      ))
)




;;; ===============================================================
;;;                       Compatibility Stuff

(unless (fboundp 'replace-regexp-in-string)
  (require 'predictive-compat)
  (defalias 'replace-regexp-in-string
            'predictive-compat-replace-regexp-in-string)
)


;;; predictive.el ends here
