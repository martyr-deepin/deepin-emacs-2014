;;; iman.el --- call man & Info viewers with completion

;; Copyright (C) 2000 Yuji 'bmonkey' Minejima <ggb01164@nifty.ne.jp>

;; Author: Yuji Minejima <ggb01164@nifty.ne.jp>
;; $Revision: 2.22 $
;; Keywords: local, help

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;; This program contains part of the code and the documentation of the
;; following program which is distributed under GPL:
;;
;;   thingatpt.el --- Get the `thing' at point
;;   Copyright (C) 1991,92,93,94,95,96,97,1998 Free Software Foundation, Inc.
;;   Author: Mike Williams <mikew@gopher.dosli.govt.nz>
;;
;; Thanks to the author for writing an excellent program.


;;; Commentary:

;; Call the viewers of Unix manual pages and GNU Info with completion.
;;
;; This package provides the `iman' command.
;; The `iman' command reads the name of a Unix manual page or an item of
;; GNU Info top directory in the minibuffer with completion, and shows
;; that page.
;;
;; The job of actually showing the page is done by calling the standard
;; viewers.


;;; Requirements:

;; Tested with FSF Emacs 20.6.2 and XEmacs 21.1.9 on Debian GNU/Linux.
;;
;; You need a man(1) program. You probably have one already if you
;; run a Unix like system.


;;; Enhancements:

;; completing-help.el (>= version 3.3)
;;   With this packege, short descriptions on completions is displayed
;;   by pressing <TAB> (XEmacs only) or `?' which looks like:
;;   -----------------------------------------------------------------
;;   Click mouse-2 on a completion to select it.
;;   In this buffer, type RET to select the completion near point.
;;    
;;   Possible completions are:
;;   find(1)             - search for files in a directory hierarchy
;;   findaffix(1)        - Interactive spelling checking
;;   finding files: (find).   Listing and operating on files that mat$
;;   findoidjoins(1)     - list joins on oids between PostgreSQL tabl$
;;   findsuper(8)        - No manpage for this program, utility or fu$
;;   -----------------------------------------------------------------
;;
;;   You can get completing-help.el at
;;   http://homepage1.nifty.com/bmonkey/emacs/elisp/completing-help.el


;;; Install:

;; 1: Put this file in one of the directories listed in `load-path'.
;;    You can see the contents of `load-path' by entering
;;    `M-x customize-option <RET> load-path'.
;;
;; 2: Enter `M-x byte-compile-file <RET>
;;          <DIR-YOU-PUT-THIS-FILE-IN>/iman.el <RET>'
;;    to speed up the execution of this package.
;;
;; 3: Put the following lines in your .emacs file.
;;
;;    (autoload 'iman "iman"
;;              "Call the viewers of man pages and GNU Info with completion."
;;              t nil)
;;
;;    If you want to bind the `iman' command to some key sequence,
;;    add the following line, too:
;;    (global-set-key "\C-cm" 'iman) ; `control c', then `m' calls `iman'
;;    You should change "\C-cm" to the key sequence you want.
;;
;; 4: Restart Emacs or enter `M-x load-library <RET> iman'.


;;; Usage:

;; * Invoke the command by `M-x iman' or the key sequence you bound to
;;   `iman'.
;;
;; * In the minibuffer, the key sequences described in
;;   "(emacs)Completion Commands" should work as usual.
;;   Here's some of the key sequences you probably use most.
;;
;;      <TAB>     Completion.
;;      <SPACE>   Word completion.
;;                You can use this key to enter ' ', '(', and '-'.
;;                The feature of entering '(' by this key is specific to
;;                this package.
;;      M-C-v     Scroll the help window from within the minibuffer.
;;      M-v       Go into the help window ("*Completions*" buffer).
;;                Inside the help window, press <RET> to select an item,
;;                or just move around then go back into the minibuffer
;;                (C-x o).


;;; Change Log:

;; Version 2.22 (29 Sep 2000)
;;  * Changed to support completing-help.el (>= version 3.3)
;;  * Cleaned up some code.
;;  * Added `iman-ignore-case'.

;; Version 2.16 (27 May 2000):
;;  * Changed the name from "bm-iman.el" to "iman.el".
;;  * Changed to use `completing-help.el', if available, to display
;;      the short descriptions of man pages.
;;  * Default man page names recognition mechanism is improved.


;;; Code:

;;; customization
(eval-when-compile
  (defvar current-prefix-arg)
  (defvar quit-flag)
  (defvar unread-command-events)
  (defvar minibuffer-completion-table)
  (defvar minibuffer-completion-predicate)
  (defvar minibuffer-local-must-match-map)
  (defvar completion-auto-help))


(defgroup iman nil
  "Call the viewers of Unix manual pages and the GNU Info with completion.

This package provides the `iman' command which reads, in the
minibuffer with completion, the name of a Unix manual page or
an item in the top directory of the GNU Info documentation tree,
and then shows that page.

The job of actually showing the page is done by calling the standard
viewers."
  :group 'help)

(defcustom iman-ignore-case nil
  "Non-nil means case is not considered significant in completion."
  :type 'boolean
  :group 'iman)

(defcustom iman-index-obarray-bucket-number 2047
  "The number of buckets of an obarray which contains the index.
\"(elisp)Creating Symbols\" recommends this value to be a prime number
or a number which is one less than a power of two."
  :type  'integer
  :group 'iman
  :link  '(info-link "(elisp)Creating Symbols"))


;;; Variables
(defvar iman-index-obarray nil
  "Obarray to store the Man page and GNU Info index.")

(defvar iman-history nil
  "History list for `iman' command.")

(defvar iman-predicate nil
  "Predicate function for `completing-read'.")

(eval-and-compile
  (autoload 'manual-entry   "man")
  (autoload 'Info-directory "info")
  (autoload 'Info-menu      "info"))

;;;###autoload
(defun iman (item)
  "Call the viewers of Unix manual pages and the GNU Info with completion.
With prefix argument (e.g. `C-u M-x iman'), rebuild the index."
  (interactive (list (progn
                       (when (or current-prefix-arg
                                 (not (vectorp iman-index-obarray)))
                         (iman-make-index))
                       (iman-minibuffer-read iman-predicate
                                             nil
                                             'iman-history))))
  (unless (string= item "")
    (string-match "\\`\\([^(]*\\)(?\\([^)]*\\)" item) ; always match
    (let ((name     (match-string 1 item))
          (section  (match-string 2 item)))
      (cond
       ((string= section "")            ; Info
        (Info-directory)
        (Info-menu item))

       (t                               ; man
        (funcall 'manual-entry (if (featurep 'xemacs)
                                   item
                                 (concat section " " name))))))))

(defvar iman-completion-commands
  '(minibuffer-complete minibuffer-complete-word ; standard commands
			MComplete-complete  MComplete-complete-word ; mcomplete.el
			PC-complete         PC-complete-word	    ; complete.el
			IMan-minibuffer-complete-word)
  "List of minibuffer completion commands.")

(eval-when-compile
  (defvar mcomplete-complete-word-high-priority-strings) ; for mcomplete.el
  (defvar completing-help-commands))			 ; for completing-help.el


(defun iman-minibuffer-read (&optional pred init history)
  "Read the name of a man page or an item of Info top directory."
  (let* ((mcomplete-complete-word-high-priority-strings '(" " "(" "-"))
         (completing-help-commands (if (boundp 'completing-help-commands)
                                       (append iman-completion-commands
                                               completing-help-commands)
                                     iman-completion-commands))
         (completion-ignore-case iman-ignore-case)
         (table iman-index-obarray)
         (must-match t)
         (default (iman-name-at-point))
         (prompt (format "iman%s: " (if default
                                        (format " (default %s)" default)
                                      "")))
         (minibuffer-local-must-match-map (copy-keymap
                                           minibuffer-local-must-match-map)))

    (define-key minibuffer-local-must-match-map " "
      'IMan-minibuffer-complete-word)
    (iman-completing-read prompt table pred must-match init history default)))


(defun iman-completing-read (prompt table
				    &optional pred must-match init history default
				    inherit-input-method)
  "Compatibility function for FSF Emacs and XEmacs."
  (let* ((common-args (list prompt table pred must-match init history))
         (args (append common-args (unless (featurep 'xemacs)
                                     (list default inherit-input-method))))
         (input (apply 'completing-read args)))
    (or (and (string= input "") default) ; default handling
        input)))


;;; Man spec or Info menu item at point
(defun iman-name-at-point ()
  "Return a string denoting a man spec.  or a GNU Info directory item at point."
  (or (iman-Man-spec-at-point)
      (let* ((case-fold-search t)
             (name (iman-symbol-name-at-point))
             name-re)
        (when name
          (setq name-re (concat "\\`" (regexp-quote name) "\\((\\|\\'\\)"))
          (catch 'found
            (mapatoms
             #'(lambda (entry-symbol)
                 (when (string-match name-re (symbol-name entry-symbol))
                   (throw 'found (symbol-name entry-symbol))))
             iman-index-obarray))))))


(defun iman-symbol-name-at-point ()
  "Return a symbol name at point.  Return nil if none is found.
Ripped off out of Mike Williams's thingatpt.el"
  (save-excursion
    (save-match-data
      (let* ((orig-pt (point))
             (bol     (prog2 (beginning-of-line) (point) (goto-char orig-pt)))
             (eol     (prog2 (end-of-line)       (point) (goto-char orig-pt)))
             (sym-beg (progn (skip-syntax-backward "w_" bol) (point)))
             (sym-end (progn (skip-syntax-forward  "w_" eol) (point))))
        (unless (= sym-beg sym-end)
          (buffer-substring sym-beg sym-end))))))


(defun iman-Man-spec-at-point ()
  "Return a man spec. at point.  Return nil if none is found."
  (save-excursion
    (save-match-data
      (let* ((orig-pt (point))
             (bol     (prog2 (beginning-of-line) (point) (goto-char orig-pt)))
             (eol     (prog2 (end-of-line)       (point) (goto-char orig-pt)))
             (rpar    (when (or (and (char-before)
                                     (char-equal (char-before) ?\)))
                                (search-forward ")" eol t))
                        (point)))       ; "`ls (1)-!-'"

             (lpar    (and rpar
                           (search-backward "(" bol t)))
                                        ; "`ls -!-(1)'"

             (sec     (when (looking-at "([1-9][a-zA-Z0-9]*)")
                        (buffer-substring lpar rpar)))
             (name-end (when sec
                         (when (and (char-before)
                                    (char-equal (char-before) ?\ ))
                           (backward-char))
                         (point))))	; "`ls-!- (1)'"
        (when sec
          (when (re-search-backward "\\s-" bol "MOVE-ON-FAIL") ; \s=whitespace
            (forward-char))				       ; "-!-`ls (1)'"
          (when (catch 'found
                  (while (and (<= (point) (min orig-pt lpar)))
                    (when (intern-soft
                           (concat (buffer-substring (point) name-end) sec)
                           iman-index-obarray)
                      (throw 'found t))
                    (forward-char)))    ; "`-!-ls (1)'" gotcha!
            (concat (buffer-substring (point) name-end) sec)))))))


;;; Index ------------------------------------------------------------
(defcustom iman-merge-Info-menu t
  "Non-nil means merging the GNU Info top directory into the index."
  :type 'boolean
  :group 'iman)


;;;###autoload
(defun iman-make-index ()
  "Make the index of man pages and Info directory items."
  (interactive)
  (setq iman-index-obarray (make-vector iman-index-obarray-bucket-number 0))
  (message "Making iman index...")
  (let ((ok nil))
    (unwind-protect
        (progn
          (iman-make-Man-index iman-index-obarray)
          (when iman-merge-Info-menu
            (iman-make-Info-menu-index iman-index-obarray))
          (setq ok t))
      (or ok (setq iman-index-obarray nil))))
  (message nil))


;;; Man index.
(defcustom iman-Man-index-command-and-args '("man" "-k" "")
  "List of a program name and its options to dump the index of man pages.
Note these values DON'T go through shell expansions.
These values are used in a `call-process' call and should get the same
output as if you enter \"man -k ''\" in a shell session, which should
print out something like the following:
-------------------------------------------------------------------
~$ man -k ''
. (1) [builtins]     - bash built-in commands, see bash(1)
.ldaprc (5) [ldaprc] - ldap configuration file
.netrc (5) [netrc]   - user configuration for ftp
2a_ctrl (4)          - (unknown)
2b_romkana (4)       - (unknown)
822-date (1)         - Print date and time in RFC822 format
.......
-------------------------------------------------------------------"
  :type  '(repeat string)
  :group 'iman)


(defvar iman-Man-index-regexp
  "^\\([^ (]+\\) *\\(([^ )]+)\\)\\(.*\\)$"
  "Regexp to recognize an entry of the man page index.

(setq entry \"alias (1) [builtins] - bash built-in commands, see bash(1)\")
(string-match iman-Man-index-regexp entry)
(match-string 1 entry) => \"alias\"
(match-string 2 entry) => \"(1)\"
(match-string 2 entry) => \"[builtins] - bash built-in commands, see bash(1)\"
")


;; This is just a note			;
;;        1             2                3                    4 ;
;;      alias          (1)          [builtins]        - bash built-in ... ;
;;"^\\([^ (]+\\) *\\(([^ )]+)\\) *\\(\\[[^]]+\\]\\)? *- \\(.*\\)$" ;


(defvar iman-Man-index-buffer-name
  "*iman-man-index*"
  "Name of the buffer to store the man page index temporarily.")


(defun iman-make-Man-index (index-obarray)
  "Intern symbols whose names are the man page names, in INDEX-OBARRAY.
Each symbol has a description string as its value."
  (when (get-buffer iman-Man-index-buffer-name)
    (kill-buffer iman-Man-index-buffer-name))
  (let* ((buffer       (get-buffer-create iman-Man-index-buffer-name))
         (program      (car iman-Man-index-command-and-args))
         (args         (cdr iman-Man-index-command-and-args))
         (exit-status  (apply 'call-process
                              program
                              nil       ; < /dev/null
                              buffer    ; > current temporary buffer
                              nil       ; no redisplay
                              args)))
    (when (not (zerop exit-status))
      (switch-to-buffer-other-window buffer)
      (goto-char (point-max))
      (error "%s program returned error exit status %s" program exit-status))

    (with-current-buffer buffer
      (goto-char (point-min))
      (while (re-search-forward iman-Man-index-regexp nil t)
        (set (intern (concat (match-string 1) (match-string 2)) index-obarray)
             (match-string 3))))
    (kill-buffer iman-Man-index-buffer-name)))


;;; GNU Info index.
(defvar iman-Info-menu-item-regexp
  "^\\* \\([^:]+\\):\\(.*\\)$"
  "Regexp to recognize a menu item of the GNU Info node.

(setq item \"*    Binutils: (binutils).         The GNU binary utilities.\")
(string-match iman-Info-menu-item-regexp item)
(match-string 1 item) => \"Binutils\"
(match-string 2 item) => \" (binutils).         The GNU binary utilities.\"")

(eval-and-compile
  (autoload 'Info-mode "info")
  (autoload 'Info-find-node "info"))

(defun iman-make-Info-menu-index (index-obarray &optional file node)
  "Intern in INDEX-OBARRAY symbols of the Info FILE NODE's menu items.
Each symbol has a description string as its value."
  (unless file (setq file "dir"))
  (unless node (setq node "top"))
  (let* (name description)
    (save-excursion
      (save-window-excursion
        (with-temp-buffer
          (Info-mode)
          (Info-find-node file node)
          (goto-char (point-min))
          (while (re-search-forward iman-Info-menu-item-regexp nil t)
            (setq name        (downcase (match-string 1))
                  description (concat " " (match-string 2)))
            (set-text-properties 0 (length name) nil name)
            (set-text-properties 0 (length description) nil description)
            (set (intern name index-obarray) description)))))))


;;; minibuffer commands
(defun iman-minibuffer-message (str &optional sec)
  "Display STR at the end of the minibuffer for SEC (default 2) seconds.
The minibuffer must be the current buffer.
Stop displaying when the next input event arrives.
Work almost the same as `minibuffer-message'."
  (unless sec (setq sec 2))
  (let ((buffer-undo-list t)            ; prevent undo recording
        (pt-max (point-max))
        (inhibit-quit t))
    (save-excursion
      (message nil)
      (goto-char (point-max))
      (insert str))
    (sit-for sec)
    (delete-region pt-max (point-max))
    (when quit-flag
      (let ((quit-char (if (fboundp 'current-input-mode)
                           (nth 3 (current-input-mode))
                         ?\^G))
            (char-to-event (if (fboundp 'character-to-event)
                               'character-to-event
                             'identity)))
        (cond
         ((boundp 'unread-command-events)
          (setq unread-command-events (list (funcall char-to-event quit-char))
                quit-flag nil))
         (t
          (error "mcomplete-message: %S is not bound."
                 'unread-command-events)))))))


(defun IMan-minibuffer-complete-word ()
  "`minibuffer-complete-word' for iman package.
You can enter `(' by hitting <SPACE>.  This is useful when there're
possible completions like ls(1), lsattr(1), lsdic(1), and lsearch(3)."
  (interactive)
  (let* ((buf-str    (buffer-string))
         (completion (try-completion buf-str
                                     minibuffer-completion-table
                                     minibuffer-completion-predicate))
         (suffix (when (stringp completion)
                   (substring completion (length buf-str)))))
    (cond
     ((null completion)
      (iman-minibuffer-message " [No match]") nil)
     ((eq completion t)
      (iman-minibuffer-message " [Sole completion]") nil)
     ((string= suffix "")
      (let ((strings '(" " "(" "-")))
        (unless (catch 'inserted
                  (while strings
                    (when (try-completion (concat (buffer-string)
                                                  (car strings))
                                          minibuffer-completion-table
                                          minibuffer-completion-predicate)
                      (goto-char (point-max))
                      (insert (car strings))
                      (throw 'inserted t))
                    (setq strings (cdr strings))))
          (if completion-auto-help
              (minibuffer-completion-help)
            (iman-minibuffer-message " [Next char not unique]")))))
     ((string-match "\\`\\sw*\\Sw?" suffix)
      (goto-char (point-max))
      (insert (match-string 0 suffix))
      t)
     (t (error "IMan-minibuffer-complete-word: logical error")))))


;;; completing-help support
(defun iman-completing-help-p ()
  (eq minibuffer-completion-table iman-index-obarray))

(defvar iman-completing-help-group
  '(:predicate iman-completing-help-p
	       :get       completing-help-user-obarray-get-info
	       :info-head ""
	       :info-tail "")
  "")

(if (featurep 'completing-help)
    (add-to-list 'completing-help-groups 'iman-completing-help-group)
  (add-hook 'completing-help-load-hook
            #'(lambda () (add-to-list 'completing-help-groups
                                      'iman-completing-help-group))))

;;; Hook
(defvar iman-load-hook nil
  "Hook to run at the end of loading iman.")

(provide 'iman)
(run-hooks 'iman-load-hook)

;;; iman.el ends here
