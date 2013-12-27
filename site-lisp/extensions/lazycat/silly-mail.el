;;; silly-mail.el --- generate bozotic mail headers

;; Compilation Copyright (C) 1993, 94, 95, 96, 97, 98, 99, 2000 Noah S. Friedman

;; Contributors: Noah Friedman, Jamie Zawinski, Jim Blandy,
;;               Thomas Bushnell, Roland McGrath,
;;               and a cast of dozens.
;; Maintainer: Noah Friedman <friedman@splode.com>
;; Keywords: extensions, mail
;; Status: works in Emacs 19 and XEmacs.

;; $Id: silly-mail.el,v 1.1 2003-04-04 20:16:10 lolando Exp $

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 59 Temple Place, Suite 330; Boston, MA 02111-1307, USA.

;;; Commentary:

;; To use this, invoke `M-x sm-add-random-header' from a mail composition
;; buffer to insert a random header.  You may call the command again to
;; substitute the inserted header by another.
;;
;; Use 'M-x sm-delete-last-header' to remove it.
;;
;; If you wish all mail messages to have a randomly chosen header, put the
;; following in your .emacs:
;;
;;    (autoload 'sm-add-random-header "silly-mail" nil t)
;;    (add-hook 'mail-setup-hook 'sm-add-random-header)
;;    (add-hook 'mh-letter-mode-hook 'sm-add-random-header)
;;
;;  or alternatively customize the variable `sm-add-ramdom-header-to-mail'.
;;
;; To setup menu-bar entries in sendmail and MH-E menus, customize the
;; variable `sm-add-menu-bar-entries'.  This has the disadvantage of
;; loading this library at Emacs startup, so might not be a good choice if
;; you rarely use silly-mail.
;;
;; You may customize silly-mail using `M-x customize-group [RET] silly-mail'.
;; The following are customizable:
;;
;;  - The list of header types used in the random selection by
;;    `sm-add-random-header'
;;  - Individual quotes may be disabled from the pool if some are offensive
;;    to you.
;;  - Whether all headers use an "X-" prefix or not.

;; I solicit more randomly generated headers commands.

;; Some of the options in this program require some external packages which
;; are not a standard part of Emacs, e.g. shop.el and flame.el (flame.el is
;; present in XEmacs and Emacs 18, but missing from Emacs 19).  These are
;; available from http://www.splode.com/users/friedman/software/emacs-lisp/

;;; History:
;; 
;;  2003-11-25 Peter S Galbraith <psg@debian.org>
;;
;;  - Added custom support.  I had to change quote variables from vectors
;;    to lists to use the `set' custom type, but this had no impact on the
;;    code.  I also had to change the format of the `sm-mail-header-table'
;;    variable (leading to a minor change in `sm-use-header-function-p').
;;    The variable `sm-mail-header-table' is not generated when the variable
;;    `sm-mail-header-used' customization is set.
;;  - Made `sm-add-random-header' replace the inserted header if called a
;;    second time.
;;  - Added `sm-delete-last-header'.
;;  - Added optional "X-" prefix for those headers that didn't have them.
;;  - Added custom variables `sm-add-ramdom-header-to-mail' and
;;    `sm-add-menu-bar-entries'

;;; Code:

;; Try without requiring sendmail, as byte-compilations fails if
;; /usr/bin/mail doesn't exist (Closes: #434104)
;;
;;(require 'sendmail)

(defgroup silly-mail nil
  "Generate bozotic mail headers."
  :group 'mail
  :group 'mh
  :group 'sendmail)

(defcustom sm-add-ramdom-header-to-mail nil
  "Setup sendmail and MH-E to call `sm-add-random-header' automatically."
  :type 'boolean
  :require 'silly-mail
  :set (lambda (symbol value)
         (set-default symbol value)
         (cond
          (value
           (add-hook 'mail-setup-hook 'sm-add-random-header)
           (add-hook 'mh-letter-mode-hook 'sm-add-random-header))
          (t
           (remove-hook 'mail-setup-hook 'sm-add-random-header)
           (remove-hook 'mh-letter-mode-hook 'sm-add-random-header))))
  :group 'silly-mail)

(defvar mail-mode-map)
(defvar mh-letter-mode-map)
(defcustom sm-add-menu-bar-entries nil
  "Setup silly-mail menu-bar entries in MH-E and sendmail."
  :type 'boolean
  :require 'silly-mail
  :set (lambda (symbol value)
         (set-default symbol value)
         (when value
           (easy-menu-define sm-menu-map nil "silly-mail mh-letter menu"
             '("Silly Mail"
               ["Add Random Header" sm-add-random-header]
               ["Delete Last Header" sm-delete-last-header]))

           (eval-after-load "sendmail"
             '(easy-menu-add-item mail-mode-map '("menu-bar" "headers")
                                  sm-menu-map))           
           (eval-after-load "mh-comp"
             '(easy-menu-add-item mh-letter-mode-map '("menu-bar" "Letter")
                                  sm-menu-map))))
  :group 'silly-mail)

(defcustom sm-always-X-prefix nil
  "Whether to use \"X-\" prefix in all silly-mail headers.
This affects headers Emacs, Microsoft and Tomato."
  :type 'boolean
  :group 'silly-mail)

(random t)

(defvar sm-header-last-inserted nil
  "Last header field inserted by silly-mail, such that it can be undone.")
(make-variable-buffer-local 'sm-header-last-inserted)

(defvar sm-mail-header-table nil
  "List of routines which generate silly mail headers.
Each element is either a symbol or a list.
If an element is a function, that function can be called.
If an element is a list, it is composed of three elements:
   1. A function to call which generates a header.
   2. A symbol naming a function required by the header-generator.
      If this function is not defined, the header-generator cannot run.
   3. The name of a library to load if the required function isn't defined.
      If the load fails, or if `sm-load-missing-libraries' is nil,
      the corresponding header-generator function won't be used.

This variable is set via `sm-mail-header-used' customization.")

(defvar sm-mail-header-translation
  '(("X-Antipastobozoticataclysm" sm-add-antipastobozoticataclysm)
    ("X-AT&T-Hype" sm-add-at&t-hype youwill "youwill")
    ("X-Drdoom-Fodder" sm-add-drdoom-fodder)
    ("X-Emacs-Acronym" sm-add-emacs-name)
    ("(X-)Emacs" sm-add-emacs-taunt)
    ("X-Flame" sm-add-flame *flame "flame")
    ("X-Horoscope" sm-add-horoscope horoscope "horoscope")
    ("X-Kibo-Says" sm-add-kibology kibologize "kibologize")
    ("X-Meat" sm-add-meat)
    ("(X-)Microsoft" sm-add-microsoft)
    ("X-NSA-Fodder" sm-add-nsa-fodder)
    ("X-Shopping-List" sm-add-shopping-list shop-string "shop")
    ("X-Tom-Swifty" sm-add-tom-swifty)
    ("(X-)Tomato" sm-add-tomato)
    ("X-Uboat-Death-Message"
     sm-add-uboat-death-message uboat-death-message "uboat")
    ("X-Windows" sm-add-x-taunt)
    ("X-Zippy-Says" sm-add-zippy-quote)))

(defcustom sm-mail-header-used
  '("X-Antipastobozoticataclysm"
    "X-AT&T-Hype"
    "X-Drdoom-Fodder"
    "X-Emacs-Acronym"
    "(X-)Emacs"
    "X-Flame"
    "X-Horoscope"
    "X-Kibo-Says"
    "X-Meat"
    "(X-)Microsoft"
    "X-NSA-Fodder"
    "X-Shopping-List"
    "X-Tom-Swifty"
    "(X-)Tomato"
    "X-Uboat-Death-Message"
    "X-Windows"
    "X-Zippy-Says")
  "Header fields used ramdomly in silly-mail."
  :type `(set
          (const "X-Antipastobozoticataclysm")
          (const "X-AT&T-Hype")
          (const "X-Drdoom-Fodder")
          (const "X-Emacs-Acronym")
          (const "(X-)Emacs")
          (const "X-Flame")
          (const "X-Horoscope")
          (const "X-Kibo-Says")
          (const "X-Meat")
          (const "(X-)Microsoft")
          (const "X-NSA-Fodder")
          (const "X-Shopping-List")
          (const "X-Tom-Swifty")
          (const "(X-)Tomato")
          (const "X-Uboat-Death-Message")
          (const "X-Windows")
          (const "X-Zippy-Says"))
  :set (lambda (symbol value)
         (set-default symbol value)
         (setq sm-mail-header-table nil)
         (when value
           (let ((the-list value))
             (while the-list
               (let ((item (car the-list)))
                 (setq sm-mail-header-table
                       (append
                        sm-mail-header-table
                        (list (cdr (assoc item sm-mail-header-translation)))))
                 (setq the-list (cdr the-list)))))))
  :group 'silly-mail)

(defcustom sm-load-missing-libraries nil
  "*If non-nil, load missing libraries for header functions.
If nil, then if a library is not already loaded, the dependent
header-generating function will not be used."
  :type 'boolean
  :group 'silly-mail)

;;;###autoload
(defun sm-add-random-header ()
  "Insert a random silly mail header.
The choice of available headers is taken from `sm-mail-header-table'.
If a random header was already inserted, it it removed in favor of a new one."
  (interactive)
  (if sm-header-last-inserted
    (sm-delete-last-header))
  (funcall (sm-random-header-function)))

;;;###autoload
(defun sm-add-all-headers ()
  "Insert one of every kind of silly mail header defined.
The choice of available headers is taken from `sm-mail-header-table'."
  (interactive)
  (let ((fns sm-mail-header-table)
        fn)
    (while fns
      (setq fn (sm-use-header-function-p (car fns)))
      (and fn
           (funcall fn))
      (setq fns (cdr fns)))))

(defun sm-random-header-function ()
  (let ((fn nil))
    (while (null fn)
      (setq fn (sm-use-header-function-p
                (nth (random (length sm-mail-header-table))
                     sm-mail-header-table))))
    fn))


(defun sm-use-header-function-p (func)
  (cond ((eq 3 (length func))
         (let ((fn (nth 0 func))
               (fbound-sym (nth 1 func))
               (lib (nth 2 func)))
           (cond ((fboundp fbound-sym)
                  fn)
                 ((and sm-load-missing-libraries
                       (load lib t)
                       (fboundp fbound-sym))
                  fn))))
        (t (car func))))


(defvar sm-fill-single-line-width   78)
(defvar sm-fill-multi-line-width    70)
(defvar sm-fill-indent-width         3)

(defun sm-sequence-item (sequence n)
  (cond ((or (vectorp sequence)
             (stringp sequence))
         (aref sequence n))
        ((listp sequence)
         (nth n sequence))
        (t
         (signal 'domain-error (list 'sequencep sequence)))))

(defsubst sm-random-sequence-item (sequence)
  (sm-sequence-item sequence (random (length sequence))))

(defsubst sm-random-range (lower upper)
  (+ lower (random (- upper lower))))

(defun sm-random-sequence-items (&optional sequence lower upper)
  (and (null lower)
       (setq lower 0))
  (let ((seqlen (length sequence))
        (count (if upper
                   (sm-random-range lower upper)
                 (random lower)))
        items tem)
    (while (not (zerop count))
      (setq tem (sm-sequence-item sequence (random seqlen)))
      (or (memq tem items)
          (setq items (cons tem items)
                count (1- count))))
    items))

(defun sm-put-header-fill-content (header contents)
  (let ((buf (generate-new-buffer " *sm-temp*"))
        (header-length (+ 2 (length header)))
        (single-width sm-fill-single-line-width)
        (multi-width sm-fill-multi-line-width)
        (indent-width sm-fill-indent-width)
        (do-fill (function
                  (lambda (fill-column)
                    (fill-region-as-paragraph (point-min) (point-max))
                    ;; Emacs 19 fill functions add an extra newline
                    (cond ((char-equal ?\C-j (char-after (1- (point-max))))
                           (goto-char (point-max))
                           (delete-char -1)))
                    (= (count-lines (point-min) (point-max)) 1)))))
    (save-excursion
      (set-buffer buf)
      (insert contents)
      (cond
       ((funcall do-fill (- single-width header-length)))
       (t
        (or (funcall do-fill (- single-width indent-width))
            (funcall do-fill (- multi-width indent-width)))
        (goto-char (point-min))
        (insert "\n")
        (indent-rigidly (point-min) (point-max) indent-width)))
      (setq contents (buffer-string))
      (kill-buffer buf)))
  (sm-put-header header contents))

(defsubst sm-put-header-contents (header items &optional separator)
  (sm-put-header header
    (mapconcat 'identity items (or separator " "))))

(defsubst sm-put-random-sequence-items (header sequence &optional range)
  (sm-put-header-contents header
    (apply 'sm-random-sequence-items sequence range)))

(defsubst sm-put-header-multiline-content (header items)
  (sm-put-header-contents header
    items
    (concat "\n" (make-string sm-fill-indent-width ?\040))))

(defun sm-put-random-sequence-items-to-eol (header sequence &optional sep)
  (or sep (setq sep " "))
  (let ((width (- sm-fill-single-line-width (length header) 2))
        (seqlen (length sequence))
        (len 0)
        (continuep t)
        items tem newlen)
    (while continuep
      (setq tem (sm-sequence-item sequence (random seqlen)))
      (setq newlen (+ len (length sep) (length tem)))
      (cond ((and (> newlen width)
                  (consp items))
             (setq continuep nil))
            ((memq tem items))
            (t
             (setq items (cons tem items))
             (setq len newlen))))
    (sm-put-header header (mapconcat 'identity items sep))))

;; Add the specified header to the current mail message, with the given
;; contents.  If the header already exists, its contents are replaced.
(defun sm-put-header (header contents)
  (save-excursion
    (let ((buf-mod-p (buffer-modified-p))
          (header-exists (mail-position-on-field header)))
      (if header-exists
          (let ((end (point))
                (beg (progn
                       (re-search-backward (concat header ": "))
                       (goto-char (match-end 0)))))
            (delete-region beg end)))
      (insert contents)
      (setq sm-header-last-inserted header)
      (set-buffer-modified-p buf-mod-p))))

(defun sm-delete-header (header)
  "Delete HEADER and its content is it exists."
  (save-excursion
    (let ((buf-mod-p (buffer-modified-p))
          (header-exists (mail-position-on-field header)))
      (if header-exists
          (delete-region (point)
                         (progn
                           (re-search-backward (concat header ": "))
                           (forward-char -1)
                           (point)))))))

(defun sm-delete-last-header ()
  "Delete the last header field inserted by silly-mail."
  (interactive)
  (if (not sm-header-last-inserted)
      (message "Nothing to delete yet")
    (sm-delete-header sm-header-last-inserted)
    (setq sm-header-last-inserted nil)))

(put 'sm-put-header-fill-content 'lisp-indent-function 1)
(put 'sm-put-header-contents 'lisp-indent-function 1)
(put 'sm-put-header 'lisp-indent-function 1)


;; A private joke

(defvar sm-antipastobozoticataclysm-header
  "X-Antipastobozoticataclysm")

(defcustom sm-antipastobozoticataclysm-table
  '("Bariumenemanilow"
    "When George Bush projectile vomits antipasto on the Japanese.")
  "List of entries for `sm-add-antipastobozoticataclysm'."
  :type '(set
          (const "Bariumenemanilow")
          (const "When George Bush projectile vomits antipasto on the Japanese."))
  :group 'silly-mail)

(defun sm-add-antipastobozoticataclysm ()
  (interactive)
  (sm-put-header-fill-content sm-antipastobozoticataclysm-header
    (sm-random-sequence-item sm-antipastobozoticataclysm-table)))


(defvar sm-at&t-hype-header "X-AT&T-Hype")

(defun sm-add-at&t-hype ()
  (interactive)
  (require 'youwill)
  (sm-put-header-fill-content sm-at&t-hype-header (youwill)))


;; This is sort of based on the same principle as the NSA Fodder header.
;; In 1991, the MOD used to break into the FSF machines and read our email,
;; looking for security-related information.

(defvar sm-drdoom-fodder-header "X-Drdoom-Fodder")

(defcustom sm-drdoom-fodder-words
  '("CERT" "crash" "crypt" "drdoom" "passwd" "security" "root" "satan")
  "List of entries for `sm-add-drdoom-fodder'."
  :type '(set
          (const "CERT")
          (const "crash")
          (const "crypt")
          (const "drdoom")
          (const "passwd")
          (const "security")
          (const "root")
          (const "satan"))
  :group 'silly-mail)

(defvar sm-drdoom-fodder-length-range
  (list 5 (length sm-drdoom-fodder-words)))

(defun sm-add-drdoom-fodder ()
  (interactive)
  (sm-put-random-sequence-items sm-drdoom-fodder-header
                                sm-drdoom-fodder-words
                                sm-drdoom-fodder-length-range))


(defvar sm-emacs-name-header "X-Emacs-Acronym")

;; These have been contributed by people all over the network
;; (see the file etc/JOKES or emacs.names in the Emacs 19 distribution).
;; I modified some of them.
(defcustom sm-emacs-name-table
  '("Each Mail A Continued Surprise"
    "Each Manual's Audience is Completely Stupified"
    "Easily Maintained with the Assistance of Chemical Solutions"
    "Easily Mangles, Aborts, Crashes and Stupifies"
    "Eating Memory And Cycle-Sucking"
    "Editing MACroS"
    "Edwardian Manifestation of All Colonial Sins"
    "Egregious Managers Actively Court Stallman"
    "Eight Megabytes And Constantly Swapping"
    "Eleven Monkeys Asynchronously Create Slogans"
    "Elsewhere Maybe All Commands are Simple"
    "Elsewhere Maybe Alternative Civilizations Survive"
    "Elvis Masterminds All Computer Software"
    "Emacs Macht Alle Computer Schoen"
    "Emacs Made Almost Completely Screwed"
    "Emacs Maintainers Are Crazy Sickos"
    "Emacs Makes A Computer Slow"
    "Emacs Makes All Computing Simple"
    "Emacs Manuals Always Cause Senility"
    "Emacs Manuals Are Cryptic and Surreal"
    "Emacs Masquerades As Comfortable Shell"
    "Emacs May Alienate Clients and Supporters"
    "Emacs May Allow Customised Screwups"
    "Emacs May Annihilate Command Structures"
    "Emacs Means A Crappy Screen"
    "Emacs: My Alternative Computer Story"
    "Embarrassed Manual-Writer Accused of Communist Subversion"
    "Embarrassingly Mundane Advertising Cuts Sales"
    "Emetic Macros Assault Core and Segmentation"
    "Energetic Merchants Always Cultivate Sales"
    "Equine Mammals Are Considerably Smaller"
    "Eradication of Memory Accomplished with Complete Simplicity"
    "Erasing Minds Allows Complete Submission"
    "Escape Meta Alt Control Shift"
    "Esoteric Malleability Always Considered Silly"
    "Even My Aunt Crashes the System"
    "Even a Master of Arts Comes Simpler"
    "Evenings, Mornings, And a Couple of Saturdays"
    "Eventually Munches All Computer Storage"
    "Ever Made A Control-key Setup?"
    "Every Male Adolescent Craves Sex"
    "Every Mode Accelerates Creation of Software"
    "Every Mode Acknowledges Customized Strokes"
    "Every Moron Assumes CCA is Superior"
    "Everyday Material Almost Compiled Successfully"
    "Excavating Mayan Architecture Comes Simpler"
    "Excellent Manuals Are Clearly Suppressed"
    "Exceptionally Mediocre Algorithm for Computer Scientists"
    "Exceptionally Mediocre Autocratic Control System"
    "Experience the Mildest Ad Campaign ever Seen"
    "Extended Macros Are Considered Superfluous"
    "Extensibility and Modifiability Aggravate Confirmed Simpletons"
    "Extraneous Macros And Commands Stink"
    "Generally Not Used (Except by Middle Aged Computer Scientists)")
  "List of EMACS acronym expansions for `sm-add-emacs-name'."
  :type '(set
          (const "Each Mail A Continued Surprise")
          (const "Each Manual's Audience is Completely Stupified")
          (const "Easily Maintained with the Assistance of Chemical Solutions")
          (const "Easily Mangles, Aborts, Crashes and Stupifies")
          (const "Eating Memory And Cycle-Sucking")
          (const "Editing MACroS")
          (const "Edwardian Manifestation of All Colonial Sins")
          (const "Egregious Managers Actively Court Stallman")
          (const "Eight Megabytes And Constantly Swapping")
          (const "Eleven Monkeys Asynchronously Create Slogans")
          (const "Elsewhere Maybe All Commands are Simple")
          (const "Elsewhere Maybe Alternative Civilizations Survive")
          (const "Elvis Masterminds All Computer Software")
          (const "Emacs Macht Alle Computer Schoen")
          (const "Emacs Made Almost Completely Screwed")
          (const "Emacs Maintainers Are Crazy Sickos")
          (const "Emacs Makes A Computer Slow")
          (const "Emacs Makes All Computing Simple")
          (const "Emacs Manuals Always Cause Senility")
          (const "Emacs Manuals Are Cryptic and Surreal")
          (const "Emacs Masquerades As Comfortable Shell")
          (const "Emacs May Alienate Clients and Supporters")
          (const "Emacs May Allow Customised Screwups")
          (const "Emacs May Annihilate Command Structures")
          (const "Emacs Means A Crappy Screen")
          (const "Emacs: My Alternative Computer Story")
          (const "Embarrassed Manual-Writer Accused of Communist Subversion")
          (const "Embarrassingly Mundane Advertising Cuts Sales")
          (const "Emetic Macros Assault Core and Segmentation")
          (const "Energetic Merchants Always Cultivate Sales")
          (const "Equine Mammals Are Considerably Smaller")
          (const "Eradication of Memory Accomplished with Complete Simplicity")
          (const "Erasing Minds Allows Complete Submission")
          (const "Escape Meta Alt Control Shift")
          (const "Esoteric Malleability Always Considered Silly")
          (const "Even My Aunt Crashes the System")
          (const "Even a Master of Arts Comes Simpler")
          (const "Evenings, Mornings, And a Couple of Saturdays")
          (const "Eventually Munches All Computer Storage")
          (const "Ever Made A Control-key Setup?")
          (const "Every Male Adolescent Craves Sex")
          (const "Every Mode Accelerates Creation of Software")
          (const "Every Mode Acknowledges Customized Strokes")
          (const "Every Moron Assumes CCA is Superior")
          (const "Everyday Material Almost Compiled Successfully")
          (const "Excavating Mayan Architecture Comes Simpler")
          (const "Excellent Manuals Are Clearly Suppressed")
          (const "Exceptionally Mediocre Algorithm for Computer Scientists")
          (const "Exceptionally Mediocre Autocratic Control System")
          (const "Experience the Mildest Ad Campaign ever Seen")
          (const "Extended Macros Are Considered Superfluous")
          (const "Extensibility and Modifiability Aggravate Confirmed Simpletons")
          (const "Extraneous Macros And Commands Stink")
          (const "Generally Not Used (Except by Middle Aged Computer Scientists)"))
  :group 'silly-mail)

(defun sm-add-emacs-name ()
  (interactive)
  (sm-put-header sm-emacs-name-header
    (sm-random-sequence-item sm-emacs-name-table)))


;; Jim Blandy (and possibly Karl Fogel?) started this and contributed
;; most of the phrases.

(defvar sm-emacs-taunt-header "Emacs")

(defcustom sm-emacs-taunt-table
  '("a mistake carried out to perfection."
    "a moment of convenience, a lifetime of regret."
    "a terminal disease."
    "all the problems and twice the bugs."
    "complex nonsolutions to simple nonproblems."
    "dissatisfaction guaranteed."
    "don't get frustrated without it."
    "even not doing anything would have been better than nothing."
    "even your dog won't like it."
    "flaky and built to stay that way."
    "flawed beyond belief."
    "foiled again."
    "form follows malfunction."
    "garbage at your fingertips."
    "graphics hacking :: Roman numerals : sqrt (pi)"
    "ignorance is our most important resource."
    "it could be worse, but it'll take time."
    "it could happen to you."
    "it was hard to write; it should be hard to use."
    "let it get in *your* way."
    "live the nightmare."
    "more than enough rope."
    "never had it, never will."
    "no hardware is safe."
    "power tools for power fools."
    "power tools for power losers."
    "putting new limits on productivity."
    "simplicity made complex."
    "some voids are better left unfilled."
    "sometimes you fill a vacuum and it still sucks."
    "the art of incompetence."
    "the cutting edge of obsolescence."
    "the defacto substandard."
    "the first fully modular software disaster."
    "the joke that kills."
    "the problem for your problem."
    "there's got to be a better way."
    "warn your friends about it."
    "you'd better sit down."
    "you'll envy the dead.")
  "List of entries for `sm-add-emacs-taunt' (What users said as they collapsed)."
  :type '(set
          (const "a mistake carried out to perfection.")
          (const "a moment of convenience, a lifetime of regret.")
          (const "a terminal disease.")
          (const "all the problems and twice the bugs.")
          (const "complex nonsolutions to simple nonproblems.")
          (const "dissatisfaction guaranteed.")
          (const "don't get frustrated without it.")
          (const "even not doing anything would have been better than nothing.")
          (const "even your dog won't like it.")
          (const "flaky and built to stay that way.")
          (const "flawed beyond belief.")
          (const "foiled again.")
          (const "form follows malfunction.")
          (const "garbage at your fingertips.")
          (const "graphics hacking :: Roman numerals : sqrt (pi)")
          (const "ignorance is our most important resource.")
          (const "it could be worse, but it'll take time.")
          (const "it could happen to you.")
          (const "it was hard to write; it should be hard to use.")
          (const "let it get in *your* way.")
          (const "live the nightmare.")
          (const "more than enough rope.")
          (const "never had it, never will.")
          (const "no hardware is safe.")
          (const "power tools for power fools.")
          (const "power tools for power losers.")
          (const "putting new limits on productivity.")
          (const "simplicity made complex.")
          (const "some voids are better left unfilled.")
          (const "sometimes you fill a vacuum and it still sucks.")
          (const "the art of incompetence.")
          (const "the cutting edge of obsolescence.")
          (const "the defacto substandard.")
          (const "the first fully modular software disaster.")
          (const "the joke that kills.")
          (const "the problem for your problem.")
          (const "there's got to be a better way.")
          (const "warn your friends about it.")
          (const "you'd better sit down.")
          (const "you'll envy the dead."))
  :group 'silly-mail)

(defun sm-add-emacs-taunt ()
  (interactive)
  (sm-put-header (concat (if sm-always-X-prefix "X-") sm-emacs-taunt-header)
    (sm-random-sequence-item sm-emacs-taunt-table)))

;;(setq bizarre-gratuitous-variable '(miscellaneous gratuitous list))


;; Add an insulting flame into your mail headers.

(defvar sm-flame-header "X-Flame")

(defun sm-add-flame ()
  (interactive)
  (or (fboundp '*flame)
      (fboundp 'flame-string)
      (load "flame"))
  (sm-put-header-fill-content sm-flame-header
    (if (fboundp 'flame-string)
        ;; friedman's flame.el
        (flame-string)
      ;; XEmacs/Emacs-18 flame.el
      (sentence-ify (string-ify (append-suffixes-hack
                                 (flatten (*flame))))))))


(defvar sm-horoscope-header "X-Horoscope")

(defun sm-add-horoscope ()
  (interactive)
  (require 'horoscope)
  (sm-put-header-fill-content sm-horoscope-header (horoscope)))


;; Add words of wisdom from the grepmeister.

(defvar sm-kibology-header "X-Kibo-Says")

(defun sm-add-kibology ()
  (interactive)
  (require 'kibologize)
  (sm-put-header-fill-content sm-kibology-header (kibologize)))


;; Contributed by David LaMacchia <dml@topped-with-meat.com>

(defvar sm-meat-header "X-Meat")

(defcustom sm-meat-table
  '("Abalone"
    "Back Bacon"
    "Bacon"
    "Beef Jerky"
    "Biltong"   ; african-style jerky, usually beef, ostrich, or antelope
    "Blood sausage"
    "Buffalo"
    "Calimari"
    "Chicken Fried Steak"
    "Chicken"
    "Clam Jerky"
    "Duck"
    "Flanken"
    "Haggis"
    "Ham"
    "Head cheese"
    "Liverwurst"
    "Lobster"
    "Long pork"
    "Molinari"
    "Olive Loaf"
    "Parma"
    "Prosciutto"
    "Ptarmigan"
    "Roo burgers"
    "Salame"
    "Spruce grouse"
    "Squirrel"
    "Swordfish"
    "Turkey Jerky"
    "Veal"
    "Venison"
    "Wallaby steak")
  "List of entries for `sm-add-meat'."
  :type '(set
          (const "Abalone")
          (const "Back Bacon")
          (const "Bacon")
          (const "Beef Jerky")
          (const "Biltong")
          (const "Blood sausage")
          (const "Buffalo")
          (const "Calimari")
          (const "Chicken Fried Steak")
          (const "Chicken")
          (const "Clam Jerky")
          (const "Duck")
          (const "Flanken")
          (const "Haggis")
          (const "Ham")
          (const "Head cheese")
          (const "Liverwurst")
          (const "Lobster")
          (const "Long pork")
          (const "Molinari")
          (const "Olive Loaf")
          (const "Parma")
          (const "Prosciutto")
          (const "Ptarmigan")
          (const "Roo burgers")
          (const "Salame")
          (const "Spruce grouse")
          (const "Squirrel")
          (const "Swordfish")
          (const "Turkey Jerky")
          (const "Veal")
          (const "Venison")
          (const "Wallaby steak"))
  :group 'silly-mail)

(defun sm-add-meat ()
  (interactive)
  (sm-put-header sm-meat-header
    (sm-random-sequence-item sm-meat-table)))


;; From Karl Fogel <kfogel@red-bean.com>

(defvar sm-microsoft-header "Microsoft")

(defcustom sm-microsoft-table
  '("I'm not laughing anymore."
    "Making the world a better place... for Microsoft."
    "Programs so large they have weather."
    "We've got the solution for the problem we sold you."
    "Where `market lock-in' means throwing away the keys."
    "Where even the version numbers aren't Y2K-compliant"
    "Where the service packs are larger than the original releases."
    "With our software, there's no limit to what you can't do!"
    "World domination wasn't enough -- we had to write bad software, too!")
  "List of entries for `sm-add-microsoft'."
  :type '(set
          (const "I'm not laughing anymore.")
          (const "Making the world a better place... for Microsoft.")
          (const "Programs so large they have weather.")
          (const "We've got the solution for the problem we sold you.")
          (const "Where `market lock-in' means throwing away the keys.")
          (const "Where even the version numbers aren't Y2K-compliant")
          (const "Where the service packs are larger than the original releases.")
          (const "With our software, there's no limit to what you can't do!")
          (const "World domination wasn't enough -- we had to write bad software, too!"))
  :group 'silly-mail)

(defun sm-add-microsoft ()
  (interactive)
  (sm-put-header (concat (if sm-always-X-prefix "X-") sm-microsoft-header)
    (sm-random-sequence-item sm-microsoft-table)))


(defvar sm-nsa-header "X-NSA-Fodder")

(defun sm-add-nsa-fodder ()
  (interactive)
  (or (fboundp 'snarf-spooks) (load "spook"))
  (sm-put-random-sequence-items-to-eol sm-nsa-header (snarf-spooks)))


;; Inspiration for this came from Brian Rice, a sicko genius.

(defvar sm-shopping-list-header "X-Shopping-List")

(defvar sm-shopping-list-count '(3 . 6))
(defvar sm-shopping-list-multi-line-p t)

(defun sm-add-shopping-list (&optional item-count)
  (interactive "P")
  (require 'shop)
  (cond ((or (null item-count)
             (and (consp item-count)
                  (null (cdr item-count))))
         (setq item-count sm-shopping-list-count)))
  (let ((items (shop-string-numbered-list (if (consp item-count)
                                              (shop-random-range
                                               (car item-count)
                                               (cdr item-count))
                                            item-count))))
    (cond (sm-shopping-list-multi-line-p
           (sm-put-header-multiline-content sm-shopping-list-header
                                            (cons "" items)))
          (t
           (sm-put-header-contents sm-shopping-list-header items "; ")))))


;; Tom Swifties.  Blame for these go mainly to Noah Friedman
;; and Thomas (nee Michael) Bushnell.

(defvar sm-tom-swifty-header "X-Tom-Swifty")

(defcustom sm-tom-swifty-table
  '("\"All the cherry trees are dead,\" Tom said fruitlessly."
    "\"And what should you set your PS1 shell variable to?\" Tom prompted."
    "\"Any fresh fruit in the kitchen?\" Tom asked peeringly."
    "\"C++ is the wave of the future,\" Tom said objectively."
    "\"Care for some `suan la chow show'?\" Tom asked wantonly."
    "\"Condensed chicken soup,\" was Tom's canned response."
    "\"Darling, what vegetable becomes an act of passion when misspelled?\", Tom breathed ravishingly."
    "\"Eat me,\" was Tom's biting response."
    "\"Ed is the Standard Text Editor,\" Tom sed."
    "\"Evergreens have always been my favorite,\" Tom opined."
    "\"He came at me out of the blue,\" Tom said airily."
    "\"I am writing lots of little verses,\"  Tom said blankly."
    "\"I can't drink alcohol,\" Tom said spiritually."
    "\"I can't get this fire started,\" Tom said woodenly."
    "\"I can't stand baby food,\" Tom said in a strained voice."
    "\"I can't wait to see the doctor,\" Tom said impatiently."
    "\"I don't WANNA get drunk,\" Tom wined."
    "\"I don't have any piano music,\"  Tom said listlessly."
    "\"I don't have the slightest idea how to milk this cow,\" Tom said in utter confusion."
    "\"I don't understand how square roots work,\" Tom said irrationally."
    "\"I don't want any champagne!\" Tom said, blowing his top."
    "\"I feel like I'm running around in circles,\"  Tom said squarely."
    "\"I got to get a text-processor that does my files the right way,\" Tom said awkwardly."
    "\"I guess I shouldn't have broken the mirror,\" Tom reflected."
    "\"I hate Frere Jacques,\" Tom said as he roundly denounced it."
    "\"I have no intention of traversing binary trees!\", Tom barked."
    "\"I have to finish sorting these writing utensils,\" Tom said pensively."
    "\"I hope this emulsion works,\" Tom said in suspense."
    "\"I just burned my hand in the blast furnace,\" Tom said, overwrought."
    "\"I just don't understand the number seventeen,\" Tom said randomly."
    "\"I just got some chicken wire,\" Tom said defensively."
    "\"I just poisoned myself,\" Tom lyed."
    "\"I just sharpened my pencil,\" Tom said pointedly."
    "\"I like Gregorian chants,\" Tom intoned."
    "\"I like amputations,\" Tom said disarmingly."
    "\"I like sun cartridge tapes,\" Tom said quickly."
    "\"I never get good bridge hands,\" Tom said in passing."
    "\"I only like black and white,\" Tom said monotonously."
    "\"I really like penguins,\" Tom said in a flighty voice."
    "\"I recommend listening to radio station ``WHAT'',\" Tom said quietly."
    "\"I think it's time we got married,\" Tom said engagingly."
    "\"I train dolphins,\" Tom said purposefully."
    "\"I'll have to grade your test again,\" Tom remarked."
    "\"I'm completely bankrupt,\" Tom said senselessly."
    "\"I'm fond of Pavarotti,\" Tom said menacingly."
    "\"I'm gainfully employed at the Weight-Watchers gymnasium,\" Tom said wastefully."
    "\"I'm getting fat,\" Tom said expansively."
    "\"I'm going to copy this tape,\" Tom said for the record."
    "\"I'm hardly ever aware of what I'm going to do next,\" Tom said unconsciously."
    "\"I'm having deja-vu,\" Tom said again."
    "\"I'm really bored,\" Tom said flatly."
    "\"I'm sorry I broke your window,\" Tom said painfully."
    "\"I'm sorry to hear I knocked you up,\" Tom said after a pregnant pause."
    "\"I've burned my tongue,\" Tom said distastefully."
    "\"I've finished counting the horses,\" Tom said summarily."
    "\"I've got a bucket full of forearms,\" Tom said wistfully."
    "\"I've just been drafted,\"  Tom said impressively."
    "\"I've made a complete ash of myself,\" Tom said brazenly."
    "\"IBM is up 3 points,\" Tom said, taking stock of the situation."
    "\"If only we could piece together this crime,\" Tom said in a puzzled voice."
    "\"It needs more seasoning,\" Tom said sagely."
    "\"It's patently obvious,\" Tom said licentiously."
    "\"It's really cold out here,\" Tom said in a muffled voice."
    "\"It's really windy outside,\" said Tom with gusto."
    "\"Lisp is such a symbol-minded language,\" Tom commonly said."
    "\"My feet hurt,\" Tom said pedantically."
    "\"My lenses will stay perfectly clear,\" Tom said optimistically."
    "\"My mouse buttons don't work,\" Tom said in a depressed voice."
    "\"My terminal is completely screwed up,\" Tom cursed."
    "\"On the other hand, eating at a table is more civilized,\" Tom countered."
    "\"Quick!  Change the baby's diaper,\" Tom said rashly."
    "\"Socialism is dead,\" Tom communicated."
    "\"The ASCII standard sucks,\" Tom said characteristically."
    "\"The GNU project will probably not be Posix conformant,\" Tom said noncommittally."
    "\"The judge sentenced him to the chair,\" Tom said dielectrically."
    "\"The printer is using too much toner,\"  Tom said darkly."
    "\"The rooster was decapitated,\" Tom said in a crestfallen voice."
    "\"The sequence `M-4' is equivalent to `C-u 4',\" Tom said metaphorically."
    "\"The sky is falling,\" Tom said in a crushed voiced."
    "\"The sun just rose over the cemetary,\" Tom said in mourning."
    "\"This anesthetic isn't very effective,\" Tom said unnervingly."
    "\"This awl is broken,\" Tom said pointlessly."
    "\"This is illegal, I just know it,\" Tom said with conviction."
    "\"Turn that fan off,\" Tom said coldly."
    "\"VI is much better than EMACS,\" Tom said with joy."
    "\"Wait! You need to enable interrupts first!\" Tom said preemptorally."
    "\"We'll have to take the stairs,\" Tom said in an elevated voice."
    "\"We're all out of flowers,\" Tom said lackadaisically."
    "\"We're going to sue you for that window system,\" Tom said inexorably."
    "\"We're going to use decimal notation,\" Tom said tentatively."
    "\"Well, I guess we should pitch camp,\" Tom said tentatively."
    "\"Well, it didn't increase at all,\" Tom said, nonplussed."
    "\"What is today's date?\" Tom asked in a timely fashion."
    "\"When will the Hurd be released?\" Tom asked Machingly."
    "\"Who drank the last beer?\" Tom asked, hopping mad."
    "\"You have new mail,\" Tom said in his usual delivery."
    "\"You light up my life,\" Tom said brightly."
    "\"You pinhead,\" Tom said pointedly.")
  "List of entries for `sm-add-tom-swifty'."
  :type '(set
          (const "\"All the cherry trees are dead,\" Tom said fruitlessly.")
          (const "\"And what should you set your PS1 shell variable to?\" Tom prompted.")
          (const "\"Any fresh fruit in the kitchen?\" Tom asked peeringly.")
          (const "\"C++ is the wave of the future,\" Tom said objectively.")
          (const "\"Care for some `suan la chow show'?\" Tom asked wantonly.")
          (const "\"Condensed chicken soup,\" was Tom's canned response.")
          (const "\"Darling, what vegetable becomes an act of passion when misspelled?\", Tom breathed ravishingly.")
          (const "\"Eat me,\" was Tom's biting response.")
          (const "\"Ed is the Standard Text Editor,\" Tom sed.")
          (const "\"Evergreens have always been my favorite,\" Tom opined.")
          (const "\"He came at me out of the blue,\" Tom said airily.")
          (const "\"I am writing lots of little verses,\"  Tom said blankly.")
          (const "\"I can't drink alcohol,\" Tom said spiritually.")
          (const "\"I can't get this fire started,\" Tom said woodenly.")
          (const "\"I can't stand baby food,\" Tom said in a strained voice.")
          (const "\"I can't wait to see the doctor,\" Tom said impatiently.")
          (const "\"I don't WANNA get drunk,\" Tom wined.")
          (const "\"I don't have any piano music,\"  Tom said listlessly.")
          (const "\"I don't have the slightest idea how to milk this cow,\" Tom said in utter confusion.")
          (const "\"I don't understand how square roots work,\" Tom said irrationally.")
          (const "\"I don't want any champagne!\" Tom said, blowing his top.")
          (const "\"I feel like I'm running around in circles,\"  Tom said squarely.")
          (const "\"I got to get a text-processor that does my files the right way,\" Tom said awkwardly.")
          (const "\"I guess I shouldn't have broken the mirror,\" Tom reflected.")
          (const "\"I hate Frere Jacques,\" Tom said as he roundly denounced it.")
          (const "\"I have no intention of traversing binary trees!\", Tom barked.")
          (const "\"I have to finish sorting these writing utensils,\" Tom said pensively.")
          (const "\"I hope this emulsion works,\" Tom said in suspense.")
          (const "\"I just burned my hand in the blast furnace,\" Tom said, overwrought.")
          (const "\"I just don't understand the number seventeen,\" Tom said randomly.")
          (const "\"I just got some chicken wire,\" Tom said defensively.")
          (const "\"I just poisoned myself,\" Tom lyed.")
          (const "\"I just sharpened my pencil,\" Tom said pointedly.")
          (const "\"I like Gregorian chants,\" Tom intoned.")
          (const "\"I like amputations,\" Tom said disarmingly.")
          (const "\"I like sun cartridge tapes,\" Tom said quickly.")
          (const "\"I never get good bridge hands,\" Tom said in passing.")
          (const "\"I only like black and white,\" Tom said monotonously.")
          (const "\"I really like penguins,\" Tom said in a flighty voice.")
          (const "\"I recommend listening to radio station ``WHAT'',\" Tom said quietly.")
          (const "\"I think it's time we got married,\" Tom said engagingly.")
          (const "\"I train dolphins,\" Tom said purposefully.")
          (const "\"I'll have to grade your test again,\" Tom remarked.")
          (const "\"I'm completely bankrupt,\" Tom said senselessly.")
          (const "\"I'm fond of Pavarotti,\" Tom said menacingly.")
          (const "\"I'm gainfully employed at the Weight-Watchers gymnasium,\" Tom said wastefully.")
          (const "\"I'm getting fat,\" Tom said expansively.")
          (const "\"I'm going to copy this tape,\" Tom said for the record.")
          (const "\"I'm hardly ever aware of what I'm going to do next,\" Tom said unconsciously.")
          (const "\"I'm having deja-vu,\" Tom said again.")
          (const "\"I'm really bored,\" Tom said flatly.")
          (const "\"I'm sorry I broke your window,\" Tom said painfully.")
          (const "\"I'm sorry to hear I knocked you up,\" Tom said after a pregnant pause.")
          (const "\"I've burned my tongue,\" Tom said distastefully.")
          (const "\"I've finished counting the horses,\" Tom said summarily.")
          (const "\"I've got a bucket full of forearms,\" Tom said wistfully.")
          (const "\"I've just been drafted,\"  Tom said impressively.")
          (const "\"I've made a complete ash of myself,\" Tom said brazenly.")
          (const "\"IBM is up 3 points,\" Tom said, taking stock of the situation.")
          (const "\"If only we could piece together this crime,\" Tom said in a puzzled voice.")
          (const "\"It needs more seasoning,\" Tom said sagely.")
          (const "\"It's patently obvious,\" Tom said licentiously.")
          (const "\"It's really cold out here,\" Tom said in a muffled voice.")
          (const "\"It's really windy outside,\" said Tom with gusto.")
          (const "\"Lisp is such a symbol-minded language,\" Tom commonly said.")
          (const "\"My feet hurt,\" Tom said pedantically.")
          (const "\"My lenses will stay perfectly clear,\" Tom said optimistically.")
          (const "\"My mouse buttons don't work,\" Tom said in a depressed voice.")
          (const "\"My terminal is completely screwed up,\" Tom cursed.")
          (const "\"On the other hand, eating at a table is more civilized,\" Tom countered.")
          (const "\"Quick!  Change the baby's diaper,\" Tom said rashly.")
          (const "\"Socialism is dead,\" Tom communicated.")
          (const "\"The ASCII standard sucks,\" Tom said characteristically.")
          (const "\"The GNU project will probably not be Posix conformant,\" Tom said noncommittally.")
          (const "\"The judge sentenced him to the chair,\" Tom said dielectrically.")
          (const "\"The printer is using too much toner,\"  Tom said darkly.")
          (const "\"The rooster was decapitated,\" Tom said in a crestfallen voice.")
          (const "\"The sequence `M-4' is equivalent to `C-u 4',\" Tom said metaphorically.")
          (const "\"The sky is falling,\" Tom said in a crushed voiced.")
          (const "\"The sun just rose over the cemetary,\" Tom said in mourning.")
          (const "\"This anesthetic isn't very effective,\" Tom said unnervingly.")
          (const "\"This awl is broken,\" Tom said pointlessly.")
          (const "\"This is illegal, I just know it,\" Tom said with conviction.")
          (const "\"Turn that fan off,\" Tom said coldly.")
          (const "\"VI is much better than EMACS,\" Tom said with joy.")
          (const "\"Wait! You need to enable interrupts first!\" Tom said preemptorally.")
          (const "\"We'll have to take the stairs,\" Tom said in an elevated voice.")
          (const "\"We're all out of flowers,\" Tom said lackadaisically.")
          (const "\"We're going to sue you for that window system,\" Tom said inexorably.")
          (const "\"We're going to use decimal notation,\" Tom said tentatively.")
          (const "\"Well, I guess we should pitch camp,\" Tom said tentatively.")
          (const "\"Well, it didn't increase at all,\" Tom said, nonplussed.")
          (const "\"What is today's date?\" Tom asked in a timely fashion.")
          (const "\"When will the Hurd be released?\" Tom asked Machingly.")
          (const "\"Who drank the last beer?\" Tom asked, hopping mad.")
          (const "\"You have new mail,\" Tom said in his usual delivery.")
          (const "\"You light up my life,\" Tom said brightly.")
          (const "\"You pinhead,\" Tom said pointedly."))
  :group 'silly-mail)

(defun sm-add-tom-swifty ()
  (interactive)
  (sm-put-header-fill-content sm-tom-swifty-header
    (sm-random-sequence-item sm-tom-swifty-table)))


;; I think Lars Bader came up with this one first.
;; Lately Jim Blandy and others have used it also.
;;
;; It's a test to see if any mailers break because they can't actually
;; implement oddly-colored tomatos, or something like that.

(defvar sm-tomato-header "Tomato")

(defcustom sm-tomato-table
  '("Beige"
    "Green"
    "Heliotrope"
    "Mauve"
    "Plaid"
    "Polka-dot")
  "List of entries for `sm-add-tomato'."
  :type '(set
          (const "Beige")
          (const "Green")
          (const "Heliotrope")
          (const "Mauve")
          (const "Plaid")
          (const "Polka-dot"))
  :group 'silly-mail)

(defun sm-add-tomato ()
  (interactive)
  (sm-put-header (concat (if sm-always-X-prefix "X-") sm-tomato-header)
    (sm-random-sequence-item sm-tomato-table)))


(defvar sm-uboat-death-message-header "X-Uboat-Death-Message")

(defun sm-add-uboat-death-message ()
  (interactive)
  (require 'uboat)
  (sm-put-header-fill-content sm-uboat-death-message-header
    (uboat-death-message)))


;; Most of these came from the unix-haters mailing list.
;; Jamie Zawinski added more later.

(defvar sm-x-taunt-header "X-Windows")

(defcustom sm-x-taunt-table
  '("a mistake carried out to perfection."
    "a moment of convenience, a lifetime of regret."
    "a terminal disease."
    "all the problems and twice the bugs."
    "complex nonsolutions to simple nonproblems."
    "dissatisfaction guaranteed."
    "don't get frustrated without it."
    "even not doing anything would have been better than nothing."
    "even your dog won't like it."
    "flaky and built to stay that way."
    "flawed beyond belief."
    "foiled again."
    "form follows malfunction."
    "garbage at your fingertips."
    "graphics hacking :: Roman numerals : sqrt (pi)"
    "ignorance is our most important resource."
    "it could be worse, but it'll take time."
    "it could happen to you."
    "it was hard to write; it should be hard to use."
    "let it get in *your* way."
    "live the nightmare."
    "more than enough rope."
    "never had it, never will."
    "no hardware is safe."
    "power tools for power fools."
    "power tools for power losers."
    "putting new limits on productivity."
    "simplicity made complex."
    "some voids are better left unfilled."
    "sometimes you fill a vacuum and it still sucks."
    "the art of incompetence."
    "the cutting edge of obsolescence."
    "the defacto substandard."
    "the first fully modular software disaster."
    "the joke that kills."
    "the problem for your problem."
    "there's got to be a better way."
    "warn your friends about it."
    "you'd better sit down."
    "you'll envy the dead.")
  "List of entries for `sm-add-x-taunt' (What users said as they collapsed)."
  :type '(set
          (const "a mistake carried out to perfection.")
          (const "a moment of convenience, a lifetime of regret.")
          (const "a terminal disease.")
          (const "all the problems and twice the bugs.")
          (const "complex nonsolutions to simple nonproblems.")
          (const "dissatisfaction guaranteed.")
          (const "don't get frustrated without it.")
          (const "even not doing anything would have been better than nothing.")
          (const "even your dog won't like it.")
          (const "flaky and built to stay that way.")
          (const "flawed beyond belief.")
          (const "foiled again.")
          (const "form follows malfunction.")
          (const "garbage at your fingertips.")
          (const "graphics hacking :: Roman numerals : sqrt (pi)")
          (const "ignorance is our most important resource.")
          (const "it could be worse, but it'll take time.")
          (const "it could happen to you.")
          (const "it was hard to write; it should be hard to use.")
          (const "let it get in *your* way.")
          (const "live the nightmare.")
          (const "more than enough rope.")
          (const "never had it, never will.")
          (const "no hardware is safe.")
          (const "power tools for power fools.")
          (const "power tools for power losers.")
          (const "putting new limits on productivity.")
          (const "simplicity made complex.")
          (const "some voids are better left unfilled.")
          (const "sometimes you fill a vacuum and it still sucks.")
          (const "the art of incompetence.")
          (const "the cutting edge of obsolescence.")
          (const "the defacto substandard.")
          (const "the first fully modular software disaster.")
          (const "the joke that kills.")
          (const "the problem for your problem.")
          (const "there's got to be a better way.")
          (const "warn your friends about it.")
          (const "you'd better sit down.")
          (const "you'll envy the dead."))
  :group 'silly-mail)


(defun sm-add-x-taunt ()
  (interactive)
  (sm-put-header sm-x-taunt-header
    (sm-random-sequence-item sm-x-taunt-table)))


;; Yow!  Am I quoted in your EMAIL yet?

(defvar sm-zippy-quote-header "X-Zippy-Says")

(defun sm-add-zippy-quote ()
  (interactive)
  (or (fboundp 'yow) (load "yow"))
  (sm-put-header-fill-content sm-zippy-quote-header (yow)))


(provide 'silly-mail)

;;; silly-mail.el ends here
