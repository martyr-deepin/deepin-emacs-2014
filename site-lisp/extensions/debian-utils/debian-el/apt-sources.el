;;; apt-sources.el --- Mode for editing apt source.list file
;;
;; Version: 0.9.9
;; $Revision:
;; $Id:
;; $Source:

;; Author: Dr. Rafael Sepúlveda. <drs@gnulinux.org.mx>
;; Mantainer: Peter S. Galbraith <psg@debian.org>
;;            (I can't find Dr. Rafael Sepúlveda)

;; Copyright (C) 2001-2003, Dr. Rafael Sepúlveda <drs@gnulinux.org.mx>
;; Copyright (C) 2009  Peter S. Galbraith <psg@debian.org>

;;   This program is free software; you can redistribute it and/or modify
;;   it under the terms of the GNU General Public License as published by
;;   the Free Software Foundation.

;;   This program is distributed in the hope that it will be useful,
;;   but WITHOUT ANY WARRANTY; without even the implied warranty of
;;   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;   GNU General Public License for more details.

;;   You should have received a copy of the GNU General Public License
;;   along with this program;  if not, write to the Free Software Foundation,
;;   Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307, USA.

;;; Commentary:

;; This mode is for editing '/etc/apt/sources.list', the APT (Advanced
;; Package Tool) configuration file found on Debian systems.

;; APT is a package retrieval tool for Debian (a GNU distribution, see
;; http://www.debian.org); for example you could install Emacs with
;; the command:
;;
;;   apt-get install emacs21
;;
;; and APT will then retrieve the package and install it for you.  The
;; sources.list file tells APT where to look for packages.  Mine looks
;; like this:
;;
;;   deb http://http.us.debian.org/debian unstable main contrib
;;   deb http://non-us.debian.org/debian-non-US unstable/non-US main
;;
;;   deb ftp://ftp.de.debian.org/debian ../project/experimental main
;;
;; This mode font-locks the file and add some things including new
;; source lines and modifying existing source lines.
;;
;; This mode can be customized in diferent parts.  You can (interactively)
;; change if you want blank lines around a new source line and comment
;; with `apt-sources-around-lines'.  Also you can change the way that
;; this mode names each source line, with variable`apt-sources-source-name';
;; if no name is entered, no commente name will be inserted.
;; To customize, try `M-x customize-group [RET] apt-sources'
;;
;; You can modify existing parts of the source line; check the mode
;; documentation for mor details.  Another thing that this mode can do is to
;; replicate an existing line (`C-c C-r') that will be changed to the 'deb' or
;; 'deb-src' corresponding line.  If it replicates a 'deb' line, an identical
;; 'deb-src' source line will be created.
;;
;; To load this mode, you can add a "Local Variables" block at the end of
;; the sources.list file with `C-c C-v' after you change the file's mode to
;; 'apt-sources'.  But this should not be needed since an entry is added to
;; `auto-mode-alist' to automatically enter this mode when editing files
;; named `sources.list'.
;;
;; You can always find the latest version of this mode on
;; 'http://people.gnulinux.org.mx/drs/emacs/apt-sources.el'

;;; TODO:

;;; History:

;; 0.9.9 2009-11-25 Peter S. Galbraith <psg@debian.org>
;;       -- Create syntax table and add comments. (Closes: #469971)
;; 0.9.8 -- Remove dependency to autoinsert, because it's no longer required.
;;           (suggested by Peter S. Galbraith <psg@debian.org>)
;; 0.9.7 -- Converted relevant defvar statements to defcustom, and added
;;           `auto-mode-alist' entry. (Peter S. Galbraith <psg@debian.org>)
;;       -- Add completion to some of the fields in `apt-sources-new-source'
;;           and the functions that change parameters. (suggested by
;;           Peter S. Galbraith <psg@debian.org>)
;;       -- Add menu support. (suggested by Peter S. Galbraith <psg@debian.org>)
;;       -- Change name from `apt-sources-insert-source' to
;;          `apt-sources-new-source'. (suggested by Peter S. Galbraith
;;           <psg@debian.org>)
;;       -- Change name from `apt-sources-insert-local-var' to
;;          `apt-sources-insert-local-vars'. (suggested by Peter S. Galbraith
;;           <psg@debian.org>)
;;       -- Change `apt-sources-change-components' to handle absence of
;;           components. (Dr. Rafael Sepúlveda <drs@gnulinux.org.mx>)
;;       -- Change the web page address form where you can find the latest version.
;;       (Dr. Rafael Sepúlveda <drs@gnulinux.org.mx>)
;; 0.9.6 -- Added a better description to what is APT and file 'sources.list'.
;;           (Ole Laursen <olau@hardworking.dk>)
;; 0.9.5 -- Fixed typo in docstring of function `apt-sources-around-lines'.
;;       -- Rewrite function `apt-sources-around-lines' with a better algorithm.
;;           (John Paul Wallington <jpw@shootybangbang.com>)
;;       -- Added a description to apt and sources.list
;;           (David Combs <dkcombs@panix.com>)
;;       -- Added name and email from contributors. :)
;;           (Dr. Rafael Sepúlveda <drs@gnulinux.org.mx>)
;; 0.9.4 -- Added a missing option in function `apt-sources-insert-source' to
;;           select 'ftp' type.
;;       -- Added URI-type 'https'.
;;       -- Fix some function's documentation mistakes.
;;           (Dr. Rafael Sepúlveda <drs@gnulinux.org.mx>)
;; 0.9.3 -- Fix a recently introduced bug that prevents keybindings work under
;;           Xemacs.
;;           (John Paul Wallington <jpw@shootybangbang.com>)
;; 0.9.2 -- Fix a bug with a keybinding that called a function with another name.
;;       -- Make variable `comment-start' buffer-local.
;;       -- Change the keybinding zone to be more compact and portable.
;;           (John Paul Wallington <jpw@shootybangbang.com>)
;;       -- Change some keybindings.
;;           (Dr. Rafael Sepúlveda <drs@gnulinux.org.mx>)
;;
;; 0.9.1 -- Corrected a bug in the 'cond' clauses that prevented to byte-compile.
;;           (Perkens-Golomb, Burkhard <burkhard.perkens-golomb@sdm.de>)'
;;       -- Make variable `comment-start-skip' buffer-local.
;;           (Stefan Monnier <monnier+gnu.emacs.sources/news/@flint.cs.yale.edu>)
;; 0.9   -- first release.
;;           (Dr. Rafael Sepúlveda <drs@gnulinux.org.mx>)

;;; Code:

;;(require 'autoinsert)

(defgroup apt-sources nil "Mode for editing apt source.list file"
  :group 'tools
  :prefix "apt-sources-")

(defcustom apt-sources-mode-hook nil
  "*Hook for customising apt-sources mode."
  :type 'hook
  :group 'apt-sources)

(defcustom apt-sources-load-hook nil
  "*Hook run when the `apt-sources-mode' is loaded."
  :type 'hook
  :group 'apt-sources)

(defcustom apt-sources-around-lines t
  "Put blank lines around the inserted source lines.
This variable can be changed by function `apt-sources-around-lines'"
  :type 'boolean
  :group 'apt-sources)

(defcustom apt-sources-source-name "##\n## %s\n##\n\n"
  "Format used in the name of a new source line.
This line is inserted by `apt-source-new-source' function.  You can
use ANSI quoting as described in the info elisp manual, chapter
'Character Type'.  The '%s' is where the name of the source line will be
inserted."
  :type 'string
  :group 'apt-sources)

(defvar apt-sources-mode-syntax-table nil
  "Syntax table used in `apt-sources-mode' buffers.")
(if apt-sources-mode-syntax-table
    ()
  (setq apt-sources-mode-syntax-table (make-syntax-table))

  ;; Support # style comments
  (modify-syntax-entry ?#  "<"  apt-sources-mode-syntax-table)
  (modify-syntax-entry ?\n "> " apt-sources-mode-syntax-table))

;;Regexps for identifying source line parts for font-lock.
(defvar apt-sources-font-lock-deb-regexp "\\(deb\\|deb-src\\)"
  "A regexp that matches 'deb' or 'deb-src' at the begining of line.")

(defvar apt-sources-font-lock-uri-regexp
  "\\([^ ]+\\)"
  "A regexp that matches the URI part of the source line.")

(defvar apt-sources-font-lock-distribution-regexp
  "\\([^ ]+\\)"
  "A regexp that matches the distribution name part of the source line.")


(defvar apt-sources-font-lock-keywords
  (list
   ;; Comments
   ;;'("^#.*$" . font-lock-comment-face)
   ;; sources.list lines:
   ;; deb http://http.us.debian.org/debian unstable main contrib
   (cons
    (concat "^"
            apt-sources-font-lock-deb-regexp " +"
            apt-sources-font-lock-uri-regexp " +"
            apt-sources-font-lock-distribution-regexp
            " +\\([^#\n]+\\)")
    '(
      (1 font-lock-constant-face)
      (2 font-lock-variable-name-face)
      (3 font-lock-type-face)
      (4 font-lock-keyword-face))))
  "Info for function `font-lock-mode'.")

(defvar apt-sources-mode-map nil
  "Keymap used in apt-sources mode.")

(unless apt-sources-mode-map
  (let ((map (make-sparse-keymap)))

    ;; Keybindings
    (define-key map (kbd "C-c C-i") 'apt-sources-new-source)
    (define-key map (kbd "C-c C-l") 'apt-sources-around-lines)
    (define-key map (kbd "C-c C-v") 'apt-sources-insert-local-vars)
    (define-key map (kbd "C-c C-r") 'apt-sources-deb-or-src-replicate)

    (define-key map (kbd "C-c C-s") 'apt-sources-change-source-type)
    (define-key map (kbd "C-c C-t") 'apt-sources-change-uri-type)
    (define-key map (kbd "C-c C-a") 'apt-sources-change-uri-address)
    (define-key map (kbd "C-c C-d") 'apt-sources-change-distribution)
    (define-key map (kbd "C-c C-c") 'apt-sources-change-components)

    (define-key map (kbd "C-c C-n") 'apt-sources-next-source-line)
    (define-key map (kbd "C-c C-p") 'apt-sources-previous-source-line)
    (setq apt-sources-mode-map map)

    ;; Menu
    (define-key apt-sources-mode-map [menu-bar] (make-sparse-keymap))
    (define-key apt-sources-mode-map [menu-bar apt-sources]
      (cons "Apt-sources" map))

    (define-key map [menu-bar apt-sources previous-source-line]
      '("Go to previous source line" . apt-sources-previous-source-line))
    (define-key map [menu-bar apt-sources next-source-line]
      '("Go to next source line" . apt-sources-next-source-line))
    (define-key map [menu-bar apt-sources separator-actions]
      '("--"))
    (define-key map [menu-bar apt-sources change-components]
      '("Change components" . apt-sources-change-components))
    (define-key map [menu-bar apt-sources change-distribution]
      '("Change distribution" . apt-sources-change-distribution))
    (define-key map [menu-bar apt-sources change-uri-address]
      '("Change URI address" . apt-sources-change-uri-address))
    (define-key map [menu-bar apt-sources change-uri-type]
      '("Change URI type" . apt-sources-change-uri-type))
    (define-key map [menu-bar apt-sources change-source-type]
      '("Change source type" . apt-sources-change-source-type))
    (define-key map [menu-bar apt-sources separator-changes]
      '("--"))
    (define-key map [menu-bar apt-sources insert-local-vars]
      '("Insert local variables" . apt-sources-insert-local-vars))
    (define-key map [menu-bar apt-sources around-lines]
      '("Toogle empty lines between source" . apt-sources-around-lines))
    (define-key map [menu-bar apt-sources deb-or-src-replicate]
      '("Copy source changing type" . apt-sources-deb-or-src-replicate))
    (define-key map [menu-bar apt-sources new-source]
      '("Add new source" . apt-sources-new-source))))



;;;###autoload
(defun apt-sources-mode ()
  "Major mode for editing apt's sources.list file.
Sets up command `font-lock-mode'.

\\{apt-sources-mode-map}"
  (interactive)
  ;;
  (kill-all-local-variables)
  (setq mode-name "apt-sources")
  (setq major-mode 'apt-sources-mode)
  (use-local-map apt-sources-mode-map)
  ;;
  (set (make-local-variable 'comment-start) "#")
  (set (make-local-variable 'comment-start-skip) "#+ *")
  (set-syntax-table apt-sources-mode-syntax-table)
  ;;
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(apt-sources-font-lock-keywords))
  ;;
  (run-hooks 'apt-sources-mode-hook))



(defun apt-sources-new-source (name)
  "Insert interactively a new source line into the current buffer.
This will insert a new source in the current line.

NAME is the name you want to this source line; it will be a comment.
     If no NAME is entered, only the line will be inserted.

You should read sources.list documentation for further explanation
of the format."
  (interactive "*sName of new source: ")

  (let ((source-type (completing-read "Source type: " '(("deb" 1) ("deb-src" 2)) nil t))
        (uri-type (completing-read "URI type: " ;type used to retrieve the URI, like http, ftp, etc.
                                   '(("cdrom:/" 1)
                                     ("copy://" 2)
                                     ("file://" 3)
                                     ("ftp://" 4)
                                     ("http://" 5)
                                     ("https://" 6)
                                     ("rsh://" 7)
                                     ("ssh://" 8)) nil t))
        (uri-address (read-from-minibuffer "URI address: ")) ;URI that will be used to specify the base
                                        ;of the Debian distribution, from which APT
                                        ;will find the information it needs.
        (distribution (completing-read "Distribution: " ;specific arquitecture or an exact path.
                                       '(("unstable" 1)
                                         ("testing" 2)
                                         ("frozen" 3)
                                         ("stable" 4)
                                         ("sid" 5)
                                         ("sarge" 6)
                                         ("woody" 7)) nil t))
        (components (completing-read "Components: " ;list of componenst used by APT.
                                     '(("main")
                                       ("main contrib")
                                       ("main contrib non-free")
                                       ("contrib")
                                       ("contrib non-free")
                                       ("non-free"))))
        (blank-line (if apt-sources-around-lines "\n" "")))

    (save-excursion
      (beginning-of-line)
      (insert blank-line)               ;insert line if `apt-sources-around-lines'
      (and (< 0 (length name))
           (insert (format apt-sources-source-name name)))
      (insert                           ;insert rest of arguments except components
       (format "%s %s%s %s" source-type uri-type uri-address distribution))

                                        ;if `distribution' ends in '/', then don't process components.
      (if (string-match "/$" distribution)
          distribution
        (insert " " components)
        (insert blank-line)))))         ;insert line if `apt-sources-around-lines'


(defun apt-sources-around-lines ()
  "Toggle insertion of newlines around a future creation of source lines.
This modifies the state of variable `apt-sources-around-lines'."
  (interactive)
  (setq apt-sources-around-lines (not apt-sources-around-lines))
  (message "Surrounding blank lines  %s"
           (if apt-sources-around-lines "On" "Off")))


(defun apt-sources-insert-local-vars ()
  "Insert the current values of buffer local variables."
  (interactive)
  (end-of-buffer)
  (and (not (bolp))
       (insert "\n"))                   ;insert a newline if the file doesn't end in a blank line.
  (insert "\n"
          comment-start " Local " "Variables:\n"
          comment-start " mode: " (format "%s\n" (or mode-name "apt-sources"))
          comment-start " End:\n"))


(defun apt-sources-next-source-line (arg)
  "Go to the next source line.

ARG is the prefix argument."
  (interactive "p")
  (let ((source-line-search (if (> arg 0)
                                (progn
                                  (end-of-line)
                                  're-search-forward)
                              're-search-backward)))
    (and (apply source-line-search
                '("^\\(deb \\|deb-src \\)" nil))
         (beginning-of-line))))


(defun apt-sources-previous-source-line ()
  "Go to the previous source line."
  (interactive)
  (apt-sources-next-source-line -1))


;;Modifying functions
(defun apt-sources-source-line-p ()
  "Return t if we are in an apt source line."
  (save-excursion
    (if (progn (beginning-of-line)
               (re-search-forward "^deb[^ ]*" (line-end-position) t 1))
        t                               ;return t if we are in an apt source line
      (message "Not in a source line!")
      nil)))                            ;return nil if we aren't in an apt source line


(defun apt-sources-change-source-type ()
  "Change the type of the source line.
TYPE is either 'd' or 's' to change the type to 'deb' or 'deb-src'.

This function will rise an error if we are not in a source line."
  (interactive)
  (and (apt-sources-source-line-p)
                                        ;type used to retrieve the URI, like http, ftp, etc.
       (let ((new-type (completing-read "'deb' or 'deb-src': " '(("deb" 1) ("deb-src" 2)) nil t)))
         (save-excursion
           (beginning-of-line)
           (delete-region (point) (re-search-forward "^deb[^ ]*" (line-end-position) nil 1))
           (insert new-type)))))


(defun apt-sources-change-uri-type ()
  "Change the URI type of the source line.

This function will rise an error if we are not in a source line."
  (interactive)

  (and (apt-sources-source-line-p)
       (let ((uri-type (completing-read "URI type: " ;type used to retrieve the URI, like http, ftp, etc.
                                        '(("cdrom:/" 1)
                                          ("copy://" 2)
                                          ("file://" 3)
                                          ("ftp://" 4)
                                          ("http://" 5)
                                          ("https://" 6)
                                          ("rsh://" 7)
                                          ("ssh://" 8)) nil t)))
         (save-excursion
           (beginning-of-line)
           (delete-region (re-search-forward "^deb[^ ]*." (line-end-position) nil 1)
                          (re-search-forward ":/*" (line-end-position) nil 1))
           (insert uri-type)))))


(defun apt-sources-change-uri-address (uri-address)
  "Change the URI address of the source line.
String URI-ADDRESS is the address (without the type of address,
ex: 'http://').

This function will rise an error if we are not in a source line."
  (interactive "sURI address: ")

  (and (apt-sources-source-line-p)
       (save-excursion
         (beginning-of-line)
         (delete-region (re-search-forward ":/*" (line-end-position) nil 1)
                        (re-search-forward "[^ ]*" (line-end-position) nil 1))
         (insert uri-address))))


(defun apt-sources-change-distribution ()
  "Change the distribution of the source line.

This function will rise an error if we are not in a source line."
  (interactive)

  (and (apt-sources-source-line-p)
       (let ((distribution (completing-read "Distribution: " ;specific arquitecture or an exact path.
                                            '(("unstable" 1)
                                              ("testing" 2)
                                              ("frozen" 3)
                                              ("stable" 4)
                                              ("sid" 5)
                                              ("sarge" 6)
                                              ("woody" 7)) nil t)))
         (save-excursion
           (beginning-of-line)
           (delete-region (re-search-forward ":/*[^ ]*." (line-end-position) t 1)
                          (re-search-forward "[^ ]*" (line-end-position) t 1))
           (insert distribution)))))


(defun apt-sources-change-components ()
  "Change the components of the source line.

This function will rise an error if we are not in a source line."
  (interactive)

  (and (apt-sources-source-line-p)
       (let ((components (completing-read "Components: " ;list of componenst used by APT.
                                          '(("main")
                                            ("main contrib")
                                            ("main contrib non-free")
                                            ("contrib")
                                            ("contrib non-free")
                                            ("non-free")))))
         (save-excursion
           (beginning-of-line)
           (delete-region (re-search-forward ":/*[^ ]* [^ ]*." (line-end-position) t 1)
                          (line-end-position))
           (and (save-excursion
                  (backward-char)
                  (looking-at "[^ ]"))
                (insert " "))
           (insert components)))))



(defun apt-sources-deb-or-src-replicate ()
  "Copy the source line and change the 'deb' to 'deb-src' or viceversa.

This function will rise an error if we are not on a source line."
  (interactive)

  (and (apt-sources-source-line-p)
       (let ((copy (buffer-substring (line-beginning-position)
                                     (line-end-position))))
         (save-excursion
           (end-of-line)
           (insert (concat "\n" copy))
           (beginning-of-line)
           (if (re-search-forward "^deb " (line-end-position) t 1)
               (progn
                 (backward-char)
                 (insert "-src"))
             (delete-region (line-beginning-position)
                            (re-search-forward "^deb[^ ]*" (line-end-position) t 1))
             (insert "deb"))))))



(run-hooks 'apt-sources-load-hook)
(add-to-list 'auto-mode-alist '("sources.list$" . apt-sources-mode))
(provide 'apt-sources)

;;; apt-sources.el ends here
