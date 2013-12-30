;;; one-key-config.el --- Configuration for one-key.el

;; Filename: one-key-config.el
;; Description: Configuration for one-key.el
;; Author: Andy Stewart lazycat.manatee@gmail.com
;; Maintainer: Andy Stewart lazycat.manatee@gmail.com
;; Copyright (C) 2008, 2009, Andy Stewart, all rights reserved.
;; Created: 2008-12-23 18:07:50
;; Version: 0.1
;; Last-Updated: 2008-12-23 18:07:50
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/one-key-config.el
;; Keywords: one-key
;; Compatibility: GNU Emacs 23.0.60.1
;;
;; Features that might be required by this library:
;;
;; `one-key'
;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; This package is some example to use `one-key'.
;;

;;; Installation:
;;
;; Put one-key-config.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'one-key-config)
;;
;; No need more.

;;; Change log:
;;
;; 2008/12/23
;;      * First released.
;;

;;; Acknowledgements:
;;
;;
;;

;;; TODO
;;
;;
;;

;;; Require
(require 'one-key)

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Variable ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar my-home-directory "")
(defvar my-mldonkey-download-directory "")
(defvar my-default-download-directory "")
(defvar my-resource-backup-directory "")
(defvar my-book-directory "")
(defvar my-reading-directory "")
(defvar my-translate-png-directory "")
(defvar my-picture-directory "")
(defvar my-lyrics-directory "")
(defvar my-emacs-lisp-package-directory "")
(defvar my-notes-directory "")
(defvar my-screenshots-storage-directory "")
(defvar my-emlue-download-directory "")
(defvar my-elisp-directory "")
(defvar my-config-directory "")
(defvar my-windows-share-directory "")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Root ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar one-key-menu-root-alist nil
  "The `one-key' menu alist for root.")

(setq one-key-menu-root-alist
      '(
        (("g" . "Gtags") . one-key-menu-gtags)
        (("c" . "Cscope") . one-key-menu-cscope)
        (("s" . "Show Hide") . one-key-menu-hideshow)))

(defun one-key-menu-root ()
  "The `one-key' menu for root."
  (interactive)
  (one-key-menu "ROOT" one-key-menu-root-alist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; EMMS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar one-key-menu-emms-alist nil
  "The `one-key' menu alist for EMMS.")

(setq one-key-menu-emms-alist
      '(
        (("g" . "Playlist Go") . emms-playlist-mode-go)
        (("d" . "Play Directory Tree") . emms-play-directory-tree)
        (("f" . "Play File") . emms-play-file)
        (("i" . "Play Playlist") . emms-play-playlist)
        (("m" . "Play Matching") . emms-play-matching)
        (("t" . "Add Directory Tree") . emms-add-directory-tree)
        (("c" . "Toggle Repeat Track") . emms-toggle-repeat-track)
        (("v" . "Jump To File") . emms-jump-to-file)
        (("w" . "Toggle Repeat Playlist") . emms-toggle-repeat-playlist)
        (("u" . "Play Now") . emms-play-now)
        (("z" . "Show") . emms-show)
        (("l" . "Lyrics Toggle Show") . emms-lyrics-toggle-display-on-minibuffer)
        (("r" . "Lyrics Re download") . emms-lyrics-redownload-lyric)
        (("e" . "Lyrics Visit") . emms-lyrics-visit-lyric)
        (("s" . "Emms Streams") . emms-streams)
        (("b" . "Emms Browser") . emms-browser)
        (("p" . "Anything Playlist") . anything-emms-playlist)
        (("o" . "Anything Directory") . anything-emms-directory)
        ((";" . "Anything File") . anything-emms-file)
        ))

(defun one-key-menu-emms ()
  "The `one-key' menu for EMMS."
  (interactive)
  (one-key-menu "EMMS" one-key-menu-emms-alist t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; EMMS Playlist Sort ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar one-key-menu-emms-playlist-sort-alist nil
  "The `one-key' menu alist for EMMS-PLAYLIST-SORT.")

(setq one-key-menu-emms-playlist-sort-alist
      '(
        (("h" . "Shuffle") . emms-shuffle)
        (("n" . "Name") . emms-playlist-sort-by-name)
        (("t" . "Title") . emms-playlist-sort-by-info-title)
        (("a" . "Artist") . emms-playlist-sort-by-info-artist)
        (("b" . "Album") . emms-playlist-sort-by-info-album)
        (("y" . "Year") . emms-playlist-sort-by-info-year)
        (("e" . "Note") . emms-playlist-sort-by-info-note)
        (("s" . "Scroe") . emms-playlist-sort-by-score)
        (("i" . "List") . emms-playlist-sort-by-list)
        (("o" . "Natural Order") . emms-playlist-sort-by-natural-order)
        (("l" . "Last Played") . emms-playlist-sort-by-last-played)
        (("c" . "Play Count") . emms-playlist-sort-by-play-count)
        ))

(defun one-key-menu-emms-playlist-sort ()
  "The `one-key' menu for EMMS-PLAYLIST-SORT."
  (interactive)
  (one-key-menu "EMMS-PLAYLIST-SORT" one-key-menu-emms-playlist-sort-alist t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; EMMS Playlist Mark ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar one-key-menu-emms-playlist-mark-alist nil
  "The `one-key' menu alist for EMMS-PLAYLIST-MARK.")

(setq one-key-menu-emms-playlist-mark-alist
      '(
        (("m" . "Mark Current and Move Next") . emms-mark-track-and-move-next)
        (("a" . "Mark All") . emms-mark-all)
        (("r" . "Mark Regexp") . emms-mark-regexp)
        (("c" . "Mark Copy") . emms-mark-copy-marked-tracks)
        (("x" . "Mark Delete") . emms-mark-delete-marked-tracks)
        (("d" . "Mark Duplicate") . emms-mark-duplicate-track)
        (("t" . "Mark Toggle") . emms-mark-toggle)
        (("u" . "Umark Current") . emms-mark-unmark-track-and-move-next)
        (("U" . "Umark All") . emms-mark-unmark-all)
        ))

(defun one-key-menu-emms-playlist-mark ()
  "The `one-key' menu for EMMS-PLAYLIST-MARK."
  (interactive)
  (one-key-menu "EMMS-PLAYLIST-MARK" one-key-menu-emms-playlist-mark-alist t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; EMMS Browser Search ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar one-key-menu-emms-browser-search-alist nil
  "The `one-key' menu alist for EMMS-BROWSER-SEARCH.")

(setq one-key-menu-emms-browser-search-alist
      '(
        (("a" . "Search Artist") . emms-browser-search-by-artist)
        (("b" . "Search Album") . emms-browser-search-by-album)
        (("c" . "Search Composer") . emms-browser-search-by-composer)
        (("n" . "Search Name") . emms-browser-search-by-names)
        (("p" . "Search Performer") . emms-browser-search-by-performer)
        (("t" . "Search Title") . emms-browser-search-by-title)
        ))

(defun one-key-menu-emms-browser-search ()
  "The `one-key' menu for EMMS-BROWSER-SEARCH."
  (interactive)
  (one-key-menu "EMMS-BROWSER-SEARCH" one-key-menu-emms-browser-search-alist t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; EMMS Browser Lookup ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar one-key-menu-emms-browser-lookup-alist nil
  "The `one-key' menu alist for EMMS-BROWSER-LOOKUP.")

(setq one-key-menu-emms-browser-lookup-alist
      '(
        (("b" . "Lookup Album Pitchfork") . EMMS-browser-lookup-album-on-pitchfork)
        (("p" . "Lookup Performer Pitchfork") . emms-browser-lookup-performer-on-pitchfork)
        (("c" . "Lookup Composer Pitchfork") . emms-browser-lookup-composer-on-pitchfork)
        (("a" . "Lookup Artist Pitchfork") . emms-browser-lookup-artist-on-pitchfork)
        (("B" . "Lookup Album Wikipedia") . emms-browser-lookup-album-on-wikipedia)
        (("P" . "Lookup Performer Wikipedia") . emms-browser-lookup-performer-on-wikipedia)
        (("C" . "Lookup Composer Wikipedia") . emms-browser-lookup-composer-on-wikipedia)
        (("A" . "Lookup Artist Wikipeda") . emms-browser-lookup-artist-on-wikipedia)
        ))

(defun one-key-menu-emms-browser-lookup ()
  "The `one-key' menu for EMMS-BROWSER-LOOKUP."
  (interactive)
  (one-key-menu "EMMS-BROWSER-LOOKUP" one-key-menu-emms-browser-lookup-alist t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ECB ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar one-key-menu-ecb-alist nil
  "The `one-key' menu alist for ECB.")

(setq one-key-menu-ecb-alist
      '(
        (("s" . "Sources") . ecb-goto-window-sources)
        (("h" . "History") . ecb-goto-window-history)
        (("d" . "Directory") . ecb-goto-window-directories)
        (("m" . "Methods") . ecb-goto-window-methods)
        (("e" . "Edit") . ecb-goto-window-edit1)))

(defun one-key-menu-ecb ()
  "The `one-key' menu for ECB."
  (interactive)
  (one-key-menu "ECB" one-key-menu-ecb-alist t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Gtags ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar one-key-menu-gtags-alist nil
  "The `one-key' menu alist for GTAGS.")

(setq one-key-menu-gtags-alist
      '(
        (("," . "Find Tag Define") . xgtags-find-tag-from-here)
        (("." . "Find Tag Reference (No Prompt)") . xgtags-find-rtag-no-prompt)
        ((">" . "Find Tag Reference") . xgtags-find-rtag)
        (("t" . "Search Tag Define") . xgtags-find-tag)
        (("s" . "Find Symbol") . xgtags-find-symbol)
        (("p" . "Find Pattern") . xgtags-find-pattern)
        (("/" . "Pop Stack") . xgtags-pop-stack)
        (("b" . "Switch Current Window") . xgtags-switch-to-buffer)
        (("o" . "Switch Other Window") . xgtags-switch-to-buffer-other-window)
        (("x" . "Parse File") . xgtags-parse-file)
        (("f" . "Find File") . xgtags-find-file)
        (("g" . "Find With Grep") . xgtags-find-with-grep)
        (("i" . "Find With Idutils") . xgtags-find-with-idutils)
        (("m" . "Make Complete List") . xgtags-make-complete-alist)
        (("q" . "Query Replace Regexp") . xgtags-query-replace-regexp)
        (("v" . "Visit Root Directory") . xgtags-visit-rootdir)
        (("r" . "Return Window") . xgtags-select-tag-return-window)))

(defun one-key-menu-gtags ()
  "The `one-key' menu for GTAGS."
  (interactive)
  (one-key-menu "GTAGS" one-key-menu-gtags-alist t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Hideshow ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar one-key-menu-hideshow-alist nil
  "The `one-key' menu alist for HIDESHOW.")

(setq one-key-menu-hideshow-alist
      '(
        (("s" . "Show Block") . hs-show-block)
        (("h" . "Hide Block") . hs-hide-block)
        (("c" . "Toggle Hiding") . hs-toggle-hiding)
        (("j" . "Show All") . hs-show-all)
        (("k" . "Hide All") . hs-hide-all)))

(defun one-key-menu-hideshow ()
  "The `one-key' menu for HIDESHOW."
  (interactive)
  (one-key-menu "HIDESHOW" one-key-menu-hideshow-alist t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Festival ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar one-key-menu-festival-alist nil
  "The `one-key' menu alist for FESTIVAL.")

(setq one-key-menu-festival-alist
      '(
        (("s" . "Stop") . festival-stop)
        (("a" . "Say") . festival-say)
        (("f" . "Read File") . festival-read-file)
        (("b" . "Read Buffer") . festival-read-buffer)
        (("r" . "Read Region") . festival-read-region)
        (("w" . "Read Word") . festival-read-word)))

(defun one-key-menu-festival ()
  "The `one-key' menu for FESTIVAL."
  (interactive)
  (one-key-menu "FESTIVAL" one-key-menu-festival-alist t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; UI ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar one-key-menu-ui-alist nil
  "The `one-key' menu alist for UI.")

(setq one-key-menu-ui-alist
      '(
        (("t" . "Tool-Bar") . tool-bar-mode)
        (("m" . "Menu-Bar") . menu-bar-mode)
        (("c" . "Scroll-Bar") . scroll-bar-mode)))

(defun one-key-menu-ui ()
  "The `one-key' menu for UI."
  (interactive)
  (one-key-menu "UI" one-key-menu-ui-alist t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Backup File ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar one-key-menu-backup-file-alist nil
  "The `one-key' menu alist for BACKUP-FILE.")

(setq one-key-menu-backup-file-alist
      '(
        (("e" . "Emacs") . (lambda () (interactive) (shell-aliase "bake")))
        (("x" . "XMonad") . (lambda () (interactive) (shell-aliase "bakx")))
        (("q" . "Qemu") . (lambda () (interactive) (shell-aliase "bakq")))
        (("v" . "VirtualBox") . (lambda () (interactive) (shell-aliase "bakv")))
        (("s" . "Stardict") . (lambda () (interactive) (shell-aliase "baks")))
        (("c" . "Configure File") . (lambda () (interactive) (shell-aliase "bakc")))
        (("p" . "Projects") . (lambda () (interactive) (shell-aliase "bakp")))
        (("l" . "Package List") . (lambda () (interactive) (shell-aliase "bakl")))
        (("h" . "Hanatee") . (lambda () (interactive) (shell-aliase "bakh")))
        (("d" . "Deepin Emacs") . (lambda () (interactive) (shell-aliase "bakd")))
        (("a" . "All") . (lambda () (interactive) (shell-aliase "bak")))
        ))

(defun one-key-menu-backup-file ()
  "The `one-key' menu for BACKUP-FILE."
  (interactive)
  (one-key-menu "BACKUP-FILE" one-key-menu-backup-file-alist t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Boxquote ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar one-key-menu-boxquote-alist nil
  "The `one-key' menu alist for BOXQUOTE.")

(setq one-key-menu-boxquote-alist
      '(
        (("z" . "Boxquote") . boxquote-boxquote)
        (("b" . "Buffer") . boxquote-buffer)
        (("d" . "Defun") . boxquote-defun)
        (("f" . "Describe Function") . boxquote-describe-function)
        (("k" . "Describe Key") . boxquote-describe-key)
        (("v" . "Describe Variable") . boxquote-describe-variable)
        (("o" . "Fill Paragraph") . boxquote-fill-paragraph)
        (("i" . "Insert File") . boxquote-insert-file)
        (("x" . "Kill") . boxquote-kill)
        (("r" . "Kill Ring Save") . boxquote-kill-ring-save)
        (("n" . "Narrow To Boxquote") . boxquote-narrow-to-boxquote)
        (("q" . "Narrow To Boxquote Content") . boxquote-narrow-to-boxquote-content)
        (("p" . "Paragraph") . boxquote-paragraph)
        (("g" . "Region") . boxquote-region)
        (("s" . "Shell Command") . boxquote-shell-command)
        (("t" . "Text") . boxquote-text)
        (("L" . "Title") . boxquote-title)
        (("u" . "Unbox") . boxquote-unbox)
        (("y" . "Yank") . boxquote-yank)
        (("w" . "Where Is") . boxquote-where-is)
        (("h" . "Unbox Region") . boxquote-unbox-region)
        ))

(defun one-key-menu-boxquote ()
  "The `one-key' menu for BOXQUOTE."
  (interactive)
  (one-key-menu "BOXQUOTE" one-key-menu-boxquote-alist t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Cscope ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar one-key-menu-cscope-alist nil
  "The `one-key' menu alist for CSCOPE.")

(setq one-key-menu-cscope-alist
      '(
        (("s" . "This Symbol") . cscope-find-this-symbol)
        (("d" . "Definition Prompt") . cscope-find-global-definition)
        (("g" . "Definition No Prompt") . cscope-find-global-definition-no-prompting)
        (("f" . "This File") . cscope-find-this-file)
        (("i" . "Including This File") . cscope-find-files-including-file)
        (("c" . "Calling This Function") . cscope-find-functions-calling-this-function)
        (("e" . "This Function Called") . cscope-find-called-functions)
        (("p" . "Pattern") . cscope-find-egrep-pattern)
        (("t" . "This String") . cscope-find-this-text-string)))

(defun one-key-menu-cscope ()
  "The `one-key' menu for CSCOPE."
  (interactive)
  (one-key-menu "CSCOPE" one-key-menu-cscope-alist t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Window Navigation ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar one-key-menu-window-navigation-alist nil
  "The `one-key' menu alist for WINDOW-NAVIGATION.")

(setq one-key-menu-window-navigation-alist
      '(
        (("j" . "Downward") . windmove-down)
        (("k" . "Upward") . windmove-up)
        (("h" . "Leftward") . windmove-left)
        (("l" . "Rightward") . windmove-right)
        (("s" . "Move Down") . buf-move-down)
        (("d" . "Move Up") . buf-move-up)
        (("a" . "Move Left") . buf-move-left)
        (("f" . "Move Right") . buf-move-right)
        (("u" . "Enlarge Down") . (lambda () (interactive) (windresize-up-inwards '-1)))
        (("i" . "Enlarge Up") . (lambda () (interactive) (windresize-down-inwards '-1)))
        (("y" . "Enlarge Left") . (lambda () (interactive) (windresize-right-inwards '-1)))
        (("o" . "Enlarge Right") . (lambda () (interactive) (windresize-left-inwards '-1)))
        (("m" . "Shrink Down") . (lambda () (interactive) (windresize-up-inwards '1)))
        (("," . "Shrink Up") . (lambda () (interactive) (windresize-down-inwards '1)))
        (("n" . "Shrink Left") . (lambda () (interactive) (windresize-right-inwards '1)))
        (("." . "Shrink Right") . (lambda () (interactive) (windresize-left-inwards '1)))
        (("x" . "Outward Window") . outward-window)
        (("c" . "Inward Window") . inward-window)
        (("7" . "Tabbar Left") . tabbar-backward-tab)
        (("8" . "Tabbar Right") . tabbar-forward-tab)
        (("9" . "Tabbar Next") . tabbar-backward-group)
        (("0" . "Tabbar Previous") . tabbar-forward-group)
        ((";" . "Kill Buffer") . kill-this-buffer)
        ((":" . "Kill Other Windows") . delete-other-windows)
        (("'" . "Kill Buffer And Window") . delete-current-buffer-and-window)
        (("b" . "Anything Mode") . anything)
        (("e" . "List Registers") . list-registers)
        (("r" . "Remember Register") . frame-configuration-to-register)
        (("t" . "Jump Register") . jump-to-register)
        (("g" . "Split Horizontally") . split-window-horizontally)
        (("v" . "Split Vertically") . split-window-vertically)
        (("z" . "Window Number Jump") . window-number-jump)
        ))

(defun one-key-menu-window-navigation ()
  "The `one-key' menu for WINDOW-NAVIGATION."
  (interactive)
  (one-key-menu "WINDOW-NAVIGATION" one-key-menu-window-navigation-alist t t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Etags ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar one-key-menu-etags-alist nil
  "The `one-key' menu alist for ETAGS.")

(setq one-key-menu-etags-alist
      '(
        (("G" . "Generate Tag Table") . generate-tag-table-of-emacs)
        (("w" . "Find Tag Window") . find-tag-window)
        (("W" . "Find Tag Window Small") . release-small-tag-window)
        (("," . "Find Tag+") . find-tag+)
        (("." . "Find Tag") . find-tag)
        (("p" . "Pop Tag Mark") . pop-tag-mark)
        (("r" . "Find Tag Regexp") . find-tag-regexp)
        (("s" . "Tags Search") . tags-search)
        (("Q" . "Tags Query Replace") . tags-query-replace)
        ))

(defun one-key-menu-etags ()
  "The `one-key' menu for ETAGS."
  (interactive)
  (one-key-menu "ETAGS" one-key-menu-etags-alist t t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Gnus Summary Sort ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar one-key-menu-gnus-summary-sort-alist nil
  "The `one-key' menu alist for GNUS-SUMMARY-SORT.")

(setq one-key-menu-gnus-summary-sort-alist
      '(
        (("a" . "Author") . gnus-summary-sort-by-author)
        (("c" . "Chars") . gnus-summary-sort-by-chars)
        (("d" . "Date") . gnus-summary-sort-by-date)
        (("e" . "Score") . gnus-summary-sort-by-score)
        (("l" . "Lines") . gnus-summary-sort-by-lines)
        (("n" . "Number") . gnus-summary-sort-by-number)
        (("o" . "Original") . gnus-summary-sort-by-original)
        (("x" . "Random") . gnus-summary-sort-by-random)
        (("s" . "Subject") . gnus-summary-sort-by-subject)
        (("i" . "Recipient") . gnus-summary-sort-by-recipient)
        (("r" . "Reverse") . gnus-summary-sort-by-reverse)
        ))

(defun one-key-menu-gnus-summary-sort ()
  "The `one-key' menu for GNUS-SUMMARY-SORT."
  (interactive)
  (one-key-menu "GNUS-SUMMARY-SORT" one-key-menu-gnus-summary-sort-alist t t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Gnus Topic Edit ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar one-key-menu-gnus-topic-edit-alist nil
  "The `one-key' menu alist for GNUS-TOPIC-EDIT.")

(setq one-key-menu-gnus-topic-edit-alist
      '(
        (("c" . "Copy Group") . gnus-topic-copy-group)
        (("C" . "Copy Matching") . gnus-topic-copy-matching)
        (("m" . "Move Group") . gnus-topic-move-group)
        (("M" . "Move Matching") . gnus-topic-move-matching)
        (("t" . "Move") . gnus-topic-move)
        (("r" . "Rename") . gnus-topic-rename)
        (("e" . "Edit Parameters") . gnus-topic-edit-parameters)
        (("h" . "Show Topic") . gnus-topic-show-topic)
        (("H" . "Hide Topic") . gnus-topic-hide-topic)
        (("j" . "Jump Topic") . gnus-topic-jump-to-topic)
        (("f" . "Create Topic") . gnus-topic-create-topic)
        (("d" . "Delete") . gnus-topic-delete)
        (("n" . "Next Topic") . gnus-topic-goto-next-topic)
        (("p" . "Previous Topic") . gnus-topic-goto-previous-topic)
        (("S" . "Sort Topic") . gnus-topic-sort-topic)
        (("#" . "Mark Topic") . gnus-topic-mark-topic)
        (("*" . "Unmark Topic") . gnus-topic-unmark-topic)
        (("TAB" . "Indent") . gnus-topic-indent)
        (("S-TAB" . "Unindent") . gnus-topic-unindent)
        (("T" . "Toggle Empty Topic") . gnus-topic-toggle-display-empty-topics)
        ))

(defun one-key-menu-gnus-topic-edit ()
  "The `one-key' menu for GNUS-TOPIC-EDIT."
  (interactive)
  (one-key-menu "GNUS-TOPIC-EDIT" one-key-menu-gnus-topic-edit-alist t t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Gnus Topic Sort ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar one-key-menu-gnus-topic-sort-alist nil
  "The `one-key' menu alist for GNUS-TOPIC-SORT.")

(setq one-key-menu-gnus-topic-sort-alist
      '(
        (("a" . "Sort Alpha") . gnus-topic-sort-groups-by-alphabet)
        (("l" . "Sort Level") . gnus-topic-sort-groups-by-level)
        (("m" . "Sort Method") . gnus-topic-sort-groups-by-method)
        (("r" . "Sort Rank") . gnus-topic-sort-groups-by-rank)
        (("c" . "Sort Score") . gnus-topic-sort-groups-by-score)
        (("w" . "Sort Server") . gnus-topic-sort-groups-by-server)
        (("u" . "Sort Unread") . gnus-topic-sort-groups-by-unread)
        ))

(defun one-key-menu-gnus-topic-sort ()
  "The `one-key' menu for GNUS-TOPIC-SORT."
  (interactive)
  (one-key-menu "GNUS-TOPIC-SORT" one-key-menu-gnus-topic-sort-alist t t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Apt-utils View ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Apt-utils Search ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Apt-utils Browse ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Yaoddmuse ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar one-key-menu-yaoddmuse-alist nil
  "The `one-key' menu alist for YAODDMUSE.")

(setq one-key-menu-yaoddmuse-alist
      '(
        ;; Edit.
        (("e" . "Edit Default") . yaoddmuse-edit-default)
        (("E" . "Edit") . yaoddmuse-edit)
        (("o" . "Follow") . yaoddmuse-follow)
        ;; Post
        (("b" . "Post Current Buffer") . yaoddmuse-post-current-buffer)
        (("B" . "Post Buffer") . yaoddmuse-post-buffer)
        (("l" . "Post Library Default") . yaoddmuse-post-library-default)
        (("L" . "Post Library") . yaoddmuse-post-library)
        (("f" . "post File Default") . yaoddmuse-post-file-default)
        (("F" . "Post File") . yaoddmuse-post-file)
        (("y" . "Post Screenshot Default") . yaoddmuse-post-screenshot-default)
        (("Y" . "Post Screenshot") . yaoddmuse-post-screenshot)
        ;; View.
        (("v" . "Browse Page Default") . yaoddmuse-browse-page-default)
        (("V" . "Browse Page") . yaoddmuse-browse-page)
        (("s" . "Brose This Page") . yaoddmuse-browse-current-page)
        ;; Misc
        (("d" . "Delete Page") . yaoddmuse-delete)
        (("D" . "Redirect Page") . yaoddmuse-redirect)
        (("r" . "Revert") . yaoddmuse-revert)
        (("i" . "Insert Pagename") . yaoddmuse-insert-pagename)
        (("x" . "Insert File Content") . yaoddmuse-insert-file-content)
        (("K" . "Kill Url") . yaoddmuse-kill-url)
        (("j" . "Anything Edit or View") . anything-yaoddmuse-emacswiki-edit-or-view)
        (("k" . "Anything Post") . anything-yaoddmuse-emacswiki-post-library)
        ))

(defun one-key-menu-yaoddmuse ()
  "The `one-key' menu for YAODDMUSE."
  (interactive)
  (one-key-menu "YAODDMUSE" one-key-menu-yaoddmuse-alist t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Paredit ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Irc-Channel ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar one-key-menu-irc-channel-alist nil
  "The `one-key' menu alist for IRC-CHANNEL.")

(setq one-key-menu-irc-channel-alist
      '(
        (("e" . "#emacs") . (lambda () (interactive) (try-to-switch-buffer "#emacs")))
        (("h" . "#haskell") . (lambda () (interactive) (try-to-switch-buffer "#haskell")))
        (("x" . "#xmonad") . (lambda () (interactive) (try-to-switch-buffer "#xmonad")))
        (("l" . "#lisp") . (lambda () (interactive) (try-to-switch-buffer "#lisp")))
        (("d" . "#debian") . (lambda () (interactive) (try-to-switch-buffer "#debian")))
        (("z" . "#zsh") . (lambda () (interactive) (try-to-switch-buffer "#zsh")))
        (("m" . "#mijamedia") . (lambda () (interactive) (try-to-switch-buffer "##manatee")))
        (("y" . "#yi") . (lambda () (interactive) (try-to-switch-buffer "#yi")))
        (("a" . "#android-dev") . (lambda () (interactive) (try-to-switch-buffer "#android-dev")))
        (("u" . "#ubuntu") . (lambda () (interactive) (try-to-switch-buffer "#ubuntu")))
        (("s" . "##English") . (lambda () (interactive) (try-to-switch-buffer "##English")))
        (("p" . "#python") . (lambda () (interactive) (try-to-switch-buffer "#python")))
        (("n" . "#mandarin") . (lambda () (interactive) (try-to-switch-buffer "#mandarin")))))

(defun one-key-menu-irc-channel ()
  "The `one-key' menu for IRC-CHANNEL."
  (interactive)
  (one-key-menu "IRC-CHANNEL" one-key-menu-irc-channel-alist t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Help ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar one-key-menu-help-alist nil
  "The `one-key' menu alist for help.")

(setq one-key-menu-help-alist
      '(
        ;; Apropos.
        (("z" . "Apropos") . apropos)
        (("{" . "Apropos Library") . apropos-library)
        (("\"" . "Apropos Value") . apropos-value)
        (("C" . "Apropos Command") . apropos-command)
        (("F" . "Apropos Function") . apropos-function)
        (("V" . "Apropos Variable") . apropos-variable)
        (("O" . "Apropos Option") . apropos-option)
        (("a" . "Apropos Command") . apropos-command)
        (("d" . "Apropos Documentation") . apropos-documentation)
        ;; Describe.
        (("/" . "Describe Input Method") . describe-input-method)
        (("f" . "Describe Function") . describe-function)
        (("g" . "Describe Gnu Project") . describe-gnu-project)
        (("h" . "Describe Hash") . describe-hash)
        (("b" . "Describe Bindings") . describe-bindings)
        (("c" . "Describe Command") . describe-command)
        (("m" . "Describe Mode") . describe-mode)
        (("k" . "Describe Key") . describe-key)
        (("K" . "Describe Key Briefly") . describe-key-briefly)
        (("o" . "Describe Option") . describe-option)
        (("p" . "Describe Char") . describe-char)
        (("s" . "Describe Syntax") . describe-syntax)
        (("u" . "Describe Unbound Keys") . describe-unbound-keys)
        (("v" . "Describe Variable") . describe-variable)
        (("L" . "Describe Language Environment") . describe-language-environment)
        (("w" . "Describe No Warranty") . describe-no-warranty)
        (("M-f" . "Describe Face") . describe-face)
        (("M-c" . "Describe Copying") . describe-copying)
        (("M-f" . "Describe File") . describe-file)
        (("M-k" . "Describe Keymap") . describe-keymap)
        (("M-t" . "Describe Option Of Type") . describe-option-of-type)
        ;; Info.
        (("M-i" . "Info") . info)
        (("M-o" . "Info Other Window") . info-other-window)
        (("M-s" . "Info Lookup Symbol") . info-lookup-symbol)
        (("M-k" . "Info Goto Emacs Key Command Node") . Info-goto-emacs-key-command-node)
        (("M-m" . "Info Emacs Manual") . info-emacs-manual)
        ;; Xray.
        (("M-B" . "Xray Help Buffer") . xray-help-buffer)
        (("M-S" . "Xray Help Symbol") . xray-help-symbol)
        (("M-W" . "Xray Help Window") . xray-help-window)
        (("M-H" . "Xray Help Hooks") . xray-help-hooks)
        (("M-M" . "Xray Help Marker") . xray-help-marker)
        (("M-O" . "Xray Help Overlay") . xray-help-overlay)
        (("M-P" . "Xray Help Position") . xray-help-position)
        (("M-E" . "Xray Help Screen") . xray-help-screen)
        (("M-Z" . "Xray Help Frame") . xray-help-frame)
        (("M-X" . "Xray Help Features") . xray-help-features)
        (("M-C" . "Xray Help Faces") . xray-help-faces)
        ;; View.
        (("C-d" . "View Emacs Debugging") . view-emacs-debugging)
        (("C-e" . "View External Packages") . view-external-packages)
        (("C-f" . "View Emacs FAQ") . view-emacs-FAQ)
        (("C-n" . "View Emacs News") . view-emacs-news)
        (("C-p" . "View Emacs Problems") . view-emacs-problems)
        (("C-t" . "View Emacs Todo") . view-emacs-todo)
        (("C-r" . "View Order Manuals") . view-order-manuals)
        (("C-E" . "View Echo Area Messages") . view-echo-area-messages)
        (("C-l" . "View Lossage") . view-lossage)
        (("C-n" . "View Emacs News") . view-emacs-news)
        ;; Misc.
        (("C-F" . "Eyedropper Background") . eyedropper-background)
        (("C-B" . "Eyedropper Foreground") . eyedropper-foreground)
        (("C-P" . "Finder By Keyword") . finder-by-keyword)
        (("C-u" . "Display Local Help") . display-local-help)
        (("C-a" . "About Emacs") . about-emacs)
        (("C-h" . "Help For Help") . help-for-help)
        (("C-H" . "Help With Tutorial") . help-with-tutorial)
        (("C-s" . "Wtf Is") . wtf-is)
        (("C-z" . "Sys Apropos") . sys-apropos)
        (("C-w" . "Where Is") . where-is)
        (("x" . "Find Function On Key") . find-function-on-key)
        ))

(defun one-key-menu-help ()
  "The `one-key' menu for help."
  (interactive)
  (one-key-menu "help" one-key-menu-help-alist t nil nil nil t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; VC ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar one-key-menu-VC-alist nil
  "The `one-key' menu alist for VC.")

(setq one-key-menu-VC-alist
      '(
        (("+" . "Vc Update") . vc-update)
        (("=" . "Vc Diff") . vc-diff)
        (("a" . "Vc Update Change Log") . vc-update-change-log)
        (("b" . "Vc Switch Backend") . vc-switch-backend)
        (("c" . "Vc Rollback") . vc-rollback)
        (("d" . "Vc Dir") . vc-dir)
        (("g" . "Vc Annotate") . vc-annotate)
        (("h" . "Vc Insert Headers") . vc-insert-headers)
        (("i" . "Vc Register") . vc-register)
        (("l" . "Vc Print Log") . vc-print-log)
        (("m" . "Vc Merge") . vc-merge)
        (("r" . "Vc Retrieve Tag") . vc-retrieve-tag)
        (("s" . "Vc Create Tag") . vc-create-tag)
        (("u" . "Vc Revert") . vc-revert)
        (("v" . "Vc Next Action") . vc-next-action)
        (("~" . "Vc Revision Other Window") . vc-revision-other-window)
        ))

(defun one-key-menu-VC ()
  "The `one-key' menu for VC."
  (interactive)
  (one-key-menu "VC" one-key-menu-VC-alist t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Anything  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar one-key-menu-anything-alist nil
  "The `one-key' menu alist for anything.")

(setq one-key-menu-anything-alist
      '(
        (("i" . "Auto Install") . anything-auto-install)
        (("e" . "Edit or View EmacsWiki page") . anything-yaoddmuse-emacswiki-edit-or-view)
        (("p" . "Post library to EmacsWiki") . anything-yaoddmuse-emacswiki-post-library)
        ))

(defun one-key-menu-anything ()
  "The `one-key' menu for anything."
  (interactive)
  (one-key-menu "anything" one-key-menu-anything-alist t))

(defvar one-key-menu-android-alist nil
  "The `one-key' menu alist for android.")

(setq one-key-menu-android-alist
      '(
        (("d" . "DDMS") . android-start-ddms)
        (("e" . "Emulator") . android-start-emulator)
        (("l" . "Logcat") . android-logcat)
        (("C" . "Clean") . android-ant-clean)
        (("c" . "Compile") . android-ant-compile)
        (("i" . "Install") . android-ant-install)
        (("r" . "Reinstall") . android-ant-reinstall)
        (("u" . "Uninstall") . android-ant-uninstall)
        (("a" . "Create") . android-create)))

(defun one-key-menu-android ()
  "The `one-key' menu for android."
  (interactive)
  (one-key-menu "Android" one-key-menu-android-alist t))

(provide 'one-key-config)

;;; one-key-config.el ends here

;;; LocalWords:  config EMMS emms Playlist redownload ECB ecb Gtags gtags rtag
;;; LocalWords:  xgtags idutils rootdir Hideshow hideshow hs UI ui Mldonkey bak
;;; LocalWords:  bakx bakq bakv baks bakc bakp Boxquote boxquote Unbox unbox VC
;;; LocalWords:  Cscope cscope egrep utime Tabbar logon Etags etags buf Scroe
;;; LocalWords:  windresize irfc SPC todo metaleft metaright metadown metaup Vc
;;; LocalWords:  shiftmetaleft shiftmetaright shiftmetadown shiftmetaup Swtich
;;; LocalWords:  shiftleft shiftright misssion subtree Umark unmark Lookup Xray
;;; LocalWords:  lookup Wikipeda Unindent unindent debian Readme readme dctrl
;;; LocalWords:  Speical utils ReadingNotes Paren Sexps sexps xray Lossage Wtf
;;; LocalWords:  lossage wtf Sys sys xmonad zsh mijamedia vc Backend backend
