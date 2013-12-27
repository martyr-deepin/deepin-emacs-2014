;;; auto-fill-mode-inhibit -- finer grained control over
;;;     auto-fill-mode (de)activation
;;; Copyright (c) 2003  Michael Weber <michaelw@debian.org>
;;

;;; Version: 20030509
;;

;;; License:
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; version 2 as published by the Free Software Foundation.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
;; 02111-1307 USA
;;
;; NO-VIRUS CLAUSE:
;; The intent of this license is to protect free redistribution and
;; reuse of the source of the licensed distribution, not to prejudice
;; the authorship rights of programmers of other code to control
;; their original inventions.
;;
;; No portion of this license is to be interpreted as forbidding the
;; reuse of this code or its constituent parts, algorithms, or
;; inventions in commercial products.
;;
;; Nor shall such inclusion be construed to require the GPLing or
;; disclosure of any portions of said commercial products other than
;; those falling under the copyright of the licensed distribution.
;;

;;; Commentary:
;;
;; To activate auto-fill-mode, add the following line to your Emacs
;; initialization:
;;   (add-hook 'text-mode-hook 'turn-on-auto-fill)

;;; History:
;;
;; 20030509:
;;   * activate advice through `defadvice' instead of call to
;;     `ad-activate'
;;   * emit message if `auto-fill-mode' gets inhibited
;;   * add (provide) line
;;   * make `auto-fill-inhibit-list' a `defcustom'
;;   * fixed docstrings to make M-x checkdoc happy
;;     (thanks to psg@debian.org for hints)
;;
;; 20011114:
;;   * Initial Version

;;; Code:
(defcustom auto-fill-inhibit-list nil
  "regexep LIST to match against buffer-name to inhibit auto-fill-mode.
An empty list of regexps (the default) retains the original
`auto-fill-mode' behaviour."
  :type '(repeat (regexp :tag "Buffer name regexp")))

(defadvice auto-fill-mode (before auto-fill-mode-inhibit activate)
  "Turn off `auto-fill-mode' on matching buffers.
Buffers which have their names `string-match' on any one regexp in
`auto-fill-inhibit-list'.  Unless something is put into this variable,
it behaves transparently to default auto-fill functionality."

  (let ((bufname (buffer-name)))
    (if (catch 'match
          (mapcar (function (lambda (s)
                              (if (string-match s bufname)
                                  (throw 'match t))))
                  auto-fill-inhibit-list)
          nil)
	(progn
	  (message "auto-fill-mode inhibited for this buffer through auto-fill-inhibit-list")
	  ;;; turn off auto-fill-mode (setting arg0 to `0')
	  (ad-set-arg 0 0)))))


(provide 'auto-fill-inhibit)

;;; auto-fill-inhibit.el ends here
