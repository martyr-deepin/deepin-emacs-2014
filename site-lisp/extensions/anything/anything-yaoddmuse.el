;;; anything-yaoddmuse.el --- Integrate yaoddmuse.el with anything.el

;; Filename: anything-yaoddmuse.el
;; Description: Integrate yaoddmuse.el with anything.el
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2009, Andy Stewart, all rights reserved.
;; Created: 2009-02-09 18:28:28
;; Version: 0.1.6
;; Last-Updated: 2009-04-04 02:58:10
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/anything-yaoddmuse.el
;; Keywords: anything, yaoddmuse
;; Compatibility: GNU Emacs 22 ~ 23
;;
;; Features that might be required by this library:
;;
;; `anything' `yaoddmuse'
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
;; Integrate yaoddmuse.el with anything.el.
;;
;; You can use command `anything-yaoddmuse-emacswiki-edit-or-view'
;; to Edit or view EmacsWiki page.
;;
;; You can use command `anything-yaoddmuse-emacswiki-post-library'
;; to post library to EmacsWiki.
;;
;; You can also make this package integrate with `anything',
;; just setup like below:
;;
;; (setq anything-sources
;;       (list
;;        anything-c-source-yaoddmuse-emacswiki-edit-or-view
;;        anything-c-source-yaoddmuse-emacswiki-post-library
;;        ))
;;

;;; Installation:
;;
;; Put anything-yaoddmuse.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'anything-yaoddmuse)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET anything-yaoddmuse RET
;;

;;; Change log:
;;
;; 2009/04/04
;;      * Add 'Copy URL' command in
;;        `anything-c-source-yaoddmuse-emacswiki-edit-or-view'.
;;
;; 2009/03/29
;;      * Add diff command.
;;
;; 2009/02/12
;;      * Add "Update page name" in
;;        `anything-c-source-yaoddmuse-emacswiki-edit-or-view'.
;;
;; 2009/02/11
;;      * Add "Create page" in
;;        `anything-c-source-yaoddmuse-emacswiki-edit-or-view'.
;;
;; 2009/02/11
;;      * Add "Brose page other window" in
;;        `anything-c-source-yaoddmuse-emacswiki-edit-or-view'.
;;
;; 2009/02/09
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
(require 'anything)
(require 'yaoddmuse)

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Variable ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar anything-c-source-yaoddmuse-emacswiki-edit-or-view
  '((name . "Yaoddmuse Edit or View (EmacsWiki)")
    (candidates . (lambda ()
                    (yaoddmuse-update-pagename t)
                    (gethash "EmacsWiki" yaoddmuse-pages-hash)))
    (action . (("Edit page" . (lambda (candidate)
                                (yaoddmuse-edit "EmacsWiki" candidate)))
               ("Browse page" . (lambda (candidate)
                                  (yaoddmuse-browse-page "EmacsWiki" candidate)))
               ("Browse page other window" . (lambda (candidate)
                                               (if (one-window-p)
                                                   (split-window-vertically))
                                               (yaoddmuse-browse-page "EmacsWiki" candidate)))
               ("Browse diff" . (lambda (candidate)
                                  (yaoddmuse-browse-page-diff "EmacsWiki" candidate)))
               ("Copy URL" . (lambda (candidate)
                               (kill-new (yaoddmuse-url "EmacsWiki" candidate))
                               (message "Have copy page %s's URL to yank." candidate)))
               ("Create page" . (lambda (candidate)
                                  (yaoddmuse-edit "EmacsWiki" anything-input)))
               ("Update page name" . (lambda (candidate)
                                       (yaoddmuse-update-pagename)))))))

(defvar anything-c-source-yaoddmuse-emacswiki-post-library
  '((name . "Yaoddmuse Post library (EmacsWiki)")
    (init . (anything-yaoddmuse-init))
    (candidates-in-buffer)
    (action . (("Post library" . (lambda (candidate)
                                   (yaoddmuse-post-file (find-library-name candidate)
                                                        "EmacsWiki"
                                                        (file-name-nondirectory (find-library-name candidate)))))
               ("Post library and Browse" . (lambda (candidate)
                                              (yaoddmuse-post-file (find-library-name candidate)
                                                                   "EmacsWiki"
                                                                   (file-name-nondirectory (find-library-name candidate))
                                                                   nil t)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Interactive Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun anything-yaoddmuse-emacswiki-edit-or-view ()
  "Edit or View EmacsWiki page."
  (interactive)
  (anything 'anything-c-source-yaoddmuse-emacswiki-edit-or-view))

(defun anything-yaoddmuse-emacswiki-post-library ()
  "Post library to EmacsWiki."
  (interactive)
  (anything 'anything-c-source-yaoddmuse-emacswiki-post-library))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Utilities Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun anything-yaoddmuse-init ()
  "Init anything buffer status."
  (let ((anything-buffer (anything-candidate-buffer 'global))
        (library-list (yaoddmuse-get-library-list)))
    (with-current-buffer anything-buffer
      ;; Insert library name.
      (dolist (library library-list)
        (insert (format "%s\n" library)))
      ;; Sort lines.
      (sort-lines nil (point-min) (point-max)))))

(provide 'anything-yaoddmuse)

;;; anything-yaoddmuse.el ends here

;;; LocalWords:  dirs
