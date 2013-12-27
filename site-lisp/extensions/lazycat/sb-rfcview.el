;;; sb-rfcview.el --- provide hierarchical speedbar menu's for RFC files

;; Copyright (c) 2005 Milton Wulei

;; Author: Milton wulei , <miltonwulei@163.com>
;; Maintainer: Milton wulei , <miltonwulei@163.com>
;; Keywords: speedbar, rfcview , RFC

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

;;; Commentary:
;;
;; This small add-on to speedbar provides an convenient way to view RFC
;; documents which shows the natural hierarchy of the document based
;; on section name tags.
;;
;; This code is a simple modification of sb-texinfo.el which comes
;; with speedbar.

;; Installation procedure:
;;
;;   Install speedbar 0.12 or later.
;;   Add the following to your ~/.emacs file:
;;   (eval-after-load "speedbar" '(load-library "sb-rfcview"))
;; (custom-set-variables
;;  '(speedbar-supported-extension-expressions
;;    (append
;;     speedbar-supported-extension-expressions
;;     '("rfc[0-9]+\\.txt"))))
;;This software alse requrie rfcview.el which was written by Neil W. Van Dyke
;;You can download it in the following URL
;;http://www.neilvandyke.org/rfcview/
;;
;; Known Problems:
;;
;;; Change Log:
;;; Code:

(require 'speedbar)
(require 'sb-texinfo)                   ; for speedbar-format-texinfo-list
(require 'rfcview)                      ;;use rfcview-mode to coperation

;; Attach these new functions to handle rfcview-mode.
(add-to-list 'speedbar-dynamic-tags-function-list
             '(speedbar-fetch-dynamic-rfcview . speedbar-insert-rfcview-list))

;; This returns t if the major mode of the current buffer is not
;; 'rfcview-mode. If it is 'rfcview-mode, then this returns a
;; list where each element is (LEVEL NAME . MARKER). LEVEL is 0, 1, 2,
;; 3, 4, or 5 corresponding to section number
;; tags. respectively. NAME is the name of the section. MARKER is
;; emacs marker that points to the beginning of the section. The
;; elements in the list returned are in ascending order of the
;; MARKER. This function along with it's parter,
;; speedbar-insert-rfcview-list, are designed to be added to the
;; speedbar-dynamic-tags-function-list list.
;;
;; This function is based on `speedbar-fetch-dynamic-texinfo'.
(defun speedbar-fetch-dynamic-rfcview ( filename )
  (set-buffer (find-file-noselect filename))
  (if (not (eq major-mode 'rfcview-mode))
      t
    (condition-case nil
        (save-excursion

          ;; Set speedbar-tag-hierarchy-method to nil so that
          ;; speedbar-create-tag-hierarchy won't reorder the list.
          ;; Make it buffer local so that the global value is not touched.
          (make-local-variable 'speedbar-tag-hierarchy-method)
          (setq speedbar-tag-hierarchy-method nil)
          (set (make-local-variable
                'speedbar-generic-list-group-expand-button-type)
               'expandtag)
          (set (make-local-variable
                'speedbar-generic-list-tag-button-type)
               'statictag)

          (let ((case-fold-search t)
                pos-beg title level alist beg head-regexp section-number)
            (goto-char (point-min))
            (setq head-regexp
                  (concat "^\\(" (regexp-opt
                                  rfcview-stock-section-names)
                          "\\)" "\\|^\\([1-9][0-9]*\\)\\(\\.[1-9][0-9]*\\)*"))
            (while (re-search-forward head-regexp nil t)
              (setq beg (match-end 0))
              (goto-char (match-beginning 0))
              (setq pos-beg (point-marker))
              (setq section-number
                    (buffer-substring (match-beginning 0)
                                      (match-end 0)))
              (setq level (speedbar-get-rfcview-toc-level section-number))
              (setq title
                    (buffer-substring (match-beginning 0) (point-at-eol)))
              (setq alist (cons (cons level (cons title pos-beg)) alist))
              (goto-char beg)
              )
            (nreverse alist)))
      (error t))))

(defun speedbar-get-rfcview-toc-level (section-number)
  "count a section number like '7.1.3.6' ,return it's level 3"
  (let ((i 0) (toc-level 0))
    (while (< i (length section-number))
      (if (char-equal
           (aref section-number i) 46) ;; the int value of ASCII '.' is 46
          (setq toc-level (1+ toc-level)))
      (setq i (+ i 1)))
    toc-level
    )
  )

(fset 'speedbar-format-rfcview-list 'speedbar-format-texinfo-list)

(defun speedbar-insert-rfcview-list (indent lst)
  (speedbar-insert-generic-list indent (speedbar-format-rfcview-list lst 0)
                                'speedbar-tag-expand
                                'speedbar-tag-find))
(provide 'sb-rfcview)

;;; sb-rfcview.el ends here
