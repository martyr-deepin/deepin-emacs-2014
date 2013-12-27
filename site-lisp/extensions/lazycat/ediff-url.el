;;; ediff-url.el --- Diffing buffer against downloaded url
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: Sat Nov 24 23:30:01 2007
;; Version: 0.55
;; Last-Updated: 2009-02-24 Tue
;; URL: http://www.emacswiki.org/cgi-bin/wiki/ediff-url.el
;; Keywords:
;; Compatibility:
;;
;; Features that might be required by this library:
;;
  ;; `mail-prsvr', `mm-util', `timer', `url-parse', `url-util',
  ;; `url-vars'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This file contains a simple function, `ediff-url', to help you
;; update a single file from the web.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'url-util)

(defvar ediff-url-read-url-history nil)

(defun ediff-url-redir-emacswiki-description-page (url)
  "Check if description page on EmacsWiki.
If URL is a description page for a file uploaded to EmacsWiki
suggest to use the download URL instead."
  ;;(let* ((desc-url "http://www.emacswiki.org/cgi-bin/wiki/")
  (let* ((desc-url "http://www.emacswiki.org/emacs/")
         (desc-len (length desc-url)))
    (if (and (< desc-len (length url))
             (string= desc-url (substring url 0 desc-len)))
        (let ((prompt
               (concat "This seem to be the description page on EmacsWiki,"
                       "\n\tdo you want the download url instead? ")))
          (when (y-or-n-p prompt)
            ;;(let ((start (+ 6 (string-match "/wiki/" url))))
            (let ((start (+ 7 (string-match "/emacs/" url))))
              (concat (substring url 0 start)
                                "download/"
                                (substring url start)))))
      ;; Not on the wiki, just return the url:
      url)))

(defcustom ediff-url-redirects '(ediff-url-redir-emacswiki-description-page)
  "List of functions checking url given to `ediff-url'.
Each function should take an URL as argument and return this URL
or a new URL."
  :type '(repeat function)
  :group 'ediff)

(defun ediff-url (url)
  "Compare current buffer to a web URL using `ediff-buffers'.
Check URL using `ediff-url-redirects' before fetching the file."
  (interactive (let ((url-init (url-get-url-at-point)))
                 (unless url-init
                   (when (eq major-mode 'emacs-lisp-mode)
                     (save-excursion
                       (goto-char (point-min))
                       (when (re-search-forward "URL:[ \t]*" nil t)
                         (setq url-init (url-get-url-at-point))))))
                 (list (read-from-minibuffer "Url: "
                                             (cons (or url-init "") 1) ;nil
                                             nil nil
                                             'ediff-url-read-url-history
                                             ;;url-init
                                             ))))
  ;; Check if URL seems reasonable
  (dolist (fun ediff-url-redirects)
    (setq url (funcall fun url)))
  ;; Fetch URL and run ediff
  (let* ((url-buf-name (concat "URL=" url))
         (url-buf (get-buffer url-buf-name)))
    (when url-buf
      (unless (y-or-n-p "Use previously downloaded url? ")
        (kill-buffer url-buf)
        (setq url-buf nil)))
    (unless url-buf
      (setq url-buf (get-buffer-create url-buf-name))
      (let ((current-major major-mode))
        (with-current-buffer url-buf
          (url-insert-file-contents url)
          ;; Assume same modes:
          (funcall current-major))))
    (ediff-buffers url-buf (current-buffer))))

(provide 'ediff-url)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ediff-url.el ends here
