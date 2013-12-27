;; -*- Mode: Emacs-Lisp -*-

;;  $Id: color-grep.el,v 2.9 2007/11/04 13:37:16 akihisa Exp $

;; Author: Matsushita Akihisa <akihisa@mail.ne.jp>
;; Keywords: grep highlight

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; color-grep highlight grep buffer, file buffer and show the
;; matching line of file buffer.

;;; Install:

;; Put this file into load-path'ed directory, and byte compile it if
;; desired.  And put the following expression into your ~/.emacs.
;;
;;     (require 'color-grep)

;; The latest version of this program can be downloaded from
;; http://www.bookshelf.jp/elc/color-grep.el

;; Usage:
;; M-x grep and C-c C-c. Then this program highlights the file buffer.
;; M-r : Run grep in the matching file.

;;; History:

;; 2005/11/16
;; Bug fix.

;; color-occur 1.0 was released to the net on 12/01/2002

;;; Code:

(defvar color-grep-sync-mode t)
(defvar color-grep-maximum-displayed-color 500)

(defvar color-grep-sync-kill-buffer nil)

;; Internal variables
(defvar color-grep-mark-search-word "")

;;grep sync
(defvar color-grep-buffer-list nil)
(make-variable-buffer-local 'color-grep-buffer-list)

(defadvice compilation-start
  (before kill-buffer-by-color-grep activate)
  (let* ((cmode
          (or (ad-get-arg 1)
              'compilation-mode))
         (name-of-mode
          (if (eq cmode t)
              (prog1 "compilation" (require 'comint))
            (replace-regexp-in-string "-mode$" "" (symbol-name cmode))))
         (cname-function (ad-get-arg 2))
         (bufname
          (compilation-buffer-name name-of-mode cname-function nil)))
    (when (buffer-live-p bufname)
      (with-current-buffer bufname
        (when (and color-grep-sync-mode
                   color-grep-sync-kill-buffer)
          (color-grep-sync-kill-buffers)
          )))))

(defun grep-narrow-down ()
  (interactive)
  (save-excursion
    (let ((dir default-directory)
          (regexp (read-from-minibuffer "grep : "))
          (str "") files com)
      (forward-line 1)
      (while (re-search-forward "^\\([^\n()]+\\):[0-9]+:" nil t)
        (if (string= str (buffer-substring (match-beginning 1)
                                           (match-end 1)))
            ()
          (setq str (buffer-substring (match-beginning 1) (match-end 1)))
          (setq files (cons str files))))
      (setq com (concat
                 grep-command "\"" regexp "\" "))
      (while files
        (let ((currfile (car files)))
          (setq files (cdr files))
          (setq com (concat
                     com " "
                     currfile))))
      (let ((default-directory dir))
        (with-temp-buffer
          (grep com)
          )))))


;; sync-mode
(add-hook 'grep-setup-hook
          (lambda ()
            (define-key grep-mode-map "\M-r" 'grep-narrow-down)
            (define-key grep-mode-map '[up] 'color-grep-prev)
            (define-key grep-mode-map '[down] 'color-grep-next)
            (define-key grep-mode-map "\M-p" 'color-grep-prev)
            (define-key grep-mode-map "\M-n" 'color-grep-next)
            (define-key grep-mode-map "\C-p" 'color-grep-prev)
            (define-key grep-mode-map "\C-n" 'color-grep-next)
            (define-key grep-mode-map "\C-c\C-s" 'color-grep-sync-toggle)
            ))

(defun color-grep-sync-toggle ()
  (interactive)
  (setq color-grep-sync-mode
        (not color-grep-sync-mode)))

(defun color-grep-sync-kill-buffers ()
  (interactive)
  (let (buf)
    (if color-grep-buffer-list
        (progn
          (while color-grep-buffer-list
            (setq buf (car color-grep-buffer-list))
            (setq color-grep-buffer-list
                  (cdr color-grep-buffer-list))
            (if (and (buffer-live-p buf)
                     (not (buffer-modified-p buf)))
                (kill-buffer buf)))
          (delete-other-windows)))))

(add-hook 'kill-buffer-hook
          '(lambda ()
             (if (string= major-mode 'grep-mode)
                 (color-grep-sync-kill-buffers))))

(defun color-grep-view-file ()
  (let (ov buf (buflst (buffer-list)))
    (condition-case nil
        (let ((compilation-context-lines nil))
          (setq compilation-current-error (point))
          (next-error-no-select 0))
      (error t))

    (other-window 1)
    (setq buf (current-buffer))
    (other-window 1)

    ;; grep buffer
    (if (or
         (memq buf buflst)
         (memq buf color-grep-buffer-list))
        ()
      (setq color-grep-buffer-list
            (cons buf color-grep-buffer-list)))))

(defun color-grep-next (arg)
  (interactive "p")
  (forward-visible-line arg)
  (when (and color-grep-sync-mode
             (save-excursion
               (beginning-of-line)
               (re-search-forward (car (car grep-regexp-alist))
                                  (line-end-position) t)))
    (color-grep-view-file)))

(defun color-grep-prev (arg)
  (interactive "p")
  (color-grep-next (- arg)))

(provide 'color-grep)

