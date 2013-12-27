;;; python-pylint.el --- minor mode for running `pylint'

;; Copyright (c) 2009, 2010 Ian Eure <ian.eure@gmail.com>

;; Author: Ian Eure <ian.eure@gmail.com>

;; Keywords: languages python
;; Last edit: 2010-02-12
;; Version: 1.01

;; python-pylint.el is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.
;;
;; It is distributed in the hope that it will be useful, but WITHOUT ANY
;; WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.
;;
;; You should have received a copy of the GNU General Public License along
;; with your copy of Emacs; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;;; Commentary:
;;
;; (autoload 'python-pylint "python-pylint")
;; (autoload 'pylint "python-pylint")
;;
;;; Code:

(defgroup python-pylint nil
  "Minor mode for running pylint"
  :prefix "python-pylint-"
  :group 'tools)

(defvar python-pylint-last-buffer nil
  "The most recent PYLINT buffer.
A PYLINT buffer becomes most recent when you select PYLINT mode in it.
Notice that using \\[next-error] or \\[compile-goto-error] modifies
`complation-last-buffer' rather than `python-pylint-last-buffer'.")

(defconst python-pylint-regexp-alist
  (let ((base "^\\(.*\\):\\([0-9]+\\):\s+\\(\\[%s.*\\)$"))
    (list
     (list (format base "[FE]") 1 2)
     (list (format base "[RWC]") 1 2 nil 1)))
  "Regexp used to match PYLINT hits.  See `compilation-error-regexp-alist'.")

(defcustom python-pylint-options '("-rn" "-f parseable")
  "Options to pass to pylint.py"
  :type '(repeat string)
  :group 'python-pylint)

(defcustom python-pylint-command "pylint"
  "PYLINT command."
  :type '(file)
  :group 'python-pylint)

(defcustom python-pylint-ask-about-save nil
  "Non-nil means \\[python-pylint] asks which buffers to save before compiling.
Otherwise, it saves all modified buffers without asking."
  :type 'boolean
  :group 'python-pylint)

(define-compilation-mode python-pylint-mode "PYLINT"
  (setq python-pylint-last-buffer (current-buffer))
  (set (make-local-variable 'compilation-error-regexp-alist)
       python-pylint-regexp-alist)
  (set (make-local-variable 'compilation-disable-input) t))

(defvar python-pylint-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map compilation-minor-mode-map)
    (define-key map " " 'scroll-up)
    (define-key map "\^?" 'scroll-down)
    (define-key map "\C-c\C-f" 'next-error-follow-minor-mode)

    (define-key map "\r" 'compile-goto-error)  ;; ?
    (define-key map "n" 'next-error-no-select)
    (define-key map "p" 'previous-error-no-select)
    (define-key map "{" 'compilation-previous-file)
    (define-key map "}" 'compilation-next-file)
    (define-key map "\t" 'compilation-next-error)
    (define-key map [backtab] 'compilation-previous-error)
    map)
  "Keymap for PYLINT buffers.
`compilation-minor-mode-map' is a cdr of this.")

;;;###autoload
(defun python-pylint ()
  "Run PYLINT, and collect output in a buffer.
While pylint runs asynchronously, you can use \\[next-error] (M-x next-error),
or \\<python-pylint-mode-map>\\[compile-goto-error] in the grep \
output buffer, to go to the lines where pylint found matches."
  (interactive)

  (save-some-buffers (not python-pylint-ask-about-save) nil)
  (let* ((tramp (tramp-tramp-file-p (buffer-file-name)))
         (file (or (and tramp
                        (aref (tramp-dissect-file-name (buffer-file-name)) 3))
                   (buffer-file-name)))
         (command (mapconcat
                   'identity
                   (list python-pylint-command
                         (mapconcat 'identity python-pylint-options " ")
                         (comint-quote-filename file)) " ")))

    (compilation-start command 'python-pylint-mode)))

;;;###autoload
(defalias 'pylint 'python-pylint)

(provide 'python-pylint)
