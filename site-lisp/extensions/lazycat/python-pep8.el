;;; python-pep8.el --- minor mode for running `pep8'

;; Copyright (c) 2009, 2010 Ian Eure <ian.eure@gmail.com>

;; Author: Ian Eure <ian.eure@gmail.com>

;; Keywords: languages python
;; Last edit: 2010-02-12
;; Version: 1.01

;; python-pep8.el is free software; you can redistribute it and/or modify it
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
;; (autoload 'python-pep8 "python-pep8")
;; (autoload 'pep8 "python-pep8")
;;
;;; Code:

(defgroup python-pep8 nil
  "Minor mode for running pep8"
  :prefix "python-pep8-"
  :group 'tools)

(defvar python-pep8-last-buffer nil
  "The most recent PEP8 buffer.
A PEP8 buffer becomes most recent when you select PEP8 mode in it.
Notice that using \\[next-error] or \\[compile-goto-error] modifies
`complation-last-buffer' rather than `python-pep8-last-buffer'.")

(defconst python-pep8-regexp-alist
  (let ((base "^\\(.*\\):\\([0-9]+\\):\\([0-9]+\\):\s+\\(%s[0-9]+ .*\\)"))
    (list (list (format base "E") 1 2 3 2)
          (list (format base "W") 1 2 3 1)))
  "Regexp used to match PEP8 hits.  See `compilation-error-regexp-alist'.")

(defcustom python-pep8-options '("--repeat")
  "Options to pass to pep8.py"
  :type '(repeat string)
  :group 'python-pep8)

(defcustom python-pep8-command "pep8"
  "PEP8 command."
  :type '(file)
  :group 'python-pep8)

(defcustom python-pep8-ask-about-save nil
  "Non-nil means \\[python-pep8] asks which buffers to save before compiling.
Otherwise, it saves all modified buffers without asking."
  :type 'boolean
  :group 'python-pep8)

(define-compilation-mode python-pep8-mode "PEP8"
  (setq python-pep8-last-buffer (current-buffer))
  (set (make-local-variable 'compilation-error-regexp-alist)
       python-pep8-regexp-alist)
  (set (make-local-variable 'compilation-disable-input) t))

(defvar python-pep8-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map compilation-minor-mode-map)
    (define-key map " " 'scroll-up)
    (define-key map "\^?" 'scroll-down)
    (define-key map "\C-c\C-f" 'next-error-follow-minor-mode)

    (define-key map "\r" 'compile-goto-error) ;; ?
    (define-key map "n" 'next-error-no-select)
    (define-key map "p" 'previous-error-no-select)
    (define-key map "{" 'compilation-previous-file)
    (define-key map "}" 'compilation-next-file)
    (define-key map "\t" 'compilation-next-error)
    (define-key map [backtab] 'compilation-previous-error)
    map)
  "Keymap for PEP8 buffers.
`compilation-minor-mode-map' is a cdr of this.")

;;;###autoload
(defun python-pep8 ()
  "Run PEP8, and collect output in a buffer.
While pep8 runs asynchronously, you can use \\[next-error] (M-x next-error),
or \\<python-pep8-mode-map>\\[compile-goto-error] in the grep \
output buffer, to go to the lines where pep8 found matches."
  (interactive)

  (save-some-buffers (not python-pep8-ask-about-save) nil)
  (let* ((tramp (tramp-tramp-file-p (buffer-file-name)))
         (file (or (and tramp
                        (aref (tramp-dissect-file-name (buffer-file-name)) 3))
                   (buffer-file-name)))
         (command (mapconcat
                   'identity
                   (list python-pep8-command
                         (mapconcat 'identity python-pep8-options " ")
                         (comint-quote-filename file)
                         "|" "sort" "-n" "-t:" "-k2") " ")))

    (compilation-start command 'python-pep8-mode)))

;;;###autoload
(defalias 'pep8 'python-pep8)

(provide 'python-pep8)
