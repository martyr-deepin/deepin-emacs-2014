;;; qml-mode.el --- Major mode for editing Qt QML files

;; Copyright (C) 2010 William Xu

;; Author: William Xu <william.xwl@gmail.com>
;; Version: 0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin St, Fifth Floor, Boston,
;; MA 02110-1301, USA.

;;; Commentary:

;; This is a simple major mode for editing editing Qt QML files.

;; Put this file into your load-path and the following into your
;; ~/.emacs:
;;           (autoload 'qml-mode "qml-mode")
;;           (add-to-list 'auto-mode-alist '("\\.qml$" . qml-mode))

;;; Code:

(require 'css-mode)
(require 'js)
(require 'cc-mode)

(defvar qml-keywords
  (concat "\\<" (regexp-opt '("import")) "\\>\\|" js--keyword-re))

(defvar qml-font-lock-keywords
  `(("/\\*.*\\*/\\|//.*"                ; comment
     (0 font-lock-comment-face t t))
    ("\\<\\(true\\|false\\|[A-Z][a-zA-Z0-9]*\\.[A-Z][a-zA-Z0-9]*\\)\\>" ; constants
     (0 font-lock-constant-face))
    ("\\<\\([A-Z][a-zA-Z0-9]*\\)\\>"    ; Elements
     (1 font-lock-function-name-face nil t)
     (2 font-lock-function-name-face nil t))
    (,(concat qml-keywords "\\|\\<parent\\>") ; keywords
     (0 font-lock-keyword-face nil t))
    ("\\<\\([a-z][a-zA-Z.]*\\|property .+\\):\\|\\<\\(anchors\\|font\\|origin\\|axis\\)\\>" ; property
     (1 font-lock-variable-name-face nil t)
     (2 font-lock-variable-name-face nil t))
    ("\\<function +\\([a-z][a-zA-Z0-9]*\\)\\>" ; method
     (1 font-lock-function-name-face)))
  "Keywords to highlight in `qml-mode'.")

(defvar qml-mode-syntax-table
  (let ((table (make-syntax-table)))
    (c-populate-syntax-table table)
    table))

;;;###autoload
(define-derived-mode qml-mode css-mode "QML"
  "Major mode for editing Qt QML files.
\\{qml-mode-map}"
  :syntax-table qml-mode-syntax-table
  (setq font-lock-defaults '(qml-font-lock-keywords))
  (set (make-local-variable 'comment-start) "/* ")
  (set (make-local-variable 'comment-end) " */")
  (run-hooks 'qml-mode-hook))

(define-key qml-mode-map (kbd "C-M-q") 'qml-indent-exp)

(defun qml-indent-exp ()
  (interactive)
  (indent-region (point) (save-excursion (forward-list) (point))))

(provide 'qml-mode)

;;; qml-mode.el ends here
