;;; preseed.el --- a major mode for editing debian-installer preseed files

;; Copyright (C) 2004 W. Borgert <debacle@debian.org>

;; This package is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This package is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;;; Code:
(require 'font-lock)

(defvar preseed-mode-abbrev-table nil
  "Abbreviation table used in d-i preseed buffers.")
(define-abbrev-table 'preseed-mode-abbrev-table ())

(defvar preseed-font-lock-keywords
  '(("^\\([a-z-]+\\)[ \t]+\\([^ ]+\\)[ \t]+\\([^ ]+\\)"
     (1 font-lock-keyword-face)
     (2 font-lock-function-name-face)
     (3 font-lock-type-face))
    ("\\(^\\s-*#.*\\)"
     (1 font-lock-comment-face)))
  "Keyword patterns for preseed-mode fontification.")

;;;###autoload
(defun preseed-mode ()
  "Major mode for editing debian-installer preseed files colourfully."
  (interactive)
  (kill-all-local-variables)
  (setq comment-start "#"
 	comment-multi-line nil
	comment-start-skip "#+[\t ]*")
  (setq major-mode 'preseed-mode
 	mode-name "Preseed"
 	local-abbrev-table preseed-mode-abbrev-table)
  (run-hooks 'preseed-mode-hook)
  (set (make-local-variable 'font-lock-defaults)
       '(preseed-font-lock-keywords nil nil ((?_ . "w")))))

(provide 'preseed)

;;; preseed.el ends here
