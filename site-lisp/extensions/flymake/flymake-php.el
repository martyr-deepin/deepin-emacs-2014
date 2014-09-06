;;; flymake-php.el --- A flymake handler for php-mode files
;;
;;; Author: Steve Purcell <steve@sanityinc.com>
;;; URL: https://github.com/purcell/flymake-php
;;; Version: DEV
;;; Package-Requires: ((flymake-easy "0.1"))
;;;
;;; Commentary:
;; Usage:
;;   (require 'flymake-php)
;;   (add-hook 'php-mode-hook 'flymake-php-load)
;;
;; Uses flymake-easy, from https://github.com/purcell/flymake-easy

;;; Code:
(require 'flymake-easy)

(defconst flymake-php-err-line-patterns
  '(("\\(?:Parse\\|Fatal\\|syntax\\) error[:,] \\(.*\\) in \\(.*\\) on line \\([0-9]+\\)" 2 3 nil 1)))

(defvar flymake-php-executable "php"
  "The php executable to use for syntax checking.")

(defun flymake-php-command (filename)
  "Construct a command that flymake can use to check php source."
  (list flymake-php-executable "-l" "-f" filename))

;;;###autoload
(defun flymake-php-load ()
  "Configure flymake mode to check the current buffer's php syntax."
  (interactive)
  (flymake-easy-load 'flymake-php-command
                     flymake-php-err-line-patterns
                     'tempdir
                     "php"))


(provide 'flymake-php)
;;; flymake-php.el ends here
