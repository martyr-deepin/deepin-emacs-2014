;;; go-flycheck.el --- goflymake flycheck checker

;; Author: Peter Vasil <mail@petervasil.net>

;;; Commentary:
;; Flycheck checker for the go programming language using goflymake tool
;;
;; Add the following lines to your .emacs:
;;
;;   (add-to-list 'load-path "$GOPATH/src/github.com/dougm/goflymake")
;;   (require 'go-flycheck)

;;; Code:

(eval-when-compile
  (require 'go-mode)
  (require 'flycheck))

(flycheck-define-checker go-goflymake
  "A Go syntax and style checker using the go utility.

See URL `https://github.com/dougm/goflymake'."
  :command ("goflymake" "-prefix=flycheck-"
            (eval (if goflymake-debug "-debug=true" "-debug=false"))
            source-inplace)
  :error-patterns ((error line-start (file-name) ":" line ": " (message) line-end))
  :modes go-mode)

(add-to-list 'flycheck-checkers 'go-goflymake)

(provide 'go-flycheck)

;;; go-flycheck.el ends here
