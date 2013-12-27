;;; hs-scan.el --- minor mode for checking Haskell source code using
;;; the scan tool.

;; Author: Liam O'Reilly <csliam@swansea.ac.uk>
;; Keywords: haskell, scan
;; Requirements: scan

;;; This has been heavily adapted from hs-lint.el by Alex Ott
;;; <alexott@gmail.com>

(require 'compile)

(defgroup hs-scan nil
  "Run scan as inferior of Emacs, parse error messages."
  :group 'tools
  :group 'haskell)

(defcustom hs-scan-command "scan"
  "The default hs-scan command for \\[hs-scan]."
  :type 'string
  :group 'hs-scan)

(defcustom hs-scan-save-files t
  "Save modified files when run hs-scan or no (ask user)"
  :type 'boolean
  :group 'hs-scan)

(defun hs-scan-process-setup ()
  "Setup compilation variables and buffer for `hs-scan'."
  (run-hooks 'hs-scan-setup-hook))

(defun hs-scan-finish-hook (buf msg)
  "Function, that is executed at the end of hs-scan execution"
  (next-error 1 t))

(define-compilation-mode hs-scan-mode "hs-scan"
  "Mode for checking Haskell source code with scan."
  (set (make-local-variable 'compilation-process-setup-function)
       'hs-scan-process-setup)
  (set (make-local-variable 'compilation-disable-input) t)
  (set (make-local-variable 'compilation-scroll-output) nil)
  (set (make-local-variable 'compilation-finish-functions)
       (list 'hs-scan-finish-hook))
  )

(defun hs-scan ()
  "Run scan for current buffer with haskell source"
  (interactive)
  (save-some-buffers hs-scan-save-files)
  (compilation-start (concat hs-scan-command " " buffer-file-name)
                     'hs-scan-mode))

(provide 'hs-scan)
;;; hs-scan.el ends here
