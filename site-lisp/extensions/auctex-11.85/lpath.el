;;; This file is only used for installing AUCTeX.
;;; It is not a part of AUCTeX itself.

;; Make sure we get the right files.
(setq load-path (cons "." load-path)
      byte-compile-warnings nil
      TeX-lisp-directory "<none>"
      TeX-auto-global "<none>")
