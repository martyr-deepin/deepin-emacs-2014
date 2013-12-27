(require 'flymake)

(defun flymake-jslint-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    ;; Change this path to match where you have created the jslint-wrapper
    (list "/usr/share/deepin-emacs/Site-Lisp/Packages/LazyCatCollect/jslint-wrapper.sh" (list local-file))))

(setq flymake-err-line-patterns
      (cons '("^[[:space:]]+#[[:digit:]]+ \\(.+\\)I BLAME BOTH FLYMAKE AND JSLINT.+// Line \\([[:digit:]]+\\), Pos \\([[:digit:]]+\\)$"
              nil 2 3 1)
            flymake-err-line-patterns))

(add-to-list 'flymake-allowed-file-name-masks
             '("\\.js\\'" flymake-jslint-init))

(provide 'flymake-jslint)
