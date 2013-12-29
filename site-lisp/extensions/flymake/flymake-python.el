(require 'flymake-cursor)
(require 'flymake-patch)

(defun flymake-create-copy-file ()
  "Create a copy local file"
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace)))
    (file-relative-name
     temp-file
     (file-name-directory buffer-file-name))))

(defun flymake-command-parse (cmdline)
  "Parses the command line CMDLINE in a format compatible
       with flymake, as:(list cmd-name arg-list)

The CMDLINE should be something like:

 flymake %f python custom.py %f

%f will be substituted with a temporary copy of the file that is
 currently being checked.
"
  (let ((cmdline-subst (replace-regexp-in-string "%f" (flymake-create-copy-file) cmdline)))
    (setq cmdline-subst (split-string-and-unquote cmdline-subst))
    (list (first cmdline-subst) (rest cmdline-subst))
    ))

(setq flymake-info-line-regex (append flymake-info-line-regex '("unused$" "^redefinition" "used$")))

(add-to-list 'flymake-allowed-file-name-masks
             (list "\\.py\\'" (apply-partially 'flymake-command-parse "pyflakes %f")))
(add-hook 'python-mode-hook 'flymake-mode)

(provide 'flymake-python)
