(let* ((features-directory
        (file-name-directory
         (directory-file-name (file-name-directory load-file-name))))
       (project-directory
        (file-name-directory
         (directory-file-name features-directory))))
  (setq ruby-tools-root-path project-directory)
  (setq ruby-tools-util-path (expand-file-name "util" ruby-tools-root-path)))

(add-to-list 'load-path ruby-tools-root-path)
(add-to-list 'load-path (expand-file-name "espuds" ruby-tools-util-path))

(require 'ruby-tools)
(require 'espuds)

(Before
 (switch-to-buffer
  (get-buffer-create "*ruby-tools*"))
 (erase-buffer)
 (deactivate-mark))
