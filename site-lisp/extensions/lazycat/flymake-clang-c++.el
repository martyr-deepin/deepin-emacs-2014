;; From Duncan Mac-Vicar's emacs setup:
;; https://github.com/dmacvicar/duncan-emacs-setup

(defun flymake-clang-c++-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    (list "clang++" (list "-fsyntax-only" "-fno-color-diagnostics" local-file))
    ))

(defun flymake-clang-c++-cleanup ()
  (flymake-simple-cleanup)
  (flymake-safe-delete-file (concat (file-name-directory buffer-file-name) "a.out"))
  )

(defun flymake-clang-c++-load ()
  (interactive)
  (unless (eq buffer-file-name nil)
    (add-to-list 'flymake-allowed-file-name-masks
                 '("\\.cpp\\'" flymake-clang-c++-init flymake-clang-c++-cleanup))
    (add-to-list 'flymake-allowed-file-name-masks
                 '("\\.cc\\'" flymake-clang-c++-init flymake-clang-c++-cleanup))
    (add-to-list 'flymake-allowed-file-name-masks
                 '("\\.c\\'" flymake-clang-c++-init flymake-clang-c++-cleanup))
    (add-to-list 'flymake-allowed-file-name-masks
                 '("\\.h\\'" flymake-clang-c++-init flymake-clang-c++-cleanup))
    (flymake-mode t)))

(provide 'flymake-clang-c++)
