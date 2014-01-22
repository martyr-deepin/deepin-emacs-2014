;; From Duncan Mac-Vicar's emacs setup:
;; https://github.com/dmacvicar/duncan-emacs-setup

(defun flymake-clang-c++-init ()
  (flymake-clang-init "clang++")
  )

(defun flymake-clang-c-init ()
  (flymake-clang-init "clang")
  )

(defun flymake-clang-init (clang-bin)
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    (list clang-bin (list "-fsyntax-only" "-fno-color-diagnostics" local-file))
    ))

(defun flymake-clang-cleanup ()
  (flymake-simple-cleanup)
  (flymake-safe-delete-file (concat (file-name-directory buffer-file-name) "a.out"))
  )

(defun flymake-clang-load ()
  (interactive)
  (unless (eq buffer-file-name nil)
    (add-to-list 'flymake-allowed-file-name-masks
                 '("\\.cpp\\'" flymake-clang-c++-init flymake-clang-cleanup))
    (add-to-list 'flymake-allowed-file-name-masks
                 '("\\.cc\\'" flymake-clang-c++-init flymake-clang-cleanup))
    (add-to-list 'flymake-allowed-file-name-masks
                 '("\\.c\\'" flymake-clang-c-init flymake-clang-cleanup))
    (add-to-list 'flymake-allowed-file-name-masks
                 '("\\.h\\'" flymake-clang-c++-init flymake-clang-cleanup))
    (flymake-mode t)))

(provide 'flymake-clang)
