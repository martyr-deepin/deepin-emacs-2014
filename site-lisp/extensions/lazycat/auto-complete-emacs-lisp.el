(require 'auto-complete)

(defvar ac-emacs-lisp-sources
  '(ac-source-symbols))

(defvar ac-emacs-lisp-features nil)
(defvar ac-source-emacs-lisp-features
  '((init
     . (lambda ()
         (unless ac-emacs-lisp-features
           (let ((suffix (concat (regexp-opt (find-library-suffixes) t) "\\'")))
             (setq
              ac-emacs-lisp-features
              (delq nil
                    (apply 'append
                           (mapcar (lambda (dir)
                                     (if (file-directory-p dir)
                                         (mapcar (lambda (file)
                                                   (if (string-match suffix file)
                                                       (substring file 0 (match-beginning 0))))
                                                 (directory-files dir))))
                                   load-path))))))))
    (candidates . (lambda () (all-completions ac-prefix ac-emacs-lisp-features)))))

(defun ac-emacs-lisp-setup ()
  (setq ac-sources (append ac-emacs-lisp-sources ac-sources))
  (setq ac-omni-completion-sources '(("require\s+'" ac-source-emacs-lisp-features))))

(defun ac-emacs-lisp-init ()
  (add-hook 'emacs-lisp-mode-hook 'ac-emacs-lisp-setup))

(provide 'auto-complete-emacs-lisp)
