(require 'auto-complete)
(require 'rcodetools)

(defvar ac-ruby-sources
  '(ac-source-rcodetools))

(defvar ac-source-rcodetools
  '((init . (lambda ()
              (condition-case x
                  (save-excursion
                    (rct-exec-and-eval rct-complete-command-name "--completion-emacs-icicles"))
                (error) (setq rct-method-completion-table nil))))
    (candidates . (lambda ()
                    (all-completions
                     ac-prefix
                     (mapcar
                      (lambda (completion)
                        (replace-regexp-in-string "\t.*$" "" (car completion)))
                      rct-method-completion-table))))))

(defun ac-ruby-setup ()
  ;(setq ac-sources (append ac-sources ac-ruby-sources))
  (setq ac-omni-completion-sources (list (cons "\\." ac-ruby-sources)
                                         (cons "::" ac-ruby-sources))))

(defun ac-ruby-init ()
  (add-hook 'ruby-mode-hook 'ac-ruby-setup))
  
(provide 'auto-complete-ruby)
