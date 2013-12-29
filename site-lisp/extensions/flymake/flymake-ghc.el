;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; flymake-ghc.el
;;;

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Mar 12, 2010

;;; Code:

(require 'flymake)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar ghc-hlint-options nil "*Hlint options")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst ghc-error-buffer-name "*GHC Errors*")

(defconst flymake-ghc-allowed-file-name-masks
  '("\\.l?hs$" flymake-ghc-init flymake-simple-cleanup flymake-get-real-file-name))

(defconst flymake-ghc-err-line-patterns
  '("^\\(.*\\.l?hs\\):\\([0-9]+\\):\\([0-9]+\\):[ ]*\\(.+\\)" 1 2 3 4))

(add-to-list 'flymake-allowed-file-name-masks
             flymake-ghc-allowed-file-name-masks)

(add-to-list 'flymake-err-line-patterns
             flymake-ghc-err-line-patterns)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun flymake-ghc-init ()
  (let ((after-save-hook nil))
    (save-buffer))
  (let ((file (file-name-nondirectory (buffer-file-name))))
    (list ghc-module-command (flymake-ghc-command file ghc-hlint-options))))

(defvar flymake-ghc-command nil) ;; nil: check, t: lint

(defun flymake-ghc-command (file opts)
  (if flymake-ghc-command
      (let ((hopts (ghc-mapconcat (lambda (x) (list "-h" x)) opts)))
        `(,@hopts "lint" ,file))
    (list "check" file)))

(defun flymake-ghc-toggle-command ()
  (interactive)
  (setq flymake-ghc-command (not flymake-ghc-command))
  (if flymake-ghc-command
      (message "Syntax check with hlint")
    (message "Syntax check with GHC")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun flymake-ghc-display-errors ()
  (interactive)
  (if (not (flymake-ghc-have-errs-p))
      (message "No errors or warnings")
    (let ((buf (get-buffer-create ghc-error-buffer-name))
          (title (flymake-ghc-err-title))
          (errs (flymake-ghc-err-list)))
      (with-current-buffer buf
        (erase-buffer)
        (flymake-ghc-insert-errors title errs))
      (display-buffer buf))))

(defun flymake-ghc-insert-errors (title errs)
  (save-excursion
    (insert title "\n\n")
    (mapc (lambda (x) (insert (ghc-replace-character x ghc-null ghc-newline) "\n")) errs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun flymake-ghc-insert-from-warning ()
  (interactive)
  (dolist (data (flymake-ghc-err-list))
    (save-excursion
      (cond
       ((string-match "Inferred type: \\([^:]+ :: \\)\\(forall [^.]+\\. \\)?\\([^\0]*\\)" data)
        (beginning-of-line)
        (insert (match-string 1 data)
                (replace-regexp-in-string "\\[Char\\]" "String" (match-string 3 data))
                "\n"))
       ((string-match "lacks an accompanying binding" data)
        (beginning-of-line)
        (when (looking-at "^\\([^ ]+\\) *::")
          (save-match-data
            (forward-line)
            (if (eobp) (insert "\n")))
          (insert (match-string 1) " = undefined\n")))
       ((string-match "Not in scope: `\\([^']+\\)'" data)
        (save-match-data
          (unless (re-search-forward "^$" nil t)
            (goto-char (point-max))
            (insert "\n")))
        (insert "\n" (match-string 1 data) " = undefined\n"))
       ((string-match "Found:\0[ ]*\\([^\0]+\\)\0Why not:\0[ ]*\\([^\0]+\\)" data)
        (let ((old (match-string 1 data))
              (new (match-string 2 data)))
          (beginning-of-line)
          (when (search-forward old nil t)
            (let ((end (point)))
              (search-backward old nil t)
              (delete-region (point) end))
            (insert new))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun flymake-ghc-err-get-title (x) (nth 0 x))
(defun flymake-ghc-err-get-errs (x) (nth 1 x))

(defalias 'flymake-ghc-have-errs-p 'flymake-ghc-data)

(defun flymake-ghc-data ()
  (let* ((line-no (flymake-current-line-no))
         (info (nth 0 (flymake-find-err-info flymake-err-info line-no))))
    (flymake-make-err-menu-data line-no info)))

(defun flymake-ghc-err-title ()
  (flymake-ghc-err-get-title (flymake-ghc-data)))

(defun flymake-ghc-err-list ()
  (mapcar 'car (flymake-ghc-err-get-errs (flymake-ghc-data))))

(provide 'flymake-ghc)
