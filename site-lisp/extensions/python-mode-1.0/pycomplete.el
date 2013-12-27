;;; Complete symbols at point using Pymacs.
;;; See pycomplete.py for the Python side of things and a short description
;;; of what to expect.

(require 'pymacs)
(require 'python-mode)

(pymacs-load "pycomplete")

(defconst pycomplete-version "$Revision: 100 $"
  "`pycomplete' version number.")

(defconst py-identifier
  "[A-Za-z_][A-Za-z_0-9]*"
  "Regular expression matching a python identifier.")


;;; regular expressions regarding import statetment
;;; based on Python Grammar

(defconst py-dotted-name-re
  (concat py-identifier "\\([.]" py-identifier "\\)*")
  "Regular expression matching a dotted_name production.")

(defconst py-dotted-as-name-re
  (concat py-dotted-name-re "\\(\\s +as\\s +" py-identifier "\\)*")
  "Regular expression matching a dotted_as_name production.")

(defconst py-dotted-as-names-re
  (concat py-dotted-as-name-re
          "\\(\\s *,\\s *"  py-dotted-as-name-re "\\)*")
  "Regular expression matching a dotted_as_names production.")

(defconst py-import-as-name-re
  (concat py-identifier "\\(\\s +as\\s +" py-identifier "\\)*" )
  "Regular expression matching a import_as_name production.")

(defconst py-import-as-names-re
  (concat py-import-as-name-re "\\(\\s *,\\s *" py-import-as-name-re "\\)*"
          "\\s *[,]?" )
  "Regular expression matching a import_as_names production.")

(defconst py-import-name-re
  (concat "^\\s *\\<import\\>\\s +" py-dotted-as-names-re)
  "Regular expression matching a import_name production.")

(defconst py-import-from-re
  (concat "^\\s *\\<from\\>\\s +" "\\([.]*" py-dotted-name-re "\\|[.]+\\)\\s +"
          "\\<import\\>\\s +" "\\([*]\\|(\\s *" py-import-as-names-re "[^)]*)"
          "\\|" py-import-as-names-re "\\)")
  "Regular expression matching a import_from production.")

(defconst py-imports-re
  (concat "\\("
          (mapconcat 'identity
                     (list py-import-name-re
                           py-import-from-re)
                     "\\|")
          "\\)")
  "Regular expression matching imports.")


;; for compatibility with python-mode 4.78
(unless (fboundp 'py-backslash-continuation-line-p)
  (defalias 'py-backslash-continuation-line-p 'py-backslash-continuation-preceding-line-p))


(defun blank-linep ()
  "check if current line is empty (only whitespaces and comments)"
  (save-excursion
    (beginning-of-line)
    (looking-at py-blank-or-comment-re)))


(defun char-before-blank ()
  "check if prev character is blank-type"
  (save-excursion
    (forward-char -1)
    (looking-at "[\n\t\r]")))


(defun pycomplete-version ()
  "Echo the current version of `pycomplete' in the minibuffer."
  (interactive)
  (message "Using `pycomplete' version %s" pycomplete-version)
  (py-keep-region-active))


(defun py-complete ()
  "show possible completions for current statement"
  (interactive)
  (let ((pymacs-forget-mutability t))
    (if (and (eolp) (not (bolp))
             (not (char-before-blank))
             (not (blank-linep)))
        (insert (pycomplete-pycomplete
                 (py-symbol-near-point)
                 (buffer-file-name)
                 (py-find-global-imports)))
      (indent-for-tab-command))))


(defun py-find-global-imports ()
  "find global import statements"
  (save-excursion
    (let ((imports nil))
      (goto-char (point-min))
      (while (< (point) (point-max))
        (if (looking-at py-imports-re)
            ;; import statement found
            (progn
              (setq imports
                    (append imports (list (buffer-substring
                                           (match-beginning 0)
                                           (match-end 0)))))
              (forward-line 1)
              ;; handle continuation backslashes
              (while (and (py-backslash-continuation-line-p) (not (eobp)))
                (goto-char (line-beginning-position))
                (skip-chars-forward " \t")
                (setq begin (point))
                (goto-char (line-end-position))
                (skip-chars-backward " \t\\")
                (if (= (char-before) ?\\)
                    (setq end (- (point) 1))
                  (setq end (point)))
                (setcar (last imports)
                        (concat (car (last imports)) " "
                                (buffer-substring begin end)))
                (forward-line 1)))
          (forward-line)))
      imports)))


(defun py-complete-python-dotexpr-begin nil
  (re-search-backward "[^a-zA-Z_0-9\\.]")
  (forward-char))

(defun py-complete-python-dotexpr-end nil
  (re-search-forward "[a-zA-Z_0-9\\.]*"))

(put 'python-dotexpr 'beginning-op 'py-complete-python-dotexpr-begin)
(put 'python-dotexpr 'end-op 'py-complete-python-dotexpr-end)


(defun py-complete-show (string)
  (display-message-or-buffer string "*PythonHelp*"))


(defun py-complete-help (string)
  "get help on a python expression"
  (interactive "sHelp: ")
  (let ((help-string
         (pycomplete-pyhelp string (buffer-file-name) (py-find-global-imports))))
    (if (and help-string (> (length help-string) 300))
        (with-output-to-temp-buffer "*Python Help*"
          (print help-string))
      (py-complete-show help-string))))


(defun py-complete-help-thing-at-point nil
  (interactive)
  (require 'thingatpt)
  (let ((sym (thing-at-point 'python-dotexpr)))
    (if sym
        (py-complete-help sym))))


(set 'py-complete-current-signature nil)

(defun py-complete-signature (function)
  "get signature of a python function or method"
  (set 'py-complete-current-signature
       (pycomplete-pysignature function (buffer-file-name))))


(defun py-complete-signature-show nil
  (require 'thingatpt)
  (let ((sym (thing-at-point 'python-dotexpr)))
    (if sym
        (progn
          (py-complete-show (py-complete-signature sym))))))


(defun py-complete-signature-expr nil
  (interactive)
  (require 'thingatpt)
  (let ((dotexpr (read-string "signature on: "
                              (thing-at-point 'python-dotexpr))))
    (if dotexpr
        (py-complete-show
         (py-complete-signature dotexpr)))))


(defun py-complete-electric-lparen nil
  "electricly insert '(', and try to get a signature for the stuff to the left"
  (interactive)
  (py-complete-signature-show)
  (self-insert-command 1))


(defun py-complete-electric-comma nil
  "electricly insert ',', and redisplay latest signature"
  (interactive)
  (self-insert-command 1)
  (if py-complete-current-signature
      (py-complete-show (format "%s" py-complete-current-signature))))


(define-key py-mode-map "\M-\C-i" 'py-complete)
(define-key py-mode-map "\t" 'py-complete)
(define-key py-mode-map [f1] 'py-complete-help-thing-at-point)
(define-key py-mode-map "(" 'py-complete-electric-lparen)
(define-key py-mode-map "," 'py-complete-electric-comma)
(define-key py-mode-map [f2] 'py-complete-signature-expr)
(define-key py-mode-map [f3] 'py-complete-help)

(provide 'pycomplete)
