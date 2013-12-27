(provide 'dict-latex-cleveref)
(require 'dict-tree)
(defvar dict-latex-cleveref nil "Dictionary dict-latex-cleveref.")
(setq dict-latex-cleveref '(DICT "dict-latex-cleveref" nil t nil nil (TSTREE [nil [nil [nil [nil [nil [nil [[nil (0) nil nil] [nil [nil [nil [nil [nil (0) nil nil] nil 101] nil 103] nil 110] nil 97] nil 114] nil 102] nil 101] nil 114] [nil [nil [nil [nil [nil (0) [nil [nil [nil [nil [nil [nil (0) nil nil] nil 101] nil 103] nil 110] nil 97] nil 114] nil] nil 102] nil 101] nil 114] nil 99] 67] nil 92] nil t] (lambda (a b) (cond ((and (null a) (null b)) 0) ((null a) -1) ((null b) 1) (t (- a b)))) (lambda (new cell) (if (null cell) (dictree--wrap-data (funcall (lambda (weight data) (cond ((not (or weight data)) 0) ((null weight) (1+ data)) ((null data) weight) (t (+ weight data)))) new nil)) (dictree--set-data cell (funcall (lambda (weight data) (cond ((not (or weight data)) 0) ((null weight) (1+ data)) ((null data) weight) (t (+ weight data)))) new (dictree--get-data cell))) cell)) lambda (a b) (funcall (lambda (a b) (if (= (cdr a) (cdr b)) (if (= (length (car a)) (length (car b))) (string< (car a) (car b)) (< (length (car a)) (length (car b)))) (> (cdr a) (cdr b)))) (cons (car a) (dictree--get-data (cdr a))) (cons (car b) (dictree--get-data (cdr b))))) (lambda (new cell) (if (null cell) (dictree--wrap-data (funcall (lambda (weight data) (cond ((not (or weight data)) 0) ((null weight) (1+ data)) ((null data) weight) (t (+ weight data)))) new nil)) (dictree--set-data cell (funcall (lambda (weight data) (cond ((not (or weight data)) 0) ((null weight) (1+ data)) ((null data) weight) (t (+ weight data)))) new (dictree--get-data cell))) cell)) (lambda (a b) (funcall (lambda (a b) (if (= (cdr a) (cdr b)) (if (= (length (car a)) (length (car b))) (string< (car a) (car b)) (< (length (car a)) (length (car b)))) (> (cdr a) (cdr b)))) (cons (car a) (dictree--get-data (cdr a))) (cons (car b) (dictree--get-data (cdr b))))) nil nil nil nil nil 0.1))
(let ((ordered-hash (make-hash-table :test 'equal))
      (tstree (dictree--tstree dict-latex-cleveref)))
  (mapc
   (lambda (entry)
     (puthash
      (car entry)
      (dictree--cache-create
       (mapcar
        (lambda (key)
          (cons key (tstree-member tstree key)))
        (dictree--cache-completions (cdr entry)))
       (dictree--cache-maxnum (cdr entry)))
      ordered-hash))
   (dictree--ordered-hash dict-latex-cleveref))
  (dictree--set-ordered-hash dict-latex-cleveref ordered-hash))
(dictree--set-filename dict-latex-cleveref (locate-library "dict-latex-cleveref"))
(unless (memq dict-latex-cleveref dictree-loaded-list) (push dict-latex-cleveref dictree-loaded-list))
