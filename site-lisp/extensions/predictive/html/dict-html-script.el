(provide 'dict-html-script)
(require 'dict-tree)
(defvar dict-html-script nil "Dictionary dict-html-script.")
(setq dict-html-script '(DICT "dict-html-script" nil t nil nil (TSTREE [nil [[[nil [nil [nil [nil [nil [nil [nil [nil (0) nil nil] nil 116] nil 101] nil 115] nil 114] nil 97] nil 104] nil 99] [nil [nil [nil [nil [nil (0) nil nil] nil 114] nil 101] nil 102] nil 101] nil 100] [nil [nil [nil [nil [nil [nil [nil [nil (0) nil nil] nil 101] nil 103] nil 97] nil 117] nil 103] nil 110] nil 97] [nil [nil [nil [nil (0) nil nil] nil 99] nil 114] [nil [nil [nil [nil [nil (0) nil nil] nil 101] nil 112] nil 121] nil 116] 115] 108] nil t] (lambda (a b) (cond ((and (null a) (null b)) 0) ((null a) -1) ((null b) 1) (t (- a b)))) (lambda (new cell) (if (null cell) (dictree--wrap-data (funcall (lambda (weight data) (cond ((not (or weight data)) 0) ((null weight) (1+ data)) ((null data) weight) (t (+ weight data)))) new nil)) (dictree--set-data cell (funcall (lambda (weight data) (cond ((not (or weight data)) 0) ((null weight) (1+ data)) ((null data) weight) (t (+ weight data)))) new (dictree--get-data cell))) cell)) lambda (a b) (funcall (lambda (a b) (if (= (cdr a) (cdr b)) (if (= (length (car a)) (length (car b))) (string< (car a) (car b)) (< (length (car a)) (length (car b)))) (> (cdr a) (cdr b)))) (cons (car a) (dictree--get-data (cdr a))) (cons (car b) (dictree--get-data (cdr b))))) (lambda (new cell) (if (null cell) (dictree--wrap-data (funcall (lambda (weight data) (cond ((not (or weight data)) 0) ((null weight) (1+ data)) ((null data) weight) (t (+ weight data)))) new nil)) (dictree--set-data cell (funcall (lambda (weight data) (cond ((not (or weight data)) 0) ((null weight) (1+ data)) ((null data) weight) (t (+ weight data)))) new (dictree--get-data cell))) cell)) (lambda (a b) (funcall (lambda (a b) (if (= (cdr a) (cdr b)) (if (= (length (car a)) (length (car b))) (string< (car a) (car b)) (< (length (car a)) (length (car b)))) (> (cdr a) (cdr b)))) (cons (car a) (dictree--get-data (cdr a))) (cons (car b) (dictree--get-data (cdr b))))) nil nil nil nil nil 0.1))
(let ((ordered-hash (make-hash-table :test 'equal))
      (tstree (dictree--tstree dict-html-script)))
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
   (dictree--ordered-hash dict-html-script))
  (dictree--set-ordered-hash dict-html-script ordered-hash))
(dictree--set-filename dict-html-script (locate-library "dict-html-script"))
(unless (memq dict-html-script dictree-loaded-list) (push dict-html-script dictree-loaded-list))
