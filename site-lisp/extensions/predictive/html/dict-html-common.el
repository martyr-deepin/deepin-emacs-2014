(provide 'dict-html-common)
(require 'dict-tree)
(defvar dict-html-common nil "Dictionary dict-html-common.")
(setq dict-html-common '(DICT "dict-html-common" nil t nil nil (TSTREE [nil [[[[[nil [nil [nil [nil [nil [nil (0) nil nil] nil 115] nil 115] nil 97] nil 108] nil 99] [nil [nil [nil (0) nil nil] nil 114] nil 105] nil 100] [nil [nil (0) nil nil] nil 100] nil 105] [nil [nil [nil [nil (0) nil nil] nil 103] nil 110] nil 97] nil 108] [nil [[[nil [nil [nil [nil [nil [nil (0) nil nil] nil 107] nil 99] nil 105] nil 108] nil 99] [nil [nil [nil [nil [nil [nil [nil [nil (0) nil nil] nil 107] nil 99] nil 105] nil 108] nil 99] nil 108] nil 98] nil 100] [nil [nil [[nil [nil [nil [nil [nil (0) nil nil] nil 110] nil 119] nil 111] nil 100] [nil [nil [nil [nil [nil (0) nil nil] nil 115] nil 115] nil 101] nil 114] [nil [nil [nil (0) nil nil] nil 112] nil 117] 112] nil 121] nil 101] [nil [nil [nil [nil [nil [nil [nil [nil [nil [nil (0) nil nil] nil 110] nil 119] nil 111] [nil [nil [nil [nil [nil (0) nil nil] nil 101] nil 118] nil 111] [nil [nil [nil [nil (0) nil nil] nil 116] [nil [nil [nil [nil (0) nil nil] nil 114] nil 101] nil 118] 117] [nil [nil [nil (0) nil nil] nil 112] nil 117] 111] 109] 100] nil 101] nil 115] nil 117] nil 111] nil 109] 107] nil 110] [nil [nil [nil [nil [nil [nil (0) nil nil] nil 101] nil 108] nil 121] nil 116] [nil [nil [nil [nil [nil [nil (0) nil nil] nil 101] nil 108] nil 116] nil 105] nil 116] 115] 111] nil t] (lambda (a b) (cond ((and (null a) (null b)) 0) ((null a) -1) ((null b) 1) (t (- a b)))) (lambda (new cell) (if (null cell) (dictree--wrap-data (funcall (lambda (weight data) (cond ((not (or weight data)) 0) ((null weight) (1+ data)) ((null data) weight) (t (+ weight data)))) new nil)) (dictree--set-data cell (funcall (lambda (weight data) (cond ((not (or weight data)) 0) ((null weight) (1+ data)) ((null data) weight) (t (+ weight data)))) new (dictree--get-data cell))) cell)) lambda (a b) (funcall (lambda (a b) (if (= (cdr a) (cdr b)) (if (= (length (car a)) (length (car b))) (string< (car a) (car b)) (< (length (car a)) (length (car b)))) (> (cdr a) (cdr b)))) (cons (car a) (dictree--get-data (cdr a))) (cons (car b) (dictree--get-data (cdr b))))) (lambda (new cell) (if (null cell) (dictree--wrap-data (funcall (lambda (weight data) (cond ((not (or weight data)) 0) ((null weight) (1+ data)) ((null data) weight) (t (+ weight data)))) new nil)) (dictree--set-data cell (funcall (lambda (weight data) (cond ((not (or weight data)) 0) ((null weight) (1+ data)) ((null data) weight) (t (+ weight data)))) new (dictree--get-data cell))) cell)) (lambda (a b) (funcall (lambda (a b) (if (= (cdr a) (cdr b)) (if (= (length (car a)) (length (car b))) (string< (car a) (car b)) (< (length (car a)) (length (car b)))) (> (cdr a) (cdr b)))) (cons (car a) (dictree--get-data (cdr a))) (cons (car b) (dictree--get-data (cdr b))))) nil nil nil nil nil 0.1))
(let ((ordered-hash (make-hash-table :test 'equal))
      (tstree (dictree--tstree dict-html-common)))
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
   (dictree--ordered-hash dict-html-common))
  (dictree--set-ordered-hash dict-html-common ordered-hash))
(dictree--set-filename dict-html-common (locate-library "dict-html-common"))
(unless (memq dict-html-common dictree-loaded-list) (push dict-html-common dictree-loaded-list))
