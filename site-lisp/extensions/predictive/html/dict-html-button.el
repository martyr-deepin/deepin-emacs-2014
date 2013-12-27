(provide 'dict-html-button)
(require 'dict-tree)
(defvar dict-html-button nil "Dictionary dict-html-button.")
(setq dict-html-button '(DICT "dict-html-button" nil t nil nil (TSTREE [nil [[[[nil [nil [nil [nil [nil [nil [nil [nil [nil [nil (0) nil nil] nil 121] nil 101] nil 107] nil 115] nil 115] nil 101] nil 99] nil 99] nil 97] [nil [nil [nil [nil [nil [nil [nil [nil (0) nil nil] nil 100] nil 101] nil 108] nil 98] nil 97] nil 115] nil 105] nil 100] [nil [nil [nil [nil (0) nil nil] nil 101] nil 109] nil 97] nil 110] [nil [nil [nil [nil [nil [nil (0) nil nil] nil 114] nil 117] nil 108] [nil [nil [nil [nil [nil [nil (0) nil nil] nil 115] nil 117] nil 99] nil 111] nil 102] 98] nil 110] [nil [nil [nil [nil [nil [nil [nil [nil [nil (0) nil nil] nil 120] nil 101] nil 100] nil 110] nil 105] nil 98] [nil [nil [nil [nil (0) nil nil] nil 101] nil 112] nil 121] 97] [nil [nil [nil [nil [nil [nil (0) nil nil] nil 101] nil 117] nil 108] nil 97] nil 118] 116] 111] nil t] (lambda (a b) (cond ((and (null a) (null b)) 0) ((null a) -1) ((null b) 1) (t (- a b)))) (lambda (new cell) (if (null cell) (dictree--wrap-data (funcall (lambda (weight data) (cond ((not (or weight data)) 0) ((null weight) (1+ data)) ((null data) weight) (t (+ weight data)))) new nil)) (dictree--set-data cell (funcall (lambda (weight data) (cond ((not (or weight data)) 0) ((null weight) (1+ data)) ((null data) weight) (t (+ weight data)))) new (dictree--get-data cell))) cell)) lambda (a b) (funcall (lambda (a b) (if (= (cdr a) (cdr b)) (if (= (length (car a)) (length (car b))) (string< (car a) (car b)) (< (length (car a)) (length (car b)))) (> (cdr a) (cdr b)))) (cons (car a) (dictree--get-data (cdr a))) (cons (car b) (dictree--get-data (cdr b))))) (lambda (new cell) (if (null cell) (dictree--wrap-data (funcall (lambda (weight data) (cond ((not (or weight data)) 0) ((null weight) (1+ data)) ((null data) weight) (t (+ weight data)))) new nil)) (dictree--set-data cell (funcall (lambda (weight data) (cond ((not (or weight data)) 0) ((null weight) (1+ data)) ((null data) weight) (t (+ weight data)))) new (dictree--get-data cell))) cell)) (lambda (a b) (funcall (lambda (a b) (if (= (cdr a) (cdr b)) (if (= (length (car a)) (length (car b))) (string< (car a) (car b)) (< (length (car a)) (length (car b)))) (> (cdr a) (cdr b)))) (cons (car a) (dictree--get-data (cdr a))) (cons (car b) (dictree--get-data (cdr b))))) nil nil nil nil nil 0.1))
(let ((ordered-hash (make-hash-table :test 'equal))
      (tstree (dictree--tstree dict-html-button)))
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
   (dictree--ordered-hash dict-html-button))
  (dictree--set-ordered-hash dict-html-button ordered-hash))
(dictree--set-filename dict-html-button (locate-library "dict-html-button"))
(unless (memq dict-html-button dictree-loaded-list) (push dict-html-button dictree-loaded-list))
