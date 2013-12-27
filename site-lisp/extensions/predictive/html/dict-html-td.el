(provide 'dict-html-td)
(require 'dict-tree)
(defvar dict-html-td nil "Dictionary dict-html-td.")
(setq dict-html-td '(DICT "dict-html-td" nil t nil nil (TSTREE [nil [[nil [[[nil [nil [nil [nil (0) nil nil] nil 114] nil 98] nil 98] [nil [nil [nil [nil (0) nil nil] nil 110] nil 103] nil 105] nil 108] [nil [nil [nil (0) nil nil] nil 115] nil 105] nil 120] nil 97] [nil [nil [nil [[nil (0) nil nil] [nil [nil [nil (0) nil nil] nil 102] nil 102] nil 111] nil 114] nil 97] [nil [nil [nil [nil [nil [nil [nil (0) nil nil] nil 110] nil 97] nil 112] nil 115] nil 108] nil 111] 104] [nil [nil [nil [nil [nil [nil [nil [nil (0) nil nil] nil 115] nil 114] nil 101] nil 100] nil 97] nil 101] [nil [nil [nil [nil [nil [nil [nil [nil (0) nil nil] nil 110] nil 97] nil 112] nil 115] nil 119] nil 111] [nil [nil [nil [nil [nil [nil (0) nil nil] nil 101] nil 112] nil 111] nil 99] [nil [nil [nil [nil [nil [nil [nil (0) nil nil] nil 110] nil 103] nil 105] nil 108] nil 97] nil 118] 115] 114] 104] 99] nil t] (lambda (a b) (cond ((and (null a) (null b)) 0) ((null a) -1) ((null b) 1) (t (- a b)))) (lambda (new cell) (if (null cell) (dictree--wrap-data (funcall (lambda (weight data) (cond ((not (or weight data)) 0) ((null weight) (1+ data)) ((null data) weight) (t (+ weight data)))) new nil)) (dictree--set-data cell (funcall (lambda (weight data) (cond ((not (or weight data)) 0) ((null weight) (1+ data)) ((null data) weight) (t (+ weight data)))) new (dictree--get-data cell))) cell)) lambda (a b) (funcall (lambda (a b) (if (= (cdr a) (cdr b)) (if (= (length (car a)) (length (car b))) (string< (car a) (car b)) (< (length (car a)) (length (car b)))) (> (cdr a) (cdr b)))) (cons (car a) (dictree--get-data (cdr a))) (cons (car b) (dictree--get-data (cdr b))))) (lambda (new cell) (if (null cell) (dictree--wrap-data (funcall (lambda (weight data) (cond ((not (or weight data)) 0) ((null weight) (1+ data)) ((null data) weight) (t (+ weight data)))) new nil)) (dictree--set-data cell (funcall (lambda (weight data) (cond ((not (or weight data)) 0) ((null weight) (1+ data)) ((null data) weight) (t (+ weight data)))) new (dictree--get-data cell))) cell)) (lambda (a b) (funcall (lambda (a b) (if (= (cdr a) (cdr b)) (if (= (length (car a)) (length (car b))) (string< (car a) (car b)) (< (length (car a)) (length (car b)))) (> (cdr a) (cdr b)))) (cons (car a) (dictree--get-data (cdr a))) (cons (car b) (dictree--get-data (cdr b))))) nil nil nil nil nil 0.1))
(let ((ordered-hash (make-hash-table :test 'equal))
      (tstree (dictree--tstree dict-html-td)))
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
   (dictree--ordered-hash dict-html-td))
  (dictree--set-ordered-hash dict-html-td ordered-hash))
(dictree--set-filename dict-html-td (locate-library "dict-html-td"))
(unless (memq dict-html-td dictree-loaded-list) (push dict-html-td dictree-loaded-list))
