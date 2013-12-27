(provide 'dict-html-col)
(require 'dict-tree)
(defvar dict-html-col nil "Dictionary dict-html-col.")
(setq dict-html-col '(DICT "dict-html-col" nil t nil nil (TSTREE [nil [[nil [nil [nil [nil [nil [nil (0) nil nil] nil 110] nil 103] nil 105] nil 108] nil 97] [nil [nil [nil [[nil (0) nil nil] [nil [nil [nil (0) nil nil] nil 102] nil 102] nil 111] nil 114] nil 97] nil 104] [nil [nil [nil [nil [nil (0) nil nil] nil 110] nil 97] nil 112] [nil [nil [nil [nil [nil [nil [nil (0) nil nil] nil 110] nil 103] nil 105] nil 108] nil 97] [nil [nil [nil [nil [nil [nil (0) nil nil] nil 104] nil 116] nil 100] nil 105] nil 119] 118] 115] 99] nil t] (lambda (a b) (cond ((and (null a) (null b)) 0) ((null a) -1) ((null b) 1) (t (- a b)))) (lambda (new cell) (if (null cell) (dictree--wrap-data (funcall (lambda (weight data) (cond ((not (or weight data)) 0) ((null weight) (1+ data)) ((null data) weight) (t (+ weight data)))) new nil)) (dictree--set-data cell (funcall (lambda (weight data) (cond ((not (or weight data)) 0) ((null weight) (1+ data)) ((null data) weight) (t (+ weight data)))) new (dictree--get-data cell))) cell)) lambda (a b) (funcall (lambda (a b) (if (= (cdr a) (cdr b)) (if (= (length (car a)) (length (car b))) (string< (car a) (car b)) (< (length (car a)) (length (car b)))) (> (cdr a) (cdr b)))) (cons (car a) (dictree--get-data (cdr a))) (cons (car b) (dictree--get-data (cdr b))))) (lambda (new cell) (if (null cell) (dictree--wrap-data (funcall (lambda (weight data) (cond ((not (or weight data)) 0) ((null weight) (1+ data)) ((null data) weight) (t (+ weight data)))) new nil)) (dictree--set-data cell (funcall (lambda (weight data) (cond ((not (or weight data)) 0) ((null weight) (1+ data)) ((null data) weight) (t (+ weight data)))) new (dictree--get-data cell))) cell)) (lambda (a b) (funcall (lambda (a b) (if (= (cdr a) (cdr b)) (if (= (length (car a)) (length (car b))) (string< (car a) (car b)) (< (length (car a)) (length (car b)))) (> (cdr a) (cdr b)))) (cons (car a) (dictree--get-data (cdr a))) (cons (car b) (dictree--get-data (cdr b))))) nil nil nil nil nil 0.1))
(let ((ordered-hash (make-hash-table :test 'equal))
      (tstree (dictree--tstree dict-html-col)))
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
   (dictree--ordered-hash dict-html-col))
  (dictree--set-ordered-hash dict-html-col ordered-hash))
(dictree--set-filename dict-html-col (locate-library "dict-html-col"))
(unless (memq dict-html-col dictree-loaded-list) (push dict-html-col dictree-loaded-list))
