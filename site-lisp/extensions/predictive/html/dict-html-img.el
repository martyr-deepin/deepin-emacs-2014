(provide 'dict-html-img)
(require 'dict-tree)
(defvar dict-html-img nil "Dictionary dict-html-img.")
(setq dict-html-img '(DICT "dict-html-img" nil t nil nil (TSTREE [nil [[[[nil [nil [nil [nil (0) nil nil] nil 116] nil 108] nil 97] [nil [nil [nil [nil [nil [nil (0) nil nil] nil 116] nil 104] nil 103] nil 105] nil 101] nil 104] [nil [nil [nil [nil [nil (0) nil nil] nil 112] nil 97] nil 109] nil 115] nil 105] [nil [nil [nil [nil [nil [nil [nil [nil (0) nil nil] nil 99] nil 115] nil 101] nil 100] nil 103] nil 110] nil 111] [nil [nil [nil [nil (0) nil nil] nil 99] nil 114] [nil [nil [nil [nil [nil [nil [nil (0) nil nil] nil 112] nil 97] nil 109] nil 101] nil 115] [nil [nil [nil [nil [nil [nil (0) nil nil] nil 104] nil 116] nil 100] nil 105] nil 119] 117] 115] 108] nil t] (lambda (a b) (cond ((and (null a) (null b)) 0) ((null a) -1) ((null b) 1) (t (- a b)))) (lambda (new cell) (if (null cell) (dictree--wrap-data (funcall (lambda (weight data) (cond ((not (or weight data)) 0) ((null weight) (1+ data)) ((null data) weight) (t (+ weight data)))) new nil)) (dictree--set-data cell (funcall (lambda (weight data) (cond ((not (or weight data)) 0) ((null weight) (1+ data)) ((null data) weight) (t (+ weight data)))) new (dictree--get-data cell))) cell)) lambda (a b) (funcall (lambda (a b) (if (= (cdr a) (cdr b)) (if (= (length (car a)) (length (car b))) (string< (car a) (car b)) (< (length (car a)) (length (car b)))) (> (cdr a) (cdr b)))) (cons (car a) (dictree--get-data (cdr a))) (cons (car b) (dictree--get-data (cdr b))))) (lambda (new cell) (if (null cell) (dictree--wrap-data (funcall (lambda (weight data) (cond ((not (or weight data)) 0) ((null weight) (1+ data)) ((null data) weight) (t (+ weight data)))) new nil)) (dictree--set-data cell (funcall (lambda (weight data) (cond ((not (or weight data)) 0) ((null weight) (1+ data)) ((null data) weight) (t (+ weight data)))) new (dictree--get-data cell))) cell)) (lambda (a b) (funcall (lambda (a b) (if (= (cdr a) (cdr b)) (if (= (length (car a)) (length (car b))) (string< (car a) (car b)) (< (length (car a)) (length (car b)))) (> (cdr a) (cdr b)))) (cons (car a) (dictree--get-data (cdr a))) (cons (car b) (dictree--get-data (cdr b))))) nil nil nil nil nil 0.1))
(let ((ordered-hash (make-hash-table :test 'equal))
      (tstree (dictree--tstree dict-html-img)))
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
   (dictree--ordered-hash dict-html-img))
  (dictree--set-ordered-hash dict-html-img ordered-hash))
(dictree--set-filename dict-html-img (locate-library "dict-html-img"))
(unless (memq dict-html-img dictree-loaded-list) (push dict-html-img dictree-loaded-list))
