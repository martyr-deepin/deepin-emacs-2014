(provide 'dict-html-link)
(require 'dict-tree)
(defvar dict-html-link nil "Dictionary dict-html-link.")
(setq dict-html-link '(DICT "dict-html-link" nil t nil nil (TSTREE [nil [[[nil [nil [nil [nil [nil [nil [nil [nil (0) nil nil] nil 116] nil 101] nil 115] nil 114] nil 97] nil 104] nil 99] [nil [nil [nil [[nil (0) nil nil] [nil [nil [nil [nil (0) nil nil] nil 103] nil 110] nil 97] nil 108] nil 102] nil 101] nil 114] nil 104] [nil [nil [nil [nil [nil (0) nil nil] nil 97] nil 105] nil 100] nil 101] [nil [nil [nil [nil (0) nil nil] [nil [nil (0) nil nil] nil 118] 108] nil 101] [nil [nil [nil [nil [nil [nil [nil (0) nil nil] nil 116] nil 101] nil 103] nil 114] [nil [nil [nil [nil (0) nil nil] nil 101] nil 112] nil 121] 97] nil 116] 114] 109] nil t] (lambda (a b) (cond ((and (null a) (null b)) 0) ((null a) -1) ((null b) 1) (t (- a b)))) (lambda (new cell) (if (null cell) (dictree--wrap-data (funcall (lambda (weight data) (cond ((not (or weight data)) 0) ((null weight) (1+ data)) ((null data) weight) (t (+ weight data)))) new nil)) (dictree--set-data cell (funcall (lambda (weight data) (cond ((not (or weight data)) 0) ((null weight) (1+ data)) ((null data) weight) (t (+ weight data)))) new (dictree--get-data cell))) cell)) lambda (a b) (funcall (lambda (a b) (if (= (cdr a) (cdr b)) (if (= (length (car a)) (length (car b))) (string< (car a) (car b)) (< (length (car a)) (length (car b)))) (> (cdr a) (cdr b)))) (cons (car a) (dictree--get-data (cdr a))) (cons (car b) (dictree--get-data (cdr b))))) (lambda (new cell) (if (null cell) (dictree--wrap-data (funcall (lambda (weight data) (cond ((not (or weight data)) 0) ((null weight) (1+ data)) ((null data) weight) (t (+ weight data)))) new nil)) (dictree--set-data cell (funcall (lambda (weight data) (cond ((not (or weight data)) 0) ((null weight) (1+ data)) ((null data) weight) (t (+ weight data)))) new (dictree--get-data cell))) cell)) (lambda (a b) (funcall (lambda (a b) (if (= (cdr a) (cdr b)) (if (= (length (car a)) (length (car b))) (string< (car a) (car b)) (< (length (car a)) (length (car b)))) (> (cdr a) (cdr b)))) (cons (car a) (dictree--get-data (cdr a))) (cons (car b) (dictree--get-data (cdr b))))) nil nil nil nil nil 0.1))
(let ((ordered-hash (make-hash-table :test 'equal))
      (tstree (dictree--tstree dict-html-link)))
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
   (dictree--ordered-hash dict-html-link))
  (dictree--set-ordered-hash dict-html-link ordered-hash))
(dictree--set-filename dict-html-link (locate-library "dict-html-link"))
(unless (memq dict-html-link dictree-loaded-list) (push dict-html-link dictree-loaded-list))
