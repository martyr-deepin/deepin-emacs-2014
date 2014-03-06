;; -*- encoding: utf-8-unix; -*-
;; File-name:    <20_indent-vline.el>
;; Create:       <2012-01-18 00:53:10 ran9er>
;; Time-stamp:   <2012-07-31 22:54:46 ran9er>
;; Mail:         <2999am@gmail.com>

(setq indent-hint-prefix "il-"
      indent-hint-key 'indent-hint-id
      indent-hint-bg 'indent-hint-bg
      indent-hint-gc-timer 5
      indent-hint-overlay-pool-minimum 10
      ;; ;
      indent-hint-counter 0
      indent-hint-gc-counter 0
      indent-hint-overlay-pool nil
      indent-hint-list nil
      indent-hint-lazy nil)
(defun indent-hint-genid ()
  (progn
    (setq indent-hint-counter (1+ indent-hint-counter))
    (intern
     (concat "*" indent-hint-prefix (number-to-string indent-hint-counter) "*"))))
(defun indent-hint-init(&optional l)
  (mapc
   (lambda(x) (or (local-variable-p x)
              (make-local-variable x)))
   '(indent-hint-counter
     indent-hint-list
     indent-hint-lazy
     indent-hint-gc-counter))
  (if l (setq indent-hint-lazy t))
  (indent-hint-bgo-init)
  (add-hook 'post-command-hook 'indent-hint-bgo-mv t t)
  (add-hook 'post-gc-hook 'indent-hint-gc t t)
  (font-lock-fontify-buffer))

(defun indent-hint-make-overlay (b e)
  (let (o)
    (if ;; (null indent-hint-overlay-pool)
        (> indent-hint-overlay-pool-minimum
           (length indent-hint-overlay-pool))
        (setq o (make-overlay b e))
      (setq o (car indent-hint-overlay-pool))
      (move-overlay o b e)
      (setq indent-hint-overlay-pool (cdr indent-hint-overlay-pool)))
    o))
(defun indent-hint-delete-overlay (o)
  (let ((ov o))
    (delete-overlay ov)
    (setq indent-hint-overlay-pool (cons ov indent-hint-overlay-pool))))

(defun indent-hint-gc ()
  (if (< indent-hint-gc-counter indent-hint-gc-timer)
      (setq indent-hint-gc-counter (1+ indent-hint-gc-counter))
    (setq indent-hint-gc-counter 0)
    (dolist (x indent-hint-list)
      (if (null (eval x))
          (and (setq indent-hint-list
                     (delq x indent-hint-list))
               (unintern x))))))

;; * indent-hint
(defun make-indent-hint-xpm (width height color &optional lor)
  (let* ((w width)
         (h height)
         (s1 (concat "\"" (make-string w (string-to-char " ")) "\""))
         (s2 (cond
              ((eq lor 0)
               (concat "\"." (make-string (1- w) (string-to-char " ")) "\""))
              ((eq lor 1)
               (concat "\"" (make-string (1- w) (string-to-char " ")) ".\""))
              ((null lor)
               (concat "\"" (make-string (- (1- w)(/ (1- w) 2))(string-to-char " "))
                       "." (make-string (/ (1- w) 2)(string-to-char " ")) "\""))))
         (sa (concat s1 ",\n" s2 ",\n")))
    (eval `(concat "/* XPM */
static char * dot_vline_xpm[] = {
\"" (number-to-string w) " " (number-to-string h) " 2 1\",
\"  c None\",
\". c " color "\",\n"
,@(mapcar (lambda(x) sa)
          (make-list (1- (/ h 2)) 0))
s1 ",\n" s2 "};"
))))

(defvar indent-hint-line-height (or (car (window-line-height)) 20))
(defvar indent-hint-img (make-indent-hint-xpm 9 indent-hint-line-height "#4D4D4D"))
(defvar indent-hint-img-lst (make-indent-hint-xpm 9 indent-hint-line-height "#6a5acd"))
(defvar indent-hint-img-blk (make-indent-hint-xpm 9 indent-hint-line-height "khaki"))

(defun kill-indent-hint (m &optional n)
  (let ((n (or n (1+ m))))
    (mapc
     (lambda(x)(let ((i (overlay-get x indent-hint-key)))
             (if i
                 (progn
                   (mapc
                    (lambda(y)(indent-hint-delete-overlay y))
                    (eval i))
                   (setq indent-hint-list
                         (delq i indent-hint-list))
                   (unintern i)
                   ))))
     (overlays-in m n))))
(defun erase-indent-hint (overlay after? beg end &optional length)
  (let ((inhibit-modification-hooks t)
        p1 p2)
    (if after?
        (save-excursion
          (forward-line)
          ;; (setq p1 (point))
          (setq p1 (line-beginning-position)
                p2 (+ p1 (current-indentation)))
          (kill-indent-hint p1 p2)
          (font-lock-fontify-block))
      (setq p1 (line-beginning-position) ;; (point)
            p2 (+ p1 (current-indentation)))
      (kill-indent-hint p1 p2))))

(defun what-overlays ()
  (interactive)
  (print
   (cons (cons (point) (current-column))
         (mapcar
          (lambda(x) (remove-if
                  nil
                  `(,x
                    ,(overlay-get x indent-hint-key)
                    ,(if (overlay-get x indent-hint-bg) 'bg)
                    ,(if (eq (overlay-get x 'face) 'hl-line) 'hl-line))))
          (overlays-at (point))))))
(defun erase-indent-hint-0 (overlay after? beg end &optional length)
  (let ((inhibit-modification-hooks t)
        (c (current-column))
        p1 p2)
    (if after?
        (save-excursion
          (forward-line)
          (move-to-column c)
          (setq p1 (point))
          (skip-chars-forward " ")
          (setq p2 (point))
          (kill-indent-hint p1 p2)
          (font-lock-fontify-block))
      (setq p1 (point)
            p2 (+ p1 (save-excursion (skip-chars-forward " "))))
      (kill-indent-hint p1 p2))))

(setq draw-indent-hint-func
      (if (display-images-p)
          (lambda(o img)
            (overlay-put o 'display
                         `(display (image
                                    :type xpm
                                    :data ,img
                                    :pointer text
                                    :ascent center
                                    :mask (heuristic t))
                                   rear-nonsticky (display)
                                   fontified t)))
        (lambda(o color)
          (overlay-put o 'display
                       "|"))))

(defun draw-indent-hint (beg end id &optional img color)
  (let ((img (or img indent-hint-img))
        (color (or color "#4D4D4D"))
        (ov (indent-hint-make-overlay beg end)))
    (overlay-put ov indent-hint-key id)
    ;; (overlay-put ov evaporate t)
    (funcall draw-indent-hint-func ov img)
    ov))

;; (if (display-images-p)
;;         (set-text-properties
;;          beg end
;;          `(display (image
;;                     :type xpm
;;                     :data ,img
;;                     :pointer text
;;                     :ascent center
;;                     :mask (heuristic t))
;;                    rear-nonsticky (display)
;;                    fontified t))
;;       (compose-region
;;        beg end
;;        (prog1 "|"
;;          (set-text-properties beg end `(font-lock-face (:foreground ,color))))
;;        'decompose-region))

(defun indent-hint-overlay-exist (p k)
  (let (r (l (overlays-at p)))
    (while (and l
                (null
                 (if (overlay-get (car l) k)
                     (setq r t)
                   nil)))
      (setq l (cdr l)))
    r))
(defun draw-indent-hint-line (&optional column img color)
  (interactive "P")
  (save-excursion
    (let* ((line (indent-hint-genid))
           (i (or column (current-indentation))))
      (make-local-variable line)
      (setq indent-hint-list (cons line indent-hint-list))
      (set line nil)
      (while (< i (if (<= (point-max)(line-end-position))
                      0
                    (forward-line)
                    (beginning-of-line)
                    (skip-chars-forward " ")
                    (current-column)))
        (move-to-column i)
        (let* ((p1 (point))(p2 (1+ p1)))
          (if indent-hint-lazy
              (if (indent-hint-overlay-exist p1 indent-hint-key) nil
                (set line (cons (draw-indent-hint p1 p2 line img color) (eval line))))
            (kill-indent-hint p1)
            (set line (cons (draw-indent-hint p1 p2 line img color) (eval line)))))))))


(defun indent-hint-bgo-init (&optional r)
  (let* ((b (line-beginning-position))
         (e (+ b (current-indentation)))
         o)
    (setq r (or r 'indent-hint-background-overlay))
    (make-local-variable r)
    (setq o (make-overlay b e))
    (overlay-put o indent-hint-bg t)
    ;; debug
    ;; (overlay-put o 'face '((t (:background "grey40"))))
    (overlay-put o 'modification-hooks '(erase-indent-hint))
    (overlay-put o 'insert-in-front-hooks '(erase-indent-hint))
    (overlay-put o 'insert-behind-hooks '(erase-indent-hint))
    (set r o)))
(defun indent-hint-bgo-mv(&optional o)
  (let* ((o (or o indent-hint-background-overlay))
         (b (line-beginning-position))
         (e (+ b (current-indentation))))
    (move-overlay o b e)))

(defun indent-hint (&optional regexp column img color)
  (interactive)
  (let ((x (or regexp "^")))
    (font-lock-add-keywords
     nil `((,x
            (0 (draw-indent-hint-line ,column ,img ,color)))))))


;; (defun indent-vline-lisp (&optional regexp)
;;   (interactive)
;;   (indent-hint (or regexp "^[ \t]*[,`#'(]")))

(defun indent-hint-current-column ()
  (save-excursion
    (goto-char (match-beginning 1))
    (current-column)))

(defun indent-hint-mode (&optional l lst)
  (interactive)
  (let* ((c '(indent-hint-current-column))
         (lst (or lst '(("^[ \t]*\\([^ \t]\\)"))))
         (lst (if l lst (reverse lst))))
    (indent-hint-init l)
    (dolist (x lst)
      (indent-hint (car x) c (cadr x)))))

(defun indent-hint-tab-mode (&optional l lst)
  (interactive)
  (let* ((c '(indent-hint-current-column))
         (lst (or lst '(("^[    ]*\\([^ ]\\)"))))
         (lst (if l lst (reverse lst))))
    (indent-hint-init l)
    (dolist (x lst)
      (indent-hint (car x) c (cadr x)))))

;; ???
(defun indent-hint-lisp ()
  (interactive)
  (indent-hint-mode
   nil
   '(("^[ \t]*\\((\\)")
     ("\\((lambda\\|(setq\\|(defvar\\)" 'indent-hint-img-lst)
     ("\\((let\\*?\\|(if\\|(while\\|(cond\\|(map.*\\|(defun\\|(save-excursion\\)" 'indent-hint-img-blk)
     ("[,`#']+\\((\\)" 'indent-hint-img-lst))))


;; example
(defun indent-vline-lisp ()
  (interactive)
  (indent-hint-init)
  (let ((c '(indent-hint-current-column))
        (blk "\\((let\\*?\\|(if\\|(while\\|(cond\\|(map.*\\|(defun\\|(save-excursion\\)"))
    (if indent-hint-lazy
        (progn
          (indent-hint "^[ \t]*\\((\\)" c)
          (indent-hint "\\((lambda\\|(setq\\|(defvar\\)" c 'indent-hint-img-lst)
          (indent-hint blk c 'indent-hint-img-blk)
          (indent-hint "[,`#']+\\((\\)" c 'indent-hint-img-lst))
      (indent-hint "[,`#']+\\((\\)" c 'indent-hint-img-lst)
      (indent-hint blk c 'indent-hint-img-blk)
      (indent-hint "\\((lambda\\|(setq\\|(defvar\\)" c 'indent-hint-img-lst)
      (indent-hint "^[ \t]*\\((\\)" c))))

(defun indent-hint-fixed(&optional img)
  (interactive)
  (indent-hint "^[ \t]*\\([^ \t]\\)"
               '(indent-hint-current-column)
               img)
  (indent-hint-init))

(defun indent-vline-test (&optional regexp)
  (interactive)
  (indent-hint (or regexp "^[   ]*\[^   ]")
               '(save-excursion
                  (goto-char (match-beginning 1))
                  (current-column)))
  (indent-hint-init))


(when
    nil
  (what-overlays)
  (length indent-hint-list)
  (dolist (x indent-hint-list)
    (if (null (eval x))
        (and (unintern x)
             (setq indent-hint-list
                   (delq x indent-hint-list)))))
  (setq overlay-no-buffer nil)
  (dolist (x indent-hint-list)
    (dolist (y (eval x))
      (if (null (overlay-buffer y))
          (setq overlay-no-buffer
                (cons y overlay-no-buffer)))))
  )

(provide 'indent-vline)
