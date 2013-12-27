(require 'anything)
(require 'anything-c-moccur)

(defun anything-c-occur-by-search-buffers ()
  (interactive)
  (let ((anything-sources
         (list '((name . "Search Buffers")
                 (type . string)
                 (candidates . anything-c-mbsb-get-candidates)
                 (match . (identity))
                 (requires-pattern . 3)
                 (delayed)
                 (volatile)
                 (action . anything-c-moccur-with-specified-named-buffer)))))
    (anything)))

(defun anything-c-moccur-with-specified-named-buffer (buf)
  (with-current-buffer buf
    (anything-c-moccur-with-anything-env (list anything-c-source-occur-by-moccur)
      (let* ((initial-pattern anything-pattern)
             (anything-c-moccur-anything-initial-pattern initial-pattern))
        (when anything-c-moccur-push-mark-flag
          (push-mark))
        (anything)))))

(defun anything-c-mbsb-get-candidates ()
  (mapcar (lambda (buf) (cons (buffer-name buf)
                              buf))
          (remove-if-not (lambda (buf)
                           (when (< 0 (length anything-pattern))
                             (with-current-buffer buf
                               (save-excursion
                                 (goto-char (point-min))
                                 (re-search-forward anything-pattern nil t 1)))))
                         (buffer-list))))

(provide 'anything-c-moccur-search-buffers)
