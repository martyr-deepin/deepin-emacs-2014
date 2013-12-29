;; With the following configuration, you can use emacs-w3m
;; to preview editing file in yahtml-mode.
(defadvice yahtml-browse-html
  (around w3m-yahtml-browse-html activate compile)
  (w3m-goto-url (ad-get-arg 0) t))
