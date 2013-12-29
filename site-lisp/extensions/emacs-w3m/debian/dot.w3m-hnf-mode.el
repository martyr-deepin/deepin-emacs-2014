; from TIPS.ja
; with this elisp, you can browse latest diary by C-c C-b in hnf-mode 
(defun w3m-hnf-browse-url-w3m (url &optional new-window)
  (interactive (browse-url-interactive-arg "URL: "))
  (save-selected-window
    (pop-to-buffer (get-buffer-create "*w3m*"))
    (w3m-browse-url url new-window)))
(setq hnf-browse-url-browser-function (function w3m-hnf-browse-url-w3m))
