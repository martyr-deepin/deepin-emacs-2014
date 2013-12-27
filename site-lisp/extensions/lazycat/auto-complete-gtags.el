(require 'auto-complete)

(defvar ac-gtags-modes
  '(c-mode cc-mode c++-mode java-mode))

(defun ac-gtags-candidate ()
  (if (memq major-mode ac-gtags-modes)
      (ignore-errors
        (with-temp-buffer
          (when (eq (call-process "global" nil t nil "-ci" ac-prefix) 0)
            (goto-char (point-min))
            (let (candidates)
              (while (and (not (eobp))
                          (push (buffer-substring-no-properties (line-beginning-position) (line-end-position)) candidates)
                          (eq (forward-line) 0)))
              (nreverse candidates)))))))

(defface ac-gtags-candidate-face
  '((t (:background "lightgray" :foreground "navy")))
  "Face for gtags candidate")

(defface ac-gtags-selection-face
  '((t (:background "navy" :foreground "white")))
  "Face for the gtags selected candidate.")

(defvar ac-source-gtags
  '((candidates . ac-gtags-candidate)
    (candidate-face . ac-gtags-candidate-face)
    (selection-face . ac-gtags-selection-face)
    (requires . 3))
  "Source for gtags.")

(provide 'auto-complete-gtags)
