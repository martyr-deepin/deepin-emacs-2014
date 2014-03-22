(require 'cl-lib)

(Then "^ruby-tools-mode should be active$"
      (lambda ()
        (cl-assert ruby-tools-mode nil "Expected `ruby-tools-mode' to be started, but was not.")))

(When "^I go to character \"\\(.+\\)\"$"
      (lambda (char)
        (goto-char (point-min))
        (let ((search (re-search-forward (format "%s" char) nil t))
              (message "Can not go to character '%s' since it does not exist in the current buffer: %s"))
          (cl-assert search nil message char (espuds-buffer-contents)))))

(When "^I go to the \\(front\\|back\\) of character \"\\(.+\\)\"$"
      (lambda (position char)
        (goto-char (point-min))
        (let ((search (re-search-forward (format "%s" char) nil t))
              (message "Can not go to character '%s' since it does not exist in the current buffer: %s"))
          (cl-assert search nil message char (espuds-buffer-contents))
          (cond
           ((string-equal "front" position) (backward-char))
           ((string-equal "back" position) (forward-char))))))


(When "^I go to the \\(front\\|end\\) of the word \"\\(.+\\)\"$"
      (lambda (pos word)
        (goto-char (point-min))
        (let ((search (re-search-forward (format "%s" word) nil t))
              (message "Can not go to character '%s' since it does not exist in the current buffer: %s"))
          (cl-assert search nil message word (espuds-buffer-contents))
          (if (string-equal "front" pos) (backward-word)))))
