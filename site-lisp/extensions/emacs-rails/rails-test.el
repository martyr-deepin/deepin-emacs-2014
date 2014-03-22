;;; rails-test.el --- tests integration with the compile library

;; Copyright (C) 2006 Dmitry Galinsky <dima dot exe at gmail dot com>

;; Authors: Dmitry Galinsky <dima dot exe at gmail dot com>,

;; Keywords: ruby rails languages oop
;; $URL: svn+ssh://rubyforge/var/svn/emacs-rails/trunk/rails-ws.el $
;; $Id: rails-ws.el 140 2007-03-27 23:33:36Z dimaexe $

;;; License

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

;;; Code:


(defcustom rails-test:quiet 't
  "Do not show test progress in minibuffer."
  :group 'rails
  :type 'boolean
  :tag "Rails Quiet Tests")

(defcustom rails-test:rake-test-all-task-name "test"
  "Rake task name to run all tests."
  :group 'rails
  :type 'string)

(defvar rails-test:history nil)


(defconst rails-test:result-regexp
  "\\([0-9]+ tests, [0-9]+ assertions, \\([0-9]+\\) failures, \\([0-9]+\\) errors\\)")

(defconst rails-test:progress-regexp
  "^[.EF]+$")

(defvar rails-test:font-lock-keywords
  '(("\\([0-9]+ tests, [0-9]+ assertions, 0 failures, 0 errors\\)"
     1 compilation-info-face)
    ("\\([0-9]+ tests, [0-9]+ assertions, [0-9]+ failures, [0-9]+ errors\\)"
     1 compilation-line-face)
    ("`\\([a-z0-9_]+\\)'"
     1 font-lock-function-name-face t)
    ("^\s+\\([0-9]+)\s+\\(Error\\|Failure\\):\\)"
     1 compilation-line-face)
    ("^\\(test_[a-z0-9_]+\\)(\\([a-zA-Z0-9:]+\\)):?$"
     (1 font-lock-function-name-face)
     (2 font-lock-type-face))
    ("^[.EF]+$" . compilation-info-face)))

(defun rails-test:line-regexp (&optional append prepend)
  (concat
   append
   "\\[?/?\\(\\(?:app\\|config\\|lib\\|test\\|spec\\|vendor\\)?/[^ \f\n\r\t\v]+?\\):\\([0-9]+\\)\\(?::in\s*`\\(.*?\\)'\\)?\\]?"
   prepend))

(defun rails-test:error-regexp-alist ()
  (list
   (list 'rails-test-trace
         (rails-test:line-regexp) 1 2 nil 1)
   (list 'rails-test-error
         (rails-test:line-regexp nil "\\(?:\\]:\\|\n$\\)") 1 2 nil 2))) ; ending of "]:" or next line is blank

(defun rails-test:count-errors-failures ()
  "Return the sum of errors and failures for a completed run."
  (let ((failures 0)
        (errors 0))
    (with-current-buffer (get-buffer rails-script:buffer-name)
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward rails-test:result-regexp (point-max) t)
          (setq failures (+ failures (string-to-number (match-string 2))))
          (setq errors (+ errors (string-to-number (match-string 3)))))))
    (+ errors
       failures)))

(defun rails-test:print-summaries ()
  "Print summary lines."
  (let ((msg nil))
    (with-current-buffer (get-buffer rails-script:buffer-name)
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward rails-test:result-regexp (point-max) t)
          (setq msg (cons (match-string-no-properties 1) msg)))))
    (when msg
      (message "%s" (strings-join " || " (reverse msg))))))

(defun rails-test:print-progress (start end len)
  (let (content)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^Started" end t)
	(line-move 1)
	(save-match-data
	  (let ((progress (string=~ rails-test:progress-regexp
				    (current-line-string) $m)))
	    (when progress
	      (setq content (concat content progress))
	      (setq rails-ui:num-errors 0
		    rails-ui:num-failures 0
		    rails-ui:num-ok 0)
	      (dolist (c (string-to-list content))
		(case c
		  (?\E (setq rails-ui:num-errors (+ 1 rails-ui:num-errors)))
		  (?\F (setq rails-ui:num-failures (+ 1 rails-ui:num-failures)))
		  (?\. (setq rails-ui:num-ok (+ 1 rails-ui:num-ok))))))))))
    (when (and content
	       (not rails-test:quiet))
      (message "Progress of %s: %s" rails-script:running-script-name content))))

(define-derived-mode rails-test:compilation-mode compilation-mode "RTest"
  "Major mode for RoR tests."
  (rails-script:setup-output-buffer)
  ; replace compilation font-lock-keywords
  (set (make-local-variable 'compilation-mode-font-lock-keywords) rails-test:font-lock-keywords)
  (set (make-local-variable 'compilation-error-regexp-alist-alist)
       (rails-test:error-regexp-alist))
  (set (make-local-variable 'compilation-error-regexp-alist)
       '(rails-test-error
         rails-test-trace))
  (add-hook 'after-change-functions 'rails-test:print-progress nil t)
  (add-hook 'rails-script:after-hook-internal 'rails-test:run-after-hooks t t)
  (add-hook 'rails-script:show-buffer-hook 'rails-test:reset-point-and-height t t))

(defcustom rails-test:run-after-hook '(rails-test:hide-rails-project-root
                                       rails-test:print-summaries)
  "Hooks ran after tests have run."
  :group 'rails
  :type 'hook)

(defcustom rails-test:run-after-fail-hook '(rails-script:popup-buffer)
  "Hooks ran after tests have run and any have failured or an error occured."
  :group 'rails
  :type 'hook)

(defcustom rails-test:run-after-pass-hook nil
  "Hooks ran after tests have run and any all passed."
  :group 'rails
  :type 'hook)

(defun rails-test:run-after-hooks ()
  (run-hooks 'rails-test:run-after-hook)
  (if (or (not (zerop rails-script:output-mode-ret-value))
          (> (rails-test:count-errors-failures) 0))
    (run-hooks 'rails-test:run-after-fail-hook)
    (run-hooks 'rails-test:run-after-pass-hook)))

(defun rails-test:hide-rails-project-root ()
  "Show files that are relative to the project root as relative filenames
As the buffer is read-only this is merely a change in appearance"
  (rails-project:with-root (root)
    (save-excursion
      (goto-char (point-min))
      (let ((file-regex (concat (regexp-quote root) "[^:]+")))
        (while (re-search-forward file-regex nil t)
          (let* ((orig-filename (match-string 0))
                 (rel-filename (file-relative-name orig-filename root)))
            (overlay-put (make-overlay (match-beginning 0) (match-end 0))
                         'display rel-filename)))))))

(defun rails-test:reset-point-and-height ()
  "Resets the point and resizes the window for the output buffer.
Used when it's determined that the output buffer needs to be shown."
  (let ((win (get-buffer-window (current-buffer))))
    (when (window-live-p win)
      (set-window-point win 0)
      (unless (buffer-visible-p (current-buffer))
        (compilation-set-window-height win)))))

(defun rails-test:list-of-tasks ()
  "Return a list contains test tasks."
  (append (list "all")
          (delete* nil
                   (mapcar
                    #'(lambda (task) (string=~ "^test\\:\\([^ ]+\\)" task $1))
                    (rails-rake:list-of-tasks))
                   :if 'null)))

(defun rails-test:run (task)
  "Run rake tests in RAILS_ROOT."
  (interactive (rails-completing-read "What test run"
                                      (rails-test:list-of-tasks)
                                      'rails-test:history t))
  (rails-ui:reset-error-count)
  (unless task
    (setq task "all")
    (add-to-list rails-test:history task))
  (let ((task-name
         (if (string= "all" task)
             rails-test:rake-test-all-task-name
           (concat "test:" task))))
    (rails-rake:task task-name 'rails-test:compilation-mode (concat "test " task))))

(defvar rails-test:previous-run-single-param nil
  "Hold params of previous run-single-file call.")

(defun rails-test:run-single-file (file &optional param)
  "Run test for single file FILE."
  (when (not (or file param))
    "Refuse to run ruby without an argument: it would never return")
  (rails-ui:reset-error-count)
  (let ((param (if param
                   (list file param)
                 (list file)))
	(test-name file))
    (if (string-match "\\([^/\\\\.]+\\)_test\.rb$" test-name)
	(setq test-name (concat "test " (match-string-no-properties 1 test-name))))
    (rails-script:run "ruby" (append (list "-Itest") param) 'rails-test:compilation-mode test-name)
    (setq rails-test:previous-run-single-param param)))

(defun rails-test:rerun-single ()
  "Rerun previous single file test."
  (interactive)
  (if rails-test:previous-run-single-param
    (apply 'rails-test:run-single-file rails-test:previous-run-single-param)
    (message "No previous single file test recorded.")))

(defun rails-test:run-current ()
  "Run a test for the current controller/model/mailer."
  (interactive)
  (let* ((model (rails-core:current-model))
         (controller (rails-core:current-controller))
         (mailer (rails-core:current-mailer))
         (func-test (rails-core:functional-test-file controller))
         (unit-test (rails-core:unit-test-file model))
         (mailer-test (rails-core:unit-test-file mailer)))
    (rails-test:run-single-file
     (cond
      ;; model
      ((and model unit-test) unit-test)
      ;; controller
      ((and controller (not (rails-core:mailer-p controller)) func-test)
       func-test)
      ;; mailer
      ((and mailer mailer-test) mailer-test)
      ;; otherwise...
      (t (if (string-match "test.*\\.rb" (buffer-file-name))
             (buffer-file-name)
           (error "Cannot determine which test file to run.")))))))

(defun rails-test:active-support-test-case-current-test ()
  (save-excursion
    (ruby-end-of-block)
    (and (search-backward-regexp "^[ \t]+test[ \t]+\\([\'\"]\\)\\(.*?\\)\\1[ \t]+do" nil t)
         (match-string-no-properties 2))))

(defun rails-test:run-current-method ()
  "Run a test for the current method."
  (interactive)
  (let ((file (substring (buffer-file-name) (length (rails-project:root))))
        (method (rails-core:current-method-name))
        (description (or (rails-shoulda:current-test) (rails-test:active-support-test-case-current-test))))
    (cond (description
           (rails-test:run-single-file file
                                       (format "--name=/%s/"
                                               (replace-regexp-in-string "^\\.\\|\\.$" ""
                                                                         (replace-regexp-in-string "[^a-z0-9,-]+" "."
                                                                                                   description)))))
          (method
           (rails-test:run-single-file file
                                       (format "--name=%s" method))))))

;; These functions were originally defined anonymously in ui. They are defined here so keys
;; can be added to them dryly
(defun rails-test:run-integration ()
  "Run Integration Tests."
  (interactive)
  (rails-test:run "integration"))
(defun rails-test:run-units ()
  "Run Unit Tests."
  (interactive)
  (rails-test:run "units"))
(defun rails-test:run-functionals ()
  "Run Functional Tests."
  (interactive)
  (rails-test:run "functionals"))
(defun rails-test:run-recent ()
  "Run Recent Tests."
  (interactive)
  (rails-test:run "recent"))
(defun rails-test:run-all ()
  "Run All Tests."
  (interactive)
  (rails-test:run "all"))

(provide 'rails-test)
