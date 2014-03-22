;;; rails-refactoring.el -- common refactoring operations on rails projects

;; Copyright (C) 2009 by Remco van 't Veer

;; Author: Remco van 't Veer
;; Keywords: ruby rails languages oop refactoring

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

(require 'cl)
(require 'rails-core)


;; Customizations

(defcustom rails-refactoring-source-extensions '("builder" "erb" "haml" "liquid" "mab" "rake" "rb" "rhtml" "rjs" "rxml" "yml" "rtex" "prawn" "rabl" "json_builder" "jbuilder" "slim")
  "List of file extensions for refactoring search and replace operations."
  :group 'rails
  :type '(repeat string))


;; Helper functions

(defmacro rails-refactoring:disclaim (name)
  `(when (interactive-p)
     (when (not (y-or-n-p (concat "Warning! " ,name " can not be undone! Are you sure you want to continue? ")))
       (error "cancelled"))
     (save-some-buffers)))

(defun rails-refactoring:decamelize (name)
  "Translate Ruby class name to corresponding file name."
  (replace-regexp-in-string "::" "/" (decamelize name)))

(assert (string= "foo_bar/quux" (rails-refactoring:decamelize "FooBar::Quux")))

(defun rails-refactoring:camelize (name)
  "Translate file name into corresponding Ruby class name."
  (replace-regexp-in-string "/" "::"
                            (replace-regexp-in-string "_\\([a-z]\\)" (lambda (match)
                                                                       (upcase (substring match 1)))
                                                      (capitalize name))))

(assert (string= "FooBar::Quux" (rails-refactoring:camelize "foo_bar/quux")))

(defun rails-refactoring:source-file-p (name)
  "Test if file has extension from `rails-refactoring-source-extensions'."
  (find-if (lambda (ext) (string-match (concat "\\." ext "$") name))
           rails-refactoring-source-extensions))

(defun rails-refactoring:source-files ()
  "Return a list of all the source files in the current rails
project.  This includes all the files in the 'app', 'config',
'lib' and 'test' directories."
  (apply #'append
         (mapcar (lambda (dirname)
                   (delete-if (lambda (file) (string-match "_flymake.rb" file))
                              (delete-if-not 'rails-refactoring:source-file-p
                                             (mapcar (lambda (f) (concat dirname f))
                                                     (directory-files-recursive (rails-core:file dirname))))))
                 '("app/" "config/" "lib/" "test/" "spec/"))))

(defun rails-refactoring:class-files ()
  "Return list of all Ruby class files."
  (delete-if-not (lambda (file) (string-match "\\.rb$" file)) (rails-refactoring:source-files)))

(defun rails-refactoring:class-from-file (file)
  "Return corresponding class/module name for given FILE."
  (let ((path (find-if (lambda (path) (string-match (concat "^" (regexp-quote path)) file))
                       '("app/models/" "app/controllers/" "app/helpers/" "lib/"
                         "test/unit/helpers/" "test/unit/" "test/functional/"
                         "spec/models/" "spec/controllers/" "spec/helpers/ spec/lib/"))))
    (when path
      (rails-refactoring:camelize
       (replace-regexp-in-string path "" (replace-regexp-in-string "\\.rb$" "" file))))))

(assert (string= "FooBar" (rails-refactoring:class-from-file "app/models/foo_bar.rb")))
(assert (string= "Foo::BarController" (rails-refactoring:class-from-file "app/controllers/foo/bar_controller.rb")))
(assert (string= "Foo::Bar::Quux" (rails-refactoring:class-from-file "lib/foo/bar/quux.rb")))
(assert (string= "FooTest" (rails-refactoring:class-from-file "test/unit/foo_test.rb")))
(assert (string= "FooHelperTest" (rails-refactoring:class-from-file "test/unit/helpers/foo_helper_test.rb")))

(defun rails-refactoring:legal-class-name-p (name)
  "Return t when NAME is a valid Ruby class name."
  (let ((case-fold-search nil))
    (not (null (string-match "^\\([A-Z][A-Za-z0-9]*\\)\\(::[A-Z][A-Za-z0-9]*\\)*$" name)))))

(assert (rails-refactoring:legal-class-name-p "FooBar"))
(assert (rails-refactoring:legal-class-name-p "Foo::Bar"))
(assert (not (rails-refactoring:legal-class-name-p "Foo Bar")))
(assert (not (rails-refactoring:legal-class-name-p "foo")))

(defun rails-refactoring:read-string (prompt &optional
                                             pred error
                                             initial-input
                                             history
                                             default-value
                                             inherit-input-method)
  "Prompt for string in minibuffer like `read-string'.  The
second argument PRED determines is the input is valid.  If the
input is invalid and the third argument ERROR is given that
message is displayed."
  (let (result)
    (while (not result)
      (let ((answer (read-string prompt initial-input history
                                 default-value inherit-input-method)))
        (if (or (null pred) (funcall pred answer))
          (setq result answer)
          (progn
            (message (or error "invalid input") answer)
            (sleep-for 1)))))
    result))

(defun rails-refactoring:read-class-name (prompt &optional initial-input history default-value inherit-input-method)
  "Prompt for a Ruby class name in minibuffer like `read-string'.
Only a legal class name is accepted."
  (rails-refactoring:read-string prompt
                                 'rails-refactoring:legal-class-name-p
                                 "`%s' is not a valid Ruby class name"
                                 initial-input history default-value
                                 inherit-input-method))


;; Refactoring methods

(defun rails-refactoring:query-replace (from to &optional dirs)
  "Replace some occurrences of FROM to TO in all the project
source files.  If DIRS argument is given the files are limited to
these directories.

The function returns nil when the user cancelled or an alist of
the form (FILE . SITES) where SITES are the replacement sites as
returned by `perform-replace' per FILE."
  (interactive "sFrom: \nsTo: ")
  (let ((result nil)
        (keep-going t)
        (files (if dirs
                 (delete-if-not (lambda (file)
                                  (find-if (lambda (dir)
                                             (string-match (concat "^" (regexp-quote dir)) file))
                                           dirs))
                                (rails-refactoring:source-files))
                 (rails-refactoring:source-files)))
        (case-fold-search (and case-fold-search (string= from (downcase from))))
        (original-buffer (current-buffer)))
    (while (and keep-going files)
      (let* ((file (car files))
             (flymake-start-syntax-check-on-find-file nil)
             (existing-buffer (get-file-buffer (rails-core:file file))))
        (set-buffer (or existing-buffer (find-file-noselect (rails-core:file file))))
        (message ".. %s .." file)
        (goto-char (point-min))
        (if (re-search-forward from nil t)
          (progn
            (switch-to-buffer (current-buffer))
            (goto-char (point-min))
            (let ((sites (perform-replace from to t t nil)))
              (if sites
                (push (cons file sites) result)
                (setq keep-going nil))))
          (unless existing-buffer (kill-buffer nil)))
        (set-buffer original-buffer))
      (setq files (cdr files)))
    (and keep-going result)))

(defun rails-refactoring:rename-class (from-file to-file)
  "Rename class given their file names; FROM-FILE to TO-FILE.
The file is renamed and the class or module definition is
modified."
  (interactive (list (completing-read "From: " (rails-refactoring:class-files) nil t)
                     (read-string "To: ")))
  (rails-refactoring:disclaim "Rename class")

  (let ((from (rails-refactoring:class-from-file from-file))
        (to (rails-refactoring:class-from-file to-file)))
    (message "rename file from %s to %s" from-file to-file)
    (rename-file (rails-core:file from-file) (rails-core:file to-file))
    (let ((buffer (get-file-buffer (rails-core:file from-file))))
      (when buffer (kill-buffer buffer)))

    (message "change definition from %s to %s" from to)
    (let ((buffer (get-file-buffer (rails-core:file to-file))))
      (when buffer (kill-buffer buffer)))
    (find-file (rails-core:file to-file))
    (goto-char (point-min))
    (while (re-search-forward (concat "^\\(class\\|module\\)[ \t]+" from) nil t)
      (replace-match (concat "\\1 " to) nil nil))
    (save-buffer))

  (when (interactive-p)
    (ignore-errors (rails-refactoring:query-replace (concat "\\b" (regexp-quote from)) to))
    (save-some-buffers)))

(defun rails-refactoring:rename-layout (from to)
  "Rename all named layouts from FROM to TO."
  (interactive (list (completing-read "From: " (rails-refactoring:layouts) nil t)
                     (read-string "To: ")))
  (rails-refactoring:disclaim "Rename layout")

  (mapc (lambda (from-file)
          (let ((to-file (concat to (substring from-file (length from)))))
            (message "renaming layout from %s to %s" from-file to-file)
            (rename-file (rails-core:file (format "app/views/layouts/%s" from-file))
                         (rails-core:file (format "app/views/layouts/%s" to-file)))))
        (delete-if-not (lambda (file) (string-match (concat "^" (regexp-quote from) "\\.") file))
                       (directory-files-recursive (rails-core:file "app/views/layouts"))))
  (when (interactive-p)
    (let ((case-fold-search nil))
      (ignore-errors (rails-refactoring:query-replace from to)))
    (save-some-buffers)))

(defun rails-refactoring:rename-controller (from to)
  "Rename controller from FROM to TO.  All appropriate files and
directories are renamed and `rails-refactoring:query-replace' is
started to do the rest."
  (interactive (list (completing-read "Rename controller: "
                                      (mapcar (lambda (name) (remove-postfix name "Controller"))
                                              (rails-core:controllers))
                                      nil t
                                      (ignore-errors (rails-core:current-controller)))
                     (rails-refactoring:read-class-name "To: ")))
  (rails-refactoring:disclaim "Rename controller")

  (mapc (lambda (func)
          (when (file-exists-p (rails-core:file (funcall func from)))
            (rails-refactoring:rename-class (funcall func from)
                                            (funcall func to))))
        '(rails-core:controller-file rails-core:functional-test-file rails-core:rspec-controller-file
                                     rails-core:helper-file rails-core:helper-test-file))

  (when (file-exists-p (rails-core:file (rails-core:views-dir from)))
    (let ((from-dir (rails-core:views-dir from))
          (to-dir (rails-core:views-dir to)))
      (message "rename view directory from %s to %s" from-dir to-dir)
      (rename-file (rails-core:file from-dir) (rails-core:file to-dir))))

  (rails-refactoring:rename-layout (rails-refactoring:decamelize from)
                                   (rails-refactoring:decamelize to))

  (when (interactive-p)
    (let ((case-fold-search nil))
      (rails-refactoring:query-replace (concat "\\b" (regexp-quote from))
                                     to
                                     '("app/controllers/"
                                       "app/helpers/"
                                       "app/views/"
                                       "test/functional/"
                                       "spec/controllers/"))
      (rails-refactoring:query-replace (concat "\\b\\(:?\\)" (regexp-quote (rails-refactoring:decamelize from)) "\\b")
                                       (concat "\\1" (rails-refactoring:decamelize to))
                                       '("app/controllers/"
                                         "app/helpers/"
                                         "app/views/"
                                         "test/functional/"
                                         "spec/controllers/"
                                         "config/routes.rb")))
    (save-some-buffers)))

(defun rails-refactoring:rename-model (from to)
  "Rename model from FROM to TO.  All appropriate files are
renamed and `rails-refactoring:query-replace' is started to do
the rest."
  (interactive (list (completing-read "Rename model: "
                                      (rails-core:models)
                                      nil t
                                      (ignore-errors (rails-core:current-model)))
                     (rails-refactoring:read-class-name "To: ")))
  (rails-refactoring:disclaim "Rename model")

  (mapc (lambda (func)
          (when (file-exists-p (rails-core:file (funcall func from)))
            (rails-refactoring:rename-class (funcall func from)
                                            (funcall func to))))
        '(rails-core:model-file rails-core:unit-test-file rails-core:rspec-model-file))

  (mapc (lambda (func)
          (when (file-exists-p (rails-core:file (funcall func from)))
            (rename-file (rails-core:file (funcall func from))
                         (rails-core:file (funcall func to)))))
        '(rails-core:fixture-file rails-core:rspec-fixture-file))

  (when (interactive-p)
    (let ((case-fold-search nil))
      (mapc (lambda (args)
              (let ((from (car args))
                    (to (cadr args)))
                (rails-refactoring:query-replace from to '("app/" "test/"))))
            (mapcar (lambda (func)
                      (list (concat "\\b\\(:?\\)" (regexp-quote (funcall func from)))
                            (concat "\\1" (funcall func to))))
                    (list 'identity
                          'pluralize-string
                          'rails-refactoring:decamelize
                          (lambda (val) (rails-refactoring:decamelize (pluralize-string val))))))))

  (let ((migration-name (concat "Rename" (pluralize-string from) "To" (pluralize-string to))))
    (rails-refactoring:enqueue-migration-edit migration-name
                                              'rails-refactoring:rename-table-migration-edit from to)
    (rails-script:generate-migration migration-name)))

(defun rails-refactoring:rename-table-migration-edit (from to)
  "Add rename table code to migration in current buffer."
  (let ((from-table (rails-refactoring:decamelize (pluralize-string from)))
        (to-table (rails-refactoring:decamelize (pluralize-string to))))
    (goto-char (point-min))
    (re-search-forward "\\bdef self.up")
    (end-of-line)
    (insert "\n")
    (indent-according-to-mode)
    (insert (format "rename_table :%s, :%s" from-table to-table))
    (re-search-forward "\\bdef self.down")
    (insert "\n")
    (indent-according-to-mode)
    (insert (format "rename_table :%s, :%s" to-table from-table))
    (save-buffer)))


;; Setup hooks

(defvar rails-refactoring:after-rails-script-jobs nil
  "Queue of jobs to be ran via
`rails-script:after-hook-internal'.  Jobs are ran by
`rails-refactoring:run-after-rails-script-jobs' and dequeued when
they return non nil.")

(defun rails-refactoring:run-after-rails-script-jobs ()
  "Run pending `rails-script:after-hook-internal' refactoring
jobs"
  (setq rails-refactoring:after-rails-script-jobs
        (delete-if (lambda (spec)
                     (funcall (car spec) (cadr spec) (cddr spec)))
                   rails-refactoring:after-rails-script-jobs)))

(add-hook 'rails-script:after-hook-internal 'rails-refactoring:run-after-rails-script-jobs)

(defmacro rails-refactoring:enqueue-migration-edit (migration function &rest arguments)
  "Enqueue migration edit to be run when
`rails-script:generation-migration' is finished and migration
file is available."
  (let ((migration-file (gensym)))
    `(push (cons (lambda (migration args)
                   (let ((,migration-file (rails-core:migration-file migration)))
                     (when ,migration-file
                       (with-current-buffer (find-file-noselect (rails-core:file ,migration-file))
                         (apply ,function args))
                       t)))
                 (list ,migration ,@arguments))
           rails-refactoring:after-rails-script-jobs)))


;; Tie up in UI

(require 'rails-ui)

(define-keys rails-minor-mode-map
  ((rails-global-key "R q") 'rails-refactoring:query-replace)
  ((rails-global-key "R m") 'rails-refactoring:rename-model)
  ((rails-global-key "R c") 'rails-refactoring:rename-controller)
  ((rails-global-key "R l") 'rails-refactoring:rename-layout))


(provide 'rails-refactoring)
