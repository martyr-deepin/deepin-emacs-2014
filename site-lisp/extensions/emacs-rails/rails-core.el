;;; rails-core.el --- core helper functions and macros for emacs-rails

;; Copyright (C) 2006 Dmitry Galinsky <dima dot exe at gmail dot com>

;; Authors: Dmitry Galinsky <dima dot exe at gmail dot com>,
;;          Rezikov Peter <crazypit13 (at) gmail.com>

;; Keywords: ruby rails languages oop
;; $URL$
;; $Id$

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

(eval-when-compile
  (require 'rails-lib))

(defcustom rails-core:class-dirs
  '("app/controllers"
    "app/views"
    "app/models"
    "app/helpers"
    "test/unit"
    "test/functional"
    "test/fixtures"
    "spec/controllers"
    "spec/requests"
    "spec/fixtures"
    "spec/lib"
    "spec/models"
    "lib")
  "Directories with Rails classes"
  :group 'rails
  :type '(repeat string))

(defun rails-core:class-by-file (filename)
  "Return the class associated with FILENAME.
   <rails-root>/(app/models|app/controllers|app/helpers|test/unit|test/functional|lib|spec/controllers|spec/lib|spec/models)/foo/bar_baz
                --> Foo::BarBaz"
  (let* ((case-fold-search nil)
         (path (replace-regexp-in-string
                (format
                 "\\(.*\\(%s\\)/\\)?\\([^\.]+\\)\\(.*\\)?"
                 (strings-join "\\|" rails-core:class-dirs)) "\\3" filename))
         (path (replace-regexp-in-string "/" "  " path))
         (path (replace-regexp-in-string "_" " " path)))
    (replace-regexp-in-string
     " " ""
     (replace-regexp-in-string
      "  " "::"
      (if (string-match "^ *\\([0-9]+ *\\)?[A-Z]" path)
          path
        (capitalize path))))))

(defun rails-core:file-by-class (classname &optional do-not-append-ext)
  "Return the filename associated with CLASSNAME.
If the optional parameter DO-NOT-APPEND-EXT is set this function
will not append \".rb\" to result."
  (concat (decamelize (replace-regexp-in-string "::" "/" classname))
          (unless do-not-append-ext ".rb")))

;;;;;;;;;; Files ;;;;;;;;;;

(defun rails-core:file (file-name)
  "Return the full path for FILE-NAME in a Rails directory."
  (when file-name
    (if (file-name-absolute-p file-name)
        file-name
      (rails-project:with-root
       (root)
       (concat root file-name)))))

(defun rails-core:quoted-file (file-name)
  "Return the quoted full path for FILE-NAME in a Rails directory."
  (concat "\"" (rails-core:file file-name) "\""))

(defun rails-core:find-file (file-name)
  "Open the file named FILE_NAME in a Rails directory."
  (when-bind (file (rails-core:file file-name))
       (find-file file)))

(defun rails-core:find-file-if-exist (file-name)
  "Open the file named FILE-NAME in a Rails directory only if the file exists."
  (let ((file-name (rails-core:file file-name)))
    (when (and file-name (file-exists-p file-name))
      (find-file file-name))))

(defun rails-core:find-or-ask-to-create (question file)
  "Open the file named FILE in a Rails directory if it exists. If
it does not exist, ask to create it using QUESTION as a prompt."
  (find-or-ask-to-create question (rails-core:file file)))

(defun rails-core:strip-namespace (class-name)
  "Strip namespace of CLASS-NAME, eg Foo::Bar -> Bar."
  (let ((name-list (split-string class-name "::")))
    (car (last name-list))))

;; Funtions, that retrun Rails objects full pathes

(defun rails-core:model-file (model-name)
  "Return the model file from the model name."
  (when model-name
    (let* ((stripped-model-file
            (rails-core:file-by-class
             (rails-core:strip-namespace model-name)))
           (model-file
            (rails-core:file-by-class model-name)))
      (cond
       ((file-exists-p
         (rails-core:file (concat "app/models/" model-file)))
        (concat "app/models/" model-file))
       ((file-exists-p
         (rails-core:file (concat "app/models/" stripped-model-file)))
        (concat "app/models/" stripped-model-file))
       (t
        (concat "app/models/" model-file))))))

(defun rails-core:model-exist-p (model-name)
  "Return t if model MODEL-NAME exist."
  (let ((model-file (rails-core:model-file model-name)))
    (when model-file
      (and (file-exists-p (rails-core:file model-file))
           (not (rails-core:observer-p model-name))
           (not (rails-core:mailer-p model-name))))))

(defun rails-core:controller-file (controller-name)
  "Return the path to the controller CONTROLLER-NAME."
  (when controller-name
    (let* ((basename (rails-core:file-by-class (rails-core:short-controller-name controller-name) t))
	   (exact (concat "app/controllers/" basename ".rb")))
      (if (file-exists-p (rails-core:file exact))
	exact
	(concat "app/controllers/" basename "_controller.rb")))))

(defun rails-core:controller-exist-p (controller-name)
  "Return t if controller CONTROLLER-NAME exist."
  (when controller-name
    (file-exists-p
     (rails-core:file
      (rails-core:controller-file controller-name)))))

(defun rails-core:controller-file-by-model (model)
  (when model
    (let* ((controller (pluralize-string model)))
           ;(controller (when controller (capitalize controller))))
      (setq controller
            (cond
             ((rails-core:controller-exist-p controller) controller) ;; pluralized
             ((rails-core:controller-exist-p model) model) ;; singularized
             (t (let ((controllers (rails-core:controllers t)))
                  (cond
                   ;; with namespace
                   ((find
                     (list controller model)
                     controllers
                     :test #'(lambda(x y)
                               (or
                                (string= (car x) (rails-core:strip-namespace y))
                                (string= (cadr x) (rails-core:strip-namespace y)))))))))))
      (when controller
        (rails-core:controller-file controller)))))

(defun rails-core:observer-file (observer-name)
  "Return the path to the observer OBSERVER-NAME."
  (when observer-name
    (rails-core:model-file (concat observer-name "Observer"))))

(defun rails-core:mailer-file (mailer)
  (when (and mailer
             (rails-core:mailer-p mailer))
    (rails-core:model-file mailer)))

(defun rails-core:mailer-exist-p (mailer)
  (when mailer
    (file-exists-p (rails-core:file (rails-core:mailer-file mailer)))))

(defun rails-core:migration-file (migration-name)
  "Return the model file from the MIGRATION-NAME."
  (when migration-name
    (let ((dir "db/migrate/")
          (name (replace-regexp-in-string
                 " " "_"
                 (rails-core:file-by-class migration-name))))
      (when (string-match "^[^0-9]+[^_]" name) ; try search when the name without migration number
        (let ((files (directory-files (rails-core:file dir)
                                      nil
                                      (concat "[0-9]+_" name "$"))))
          (setq name (if files
                         (car files)
                       nil))))
      (when name
        (concat dir name)))))

(defun rails-core:migration-file-by-model (model)
  (when model
    (rails-core:migration-file
     (concat "Create" (rails-core:class-by-file (pluralize-string model))))))

(defun rails-core:model-by-migration-filename (migration-filename)
  (when migration-filename
    (let ((model-name (singularize-string
                       (string=~ "[0-9]+_create_\\(\\w+\\)\.rb" (buffer-name) $1))))
      (when (and model-name
                 (rails-core:model-exist-p model-name))
        model-name))))

(defun rails-core:configuration-file (file)
  "Return the path to the configuration FILE."
  (when file
    (concat "config/" file)))

(defun rails-core:plugin-file (plugin file)
  "Return the path to the FILE in Rails PLUGIN."
  (concat "vendor/plugins/" plugin "/" file))

(defun rails-core:layout-file (layout)
  "Return the path to the layout file named LAYOUT."
  (let ((its rails-templates-list)
        filename)
    (while (and (car its)
                (not filename))
      (when (file-exists-p (format "%sapp/views/layouts/%s.%s" (rails-project:root) layout (car its)))
        (setq filename (format "app/views/layouts/%s.%s" layout (car its))))
      (setq its (cdr its)))
    filename))

(defun rails-core:js-file (js)
  "Return the path to the JavaScript file named JS."
  (concat "public/javascripts/" js ".js"))

(defun rails-core:partial-name (name)
  "Return the file name of partial NAME."
  (if (string-match "/" name)
      (concat "app/views/"
        (replace-regexp-in-string "\\([^/]*\\)$" "_\\1.html.erb" name))
    (concat (rails-core:views-dir (rails-core:current-controller))
      "_" name ".html.erb")))

(defun rails-core:view-name (name)
  "Return the file name of view NAME."
  (concat (rails-core:views-dir (rails-core:current-controller))
          name ".html.erb")) ;; BUG: will fix it

(defun rails-core:helper-file (controller)
  "Return the helper file name for the controller named
CONTROLLER."
  (if (string= "Test/TestHelper" controller)
      (rails-core:file (rails-core:file-by-class "Test/TestHelper"))
    (when controller
      (format "app/helpers/%s_helper.rb"
              (replace-regexp-in-string "_controller" ""
                                        (rails-core:file-by-class controller t))))))

(defun rails-core:helper-test-file (controller)
  (when controller
    (format "test/unit/helpers/%s_helper_test.rb" (rails-core:file-by-class controller t))))
(assert (string= "test/unit/helpers/foo/bar_quux_helper_test.rb" (rails-core:helper-test-file "Foo::BarQuux")))

(defun rails-core:functional-test-file (controller)
  "Return the functional test file name for the controller named
CONTROLLER."
  (when controller
    (format "test/functional/%s_test.rb"
            (rails-core:file-by-class (rails-core:long-controller-name controller) t))))

(defun rails-core:unit-test-file (model)
  "Return the unit test file name for the model named MODEL."
  (when model
    (format "test/unit/%s_test.rb" (rails-core:file-by-class model t))))

(defun rails-core:unit-test-exist-p (model)
  "Return the unit test file name for the model named MODEL."
  (let ((test (rails-core:unit-test-file model)))
    (when test
      (file-exists-p (rails-core:file test)))))

(defun rails-core:fixture-file (model)
  "Return the fixtures file name for the model named MODEL."
  (when model
    (format "test/fixtures/%s.yml" (pluralize-string (rails-core:file-by-class model t)))))

(defun rails-core:fixture-exist-p (model)
  (when model
    (file-exists-p
     (rails-core:file (rails-core:fixture-file model)))))

(defun rails-core:views-dir (controller)
  "Return the view directory name for the controller named CONTROLLER."
  (format "app/views/%s/" (replace-regexp-in-string "_controller" "" (rails-core:file-by-class controller t))))

(defun rails-core:stylesheet-name (name)
  "Return the file name of the stylesheet named NAME."
  (concat "public/stylesheets/" name ".css"))

(defun rails-core:controller-name (controller-file)
  "Return the class name of the controller named CONTROLLER.
   Bar in Foo dir -> Foo::Bar"
  (rails-core:class-by-file
   (if (eq (elt controller-file 0) 47) ;;; 47 == '/'
       (subseq controller-file 1)
     (let ((current-controller (rails-core:current-controller)))
       (if (string-match ":" current-controller)
     (concat (replace-regexp-in-string "[^:]*$" "" current-controller)
       controller-file)
   controller-file)))))

(defun rails-core:short-controller-name (controller)
  "Convert FooController -> Foo."
  (remove-postfix  controller "Controller" ))

(defun rails-core:long-controller-name (controller)
  "Convert Foo/FooController -> FooController."
  (if  (string-match "Controller$" controller)
      controller
    (concat controller "Controller")))

(defun rails-core:rspec-controller-files (controller)
  "Return the controller spec file name for the controller named
CONTROLLER."
  (if controller
    (remove-if-not #'file-exists-p
                   (mapcar (lambda (pattern)
                             (rails-core:file (format pattern
                                                      (rails-core:file-by-class controller t))))
                           '("spec/controllers/%s_spec.rb"
                             "spec/controllers/%s_controller_spec.rb"
                             "spec/requests/%s_spec.rb")))))

(defun rails-core:rspec-controller-file (controller)
  "Return the controller spec file name for the controller named
CONTROLLER."
  (or (car (rails-core:rspec-controller-files controller))
      (rails-core:file (format "spec/controllers/%s_controller_spec.rb" (rails-core:file-by-class controller t)))))

(defun rails-core:lib-file (lib-name)
  "Return the model file from the lib name."
  (when lib-name
    (concat "lib/" (rails-core:file-by-class lib-name))))

(defun rails-core:rspec-lib-file (lib)
  "Return the lib spec file name for the lib named LIB."
  (when lib
    (format "spec/lib/%s_spec.rb" (rails-core:file-by-class lib t))))

(defun rails-core:rspec-model-file (model)
  "Return the model spec file name for the model named MODEL."
  (when model
    (format "spec/models/%s_spec.rb" (rails-core:file-by-class model t))))

(defun rails-core:rspec-fixture-file (model)
  "Return the rspec fixtures file name for the model named MODEL."
  (when model
    (format "spec/fixtures/%s.yml" (pluralize-string (rails-core:file-by-class model t)))))

(defun rails-core:rspec-lib-exist-p (lib)
  "Return the lib spec file name for the model named MODEL."
  (let ((spec (rails-core:rspec-lib-file lib)))
    (when spec
      (file-exists-p (rails-core:file spec)))))

(defun rails-core:rspec-model-exist-p (model)
  "Return the model spec file name for the model named MODEL."
  (let ((spec (rails-core:rspec-model-file model)))
    (when spec
      (file-exists-p (rails-core:file spec)))))

(defun rails-core:rspec-fixture-exist-p (model)
  (when model
    (file-exists-p
     (rails-core:file (rails-core:rspec-fixture-file model)))))

;;;;;;;;;; Functions that return collection of Rails objects  ;;;;;;;;;;
(defun rails-core:observer-p (name)
  (when name
    (if (string-match "\\(Observer\\|_observer\\)\\(\\.rb\\)?$" name)
        t nil)))

(defun rails-core:mailer-p (name)
  (when name
    (if (string-match "\\(Mailer\\|Notifier\\|_mailer\\|_notifier\\)\\(\\.rb\\)?$" name)
        t nil)))


(defun rails-core:controllers (&optional cut-contoller-suffix)
  "Return a list of Rails controllers. Remove the '_controller'
suffix if CUT-CONTOLLER-SUFFIX is non nil."
  (mapcar
   #'(lambda (controller)
       (rails-core:class-by-file
        (if cut-contoller-suffix
            (replace-regexp-in-string "_controller\\." "." controller)
          controller)))
   (delete-if-not
    #'(lambda (controller)
        (string-match "\\(application\\|[a-z0-9_]+_controller\\)\\.rb$"
                      controller))
    (directory-files-recursive (rails-core:file "app/controllers/") nil "\\.rb$"))))

(defun rails-core:functional-tests ()
  "Return a list of Rails functional tests."
  (mapcar
   #'(lambda(it)
       (remove-postfix (rails-core:class-by-file it)
                       "ControllerTest"))
   (directory-files-recursive (rails-core:file "test/functional/") nil "\\.rb$")))

(defun rails-core:models ()
  "Return a list of Rails models."
  (mapcar
   #'rails-core:class-by-file
   (delete-if
    #'(lambda (file) (or (rails-core:observer-p file)
                         (rails-core:mailer-p file)))
    (directory-files-recursive (rails-core:file "app/models/") nil "\\.rb$"))))

(defun rails-core:unit-tests ()
  "Return a list of Rails functional tests."
  (mapcar
   #'(lambda(it)
       (remove-postfix (rails-core:class-by-file it)
                       "Test"))
   (directory-files-recursive (rails-core:file "test/unit/") nil "\\.rb$")))

(defun rails-core:observers ()
  "Return a list of Rails observers."
  (mapcar
   #'(lambda (observer) (replace-regexp-in-string "Observer$" "" observer))
   (mapcar
    #'rails-core:class-by-file
    (directory-files-recursive (rails-core:file "app/models/") nil "\\(_observer\\)\\.rb$"))))

(defun rails-core:mailers ()
  "Return a list of Rails mailers."
  (mapcar
   #'rails-core:class-by-file
   (directory-files-recursive (rails-core:file "app/models/") nil "\\(_mailer\\|_notifier\\)\\.rb$")))

(defun rails-core:helpers ()
  "Return a list of Rails helpers."
  (append
   (mapcar
    #'(lambda (helper) (replace-regexp-in-string "Helper$" "" helper))
    (mapcar
     #'rails-core:class-by-file
     (directory-files-recursive (rails-core:file "app/helpers/") nil "_helper\\.rb$")))
   (list "Test/TestHelper")))

(defun rails-core:migrations (&optional strip-numbers)
  "Return a list of Rails migrations."
  (let (migrations)
    (setq
     migrations
     (reverse
      (mapcar
       #'(lambda (migration)
           (replace-regexp-in-string "^\\([0-9]+\\)" "\\1 " migration))
       (mapcar
        #'rails-core:class-by-file
        (directory-files-recursive (rails-core:file "db/migrate") nil "^[0-9]+_.*\\.rb$")))))
    (if strip-numbers
        (mapcar #'(lambda(i) (car (last (split-string i " "))))
                migrations)
      migrations)))

(defun rails-core:migration-versions (&optional with-zero)
  "Return a list of migtaion versions as the list of strings. If
second argument WITH-ZERO is present, append the \"000\" version
of migration."
  (let ((ver (mapcar
              #'(lambda(it) (car (split-string it " ")))
              (rails-core:migrations))))
    (if with-zero
        (append ver '("000"))
      ver)))

(defun rails-core:plugins ()
  "Return a list of Rails plugins."
  (mapcar
   #'file-name-nondirectory
   (delete-if-not
    #'file-directory-p
    (directory-files (rails-core:file "vendor/plugins") t "^[^\\.]"))))

(defun rails-core:plugin-files (plugin)
  "Return a list of files in specific Rails plugin."
  (directory-files-recursive (rails-core:file (concat "vendor/plugins/" plugin)) nil  "^[^.]"))

(defun rails-core:layouts ()
  "Return a list of Rails layouts."
  (mapcar
   #'(lambda (l)
       (replace-regexp-in-string "\\.[^.]+$" "" l))
   (directory-files-recursive  (rails-core:file "app/views/layouts") nil (rails-core:regex-for-match-view))))

(defun rails-core:fixtures ()
  "Return a list of Rails fixtures."
  (mapcar
   #'(lambda (l)
       (replace-regexp-in-string "\\.[^.]+$" "" l))
   (directory-files-recursive (rails-core:file "test/fixtures/") nil "\\.yml$")))

(defun rails-core:configuration-files ()
  "Return a files of files from config folder."
  (directory-files-recursive (rails-core:file "config/")))

(defun rails-core:regex-for-match-view ()
  "Return a regex to match Rails view templates.
The file extensions used for views are defined in `rails-templates-list'."
  (format "\\.\\(%s\\)$" (strings-join "\\|" rails-templates-list)))

(defun rails-core:get-view-files (controller-class &optional action)
  "Retun a list containing the view file for CONTROLLER-CLASS#ACTION.
If the action is nil, return all views for the controller."
    (rails-project:with-root
     (root)
     (directory-files
      (rails-core:file
       (rails-core:views-dir
        (rails-core:short-controller-name controller-class))) t
        (if action
            (concat "^" action (rails-core:regex-for-match-view))
          (rails-core:regex-for-match-view)))))

(defun rails-core:extract-ancestors (classes)
  "Return the parent classes from a list of classes named CLASSES."
  (delete ""
   (uniq-list
   (mapcar (lambda (class)
       (replace-regexp-in-string
        "::[^:]*$" "::"
        (replace-regexp-in-string "^[^:]*$" "" class)))
     classes))))

(defun rails-core:models-ancestors ()
  "Return the parent classes of models."
  (rails-core:extract-ancestors (rails-core:models)))

(defun rails-core:controllers-ancestors ()
  "Return the parent classes of controllers."
  (rails-core:extract-ancestors (rails-core:controllers)))

(defun rails-core:rspec-controllers ()
  "Return a list of Rails controller specs."
  (mapcar
   #'(lambda(it)
       (remove-postfix (rails-core:class-by-file it)
                       "Spec"))
   (directory-files-recursive (rails-core:file "spec/controllers/") nil "\\.rb$")))

(defun rails-core:rspec-models ()
  "Return a list of Rails model specs."
  (mapcar
   #'(lambda(it)
       (remove-postfix (rails-core:class-by-file it)
                       "Spec"))
   (directory-files-recursive (rails-core:file "spec/models/") nil "\\.rb$")))

(defun rails-core:rspec-fixtures ()
  "Return a list of Rails RSpec fixtures."
  (mapcar
   #'(lambda (l)
       (replace-regexp-in-string "\\.[^.]+$" "" l))
   (directory-files-recursive (rails-core:file "spec/fixtures/") nil "\\.yml$")))

;;;;;;;;;; Getting Controllers/Model/Action from current buffer ;;;;;;;;;;

(defun rails-core:current-controller ()
  "Return the current Rails controller."
  (let* ((file-class (rails-core:class-by-file (buffer-file-name))))
    (unless (rails-core:mailer-p file-class)
      (case (rails-core:buffer-type)
        (:controller (rails-core:short-controller-name file-class))
        (:view (rails-core:class-by-file
                (directory-file-name (directory-of-file (buffer-file-name)))))
        (:helper (remove-postfix file-class "Helper"))
        (:functional-test (remove-postfix file-class "ControllerTest"))
        (:rspec-controller (remove-postfix file-class "Spec"))))))

(defun rails-core:current-model ()
  "Return the current Rails model."
  (let* ((file-class (rails-core:class-by-file (buffer-file-name))))
    (unless (rails-core:mailer-p file-class)
      (case (rails-core:buffer-type)
        (:migration (rails-core:model-by-migration-filename (buffer-name)))
        (:model file-class)
        (:unit-test (remove-postfix file-class "Test"))
        (:fixture (singularize-string file-class))
        (:rspec-fixture (singularize-string file-class))
        (:rspec-model (remove-postfix file-class "Spec"))))))

(defun rails-core:current-lib ()
  "Return the current lib."
  (let* ((file-class (rails-core:class-by-file (buffer-file-name))))
    (unless (rails-core:mailer-p file-class)
      (case (rails-core:buffer-type)
        (:lib file-class)
        (:rspec-lib (remove-postfix file-class "Spec"))))))

(defun rails-core:current-mailer ()
  "Return the current Rails Mailer, else return nil."
  (let* ((file-class (rails-core:class-by-file (buffer-file-name)))
         (test (remove-postfix file-class "Test"))
         (mailer-class (case (rails-core:buffer-type)
                         (:mailer    file-class)
                         (:unit-test test)
                         (:view      (rails-core:class-by-file
                                      (directory-file-name (directory-of-file (buffer-file-name))))))))
    (and (rails-core:mailer-p mailer-class) mailer-class)))

(defun rails-core:current-action ()
  "Return the current action in the current Rails controller."
  (case (rails-core:buffer-type)
    (:controller (rails-core:current-method-name))
    (:mailer (rails-core:current-method-name))
    (:view (string-match "/\\([a-z0-9_]+\\)\.[a-z]+$" (buffer-file-name))
           (match-string 1 (buffer-file-name)))))

(defun rails-core:current-helper ()
  "Return the current helper"
  (rails-core:current-controller))

(defun rails-core:current-plugin ()
  "Return the current plugin name."
  (let ((name (buffer-file-name)))
    (when (string-match "vendor\\/plugins\\/\\([^\\/]+\\)" name)
      (match-string 1 name))))

(defun rails-core:current-method-name ()
  (save-excursion
    (when (search-backward-regexp "^[ ]*def \\([a-z0-9_]+\\)" nil t)
      (match-string-no-properties 1))))

(defun rails-core:current-migration-version ()
  "Return the current migration version"
  (let ((name (buffer-file-name)))
    (when (string-match "db\\/migrate\\/\\([0-9]+\\)[a-z0-9_]+\.[a-z]+$" name)
      (match-string 1 name))))

(defun rails-core:grep-from-file (file regexp replacement)
  (and file
       (file-exists-p file)
       (with-temp-buffer
         (insert-file-contents file)
         (goto-char (point-min))
         (and (re-search-forward regexp nil t)
              (match-substitute-replacement replacement)))))

(defun rails-core:grep-from-runner (stmt regexp replacement)
  (and (rails-core:file "script/runner")
       (with-temp-buffer
         (let ((default-directory (rails-project:root)))
           (shell-command (concat rails-ruby-command " script/runner '" stmt "'")
                          (current-buffer)
                          nil)
           (goto-char (point-min))
           (and (re-search-forward regexp)
                (match-substitute-replacement replacement))))))

(defun rails-core:current-rails-version ()
  "Return the rails version of the current project"
  (let* ((version-rb-re (concat "MAJOR[[:space:]]+=[[:space:]]+\\([[:digit:]]+\\)"
                                "[[:space:]]+MINOR[[:space:]]+=[[:space:]]\\([[:digit:]]+\\)"
                                "[[:space:]]+TINY[[:space:]]+=[[:space:]]\\([[:digit:]]+\\)"))
         (fns
          (list
           (lambda ()
             (rails-core:grep-from-file (rails-core:file "Gemfile.lock")
                                        "^[[:space:]]+rails[[:space:]]+(\\([[:digit:]]+\.[[:digit:]]+\.[[:digit:]]+\\))"
                                        "\\1"))
           (lambda ()
             (rails-core:grep-from-file (rails-core:file "config/environment.rb")
                                        "^RAILS_GEM_VERSION[[:space:]]+=[[:space:]]\\(['\"]\\)\\(.*?\\)\\1"
                                        "\\2"))
           (lambda ()
             (rails-core:grep-from-file (rails-core:file "vendor/rails/railties/lib/rails/version.rb")
                                        version-rb-re
                                        "\\1.\\2.\\3"))
           (lambda ()
             (rails-core:grep-from-file (rails-core:file "vendor/rails/railties/lib/rails_version.rb")
                                        version-rb-re
                                        "\\1.\\2.\\3"))
           (lambda ()
             (rails-core:grep-from-runner "puts Rails::VERSION::STRING"
                                          "^[[:digit:]]+\.[[:digit:]]+\.[[:digit:]]+$"
                                          "\\&"))))
         (version nil))
    (while (and (not version) fns)
      (setq version (funcall (car fns))
            fns (cdr fns)))
    version))

(defun rails-core:current-rails-major-version ()
  "Return project major version of rails."
  (let ((version (rails-core:current-rails-version)))
    (when (string-match "^[0-9]+" version)
      (string-to-number (match-string 0 version)))))

;;;;;;;;;; Determination of buffer type ;;;;;;;;;;

(defun rails-core:buffer-file-match (regexp)
  "Match the current buffer file name to RAILS_ROOT + REGEXP."
  (when-bind (file (rails-core:file regexp))
             (string-match file
                           (buffer-file-name (current-buffer)))))

(defun rails-core:buffer-type ()
  "Return the type of the current Rails file or nil if the type
cannot be determinated."
  (loop for (type dir func) in rails-directory<-->types
        when (and (rails-core:buffer-file-match dir)
                  (if func
                      (apply func (list (buffer-file-name (current-buffer))))
                    t))
        do (return type)))


;;;;;;;;;; Rails minor mode Buttons ;;;;;;;;;;

(define-button-type 'rails-button
  'follow-link t
  'action #'rails-core:button-action)

(defun rails-core:button-action (button)
  (let* ((file-name (button-get button :rails:file-name))
         (line-number (button-get button :rails:line-number))
         (file (rails-core:file file-name)))
    (when (and file
               (file-exists-p file))
      (find-file-other-window file)
      (when line-number
        (goto-line line-number)))))

;;;;;;;;;; Rails minor mode logs ;;;;;;;;;;

(defun rails-log-add (message)
  "Add MESSAGE to the Rails minor mode log in RAILS_ROOT."
  (rails-project:with-root
   (root)
   (append-string-to-file (rails-core:file "log/rails-minor-mode.log")
                          (format "%s: %s\n"
                                  (format-time-string "%Y/%m/%d %H:%M:%S") message))))

(defun rails-logged-shell-command (command buffer)
  "Execute a shell command in the buffer and write the results to
the Rails minor mode log."
  (shell-command (format "%s %s" rails-ruby-command command) buffer)
  (rails-log-add
   (format "\n%s> %s\n%s" (rails-project:name)
           command (buffer-string-by-name buffer))))

;;;;;;;;;; Rails menu ;;;;;;;;;;

(defun rails-core:menu-separator ()
  (unless (rails-use-text-menu) 'menu (list "--" "--")))

(if (fboundp 'completion-posn-at-point-as-event)
    (defun rails-core:menu-position ()
      (completion-posn-at-point-as-event nil nil nil (+ (frame-char-height) 2)))
  (defun rails-core:menu-position ()
    (list '(300 50) (get-buffer-window (current-buffer)))))

;; fixup emacs-rails menu specs to work with tmm-prompt
(defun rails-core:tmm-menu (menu)
  (symbol-name (tmm-prompt (cons (car menu)
				 (mapcar (lambda (pane)
					   (cons (car pane)
						 (mapcar (lambda (item)
							   (if (symbolp (cdr item))
							       item
							     (cons (car item)
								   (intern (cdr item)))))
							 (cdr pane))))
					 (cdr menu))))))

(defun rails-core:ido-menu (menu)
  (let* ((prompt (car (car (cdr menu))))
         (mappings (cdr (car (cdr menu))))
         (choices (delete-if #'not (mapcar (lambda (item) (car item)) mappings)))
         (default (if (find-if (lambda (val) (string= (word-at-point) val)) choices)
                    (word-at-point)))
         (result (ido-completing-read prompt choices nil nil default)))
    (or (cdr (assoc result mappings)) result)))

(defun rails-core:menu (menu)
  "Show a menu."
  (let ((result
         (if (rails-use-text-menu)
           (funcall (or rails-text-menu-function
                        (and (boundp 'ido-mode) ido-mode #'rails-core:ido-menu)
                        #'rails-core:tmm-menu) menu)
           (x-popup-menu (rails-core:menu-position)
                         (rails-core:prepare-menu menu)))))
    (if (listp result)
        (first result)
      result)))

(defvar rails-core:menu-letters-list
  (let ((res '()))
    (loop for i from (string-to-char "1") upto (string-to-char "9")
          do (add-to-list 'res (char-to-string i) t))
    (loop for i from (string-to-char "a") upto (string-to-char "z")
          do (add-to-list 'res (char-to-string i) t))
    res)
  "List contains 0-9a-z letter")

(defun rails-core:prepare-menu (menu)
  "Append a prefix to each label of menu-item from MENU."
  (let ((title (car menu))
        (menu (cdr menu))
        (result '())
        (result-line '())
        (letter 0))
    (dolist (line menu)
      (setq result-line '())
      (dolist (it line)
        (typecase it
          (cons
           (if (and (string= (car (rails-core:menu-separator)) (car it))
                    (string= (cadr (rails-core:menu-separator)) (cadr it)))
               (add-to-list 'result-line it t)
             (progn
               (add-to-list 'result-line (cons
                                          (format "%s) %s"
                                                  (nth letter rails-core:menu-letters-list)
                                                  (car it))
                                          (cdr it))
                            t)
               (setq letter (+ 1 letter)))))
          (t
           (add-to-list 'result-line it t))))
      (add-to-list 'result result-line t))
    (cons title result)))

;;;;;;;;;; Misc ;;;;;;;;;;

(defun rails-core:erb-block-string ()
  "Return the contents of the current ERb block."
  (save-excursion
    (save-match-data
      (let ((start (point)))
        (search-backward-regexp "<%[=]?")
        (let ((from (match-end 0)))
          (search-forward "%>")
          (let ((to (match-beginning 0)))
            (when (>= to start)
              (buffer-substring-no-properties from to))))))))

(defun rails-core:rhtml-buffer-p ()
  "Return non nil if the current buffer is rhtml file."
  (string-match "\\.rhtml\\|\\.html\\.erb$" (buffer-file-name)))

(defun rails-core:spec-exist-p ()
  "Return non nil if spec directory is exist."
  (file-exists-p (rails-core:file "spec")))

(defun rails-core:prepare-command (command)
  (if (and rails-rake-use-bundler-when-possible (file-exists-p (rails-core:file "Gemfile")))
      (concat "bundle exec " command)
    command))

(provide 'rails-core)
