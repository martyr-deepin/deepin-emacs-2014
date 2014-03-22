;;; rails-ui.el --- emacs-rails user interface

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


;;;;;;;;;; Some init code ;;;;;;;;;;

(defconst rails-minor-mode-log-menu-bar-map
  (let ((map (make-sparse-keymap)))
    (define-keys map
      ([test]      '("test.log"         . rails-log:open-test))
      ([pro]       '("production.log"   . rails-log:open-production))
      ([dev]       '("development.log"  . rails-log:open-development))
      ([separator] '("---"))
      ([open]      '("Open Log File..." . rails-log:open)))
    map))

(defconst rails-minor-mode-config-menu-bar-map
  (let ((map (make-sparse-keymap)))
    (define-keys map
      ([routes]      '("routes.rb" .
                       (lambda () (interactive)
                         (rails-core:find-file "config/routes.rb"))))
      ([environment] '("environment.rb" .
                       (lambda() (interactive)
                         (rails-core:find-file "config/environment.rb"))))
      ([database]    '("database.yml" .
                       (lambda() (interactive)
                         (rails-core:find-file "config/database.yml"))))
      ([boot]        '("boot.rb" .
                       (lambda() (interactive)
                         (rails-core:find-file "config/boot.rb"))))
      ([env] (cons "environments" (make-sparse-keymap "environments")))
      ([env test]        '("test.rb" .
                           (lambda() (interactive)
                             (rails-core:find-file "config/environments/test.rb"))))
      ([env production]  '("production.rb" .
                           (lambda() (interactive)
                             (rails-core:find-file "config/environments/production.rb"))))
      ([env development] '("development.rb" .
                           (lambda()(interactive)
                             (rails-core:find-file "config/environments/development.rb")))))
    map))

(defconst rails-minor-mode-nav-menu-bar-map
  (let ((map (make-sparse-keymap)))
    (define-keys map
      ([goto-fixtures]    '("Go to Fixtures"         . rails-nav:goto-fixtures))
      ([goto-plugins]     '("Go to Plugins"          . rails-nav:goto-plugins))
      ([goto-migrate]     '("Go to Migrations"       . rails-nav:goto-migrate))
      ([goto-layouts]     '("Go to Layouts"          . rails-nav:goto-layouts))
      ([goto-stylesheets] '("Go to Stylesheets"      . rails-nav:goto-stylesheets))
      ([goto-javascripts] '("Go to Javascripts"      . rails-nav:goto-javascripts))
      ([goto-helpers]     '("Go to Helpers"          . rails-nav:goto-helpers))
      ([goto-mailers]     '("Go to Mailers"          . rails-nav:goto-mailers))
      ([goto-observers]   '("Go to Observers"        . rails-nav:goto-observers))
      ([goto-unit-tests]  '("Go to Unit Tests"       . rails-nav:goto-unit-tests))
      ([goto-func-tests]  '("Go to Functional Tests" . rails-nav:goto-functional-tests))
      ([goto-models]      '("Go to Models"           . rails-nav:goto-models))
      ([goto-controllers] '("Go to Controllers"      . rails-nav:goto-controllers))
      ([goto-rspec-controllers] '("Go to RSpec Controllers" . rails-nav:goto-rspec-controllers))
      ([goto-rspec-lib]         '("Go to RSpec Lib"         . rails-nav:goto-rspec-lib))
      ([goto-rspec-models]      '("Go to RSpec Models"      . rails-nav:goto-rspec-models))
      ([goto-rspec-fixtures]    '("Go to RSpec Fixtures"    . rails-nav:goto-rspec-fixtures)))
    map))

(defconst rails-minor-mode-tests-menu-bar-map
  (let ((map (make-sparse-keymap)))
    (define-keys map
      ([kill-current] '(menu-item "Kill Test" rails-script:kill-script :enable (rails-script:running-p)))
      ([separator0]   '("--"))
      ([integration] '("Integration Tests" . rails-test:run-integration))
      ([unit]        '("Unit Tests"        . rails-test:run-units))
      ([functional]  '("Functional Tests"  . rails-test:run-functionals))
      ([recent]      '("Recent Tests"      . rails-test:run-recent))
      ([tests]       '("All"               . rails-test:run-all))
      ([separator1]   '("--"))
      ([toggle]      '(menu-item "Toggle Output Window" rails-script:toggle-output-window
                                 :enable (get-buffer rails-script:buffer-name)))
      ([run-current] '("Test Current Model/Controller/Mailer" . rails-test:run-current))
      ([run]         '("Run Tests ..."                        . rails-test:run)))
    map))

(defconst rails-minor-mode-db-menu-bar-map
  (let ((map (make-sparse-keymap)))
    (define-keys map
      ([clone-db]    '("Clone Development DB to Test DB" . rails-rake:clone-development-db-to-test-db))
      ([load-schema] '("Load schema.rb to DB"               . (lambda() (interactive) (rails-rake:task "db:schema:load"))))
      ([dump-schema] '("Dump DB to schema.rb"               . (lambda() (interactive) (rails-rake:task "db:schema:dump"))))
      ([sep]         '("--"))
      ([prev]        '("Migrate to Previous Version" . rails-rake:migrate-to-prev-version))
      ([version]     '("Migrate to Version ..."      . rails-rake:migrate-to-version))
      ([migrate]     '("Migrate"                     . rails-rake:migrate)))
    map))

(define-keys rails-minor-mode-menu-bar-map

  ([rails] (cons "RoR" (make-sparse-keymap "RubyOnRails")))

  ([rails rails-customize] '("Customize" . (lambda () (interactive) (customize-group 'rails))))
  ([rails separator0] '("--"))
  ([rails scm-status]        '("SCM Status"             . rails-scm-status-into-root))
  ([rails api-doc]           '("Rails API Doc at Point" . rails-browse-api-at-point))
  ([rails sql]               '("SQL Rails Buffer"       . rails-run-sql))
  ([rails tag]               '("Update TAGS File"       . rails-create-tags))
  ([rails ri]                '("Search Documentation"   . rails-search-doc))
  ([rails goto-file-by-line] '("Go to File by Line"     . rails-goto-file-on-current-line))
  ([rails switch-file-menu]  '("Switch file Menu..."    . rails-lib:run-secondary-switch))
  ([rails switch-file]       '("Switch File"            . rails-lib:run-primary-switch))
  ([rails separator1]        '("--"))

  ([rails scr] (cons "Scripts" (make-sparse-keymap "Scripts")))
  ([rails scr kill]    '(menu-item "Kill current script" rails-script:kill-script :enable (rails-script:running-p)))
  ([rails scr separator]  '("--"))

  ([rails scr gen] (cons "Generate" (make-sparse-keymap "Generate")))
  ([rails scr destr] (cons "Destroy" (make-sparse-keymap "Generators")))

  ([rails scr destr resource]   '("Resource"        . rails-script:destroy-resource))
  ([rails scr destr observer]   '("Observer"        . rails-script:destroy-observer))
  ([rails scr destr mailer]     '("Mailer"          . rails-script:destroy-mailer))
  ([rails scr destr plugin]     '("Plugin"          . rails-script:destroy-plugin))
  ([rails scr destr migration]  '("Migration"       . rails-script:destroy-migration))
  ([rails scr destr scaffold]   '("Scaffold"        . rails-script:destroy-scaffold))
  ([rails scr destr model]      '("Model"           . rails-script:destroy-model))
  ([rails scr destr controller] '("Controller"      . rails-script:destroy-controller))
  ([rails scr destr separator]  '("--"))
  ([rails scr destr run]        '("Run Destroy ..." . rails-script:destroy))

  ([rails scr gen resource]   '("Resource"         . rails-script:generate-resource))
  ([rails scr gen observer]   '("Observer"         . rails-script:generate-observer))
  ([rails scr gen mailer]     '("Mailer"           . rails-script:generate-mailer))
  ([rails scr gen plugin]     '("Plugin"           . rails-script:generate-plugin))
  ([rails scr gen migration]  '("Migration"        . rails-script:generate-migration))
  ([rails scr gen scaffold]   '("Scaffold"         . rails-script:generate-scaffold))
  ([rails scr gen model]      '("Model"            . rails-script:generate-model))
  ([rails scr gen controller] '("Controller"       . rails-script:generate-controller))
  ([rails scr gen separator]  '("--"))
  ([rails scr gen run]        '("Run Generate ..." . rails-script:generate))

  ([rails scr break]   '("Breakpointer"         . rails-script:breakpointer))
  ([rails scr console] '("Console"              . rails-script:console))
  ([rails scr rake]    '("Rake..."              . rails-rake:task))

  ([rails nav]    (cons "Navigation"    rails-minor-mode-nav-menu-bar-map))
  ([rails config] (cons "Configuration" rails-minor-mode-config-menu-bar-map))
  ([rails log]    (cons "Log Files"     rails-minor-mode-log-menu-bar-map))

  ([rails ws] (cons "WebServer" (make-sparse-keymap "WebServer")))

  ([rails ws use-webrick]  '(menu-item "Use WEBrick" (lambda() (interactive)
                                                       (rails-ws:switch-default-server-type "webrick"))
                                       :button (:toggle . (rails-ws:default-server-type-p "webrick"))))
  ([rails ws use-lighttpd] '(menu-item "Use Lighty" (lambda() (interactive)
                                                      (rails-ws:switch-default-server-type "lighttpd"))
                                       :button (:toggle . (rails-ws:default-server-type-p "lighttpd"))))
  ([rails ws use-mongrel]  '(menu-item "Use Mongrel" (lambda() (interactive)
                                                       (rails-ws:switch-default-server-type "mongrel"))
                                       :button (:toggle . (rails-ws:default-server-type-p "mongrel"))))
  ([rails ws use-thin]  '(menu-item "Use Thin" (lambda() (interactive)
                                                    (rails-ws:switch-default-server-type "thin"))
                                       :button (:toggle . (rails-ws:default-server-type-p "thin"))))
  ([rails ws separator] '("--"))

  ([rails ws brows]      '(menu-item "Open Browser..." rails-ws:open-browser-on-controller
                                     :enable (rails-ws:running-p)))
  ([rails ws auto-brows] '(menu-item "Open Browser on Current Action" rails-ws:auto-open-browser
                                     :enable (rails-ws:running-p)))
  ([rails ws url]        '(menu-item "Open Browser" rails-ws:open-browser
                                     :enable (rails-ws:running-p)))
  ([rails ws separator2] '("--"))

  ([rails ws test]        '(menu-item "Start Test" rails-ws:start-test
                                      :enable (not (rails-ws:running-p))))
  ([rails ws production]  '(menu-item "Start Production" rails-ws:start-production
                                      :enable (not (rails-ws:running-p))))
  ([rails ws development] '(menu-item "Start Development" rails-ws:start-development
                                      :enable (not (rails-ws:running-p))))
  ([rails ws separator3] '("--"))

  ([rails ws status]  '(menu-item "Print Status"                                     rails-ws:print-status))
  ([rails ws default] '(menu-item "Start/Stop Web Server (With Default Environment)" rails-ws:toggle-start-stop))
  )

(defcustom rails-minor-mode-local-prefix-key "\C-c ,"
  "Key prefix for rails commands that depend on current file (controller, migration etc.)."
  :group 'rails)

(defmacro rails-local-key (key)
  `(kbd ,(concat rails-minor-mode-local-prefix-key " " key)))

(defcustom rails-minor-mode-global-prefix-key "\C-c , ,"
  "Key following `rails-minor-mode-prefix-key` prefixing global rails-mode commands."
  :group 'rails)

(defmacro rails-global-key (key)
  `(kbd ,(concat rails-minor-mode-global-prefix-key " " key)))

(defconst rails-minor-mode-test-current-method-key (rails-global-key ", ,"))

(defvar rails-minor-mode-map
  (make-sparse-keymap))

(define-keys rails-minor-mode-map
  ([menu-bar] rails-minor-mode-menu-bar-map)
  ([menu-bar rails-tests] (cons "Tests" rails-minor-mode-tests-menu-bar-map))
  ([menu-bar rails-db] (cons "Database" rails-minor-mode-db-menu-bar-map))

  ;; Goto
  ((rails-global-key "g m") 'rails-nav:goto-models)
  ((rails-global-key "g c") 'rails-nav:goto-controllers)
  ((rails-global-key "g o") 'rails-nav:goto-observers)
  ((rails-global-key "g n") 'rails-nav:goto-mailers)
  ((rails-global-key "g h") 'rails-nav:goto-helpers)
  ((rails-global-key "g l") 'rails-nav:goto-layouts)
  ((rails-global-key "g s") 'rails-nav:goto-stylesheets)
  ((rails-global-key "g j") 'rails-nav:goto-javascripts)
  ((rails-global-key "g g") 'rails-nav:goto-migrate)
  ((rails-global-key "g p") 'rails-nav:goto-plugins)
  ((rails-global-key "g x") 'rails-nav:goto-fixtures)
  ((rails-global-key "g f") 'rails-nav:goto-functional-tests)
  ((rails-global-key "g u") 'rails-nav:goto-unit-tests)
  ((rails-global-key "g r c") 'rails-nav:goto-rspec-controllers)
  ((rails-global-key "g r f") 'rails-nav:goto-rspec-fixtures)
  ((rails-global-key "g r l") 'rails-nav:goto-rspec-lib)
  ((rails-global-key "g r m") 'rails-nav:goto-rspec-models)

  ;; Switch
  ((kbd "<M-S-up>")      'rails-lib:run-primary-switch)
  ((kbd "<M-S-down>")    'rails-lib:run-secondary-switch)
  ((rails-global-key "<up>")     'rails-lib:run-primary-switch)
  ((rails-global-key "<down>")   'rails-lib:run-secondary-switch)
  ((rails-global-key "<return>")    'rails-goto-file-on-current-line)

  ;; Scripts & SQL
  ((rails-global-key "k")   'rails-script:kill-script)
  ((rails-global-key "e")   'rails-script:generate)
  ((rails-global-key "x")   'rails-script:destroy)
  ((rails-global-key "s c") 'rails-script:console)
  ((kbd "C-c C-s")        'rails-script:console)
  ((kbd "C-c C-z")        'rails-script:console)
  ((rails-global-key "s b") 'rails-script:breakpointer)
  ((rails-global-key "s s") 'rails-run-sql)
  ((rails-global-key "w s") 'rails-ws:toggle-start-stop)
  ((rails-global-key "w d") 'rails-ws:start-development)
  ((rails-global-key "w p") 'rails-ws:start-production)
  ((rails-global-key "w t") 'rails-ws:start-test)
  ((rails-global-key "w i") 'rails-ws:print-status)
  ((rails-global-key "w a") 'rails-ws:auto-open-browser)

  ;; Rails finds
  ((rails-global-key "f m") 'rails-find:models)
  ((rails-global-key "f c") 'rails-find:controller)
  ((rails-global-key "f h") 'rails-find:helpers)
  ((rails-global-key "f l") 'rails-find:layout)
  ((rails-global-key "f s") 'rails-find:stylesheets)
  ((rails-global-key "f j") 'rails-find:javascripts)
  ((rails-global-key "f g") 'rails-find:migrate)
  ((rails-global-key "f b") 'rails-find:lib)
  ((rails-global-key "f t") 'rails-find:tasks)
  ((rails-global-key "f v") 'rails-find:view)
  ((rails-global-key "f d") 'rails-find:db)
  ((rails-global-key "f p") 'rails-find:public)
  ((rails-global-key "f f") 'rails-find:fixtures)
  ((rails-global-key "f o") 'rails-find:config)
  ;; Spec finds
  ((rails-global-key "f r s") 'rails-find:spec)
  ((rails-global-key "f r c") 'rails-find:spec-controllers)
  ((rails-global-key "f r m") 'rails-find:spec-models)
  ((rails-global-key "f r h") 'rails-find:spec-helpers)
  ((rails-global-key "f r v") 'rails-find:spec-views)
  ((rails-global-key "f r f") 'rails-find:spec-fixtures)

  ((rails-global-key "d m") 'rails-rake:migrate)
  ((rails-global-key "d v") 'rails-rake:migrate-to-version)
  ((rails-global-key "d p") 'rails-rake:migrate-to-prev-version)
  ((rails-global-key "d u") 'rails-rake:migration-version-up)
  ((rails-global-key "d d") 'rails-rake:migration-version-down)
  ((rails-global-key "d t") 'rails-rake:clone-development-db-to-test-db)

  ;; Tests
  ((rails-global-key "r")   'rails-rake:task)
  ((rails-global-key "t")   'rails-test:run)
  ((rails-global-key ".")   'rails-test:run-current)
  ((rails-global-key "/")   'rails-test:rerun-single)
  ((rails-global-key "y i") 'rails-test:run-integration)
  ((rails-global-key "y u") 'rails-test:run-units)
  ((rails-global-key "y f") 'rails-test:run-functionals)
  ((rails-global-key "#")   'rails-test:run-recent)
  ((rails-global-key "y a") 'rails-test:run-all)

  ;; RSpec
  ((rails-global-key "z f") 'rails-spec:run-files)
  ((rails-global-key "z .") 'rails-spec:run-current)
  ((rails-global-key "z a") 'rails-spec:run-all)
  ((rails-global-key "z l") 'rails-spec:run-last)
  ((rails-global-key "z s") 'rails-spec:run-this-spec)

  ;; Log files
  ((rails-global-key "l")    'rails-log:open)
  ((rails-global-key "o t")    'rails-log:open-test)
  ((rails-global-key "o p")    'rails-log:open-production)
  ((rails-global-key "o d")    'rails-log:open-development)

  ;; Tags
  ((rails-global-key "\C-t") 'rails-create-tags)

  ;; Documentation
  ((rails-global-key "/")        'rails-script:toggle-output-window)

  ;; Other
  ((rails-global-key "s")       'rails-grep-project)
  ((rails-global-key "C-M-z")   'rails-shell)
  ((rails-global-key "M-z")     'rails-shell)
  ([f9]                  'rails-scm-status-into-root))

;; Global keys and menubar

(global-set-key (rails-global-key "j") 'rails-script:create-project)

(when-bind (map (lookup-key global-map  [menu-bar file]))
  (define-key-after
    map
    [create-rails-project]
    '("Create Rails Project" . rails-script:create-project) 'insert-file))

;; mode-line info

(defvar rails-ui:mode-line-script-name ""
  "A short name for the currently running script")

(defvar rails-ui:num-errors 0
  "Number of errors in the latest test task.")
(defvar rails-ui:num-failures 0
  "Number of failures in the latest test task.")
(defvar rails-ui:num-ok 0
  "Number of tests in the latest test task.")

(defun rails-ui:reset-error-count ()
  "Reset number of tests/errors/failures to 0."
  (setq rails-ui:num-ok 0
	rails-ui:num-errors 0
	rails-ui:num-failures 0))

(defvar rails-ui:mode-line '(:propertize (" " rails-ui:mode-line-script-string rails-ui:mode-line-test-results))
  "`rails-mode''s mode-line construct")

(put 'rails-ui:mode-line 'risky-local-variable t)

(defvar rails-ui:idle-script-line (propertize "idle"
					      'help-echo "No script running.\nmouse-1: toggle output window"
					      'mouse-face 'mode-line-highlight
					      'local-map '(keymap (mode-line keymap
							 (mouse-1 . rails-script:toggle-output-window)))))

(defvar rails-ui:mode-line-script-string
  '(:eval (if (rails-script:running-p)
	      (propertize rails-ui:mode-line-script-name
			  'help-echo (concat "running "
					     rails-script:running-script-name
					     "\nmouse-1: toggle output window\nmouse-2: kill script")
			  'face 'bold
			  'mouse-face 'mode-line-highlight
			  'local-map '(keymap (mode-line keymap
							 (mouse-1 . rails-script:toggle-output-window))
					      (mouse-2 . rails-script:kill-script)))
	    rails-ui:idle-script-line)))

(put 'rails-ui:mode-line-script-string 'risky-local-variable t)

(defcustom rails-ui:show-mode-line
  't
  "Show test and script status in the mode-line"
  :type 'boolean
  :group 'rails
  :tag "Rails show mode line")

(defvar rails-ui:mode-line-test-results
  '(:eval
    (let ((results (propertize
		    (format "%dE%dF%d" rails-ui:num-ok rails-ui:num-errors rails-ui:num-failures)
		    'help-echo (format "%d Tests, %d Errors, %d Failures."
				       rails-ui:num-ok
				       rails-ui:num-errors
				       rails-ui:num-failures))))
      (if (> (+ rails-ui:num-errors rails-ui:num-failures) 0)
	  (setq results (propertize results 'face 'compilation-error)))
      (concat " [" results "]"))))

(put 'rails-ui:mode-line-test-results 'risky-local-variable t)

(defun rails-ui:enable-mode-line ()
  "Add the rails mode-line to the global mode-string."
  (interactive)
  (unless (memq 'rails-ui:mode-line global-mode-string)
    (setq global-mode-string
	  (append global-mode-string '(rails-ui:mode-line)))))

(defun rails-ui:disable-mode-line ()
  "Remove the rails mode-line from the global mode-string."
  (interactive)
  (setq global-mode-string
	(remove 'rails-ui:mode-line global-mode-string)))

(add-hook 'rails-minor-mode-hook (lambda ()
				   (if rails-ui:show-mode-line
				       (rails-ui:enable-mode-line))))

(provide 'rails-ui)
