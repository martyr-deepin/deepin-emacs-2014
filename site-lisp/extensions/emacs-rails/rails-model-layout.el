;;; rails-model-layout.el ---

;; Copyright (C) 2006 Dmitry Galinsky <dima dot exe at gmail dot com>

;; Authors: Dmitry Galinsky <dima dot exe at gmail dot com>

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

;;; Code:

(defun rails-model-layout:keymap (type)
  (let* ((name (capitalize (substring (symbol-name type) 1)))
         (map (make-sparse-keymap))
         (menu (make-sparse-keymap)))
    (when type
      (define-keys menu
        ([goto-model]      '(menu-item "Go to Model"
                                       rails-model-layout:switch-to-model
                                       :enable (and (not (eq (rails-core:buffer-type) :model))
                                                    (rails-core:model-exist-p (rails-core:current-model)))))
        ([goto-utest]      '(menu-item "Go to Unit Test"
                                       rails-model-layout:switch-to-unit-test
                                       :enable (and (not (eq (rails-core:buffer-type) :unit-test))
                                                    (rails-core:unit-test-exist-p (or (rails-core:current-model)
                                                                                      (rails-core:current-mailer))))))
        ([goto-rspec]      '(menu-item "Go to RSpec"
                                       rails-model-layout:switch-to-rspec-model
                                       :enable (and (not (eq (rails-core:buffer-type) :rspec-model))
                                                    (rails-core:rspec-model-exist-p (or (rails-core:current-model)
                                                                                      (rails-core:current-mailer))))))
        ([goto-migration]  '(menu-item "Go to Migration"
                                       rails-model-layout:switch-to-migration
                                       :enable (and (not (eq (rails-core:buffer-type) :migration))
                                                    (rails-core:migration-file-by-model (rails-core:current-model)))))
        ([goto-controller] '(menu-item "Go to Controller"
                                       rails-model-layout:switch-to-controller
                                       :enable (rails-core:controller-file-by-model (rails-core:current-model))))
        ([goto-fixture]    '(menu-item "Go to Fixture"
                                       rails-model-layout:switch-to-fixture
                                       :enable (and (not (eq (rails-core:buffer-type) :fixture))
                                                    (rails-core:fixture-exist-p (rails-core:current-model)))))
        ([goto-rspec-fixture]    '(menu-item "Go to RSpec Fixture"
                                       rails-model-layout:switch-to-rspec-fixture
                                       :enable (and (not (eq (rails-core:buffer-type) :rspec-fixture))
                                                    (rails-core:rspec-fixture-exist-p (rails-core:current-model)))))
        ([goto-mailer]     '(menu-item "Go to Mailer"
                                       rails-model-layout:switch-to-mailer
                                       :enable (rails-core:mailer-exist-p (rails-core:current-mailer)))))
      (define-keys map
        ((rails-local-key "m")         'rails-model-layout:switch-to-model)
        ((rails-local-key "u")         'rails-model-layout:switch-to-unit-test)
        ((rails-local-key "r")         'rails-model-layout:switch-to-rspec-model)
        ((rails-local-key "g")         'rails-model-layout:switch-to-migration)
        ((rails-local-key "c")         'rails-model-layout:switch-to-controller)
        ((rails-local-key "x")         'rails-model-layout:switch-to-fixture)
        ((rails-local-key "z")         'rails-model-layout:switch-to-rspec-fixture)
        ((rails-local-key "n")         'rails-model-layout:switch-to-mailer)
        ([menu-bar rails-model-layout] (cons name menu))))
    map))

(defun rails-model-layout:switch-to (type)
  (let* ((name (capitalize (substring (symbol-name type) 1)))
         (model (rails-core:current-model))
         (controller (rails-core:current-controller))
         (mailer (rails-core:current-mailer))
         (item (if controller controller model))
         (item (case type
                 (:mailer (rails-core:mailer-file mailer))
                 (:controller (rails-core:controller-file-by-model model))
                 (:fixture (rails-core:fixture-file model))
                 (:rspec-fixture (rails-core:rspec-fixture-file model))
                 (:unit-test (rails-core:unit-test-file item))
                 (:rspec-model (rails-core:rspec-model-file item))
                 (:model (rails-core:model-file model))
                 (:migration (rails-core:migration-file-by-model model)))))
    (if item
      (find-or-ask-to-create (format "%s does not exists do you want to create it? " item)
                             (rails-core:file item))
      (message "%s not found" name))))

(defun rails-model-layout:switch-to-mailer () (interactive) (rails-model-layout:switch-to :mailer))
(defun rails-model-layout:switch-to-controller () (interactive) (rails-model-layout:switch-to :controller))
(defun rails-model-layout:switch-to-fixture () (interactive) (rails-model-layout:switch-to :fixture))
(defun rails-model-layout:switch-to-rspec-fixture () (interactive) (rails-model-layout:switch-to :rspec-fixture))
(defun rails-model-layout:switch-to-unit-test () (interactive) (rails-model-layout:switch-to :unit-test))
(defun rails-model-layout:switch-to-rspec-model () (interactive) (rails-model-layout:switch-to :rspec-model))
(defun rails-model-layout:switch-to-model () (interactive) (rails-model-layout:switch-to :model))
(defun rails-model-layout:switch-to-migration () (interactive) (rails-model-layout:switch-to :migration))

(defun rails-model-layout:menu ()
  (interactive)
  (let* ((item (list))
         (type (rails-core:buffer-type))
         (title (capitalize (substring (symbol-name type) 1)))
         (model (rails-core:current-model))
         (controller (pluralize-string model))
         (mailer (rails-core:current-mailer)))
    (when model
      (when (and (not (eq type :migration))
                 (rails-core:migration-file-by-model model))
        (add-to-list 'item (cons "Migration" :migration)))
      (unless (eq type :fixture)
        (add-to-list 'item (cons "Fixture" :fixture)))
      (unless (eq type :rspec-fixture)
        (add-to-list 'item (cons "RSpec Fixture" :rspec-fixture)))
      (when (rails-core:controller-exist-p controller)
        (add-to-list 'item (cons "Controller" :controller)))
      (unless (eq type :unit-test)
        (add-to-list 'item (cons "Unit Test" :unit-test)))
      (unless (eq type :rspec-model)
        (add-to-list 'item (cons "RSpec" :rspec-model)))
      (unless (eq type :model)
        (add-to-list 'item (cons "Model" :model))))
    (when mailer
        (setq item (rails-controller-layout:views-menu model))
        (add-to-list 'item (rails-core:menu-separator))
        (add-to-list 'item (cons "Mailer" :mailer)))
    (when item
      (setq item
            (rails-core:menu
             (list (concat title " " model)
                   (cons "Please select.."
                         item))))
      (typecase item
        (symbol (rails-model-layout:switch-to item))
        (string (rails-core:find-file-if-exist item))))))

(provide 'rails-model-layout)
