(defun rails-lib-layout:keymap (type)
  (let* ((name (capitalize (substring (symbol-name type) 1)))
         (map (make-sparse-keymap))
         (menu (make-sparse-keymap)))
    (when type
      (define-keys menu
        ([goto-lib]      '(menu-item "Go to Lib"
                                     rails-lib-layout:switch-to-lib
                                     :enable (and (not (eq (rails-core:buffer-type) :lib))
                                                  (rails-core:lib-exist-p (rails-core:current-lib)))))
        ([goto-rspec]    '(menu-item "Go to RSpec"
                                     rails-lib-layout:switch-to-rspec-lib
                                     :enable (and (not (eq (rails-core:buffer-type) :rspec-lib))
                                                  (rails-core:rspec-lib-exist-p (rails-core:current-lib))))))
      (define-keys map
        ((rails-local-key "l")         'rails-lib-layout:switch-to-lib)
        ((rails-local-key "r")         'rails-lib-layout:switch-to-unit-test)
        ([menu-bar rails-lib-layout] (cons name menu))))
    map))

(defun rails-lib-layout:switch-to (type)
  (let* ((name (capitalize (substring (symbol-name type) 1)))
         (lib (rails-core:current-lib))
         (item (if lib lib))
         (item (case type
                 (:rspec-lib (rails-core:rspec-lib-file item))
                 (:lib (rails-core:lib-file lib)))))
    (if item
        (let ((file (rails-core:file item)))
          (if (file-exists-p file)
              (progn
                (find-file file)
                (message (format "%s: %s" (substring (symbol-name type) 1) item)))
            (message "File %s not exists" file)))
      (message "%s not found" name))))

(defun rails-lib-layout:switch-to-rspec-lib () (interactive) (rails-lib-layout:switch-to :rspec-lib))
(defun rails-lib-layout:switch-to-lib () (interactive) (rails-lib-layout:switch-to :lib))

(defun rails-lib-layout:menu ()
  (interactive)
  (let* ((item (list))
         (type (rails-core:buffer-type))
         (title (capitalize (substring (symbol-name type) 1)))
         (lib (rails-core:current-lib)))
    (when lib
      (unless (eq type :rspec-lib)
        (add-to-list 'item (cons "RSpec" :rspec-lib)))
      (unless (eq type :lib)
        (add-to-list 'item (cons "Lib" :lib))))
    (when item
      (setq item
            (rails-core:menu
             (list (concat title " " lib)
                   (cons "Please select.."
                         item))))
      (typecase item
        (symbol (rails-lib-layout:switch-to item))
        (string (rails-core:find-file-if-exist item))))))

(provide 'rails-lib-layout)
