(define-minor-mode rails-rspec-minor-mode
  "Minor mode for RubyOnRails RSpec."
  :lighter " RSpec"
  :keymap (let ((map (rails-model-layout:keymap :rspec)))
            ; (define-key map rails-minor-mode-test-current-method-key 'rails-test:run-current-method)
            ; (define-key map [menu-bar rails-model-layout run] '("Test current method" . rails-test:run-current-method))
            map)
  (setq rails-primary-switch-func (lambda()
                                    (interactive)
                                    (if (rails-core:mailer-p (rails-core:current-model))
                                        (rails-model-layout:switch-to-mailer)
                                      (rails-model-layout:switch-to-model))))
  (setq rails-secondary-switch-func 'rails-model-layout:menu))

(provide 'rails-rspec-minor-mode)